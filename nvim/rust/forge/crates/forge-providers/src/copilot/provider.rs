use super::handle::CopilotSession;
use super::runtime::CopilotRuntime;
use super::sessions::{CopilotSessions, PreparedSession, SessionReuse, SessionSelection};
use super::turn::CopilotTurnEvent;
use async_trait::async_trait;
use forge_provider_api::catalog::{McpQuery, QueryContext, WorkspaceContext};
use forge_provider_api::control::{ProviderTools, ToolCall};
use forge_provider_api::events::{self, ProviderEventSink, ProviderEventStream};
use forge_provider_api::generation::ReasoningSelection;
use forge_provider_api::permissions::{PermissionRequest, PermissionResponse, ProviderPermissions};
use forge_provider_api::session::{
    BindingId, InputAcceptance, ModelSelection, PromptRequest, RecoveryToken, ResourceLease,
    SessionBinding, SessionMode, SessionRequest, SteeringAcceptance, SteeringRequest,
};
use forge_provider_api::turn_control::{HostTurnStatus, TurnControl};
use forge_provider_api::{
    CancelRequest, CompactionResult, ConfiguredSession, ForkSource, InputReconcileRequest,
    InputRetractOutcome, InputRetractRequest, InvocationSettlement, OpenedSession,
    OperationContext, SessionConfiguration, SessionForkRequest, SessionHistory,
};
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

/// Keep every operation payload and result typed while enforcing one provider-wide identity space.
struct Operations {
    identities: Arc<forge_provider_api::retained_operation::IdentityRegistry>,
    open: forge_provider_api::retained_operation::Store<
        forge_provider_api::SessionRequestIdentity,
        OpenedSession,
    >,
    configure: forge_provider_api::retained_operation::Store<
        (BindingId, SessionConfiguration),
        ConfiguredSession,
    >,
    fork: forge_provider_api::retained_operation::Store<
        SessionForkRequest,
        forge_provider_api::ProviderForkResult,
    >,
    compact: forge_provider_api::retained_operation::Store<
        (BindingId, forge_provider_api::CompactionRequest),
        CompactionResult,
    >,
    close: forge_provider_api::retained_operation::Store<BindingId, ()>,
    cancel: forge_provider_api::retained_operation::Store<
        (BindingId, CancelRequest),
        InvocationSettlement,
    >,
    submit: forge_provider_api::retained_operation::Store<
        (BindingId, forge_provider_api::PromptRequestIdentity),
        InputAcceptance,
    >,
    steer: forge_provider_api::retained_operation::Store<
        (BindingId, forge_provider_api::SteeringRequestIdentity),
        SteeringAcceptance,
    >,
    reconcile:
        forge_provider_api::retained_operation::Store<InputReconcileRequest, SubmissionEvidence>,
    retract: forge_provider_api::retained_operation::Store<
        (BindingId, InputRetractRequest),
        InputRetractOutcome,
    >,
    inspect: forge_provider_api::retained_operation::Store<
        WorkspaceContext,
        forge_provider_api::ProviderSetup,
    >,
    authenticate: forge_provider_api::retained_operation::Store<
        (WorkspaceContext, forge_provider_api::AuthenticationMethodId),
        (),
    >,
    shutdown: forge_provider_api::retained_operation::Store<(), ()>,
    models: forge_provider_api::retained_operation::Store<
        QueryContext,
        Vec<forge_provider_api::ProviderModel>,
    >,
    skills: forge_provider_api::retained_operation::Store<
        QueryContext,
        Vec<forge_provider_api::catalog::SkillDefinition>,
    >,
    mcp: forge_provider_api::retained_operation::Store<
        McpQuery,
        Vec<forge_provider_api::catalog::McpDefinition>,
    >,
    terminals: forge_provider_api::retained_operation::Store<
        BindingId,
        forge_provider_api::terminal::TerminalSnapshot,
    >,
    terminate: forge_provider_api::retained_operation::Store<
        (BindingId, forge_provider_api::terminal::TerminalId),
        (),
    >,
}

impl Operations {
    /// Create the provider-wide identity ledger before admitting native work.
    fn new() -> Self {
        let identities = Arc::new(forge_provider_api::retained_operation::IdentityRegistry::new());
        Self {
            open: forge_provider_api::retained_operation::Store::registered(identities.clone()),
            configure: forge_provider_api::retained_operation::Store::registered(
                identities.clone(),
            ),
            fork: forge_provider_api::retained_operation::Store::registered(identities.clone()),
            compact: forge_provider_api::retained_operation::Store::registered(identities.clone()),
            close: forge_provider_api::retained_operation::Store::registered(identities.clone()),
            cancel: forge_provider_api::retained_operation::Store::registered(identities.clone()),
            submit: forge_provider_api::retained_operation::Store::registered(identities.clone()),
            steer: forge_provider_api::retained_operation::Store::registered(identities.clone()),
            reconcile: forge_provider_api::retained_operation::Store::registered(
                identities.clone(),
            ),
            retract: forge_provider_api::retained_operation::Store::registered(identities.clone()),
            inspect: forge_provider_api::retained_operation::Store::registered(identities.clone()),
            authenticate: forge_provider_api::retained_operation::Store::registered(
                identities.clone(),
            ),
            shutdown: forge_provider_api::retained_operation::Store::registered(identities.clone()),
            models: forge_provider_api::retained_operation::Store::registered(identities.clone()),
            skills: forge_provider_api::retained_operation::Store::registered(identities.clone()),
            mcp: forge_provider_api::retained_operation::Store::registered(identities.clone()),
            terminals: forge_provider_api::retained_operation::Store::registered(
                identities.clone(),
            ),
            terminate: forge_provider_api::retained_operation::Store::registered(
                identities.clone(),
            ),
            identities,
        }
    }

    /// Join retained operations after native producers have stopped, excluding the shutdown owner.
    async fn collect(
        &self,
        excluded: &forge_provider_api::OperationId,
    ) -> Result<(), ProviderError> {
        let mut failures = Vec::new();
        if let Err(error) = self.open.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.configure.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.fork.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.compact.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.close.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.cancel.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.submit.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.steer.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.reconcile.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.retract.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.inspect.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.authenticate.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.models.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.skills.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.mcp.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.terminals.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.terminate.join_all_except(excluded).await {
            failures.push(error);
        }
        if failures.is_empty() {
            return Ok(());
        }
        let primary = failures.remove(0);
        Err(if failures.is_empty() {
            primary
        } else {
            ProviderError::Combined {
                primary: Box::new(primary),
                secondary: failures,
            }
        })
    }
}

/// Keep interruption cleanup scoped to the resource actually mutated by the operation.
#[derive(Clone)]
enum InterruptedResource {
    Observe,
    RetainNative,
    Binding(BindingId),
    Invocation {
        binding: BindingId,
        scope: forge_provider_api::InvocationScope,
    },
    Opening {
        owner: forge_provider_api::ExecutionOwner,
        operation: forge_provider_api::OperationId,
    },
}

type ProfileSlot = Arc<std::sync::Mutex<ProfileContents>>;

struct ProfileContents {
    profile: Option<crate::profile::ResourceProfile>,
    lease: Option<Arc<dyn ResourceLease>>,
}

struct RetainedResources {
    _profile: RetainedProfile,
}

/// Keep the allocation registered even when native preparation or implicit cleanup fails.
struct RetainedProfile {
    slot: ProfileSlot,
}

impl RetainedProfile {
    /// Get the live private layout without exposing a lock guard to native callers.
    fn path(&self) -> PathBuf {
        self.slot
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .profile
            .as_ref()
            .expect("native profile remains live until explicit collection")
            .path()
            .to_owned()
    }

    /// Collect the allocation while preserving a failed native profile for another attempt.
    fn close(&self, operation: ProviderOperation) -> Result<(), ProviderError> {
        close_profile_slot(&self.slot, operation)
    }
}

impl Drop for RetainedProfile {
    /// Leave failed cleanup in the provider registry instead of invoking an unowned keep fallback.
    fn drop(&mut self) {
        let _ = self.close(ProviderOperation::SessionClose);
    }
}

/// Collect only slots whose native leases have ended, leaving active ownership intact.
fn close_profile_slot(
    slot: &ProfileSlot,
    operation: ProviderOperation,
) -> Result<(), ProviderError> {
    let mut retained = slot
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if let Some(profile) = retained.profile.as_mut() {
        profile
            .close()
            .map_err(|error| error.into_provider_error(operation))?;
    }
    retained.profile.take();
    retained.lease.take();
    Ok(())
}

#[derive(Clone)]
struct InvocationCallbacks {
    scope: forge_provider_api::InvocationScope,
    generation: u64,
    permissions: Arc<dyn ProviderPermissions>,
    tools: Arc<dyn ProviderTools>,
    control: Arc<dyn TurnControl>,
    allowed_tools: HashSet<String>,
    _resource_lease: Option<Arc<dyn ResourceLease>>,
    session_directory: Option<super::session_directory::SessionDirectory>,
}

impl InvocationCallbacks {
    fn authorize_tool(&self, call: &ToolCall) -> Result<(), ProviderError> {
        if self.allowed_tools.contains(&call.name) {
            return Ok(());
        }
        Err(failure(
            ProviderOperation::InputSubmit,
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
    revoked: std::sync::Mutex<CallbackAdmission>,
    running_callbacks: AtomicU64,
    callbacks_changed: tokio::sync::Notify,
    provider_closed: Option<Arc<std::sync::atomic::AtomicBool>>,
}

#[derive(Default)]
enum CallbackAdmission {
    #[default]
    Open,
    Settled,
    Closed,
}

/// Retain one admitted native callback through asynchronous permission and effect work.
struct NativeCallbackGuard<'a>(&'a CallbackRouter);

impl Drop for NativeCallbackGuard<'_> {
    fn drop(&mut self) {
        self.0.running_callbacks.fetch_sub(1, Ordering::AcqRel);
        self.0.callbacks_changed.notify_waiters();
    }
}

impl CallbackRouter {
    /// Share provider-wide revocation across all bindings before shutdown can await one of them.
    fn new(provider_closed: Arc<std::sync::atomic::AtomicBool>) -> Self {
        Self {
            provider_closed: Some(provider_closed),
            ..Self::default()
        }
    }

    /// Admit a callback atomically with revocation so close can collect already admitted work.
    fn begin_callback(&self) -> Result<NativeCallbackGuard<'_>, ProviderError> {
        let revoked = self
            .revoked
            .lock()
            .map_err(|_| failure(ProviderOperation::InputSubmit, ProviderFailureKind::Runtime))?;
        if self
            .provider_closed
            .as_ref()
            .is_some_and(|closed| closed.load(Ordering::Acquire))
            || !matches!(*revoked, CallbackAdmission::Open)
        {
            return Err(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::BindingClosed,
            ));
        }
        self.running_callbacks.fetch_add(1, Ordering::AcqRel);
        Ok(NativeCallbackGuard(self))
    }

    /// Revoke native effect admission without waiting for network or callback completion.
    fn revoke(&self) {
        let mut admission = self
            .revoked
            .lock()
            .expect("callback admission lock is not poisoned");
        if !matches!(*admission, CallbackAdmission::Closed) {
            *admission = CallbackAdmission::Settled;
        }
    }

    /// Permanently revoke a binding so no late invocation installation can restore admission.
    fn close_admission(&self) {
        *self
            .revoked
            .lock()
            .expect("callback admission lock is not poisoned") = CallbackAdmission::Closed;
    }

    /// Check authority again after an asynchronous permission barrier.
    fn permits_effect(&self) -> bool {
        !self
            .provider_closed
            .as_ref()
            .is_some_and(|closed| closed.load(Ordering::Acquire))
            && matches!(
                *self
                    .revoked
                    .lock()
                    .expect("callback admission lock is not poisoned"),
                CallbackAdmission::Open
            )
    }

    /// Await callbacks already admitted before revocation while retaining their worker owners.
    async fn collect_callbacks(&self) {
        loop {
            let changed = self.callbacks_changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            if self.running_callbacks.load(Ordering::Acquire) == 0 {
                return;
            }
            changed.await;
        }
    }

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
        {
            let mut admission = self
                .revoked
                .lock()
                .expect("callback admission lock is not poisoned");
            if self
                .provider_closed
                .as_ref()
                .is_some_and(|closed| closed.load(Ordering::Acquire))
                || matches!(*admission, CallbackAdmission::Closed)
            {
                return Err(failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::BindingClosed,
                ));
            }
            *admission = CallbackAdmission::Open;
        }
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
            .ok_or_else(|| {
                failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::WrongInvocation,
                )
            })
    }

    async fn retire_all(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let _ = binding;
        self.revoke();
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
            .ok_or_else(|| {
                failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::Rejected,
                )
            })
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
impl NativeCallbackRouter for CallbackRouter {
    async fn call_native(
        &self,
        call_id: &str,
        mut call: ToolCall,
    ) -> Result<String, ProviderError> {
        let _callback = self.begin_callback()?;
        call.id = forge_provider_api::control::ToolCallId::new(call_id).map_err(|_| {
            failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Protocol,
            )
        })?;
        let callbacks = self.callbacks_for_native_call(call_id).await?;
        callbacks.authorize_tool(&call)?;
        callbacks.permissions.before_tool(&callbacks.scope).await?;
        if !self.permits_effect() {
            return Err(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::BindingClosed,
            ));
        }
        callbacks.tools.call(&callbacks.scope, call).await
    }

    async fn authorize_native(
        &self,
        call_id: &str,
        mut request: PermissionRequest,
    ) -> PermissionResponse {
        let Ok(_callback) = self.begin_callback() else {
            return PermissionResponse::Cancelled;
        };
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
        let response = match self.callbacks_for_native_call(call_id).await {
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
        };
        if self.permits_effect() {
            response
        } else {
            PermissionResponse::Cancelled
        }
    }

    async fn before_tool_native(&self, call_id: Option<&str>) -> Result<(), ProviderError> {
        let _callback = self.begin_callback()?;
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
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Rejected,
            ));
        }
        for callbacks in callbacks {
            callbacks.permissions.before_tool(&callbacks.scope).await?;
        }
        if !self.permits_effect() {
            return Err(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::BindingClosed,
            ));
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
    open_operation: forge_provider_api::OperationId,
    owner: forge_provider_api::ExecutionOwner,
    runtime_key: String,
    session: RwLock<Arc<CopilotSession>>,
    request: Mutex<SessionRequest>,
    sink: ProviderEventSink,
    stream: Mutex<Option<ProviderEventStream>>,
    turn: Arc<CopilotTurnEvent>,
    callbacks: Arc<CallbackRouter>,
    admission: Mutex<()>,
    active: Mutex<Option<ActiveExecution>>,
    closing: std::sync::atomic::AtomicBool,
    unsafe_history: std::sync::atomic::AtomicBool,
    settlements: Mutex<
        HashMap<forge_provider_api::InvocationScope, Result<InvocationSettlement, ProviderError>>,
    >,
    cancellations: Mutex<
        HashMap<forge_provider_api::InvocationScope, Result<InvocationSettlement, ProviderError>>,
    >,
    retained_resources: Mutex<Option<Arc<RetainedResources>>>,
    retired_resources: Mutex<Vec<Arc<RetainedResources>>>,
    settled: tokio::sync::Notify,
    retractions: Mutex<HashMap<forge_provider_api::OperationId, RetainedRetraction>>,
    retracted_inputs:
        Mutex<HashMap<forge_provider_api::SubmissionId, forge_provider_api::OperationId>>,
    inputs: Mutex<
        HashMap<
            forge_provider_api::SubmissionId,
            (
                forge_provider_api::InvocationScope,
                forge_provider_api::DeliveryAttemptId,
            ),
        >,
    >,
    unconsumed: Mutex<HashSet<forge_provider_api::SubmissionId>>,
    close_failure: Mutex<Option<ProviderError>>,
    compactions:
        Mutex<HashMap<forge_provider_api::OperationId, Result<CompactionResult, ProviderError>>>,
    terminals: Mutex<super::terminal::TerminalRegistry>,
    terminals_collected: std::sync::atomic::AtomicBool,
    native_retired: std::sync::atomic::AtomicBool,
    history_deleted: std::sync::atomic::AtomicBool,
}

struct RetainedFork {
    context: OperationContext,
    request: SessionForkRequest,
    result: Result<forge_provider_api::ProviderForkResult, ProviderError>,
}

struct RetainedRetraction {
    context: OperationContext,
    request: InputRetractRequest,
    outcome: InputRetractOutcome,
}

impl RetainedRetraction {
    /// Replay only the entire original authorization, including the retained operation identity.
    fn replay(
        &self,
        context: &OperationContext,
        request: &InputRetractRequest,
    ) -> Result<InputRetractOutcome, ProviderError> {
        if self.context != *context || self.request != *request {
            return Err(failure(
                ProviderOperation::InputRetract,
                ProviderFailureKind::HistoryConflict,
            ));
        }
        Ok(self.outcome.clone())
    }
}

struct ActiveExecution {
    scope: forge_provider_api::InvocationScope,
    task: tokio::task::JoinHandle<Result<(), ProviderError>>,
}

impl BindingState {
    /// Publish an invocation receipt through the retained binding stream at its exact scope.
    async fn publish_invocation_settlement(
        &self,
        settlement: InvocationSettlement,
    ) -> Result<InvocationSettlement, ProviderError> {
        self.sink
            .invocation_sink(&settlement.scope)?
            .send_settlement(settlement)
            .await
            .map_err(Into::into)
    }

    /// Keep failed collection scoped to the invocation without poisoning the binding stream.
    async fn publish_invocation_settlement_failure(
        &self,
        scope: &forge_provider_api::InvocationScope,
        failure: ProviderFailure,
    ) -> Result<(), ProviderError> {
        self.sink
            .invocation_sink(scope)?
            .send_wait(forge_provider_api::ProviderEvent::SettlementFailed {
                scope: scope.clone(),
                failure,
            })
            .await
            .map_err(Into::into)
    }

    /// Fence a committed configuration whose confirmation or original operation authority was lost.
    fn invalidate_configuration(&self, fallback: ProviderFailureKind) -> ProviderError {
        self.closing.store(true, Ordering::Release);
        self.callbacks.close_admission();
        let kind = super::operation::current()
            .and_then(|operation| {
                operation
                    .context
                    .check_admission(ProviderOperation::SessionConfigure)
                    .err()
            })
            .map(|cause| super::error::trace_failure_kind(&cause))
            .unwrap_or(fallback);
        ProviderError::Failure(
            ProviderFailure::new(ProviderOperation::SessionConfigure, kind).at_phase(
                forge_provider_api::ProviderPhase::Observation,
                forge_provider_api::FailureDisposition::Invalidated,
            ),
        )
    }

    /// Recheck after each final await without reporting committed native writes as unchanged.
    fn confirm_configuration_health(&self, provider_closed: bool) -> Result<(), ProviderError> {
        if provider_closed
            || self.closing.load(Ordering::Acquire)
            || super::operation::current().is_some_and(|operation| {
                operation
                    .context
                    .check_admission(ProviderOperation::SessionConfigure)
                    .is_err()
            })
        {
            return Err(self.invalidate_configuration(ProviderFailureKind::BindingClosed));
        }
        Ok(())
    }

    /// Retry only this binding's replaced layouts, retaining each failed profile under its owner.
    async fn collect_retired_profiles(
        &self,
        operation: ProviderOperation,
    ) -> Result<(), ProviderError> {
        let mut retired = self.retired_resources.lock().await;
        let mut failures = Vec::new();
        retired.retain(|resources| match resources._profile.close(operation) {
            Ok(()) => false,
            Err(error) => {
                failures.push(error);
                true
            }
        });
        if failures.is_empty() {
            return Ok(());
        }
        let primary = failures.remove(0);
        Err(if failures.is_empty() {
            primary
        } else {
            ProviderError::Combined {
                primary: Box::new(primary),
                secondary: failures,
            }
        })
    }

    /// Fence native handoff and stop this already selected execution without consuming settlement.
    async fn interrupt(&self) -> Result<(), ProviderError> {
        self.turn
            .request_stop()
            .map_err(|_| failure(ProviderOperation::Cancel, ProviderFailureKind::Runtime))?;
        if !self.turn.is_active() {
            return Ok(());
        }
        let session = self.session.read().await.clone();
        tokio::time::timeout(std::time::Duration::from_secs(10), async {
            // Stop and writer admission share one state lock; every admitted frame
            // precedes abort, while handoffs attempted after stop are rejected.
            let abort = session.abort().await;
            let pending = self.turn.finish_pending_sends().await;
            match (pending, abort) {
                (Ok(()), result) | (result, Ok(())) => result,
                (Err(primary), Err(secondary)) => Err(ProviderError::Combined {
                    primary: Box::new(primary),
                    secondary: vec![secondary],
                }),
            }
        })
        .await
        .unwrap_or_else(|_| {
            Err(failure(
                ProviderOperation::Cancel,
                ProviderFailureKind::Timeout,
            ))
        })
    }

    /// Join native execution while retaining ownership if the waiter is cancelled or times out.
    async fn collect_execution(&self) -> Result<Option<Result<(), ProviderError>>, ProviderError> {
        let mut active = self.active.lock().await;
        let Some(task) = active.as_mut() else {
            return Ok(None);
        };
        let result = match tokio::time::timeout(std::time::Duration::from_secs(30), &mut task.task)
            .await
        {
            Ok(result) => result,
            Err(_) => {
                let failure =
                    ProviderFailure::new(ProviderOperation::Cancel, ProviderFailureKind::Timeout)
                        .at_phase(
                            forge_provider_api::ProviderPhase::Collection,
                            forge_provider_api::FailureDisposition::Invalidated,
                        );
                self.closing.store(true, Ordering::Release);
                self.callbacks.revoke();
                let _ = self
                    .publish_invocation_settlement_failure(&task.scope, failure)
                    .await;
                return Err(ProviderError::Failure(failure));
            }
        };
        active.take();
        result
            .map(Some)
            .map_err(|_| failure(ProviderOperation::Cancel, ProviderFailureKind::Runtime))
    }
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
            ProviderOperation::InputSubmit,
            ProviderFailureKind::Rejected,
        ))
    }
    async fn authorize_native(&self, _: &str, _: PermissionRequest) -> PermissionResponse {
        PermissionResponse::Cancelled
    }
    async fn before_tool_native(&self, _: Option<&str>) -> Result<(), ProviderError> {
        Err(failure(
            ProviderOperation::InputSubmit,
            ProviderFailureKind::Rejected,
        ))
    }
    async fn bind_native_call(&self, _: &str, _: &forge_provider_api::InvocationScope) {}
    async fn release_native_call(&self, _: &str) {}
}

/// Own Copilot SDK sessions, callback routers, and normalized event streams.
#[derive(Clone)]
pub struct CopilotProvider {
    runtime: Arc<CopilotRuntime>,
    sessions: Arc<CopilotSessions<RetainedResources>>,
    runtime_root: PathBuf,
    profiles: Arc<std::sync::Mutex<Vec<ProfileSlot>>>,
    bindings: Arc<Mutex<HashMap<BindingId, Arc<BindingState>>>>,
    owners: Arc<Mutex<HashMap<forge_provider_api::ExecutionOwner, BindingId>>>,
    initialization_lock: Arc<Mutex<()>>,
    shutdown_lock: Arc<Mutex<()>>,
    operations: Arc<Operations>,
    pending_inputs: Arc<crate::pending_input::PendingInputs>,
    closed_bindings: Arc<std::sync::Mutex<HashSet<BindingId>>>,
    closed: Arc<std::sync::atomic::AtomicBool>,
    forks: Arc<Mutex<HashMap<forge_provider_api::OperationId, RetainedFork>>>,
}

impl CopilotProvider {
    /// Create the native owner with its private runtime resource root.
    pub fn new(command: Vec<String>, runtime_root: PathBuf) -> Result<Self, ProviderError> {
        let trace: Arc<dyn forge_provider_api::trace::ProviderTrace> = Arc::new(NullTrace);
        Ok(Self {
            runtime: Arc::new(CopilotRuntime::new(command)),
            sessions: Arc::new(CopilotSessions::new(trace)),
            runtime_root,
            profiles: Arc::new(std::sync::Mutex::new(Vec::new())),
            bindings: Arc::new(Mutex::new(HashMap::new())),
            owners: Arc::new(Mutex::new(HashMap::new())),
            initialization_lock: Arc::new(Mutex::new(())),
            shutdown_lock: Arc::new(Mutex::new(())),
            operations: Arc::new(Operations::new()),
            pending_inputs: Arc::new(crate::pending_input::PendingInputs::new()),
            closed_bindings: Arc::new(std::sync::Mutex::new(HashSet::new())),
            closed: Arc::new(std::sync::atomic::AtomicBool::new(false)),
            forks: Arc::new(Mutex::new(HashMap::new())),
        })
    }

    async fn binding(
        &self,
        id: &BindingId,
        operation: ProviderOperation,
    ) -> Result<Arc<BindingState>, ProviderError> {
        let state = self.bindings.lock().await.get(id).cloned();
        if self
            .closed_bindings
            .lock()
            .expect("closed binding registry is not poisoned")
            .contains(id)
        {
            return Err(ProviderError::Failure(
                ProviderFailure::new(operation, ProviderFailureKind::BindingClosed).at_phase(
                    forge_provider_api::ProviderPhase::Admission,
                    forge_provider_api::FailureDisposition::Closed,
                ),
            ));
        }
        state.ok_or_else(|| failure(operation, ProviderFailureKind::SessionMissing))
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
                drop(self.pending_inputs.fence_binding(&state.binding.id));
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
            // The failed native RPC generation is joined before waiting for preparation handoff.
            tokio::time::timeout(
                std::time::Duration::from_secs(30),
                self.pending_inputs.cancel_binding(&state.binding.id),
            )
            .await
            .map_err(|_| {
                ProviderError::Failure(
                    ProviderFailure::new(
                        ProviderOperation::SessionClose,
                        ProviderFailureKind::Timeout,
                    )
                    .at_phase(
                        forge_provider_api::ProviderPhase::Collection,
                        forge_provider_api::FailureDisposition::Invalidated,
                    ),
                )
            })?;
            // The observer already published terminal failure; only collection itself can block recovery.
            let _terminal = state.collect_execution().await?;
        }
        self.runtime.finish_recovery(sessions).await?;
        for state in bindings {
            self.closed_bindings
                .lock()
                .expect("closed binding registry is not poisoned")
                .insert(state.binding.id.clone());
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
    async fn session_open(
        &self,
        context: OperationContext,
        request: SessionRequest,
    ) -> Result<OpenedSession, ProviderError> {
        let admission =
            super::operation::Admission::new(context.clone(), ProviderOperation::SessionOpen);
        let resource = InterruptedResource::Opening {
            owner: request.owner.clone(),
            operation: context.id().clone(),
        };
        let provider = self.clone();
        let cleanup = self.clone();
        let boundary = admission.clone();
        let payload = request.identity();
        let (finished, mut completed) = tokio::sync::watch::channel(false);
        let work = async move {
            let result = admission
                .scope(async move { provider.open_binding(request).await })
                .await;
            finished.send_replace(true);
            result
        };
        self.operations
            .open
            .take(
                context,
                ProviderOperation::SessionOpen,
                payload,
                work,
                move |reason| {
                    let dispatched = boundary.dispatched();
                    let observation = reason.failure(
                        ProviderOperation::SessionOpen,
                        if dispatched {
                            forge_provider_api::ProviderPhase::Observation
                        } else {
                            forge_provider_api::ProviderPhase::Admission
                        },
                        if dispatched {
                            forge_provider_api::FailureDisposition::Uncertain
                        } else {
                            forge_provider_api::FailureDisposition::Unchanged
                        },
                    );
                    forge_provider_api::retained_operation::Interruption::new(
                        observation,
                        async move {
                            if !dispatched {
                                return Ok(());
                            }
                            loop {
                                let done = *completed.borrow_and_update();
                                if done {
                                    break;
                                }
                                if completed.changed().await.is_err() {
                                    break;
                                }
                            }
                            cleanup.cleanup_interrupted(resource).await
                        },
                    )
                },
            )
            .await
    }

    async fn session_configure(
        &self,
        context: OperationContext,
        binding: &BindingId,
        configuration: SessionConfiguration,
    ) -> Result<ConfiguredSession, ProviderError> {
        let resource = InterruptedResource::Binding(binding.clone());
        let payload = (binding.clone(), configuration.clone());
        let provider = self.clone();
        let binding = binding.clone();
        self.observe(
            &self.operations.configure,
            context,
            ProviderOperation::SessionConfigure,
            payload,
            resource,
            async move { provider.configure_binding(&binding, configuration).await },
        )
        .await
    }

    async fn session_fork(
        &self,
        context: OperationContext,
        request: SessionForkRequest,
    ) -> Result<forge_provider_api::ProviderForkResult, ProviderError> {
        let resource = InterruptedResource::RetainNative;
        let payload = request.clone();
        let provider = self.clone();
        self.observe(
            &self.operations.fork,
            context,
            ProviderOperation::SessionFork,
            payload,
            resource,
            async move { provider.fork_history(request).await },
        )
        .await
    }

    async fn session_compact(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: forge_provider_api::CompactionRequest,
    ) -> Result<CompactionResult, ProviderError> {
        let resource = InterruptedResource::Binding(binding.clone());
        let payload = (binding.clone(), request.clone());
        let provider = self.clone();
        let binding = binding.clone();
        self.observe(
            &self.operations.compact,
            context,
            ProviderOperation::SessionCompact,
            payload,
            resource,
            async move { provider.compact_history(&binding, request).await },
        )
        .await
    }

    async fn session_close(
        &self,
        context: OperationContext,
        binding: &BindingId,
    ) -> Result<(), ProviderError> {
        let resource = InterruptedResource::RetainNative;
        let payload = binding.clone();
        let provider = self.clone();
        let binding = binding.clone();
        self.observe(
            &self.operations.close,
            context,
            ProviderOperation::SessionClose,
            payload,
            resource,
            async move {
                {
                    super::operation::dispatch()?;
                    provider.close_binding(&binding).await
                }
            },
        )
        .await
    }

    async fn cancel(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: CancelRequest,
    ) -> Result<InvocationSettlement, ProviderError> {
        let resource = InterruptedResource::RetainNative;
        let payload = (binding.clone(), request.clone());
        let provider = self.clone();
        let binding = binding.clone();
        self.observe(
            &self.operations.cancel,
            context,
            ProviderOperation::Cancel,
            payload,
            resource,
            async move { provider.interrupt_invocation(&binding, request).await },
        )
        .await
    }

    async fn input_submit(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: PromptRequest,
    ) -> Result<InputAcceptance, ProviderError> {
        let resource = InterruptedResource::Invocation {
            binding: binding.clone(),
            scope: request.scope.clone(),
        };
        // Own the queued candidate before the retained Store can spawn or discard its work future.
        let pending = self
            .pending_inputs
            .register(binding, &request.scope, context.id());
        let payload = (binding.clone(), request.identity());
        let provider = self.clone();
        let binding = binding.clone();
        self.observe(
            &self.operations.submit,
            context,
            ProviderOperation::InputSubmit,
            payload,
            resource,
            async move { provider.submit(&binding, request, &pending).await },
        )
        .await
    }

    async fn input_steer(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<SteeringAcceptance, ProviderError> {
        let resource = InterruptedResource::Invocation {
            binding: binding.clone(),
            scope: request.scope.clone(),
        };
        let payload = (binding.clone(), request.identity());
        let provider = self.clone();
        let binding = binding.clone();
        self.observe(
            &self.operations.steer,
            context,
            ProviderOperation::InputSteer,
            payload,
            resource,
            async move { provider.steer_input(&binding, request).await },
        )
        .await
    }

    async fn input_reconcile(
        &self,
        context: OperationContext,
        request: InputReconcileRequest,
    ) -> Result<SubmissionEvidence, ProviderError> {
        let resource = InterruptedResource::Observe;
        let payload = request.clone();
        let provider = self.clone();
        self.observe(
            &self.operations.reconcile,
            context,
            ProviderOperation::InputReconcile,
            payload,
            resource,
            async move { provider.reconcile_input(request).await },
        )
        .await
    }

    async fn input_retract(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: InputRetractRequest,
    ) -> Result<InputRetractOutcome, ProviderError> {
        let resource = InterruptedResource::Binding(binding.clone());
        let payload = (binding.clone(), request.clone());
        let provider = self.clone();
        let binding = binding.clone();
        self.observe(
            &self.operations.retract,
            context,
            ProviderOperation::InputRetract,
            payload,
            resource,
            async move { provider.retract_input(&binding, request).await },
        )
        .await
    }

    async fn inspect(
        &self,
        context: OperationContext,
        workspace: WorkspaceContext,
    ) -> Result<forge_provider_api::ProviderSetup, ProviderError> {
        let resource = InterruptedResource::Observe;
        let payload = workspace.clone();
        let provider = self.clone();
        self.observe(
            &self.operations.inspect,
            context,
            ProviderOperation::Inspect,
            payload,
            resource,
            async move { provider.inspect_runtime(workspace).await },
        )
        .await
    }

    async fn authenticate(
        &self,
        context: OperationContext,
        workspace: WorkspaceContext,
        method: forge_provider_api::AuthenticationMethodId,
    ) -> Result<(), ProviderError> {
        let resource = InterruptedResource::Observe;
        let payload = (workspace.clone(), method.clone());

        self.observe(
            &self.operations.authenticate,
            context,
            ProviderOperation::Authenticate,
            payload,
            resource,
            async move {
                Err(forge_provider_api::capability::UnsupportedCapability(
                    forge_provider_api::capability::ProviderFeature::Authentication,
                )
                .into())
            },
        )
        .await
    }

    async fn shutdown(&self, context: OperationContext) -> Result<(), ProviderError> {
        let resource = InterruptedResource::RetainNative;
        let payload = ();
        let provider = self.clone();
        self.observe(
            &self.operations.shutdown,
            context,
            ProviderOperation::Shutdown,
            payload,
            resource,
            async move {
                {
                    super::operation::dispatch()?;
                    provider.shutdown_native().await
                }
            },
        )
        .await
    }

    async fn model_list(
        &self,
        context: OperationContext,
        query: QueryContext,
    ) -> Result<Vec<forge_provider_api::ProviderModel>, ProviderError> {
        let resource = InterruptedResource::Observe;
        let payload = query.clone();
        let provider = self.clone();
        self.observe(
            &self.operations.models,
            context,
            ProviderOperation::ModelList,
            payload,
            resource,
            async move { provider.models(query).await },
        )
        .await
    }

    async fn skill_list(
        &self,
        context: OperationContext,
        query: QueryContext,
    ) -> Result<Vec<forge_provider_api::catalog::SkillDefinition>, ProviderError> {
        let resource = InterruptedResource::Observe;
        let payload = query.clone();
        let provider = self.clone();
        self.observe(
            &self.operations.skills,
            context,
            ProviderOperation::SkillList,
            payload,
            resource,
            async move { provider.skills(query).await },
        )
        .await
    }

    async fn mcp_list(
        &self,
        context: OperationContext,
        query: McpQuery,
    ) -> Result<Vec<forge_provider_api::catalog::McpDefinition>, ProviderError> {
        let resource = InterruptedResource::Observe;
        let payload = query.clone();
        let provider = self.clone();
        self.observe(
            &self.operations.mcp,
            context,
            ProviderOperation::McpList,
            payload,
            resource,
            async move { provider.mcp(query).await },
        )
        .await
    }

    async fn terminal_list(
        &self,
        context: OperationContext,
        binding: &BindingId,
    ) -> Result<forge_provider_api::terminal::TerminalSnapshot, ProviderError> {
        let resource = InterruptedResource::Observe;
        let payload = binding.clone();
        let provider = self.clone();
        let binding = binding.clone();
        self.observe(
            &self.operations.terminals,
            context,
            ProviderOperation::TerminalList,
            payload,
            resource,
            async move { provider.terminals(&binding).await },
        )
        .await
    }

    async fn terminal_terminate(
        &self,
        context: OperationContext,
        binding: &BindingId,
        terminal: &forge_provider_api::terminal::TerminalId,
    ) -> Result<(), ProviderError> {
        let resource = InterruptedResource::RetainNative;
        let payload = (binding.clone(), terminal.clone());
        let provider = self.clone();
        let binding = binding.clone();
        let terminal = terminal.clone();
        self.observe(
            &self.operations.terminate,
            context,
            ProviderOperation::TerminalTerminate,
            payload,
            resource,
            async move { provider.terminate(&binding, &terminal).await },
        )
        .await
    }
}

impl CopilotProvider {
    /// Retain typed native work independently of observation deadlines and dropped awaiters.
    async fn observe<P, O, F>(
        &self,
        store: &forge_provider_api::retained_operation::Store<P, O>,
        context: OperationContext,
        operation: ProviderOperation,
        payload: P,
        resource: InterruptedResource,
        work: F,
    ) -> Result<O, ProviderError>
    where
        P: Eq + Send + Sync + 'static,
        O: Clone + Send + 'static,
        F: std::future::Future<Output = Result<O, ProviderError>> + Send + 'static,
    {
        let admission = super::operation::Admission::new(context.clone(), operation);
        let boundary = admission.clone();
        let provider = self.clone();
        let (finished, mut completed) = tokio::sync::watch::channel(false);
        let work_boundary = admission.clone();
        let work = async move {
            let result = admission.scope(work).await;
            let result = if work_boundary.dispatched()
                && matches!(
                    operation,
                    ProviderOperation::InputSubmit | ProviderOperation::InputSteer
                ) {
                result.map_err(|cause| super::error::after_input_dispatch(cause, operation))
            } else {
                result
            };
            finished.send_replace(true);
            result
        };
        store
            .observe(context, operation, payload, work, move |reason| {
                let dispatched = boundary.dispatched();
                if dispatched {
                    boundary.revoke();
                }
                let read_only = matches!(resource, InterruptedResource::Observe);
                let phase = if dispatched {
                    match operation {
                        ProviderOperation::SessionClose | ProviderOperation::Shutdown => {
                            forge_provider_api::ProviderPhase::Close
                        }
                        ProviderOperation::Cancel => forge_provider_api::ProviderPhase::Collection,
                        ProviderOperation::SessionOpen
                        | ProviderOperation::SessionConfigure
                        | ProviderOperation::SessionFork
                        | ProviderOperation::SessionCompact
                        | ProviderOperation::InputSubmit
                        | ProviderOperation::InputSteer
                        | ProviderOperation::InputReconcile
                        | ProviderOperation::InputRetract
                        | ProviderOperation::Inspect
                        | ProviderOperation::Authenticate
                        | ProviderOperation::ModelList
                        | ProviderOperation::SkillList
                        | ProviderOperation::McpList
                        | ProviderOperation::TerminalList
                        | ProviderOperation::TerminalTerminate => {
                            forge_provider_api::ProviderPhase::Observation
                        }
                    }
                } else if read_only {
                    forge_provider_api::ProviderPhase::Observation
                } else {
                    forge_provider_api::ProviderPhase::Admission
                };
                let disposition =
                    if dispatched && matches!(resource, InterruptedResource::Binding(_)) {
                        forge_provider_api::FailureDisposition::Invalidated
                    } else if dispatched {
                        forge_provider_api::FailureDisposition::Uncertain
                    } else {
                        forge_provider_api::FailureDisposition::Unchanged
                    };
                let observation = reason.failure(operation, phase, disposition);
                forge_provider_api::retained_operation::Interruption::new(observation, async move {
                    if !dispatched {
                        return Ok(());
                    }
                    if matches!(resource, InterruptedResource::Binding(_)) {
                        loop {
                            let done = *completed.borrow_and_update();
                            if done {
                                break;
                            }
                            if completed.changed().await.is_err() {
                                break;
                            }
                        }
                    }
                    provider.cleanup_interrupted(resource).await
                })
            })
            .await
    }

    /// Revoke only the resource whose native mutation crossed its dispatch boundary.
    async fn cleanup_interrupted(
        &self,
        resource: InterruptedResource,
    ) -> Result<(), ProviderError> {
        match resource {
            InterruptedResource::Observe | InterruptedResource::RetainNative => Ok(()),
            InterruptedResource::Binding(binding) => self.close_binding(&binding).await,
            InterruptedResource::Invocation { binding, scope } => {
                let state = self.bindings.lock().await.get(&binding).cloned();
                let targeted = if let Some(state) = state {
                    state
                        .active
                        .lock()
                        .await
                        .as_ref()
                        .is_some_and(|active| active.scope == scope)
                } else {
                    false
                };
                if targeted {
                    self.interrupt_invocation(&binding, CancelRequest { scope })
                        .await
                        .map(|_| ())
                } else {
                    Ok(())
                }
            }
            InterruptedResource::Opening { owner, operation } => {
                let _initialization = self.initialization_lock.lock().await;
                let binding = self.owners.lock().await.get(&owner).cloned();
                if let Some(binding) = binding {
                    let state = self.bindings.lock().await.get(&binding).cloned();
                    if state.is_some_and(|state| state.open_operation == operation) {
                        return self.close_binding(&binding).await;
                    }
                }
                Ok(())
            }
        }
    }

    async fn reconcile_input(
        &self,
        request: InputReconcileRequest,
    ) -> Result<SubmissionEvidence, ProviderError> {
        super::operation::check()?;
        let submission_id = &request.submission_id;
        let token = request.token.as_ref();
        let accepted_receipts = &request.accepted_receipts;
        let Some(token) = token else {
            return Ok(SubmissionEvidence::Unknown);
        };
        if let NativeRecoveryEvidence::Accepted {
            version: 2,
            submission_id: accepted_submission,
            attempt_id,
            owner: _,
            session_id,
            message_id: _,
        } = super::prompt::decode_recovery_token(token)?
        {
            if accepted_submission == *submission_id
                && attempt_id == request.attempt_id
                && request
                    .provider_session_id
                    .as_ref()
                    .is_none_or(|expected| expected.as_str() == session_id)
            {
                return Ok(SubmissionEvidence::Accepted {
                    receipt: Some(token.clone()),
                    provider_session_id: Some(session_id.try_into().map_err(|_| {
                        failure(
                            ProviderOperation::InputReconcile,
                            ProviderFailureKind::Protocol,
                        )
                    })?),
                });
            }
            return Ok(SubmissionEvidence::Unknown);
        }
        let NativeRecoveryEvidence::Sending {
            version,
            submission_id: token_submission,
            attempt_id: token_attempt,
            owner,
            workspace,
            session_id,
            cursor: _,
            anchor,
            content_digest,
        } = super::prompt::decode_recovery_token(token)?
        else {
            return Ok(SubmissionEvidence::Unknown);
        };
        if version != 2
            || token_submission != *submission_id
            || token_attempt != request.attempt_id
            || request
                .provider_session_id
                .as_ref()
                .is_some_and(|expected| expected.as_str() != session_id)
        {
            return Ok(SubmissionEvidence::Unknown);
        }
        let known_receipts = accepted_message_ids(
            submission_id,
            &request.attempt_id,
            &owner,
            &session_id,
            accepted_receipts,
        )?;
        let client = self.runtime.client(&workspace).await?;
        let receipt = CopilotSession::find_unclaimed_submission_in(
            &client.inner,
            &session_id,
            anchor.as_deref(),
            &known_receipts,
            Some(&content_digest),
        )
        .await?;
        let message_id = match receipt {
            super::handle::SubmissionReconciliation::Found(message_id) => message_id,
            super::handle::SubmissionReconciliation::NotFound => {
                return Ok(SubmissionEvidence::Unknown);
            }
            super::handle::SubmissionReconciliation::Ambiguous
            | super::handle::SubmissionReconciliation::ContentMismatch => {
                return Ok(SubmissionEvidence::Unknown);
            }
        };
        let recovered_receipt = super::prompt::accepted_recovery_token(
            submission_id.clone(),
            request.attempt_id,
            owner,
            session_id.clone(),
            message_id,
        )?;
        let provider_session_id = session_id.try_into().map_err(|_| {
            failure(
                ProviderOperation::InputReconcile,
                ProviderFailureKind::Protocol,
            )
        })?;
        Ok(SubmissionEvidence::Accepted {
            receipt: Some(recovered_receipt),
            provider_session_id: Some(provider_session_id),
        })
    }

    async fn inspect_runtime(
        &self,
        context: WorkspaceContext,
    ) -> Result<forge_provider_api::ProviderSetup, ProviderError> {
        super::operation::check()?;
        let result = self.runtime.client(&context.workspace).await;
        let (runtime, version) = match &result {
            Ok(_) => (
                forge_provider_api::RuntimeAvailability::Available,
                self.runtime.validated_version().await.map_or(
                    forge_provider_api::VersionObservation::NotReported,
                    |version| forge_provider_api::VersionObservation::Known { version },
                ),
            ),
            Err(ProviderError::RuntimeVersion(
                forge_provider_api::version::RuntimeVersionError::Unsupported { detected, .. },
            )) => (
                forge_provider_api::RuntimeAvailability::Incompatible,
                forge_provider_api::VersionObservation::Known {
                    version: detected.to_string(),
                },
            ),
            Err(ProviderError::Failure(error)) if error.kind() == ProviderFailureKind::Runtime => (
                forge_provider_api::RuntimeAvailability::Missing,
                forge_provider_api::VersionObservation::NotReported,
            ),
            Err(error) => (
                forge_provider_api::RuntimeAvailability::Unavailable,
                forge_provider_api::VersionObservation::Failed {
                    failure: super::error::trace_failure_kind(error),
                },
            ),
        };
        Ok(forge_provider_api::ProviderSetup {
            runtime,
            origin: forge_provider_api::RuntimeOrigin::Configured,
            versions: forge_provider_api::RuntimeVersions {
                provider: version,
                adapter: forge_provider_api::VersionObservation::NotReported,
            },
            authentication: Vec::new(),
            capabilities: forge_provider_api::capability::CapabilityManifest::from_descriptor(
                self.descriptor(),
            ),
        })
    }

    async fn shutdown_native(&self) -> Result<(), ProviderError> {
        let _shutdown = self.shutdown_lock.lock().await;
        let operation_id = super::operation::id()?;
        self.operations.identities.close_admission();
        self.closed.store(true, Ordering::Release);
        self.runtime.close_admission();
        let bindings = self
            .bindings
            .lock()
            .await
            .values()
            .cloned()
            .collect::<Vec<_>>();
        for state in &bindings {
            state.closing.store(true, Ordering::Release);
            state.callbacks.close_admission();
            // Fence all siblings synchronously; their phase waits belong to retained close below.
            drop(self.pending_inputs.fence_binding(&state.binding.id));
        }
        let mut failures = Vec::new();
        for state in bindings {
            if let Err(error) = self.close_binding(&state.binding.id).await {
                failures.push(error);
            }
        }
        let runtime_result = self.runtime.shutdown().await;
        if let Err(error) = self.operations.collect(&operation_id).await {
            failures.push(error);
        }
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
        if let Err(error) = collect_profiles(&self.profiles, ProviderOperation::Shutdown) {
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

    async fn models(
        &self,
        context: QueryContext,
    ) -> Result<Vec<forge_provider_api::ProviderModel>, ProviderError> {
        super::operation::check()?;
        let workspace = self
            .query_workspace(context, ProviderOperation::ModelList)
            .await?;
        self.runtime.client(&workspace).await?.models().await
    }

    async fn skills(
        &self,
        context: QueryContext,
    ) -> Result<Vec<forge_provider_api::catalog::SkillDefinition>, ProviderError> {
        super::operation::check()?;
        match context {
            QueryContext::Binding(binding) => {
                let state = self.binding(&binding, ProviderOperation::SkillList).await?;
                let session = state.session.read().await.clone();
                session.skills().await
            }
            QueryContext::Workspace(_) => {
                Err(forge_provider_api::capability::UnsupportedCapability(
                    forge_provider_api::capability::ProviderFeature::SkillCatalog,
                )
                .into())
            }
        }
    }

    async fn mcp(
        &self,
        query: McpQuery,
    ) -> Result<Vec<forge_provider_api::catalog::McpDefinition>, ProviderError> {
        super::operation::check()?;
        let (context, observed) = match query {
            McpQuery::Configured(context) => (context, false),
            McpQuery::Observed(context) => (context, true),
        };
        let state = match &context {
            QueryContext::Binding(binding) => {
                Some(self.binding(binding, ProviderOperation::McpList).await?)
            }
            QueryContext::Workspace(_) => None,
        };
        let workspace = self
            .query_workspace(context, ProviderOperation::McpList)
            .await?;
        let client = self.runtime.client(&workspace).await?;
        let mut definitions = client.discover_mcp(&workspace).await?;
        let Some(state) = state else {
            return Ok(definitions);
        };
        let resources = state.request.lock().await.configuration.resources.clone();
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
        if !observed {
            return Ok(definitions);
        }
        let session = state.session.read().await.clone();
        let startup_failures = self.sessions.mcp_failures(&state.runtime_key).await;
        session.mcp_list(definitions, &startup_failures).await
    }

    async fn terminals(
        &self,
        binding: &BindingId,
    ) -> Result<forge_provider_api::terminal::TerminalSnapshot, ProviderError> {
        super::operation::check()?;
        let state = self
            .binding(binding, ProviderOperation::TerminalList)
            .await?;
        let mut snapshot = self
            .sessions
            .terminals(&state.runtime_key, state.owner.session_id.as_str())
            .await
            .map_err(|_| {
                failure(
                    ProviderOperation::TerminalList,
                    ProviderFailureKind::Runtime,
                )
            })?;
        state.terminals.lock().await.issue(&mut snapshot)?;
        Ok(snapshot)
    }

    async fn terminate(
        &self,
        binding: &BindingId,
        terminal: &forge_provider_api::terminal::TerminalId,
    ) -> Result<(), ProviderError> {
        super::operation::check()?;
        let state = self
            .binding(binding, ProviderOperation::TerminalTerminate)
            .await?;
        let native = state
            .terminals
            .lock()
            .await
            .resolve(terminal)
            .ok_or_else(|| {
                failure(
                    ProviderOperation::TerminalTerminate,
                    ProviderFailureKind::WrongInvocation,
                )
            })?;
        self.sessions
            .terminate(&state.runtime_key, &native)
            .await
            .map_err(|_| {
                failure(
                    ProviderOperation::TerminalTerminate,
                    ProviderFailureKind::Runtime,
                )
            })?;
        state.terminals.lock().await.retire(terminal);
        Ok(())
    }

    async fn open_binding(&self, request: SessionRequest) -> Result<OpenedSession, ProviderError> {
        super::operation::check()?;
        let _initialization = self.initialization_lock.lock().await;
        if self.closed.load(Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::BindingClosed,
            ));
        }
        if matches!(request.history, SessionHistory::IsolatedText(_))
            && (!request.tools.is_empty()
                || request.tool_endpoint.is_some()
                || !request.configuration.resources.skills.is_empty()
                || !request.configuration.resources.mcp.is_empty())
        {
            return Err(failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::Rejected,
            ));
        }
        if request.owner.session_id.as_str().is_empty() || request.workspace.trim().is_empty() {
            return Err(failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::Rejected,
            ));
        }
        // Isolated text has no executable tools or permission authority; its mandatory native
        // suppression policy enforces filesystem denial without requiring the ordinary tool sandbox.
        if !matches!(request.history, SessionHistory::IsolatedText(_)) {
            self.descriptor()
                .capability
                .validate_access(&request.configuration.access)
                .map_err(|_| {
                    failure(
                        ProviderOperation::SessionOpen,
                        ProviderFailureKind::Rejected,
                    )
                })?;
        }
        self.collect_failed_runtime().await?;
        if self.owners.lock().await.contains_key(&request.owner) {
            return Err(failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::Rejected,
            ));
        }
        if let Some(resume) = request.history.resume_id() {
            let bindings = self.bindings.lock().await;
            if bindings
                .values()
                .any(|state| state.binding.provider_session_id.as_ref() == Some(resume))
            {
                return Err(failure(
                    ProviderOperation::SessionOpen,
                    ProviderFailureKind::ResourceConflict,
                ));
            }
        }
        let runtime_key = request.owner.runtime_key();
        let binding_id = BindingId::new(uuid::Uuid::new_v4().to_string())
            .map_err(|_| failure(ProviderOperation::SessionOpen, ProviderFailureKind::Unknown))?;
        let session_scope = forge_provider_api::EventScope::Binding {
            owner: request.owner.clone(),
        };
        let (sink, stream) = events::channel(binding_id.clone(), session_scope);
        self.sessions
            .subscribe(runtime_key.clone(), sink.clone())
            .await;
        let callbacks = Arc::new(CallbackRouter::new(self.closed.clone()));
        let client_runtime = self.runtime.clone();
        let workspace = request.workspace.clone();
        let model_selection = request.configuration.model.clone();
        let capability = self.descriptor().capability;
        let client = async move {
            let client = client_runtime.client(&workspace).await?;
            let models = client.models().await?;
            let (_, reasoning) = resolve_model_selection(&model_selection, &models, &capability)?;
            Ok((client, reasoning))
        };
        let profile = Arc::new(Mutex::new(Some(make_profile(
            &self.profiles,
            &self.runtime_root,
            &request,
        )?)));
        let worker_profile = profile.clone();
        let skills = request.configuration.resources.skills.clone();
        let mcp = request
            .configuration
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
            .configuration
            .resources
            .mcp
            .iter()
            .map(|resource| resource.name.clone())
            .collect::<Vec<_>>();
        let tools = request.tools.clone();
        let mode = session_mode(request.mode);
        let options = super::session::SessionOptions {
            isolated_text: matches!(request.history, SessionHistory::IsolatedText(_)),
            session_id: request
                .history
                .resume_id()
                .as_ref()
                .map(|id| id.as_str().to_owned()),
            model: request.configuration.model.model.clone(),
            reasoning_effort: None,
            context_tier: request.configuration.model.context_window.clone(),
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
                .history
                .resume_id()
                .as_ref()
                .map(|id| id.as_str().to_owned()),
            model: request.configuration.model.model.clone(),
            context_tier: request.configuration.model.context_window.clone(),
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
                        ProviderOperation::SessionOpen,
                        ProviderFailureKind::Initialization,
                    )
                })
            })?;
        callbacks
            .register_mcp_tools(&session, &request.configuration.resources)
            .await;
        let turn = self.sessions.turn(&runtime_key).await;
        let provider_session_id = session.id().try_into().map_err(|_| {
            failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::Protocol,
            )
        })?;
        let binding = SessionBinding {
            id: binding_id.clone(),
            provider_session_id: (!matches!(request.history, SessionHistory::IsolatedText(_)))
                .then_some(provider_session_id),
        };
        let state = Arc::new(BindingState {
            binding: binding.clone(),
            open_operation: super::operation::id()?,
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
            closing: std::sync::atomic::AtomicBool::new(false),
            unsafe_history: std::sync::atomic::AtomicBool::new(false),
            settlements: Mutex::new(HashMap::new()),
            cancellations: Mutex::new(HashMap::new()),
            retained_resources: Mutex::new(self.sessions.resources(&runtime_key).await),
            retired_resources: Mutex::new(Vec::new()),
            settled: tokio::sync::Notify::new(),
            retractions: Mutex::new(HashMap::new()),
            retracted_inputs: Mutex::new(HashMap::new()),
            inputs: Mutex::new(HashMap::new()),
            unconsumed: Mutex::new(HashSet::new()),
            close_failure: Mutex::new(None),
            compactions: Mutex::new(HashMap::new()),
            terminals: Mutex::new(super::terminal::TerminalRegistry::default()),
            terminals_collected: std::sync::atomic::AtomicBool::new(false),
            native_retired: std::sync::atomic::AtomicBool::new(false),
            history_deleted: std::sync::atomic::AtomicBool::new(false),
        });
        self.bindings
            .lock()
            .await
            .insert(binding_id.clone(), state.clone());
        self.owners.lock().await.insert(request.owner, binding_id);
        if self.closed.load(Ordering::Acquire) {
            self.close_binding(&binding.id).await?;
            return Err(ProviderError::Failure(
                ProviderFailure::new(
                    ProviderOperation::SessionOpen,
                    ProviderFailureKind::BindingClosed,
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Close,
                    forge_provider_api::FailureDisposition::Closed,
                ),
            ));
        }
        if let Some(admission) = super::operation::current()
            && admission
                .context
                .check_admission(ProviderOperation::SessionOpen)
                .is_err()
        {
            let reason = admission.context.interrupted().await;
            self.close_binding(&binding.id).await?;
            return Err(reason.failure(
                ProviderOperation::SessionOpen,
                forge_provider_api::ProviderPhase::Close,
                forge_provider_api::FailureDisposition::Closed,
            ));
        }
        let events = state
            .stream
            .lock()
            .await
            .take()
            .expect("new binding owns its sole stream");
        Ok(OpenedSession {
            binding,
            capabilities: forge_provider_api::capability::CapabilityManifest::from_descriptor(
                self.descriptor(),
            ),
            events,
        })
    }

    async fn configure_binding(
        &self,
        binding: &BindingId,
        configuration: SessionConfiguration,
    ) -> Result<ConfiguredSession, ProviderError> {
        super::operation::check()?;
        let state = self
            .binding(binding, ProviderOperation::SessionConfigure)
            .await?;
        let weak_state = Arc::downgrade(&state);
        super::operation::fence(move || {
            if let Some(state) = weak_state.upgrade() {
                state.closing.store(true, Ordering::Release);
                state.callbacks.close_admission();
            }
        });
        let _admission = state.admission.lock().await;
        if self.closed.load(Ordering::Acquire) || state.closing.load(Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::BindingClosed,
            ));
        }
        if state.unsafe_history.load(Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::BindingInvalidated,
            ));
        }
        if state
            .active
            .lock()
            .await
            .as_ref()
            .is_some_and(|active| !active.task.is_finished())
        {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let mut next_request = state.request.lock().await.clone();
        if configuration == next_request.configuration {
            return Ok(ConfiguredSession {
                configuration,
                capabilities: forge_provider_api::capability::CapabilityManifest::from_descriptor(
                    self.descriptor(),
                ),
                watermark: state.sink.watermark(),
            });
        }
        if matches!(next_request.history, SessionHistory::IsolatedText(_)) {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::Policy,
            ));
        }
        let session = state.session.read().await.clone();
        let terminals =
            session
                .terminal_snapshot()
                .await
                .map_err(|cause| ProviderError::Combined {
                    primary: Box::new(ProviderError::Failure(
                        ProviderFailure::new(
                            ProviderOperation::SessionConfigure,
                            super::error::trace_failure_kind(&cause),
                        )
                        .at_phase(
                            forge_provider_api::ProviderPhase::Observation,
                            forge_provider_api::FailureDisposition::Unchanged,
                        ),
                    )),
                    secondary: vec![cause],
                })?;
        if !terminals.terminal.is_empty() {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::ResourceConflict,
            ));
        }
        state.turn.reset().map_err(|_| {
            failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::Protocol,
            )
        })?;

        if matches!(next_request.history, SessionHistory::Durable { .. }) {
            next_request.history = SessionHistory::Durable {
                resume: state.binding.provider_session_id.clone(),
            };
        }
        next_request.configuration = configuration.clone();
        self.descriptor()
            .capability
            .validate_access(&next_request.configuration.access)
            .map_err(|_| {
                failure(
                    ProviderOperation::SessionConfigure,
                    ProviderFailureKind::Rejected,
                )
            })?;

        let profile = Arc::new(Mutex::new(Some(make_profile(
            &self.profiles,
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
        let model_selection = next_request.configuration.model.clone();
        let capability = self.descriptor().capability;
        let client = async move {
            let client = client_runtime.client(&workspace).await?;
            let models = client.models().await?;
            let (_, reasoning) = resolve_model_selection(&model_selection, &models, &capability)?;
            Ok((client, reasoning))
        };
        let skills = next_request.configuration.resources.skills.clone();
        let mcp = next_request
            .configuration
            .resources
            .mcp
            .iter()
            .map(mcp_config)
            .collect::<Vec<_>>();
        let selected_mcp = next_request
            .configuration
            .resources
            .mcp
            .iter()
            .map(|resource| resource.name.clone())
            .collect::<Vec<_>>();
        let tools = next_request.tools.clone();
        let mode = session_mode(next_request.mode);
        let options = super::session::SessionOptions {
            isolated_text: matches!(next_request.history, SessionHistory::IsolatedText(_)),
            session_id: next_request
                .history
                .resume_id()
                .as_ref()
                .map(|id| id.as_str().to_owned()),
            model: next_request.configuration.model.model.clone(),
            reasoning_effort: None,
            context_tier: next_request.configuration.model.context_window.clone(),
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
                .history
                .resume_id()
                .as_ref()
                .map(|id| id.as_str().to_owned()),
            model: next_request.configuration.model.model.clone(),
            context_tier: next_request.configuration.model.context_window.clone(),
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
                let cause = error.downcast::<ProviderError>().unwrap_or_else(|_| {
                    failure(
                        ProviderOperation::SessionConfigure,
                        ProviderFailureKind::Runtime,
                    )
                });
                if super::operation::current().is_some_and(|operation| !operation.dispatched()) {
                    return Err(cause);
                }
                state.closing.store(true, Ordering::Release);
                state.callbacks.revoke();
                state
                    .close_failure
                    .lock()
                    .await
                    .get_or_insert(cause.clone());
                return Err(ProviderError::Combined {
                    primary: Box::new(ProviderError::Failure(
                        ProviderFailure::new(
                            ProviderOperation::SessionConfigure,
                            super::error::trace_failure_kind(&cause),
                        )
                        .at_phase(
                            forge_provider_api::ProviderPhase::Preparation,
                            forge_provider_api::FailureDisposition::Invalidated,
                        ),
                    )),
                    secondary: vec![cause],
                });
            }
        };
        state.callbacks.retire_all(&state.binding.id).await?;
        state
            .callbacks
            .register_mcp_tools(&session, &next_request.configuration.resources)
            .await;
        let resources = self.sessions.resources(&state.runtime_key).await;
        *state.session.write().await = session;
        let replaced = std::mem::replace(&mut *state.retained_resources.lock().await, resources);
        if let Some(replaced) = replaced {
            state.retired_resources.lock().await.push(replaced);
        }
        *state.request.lock().await = next_request;
        if let Err(cause) = state
            .collect_retired_profiles(ProviderOperation::SessionConfigure)
            .await
        {
            state.closing.store(true, Ordering::Release);
            state.callbacks.close_admission();
            state
                .close_failure
                .lock()
                .await
                .get_or_insert(cause.clone());
            return Err(ProviderError::Combined {
                primary: Box::new(ProviderError::Failure(
                    ProviderFailure::new(
                        ProviderOperation::SessionConfigure,
                        super::error::trace_failure_kind(&cause),
                    )
                    .at_phase(
                        forge_provider_api::ProviderPhase::Collection,
                        forge_provider_api::FailureDisposition::Invalidated,
                    ),
                )),
                secondary: vec![cause],
            });
        }
        state.confirm_configuration_health(self.closed.load(Ordering::Acquire))?;
        if let Err(error) = state
            .sink
            .send_wait(forge_provider_api::ProviderEvent::CapabilitiesChanged {
                scope: forge_provider_api::EventScope::Binding {
                    owner: state.owner.clone(),
                },
                capabilities: forge_provider_api::capability::CapabilityManifest::from_descriptor(
                    self.descriptor(),
                ),
            })
            .await
        {
            let cause = ProviderError::from(error);
            let primary = state.invalidate_configuration(ProviderFailureKind::Transport);
            return Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![cause],
            });
        }
        state.confirm_configuration_health(self.closed.load(Ordering::Acquire))?;
        Ok(ConfiguredSession {
            configuration,
            capabilities: forge_provider_api::capability::CapabilityManifest::from_descriptor(
                self.descriptor(),
            ),
            watermark: state.sink.watermark(),
        })
    }

    async fn steer_input(
        &self,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<SteeringAcceptance, ProviderError> {
        super::operation::check()?;
        let state = self.binding(binding, ProviderOperation::InputSteer).await?;
        let weak_state = Arc::downgrade(&state);
        let target_scope = request.scope.clone();
        super::operation::fence(move || {
            if let Some(state) = weak_state.upgrade() {
                let _ = state.turn.request_stop_for(&target_scope);
            }
        });
        if self.closed.load(Ordering::Acquire) || state.closing.load(Ordering::Acquire) {
            return Err(ProviderError::Failure(
                ProviderFailure::new(
                    ProviderOperation::InputSteer,
                    ProviderFailureKind::BindingClosed,
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Admission,
                    forge_provider_api::FailureDisposition::Invalidated,
                ),
            ));
        }
        if state.unsafe_history.load(Ordering::Acquire) {
            return Err(ProviderError::Failure(
                ProviderFailure::new(
                    ProviderOperation::InputSteer,
                    ProviderFailureKind::BindingInvalidated,
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Admission,
                    forge_provider_api::FailureDisposition::Invalidated,
                ),
            ));
        }
        if request.scope.owner != state.owner {
            return Err(failure(
                ProviderOperation::InputSteer,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        let submission_id = request.delivery.submission_id().cloned();
        let receipt = request.delivery.receipt().cloned();
        if let Some(receipt) = &receipt {
            receipt.attempt_id().map_err(|_| {
                failure(
                    ProviderOperation::InputSteer,
                    ProviderFailureKind::WrongInvocation,
                )
            })?;
        }
        let active_scope = state
            .active
            .lock()
            .await
            .as_ref()
            .map(|active| active.scope.clone());
        if active_scope
            .as_ref()
            .is_some_and(|scope| scope != &request.scope)
        {
            return Err(failure(
                ProviderOperation::InputSteer,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        if !state.turn.is_active() || active_scope.is_none() {
            return Err(ProviderError::NotConsumed);
        }
        let callbacks = state
            .callbacks
            .callbacks(&request.scope)
            .await
            .map_err(|cause| cause.for_operation(ProviderOperation::InputSteer))?;
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
            .steer_native(submission_id.clone(), |authority| async {
                session
                    .steer(input, receipt, &request.scope, &workspace, authority)
                    .await
                    .map_err(anyhow::Error::new)
            })
            .await
            .map_err(|error| {
                error
                    .downcast_ref::<ProviderError>()
                    .cloned()
                    .unwrap_or(ProviderError::DeliveryUncertain)
            })?;
        Ok(match submission_id {
            Some(submission_id) => SteeringAcceptance::Journaled { submission_id },
            None => SteeringAcceptance::Context,
        })
    }

    async fn close_binding(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let pending_preparation = self.pending_inputs.fence_binding(binding);
        let Some(state) = self.bindings.lock().await.get(binding).cloned() else {
            return tokio::time::timeout(std::time::Duration::from_secs(30), pending_preparation)
                .await
                .map_err(|_| {
                    ProviderError::Failure(
                        ProviderFailure::new(
                            ProviderOperation::SessionClose,
                            ProviderFailureKind::Timeout,
                        )
                        .at_phase(
                            forge_provider_api::ProviderPhase::Collection,
                            forge_provider_api::FailureDisposition::Invalidated,
                        ),
                    )
                });
        };
        // Revoke authority before waiting for any admitted mutation or native I/O.
        state.closing.store(true, Ordering::Release);
        state.callbacks.close_admission();
        let mut failures = Vec::new();
        let should_stop =
            match tokio::time::timeout(std::time::Duration::from_secs(10), state.admission.lock())
                .await
            {
                Ok(_admission) => state
                    .active
                    .lock()
                    .await
                    .as_ref()
                    .is_some_and(|active| !active.task.is_finished()),
                Err(_) => {
                    failures.push(ProviderError::Failure(
                        ProviderFailure::new(
                            ProviderOperation::SessionClose,
                            ProviderFailureKind::Timeout,
                        )
                        .at_phase(
                            forge_provider_api::ProviderPhase::Collection,
                            forge_provider_api::FailureDisposition::Invalidated,
                        ),
                    ));
                    state
                        .active
                        .lock()
                        .await
                        .as_ref()
                        .is_some_and(|active| !active.task.is_finished())
                }
            };
        if should_stop && let Err(error) = state.interrupt().await {
            failures.push(error);
        }
        match state.collect_execution().await {
            Ok(None | Some(Ok(()))) => {}
            Ok(Some(Err(error))) | Err(error) => failures.push(error),
        }

        // Attempt independent cleanup even when foreground collection failed. The binding
        // retains its profile, callbacks, native handle, and each unfinished phase for retry.
        let session = state.session.read().await.clone();
        if !state.terminals_collected.load(Ordering::Acquire) {
            let terminals = if session.client_collected() {
                Ok(())
            } else {
                tokio::time::timeout(std::time::Duration::from_secs(10), session.terminate_all())
                    .await
                    .unwrap_or_else(|_| {
                        Err(failure(
                            ProviderOperation::SessionClose,
                            ProviderFailureKind::Timeout,
                        ))
                    })
            };
            match terminals {
                Ok(()) => state.terminals_collected.store(true, Ordering::Release),
                Err(error) => failures.push(error),
            }
        }
        if !state.native_retired.load(Ordering::Acquire) {
            match tokio::time::timeout(
                std::time::Duration::from_secs(30),
                self.sessions.retire(&state.runtime_key),
            )
            .await
            {
                Ok(Ok(())) => state.native_retired.store(true, Ordering::Release),
                Ok(Err(error)) => {
                    failures.push(error.downcast::<ProviderError>().unwrap_or_else(|_| {
                        failure(
                            ProviderOperation::SessionClose,
                            ProviderFailureKind::Transport,
                        )
                    }))
                }
                Err(_) => failures.push(failure(
                    ProviderOperation::SessionClose,
                    ProviderFailureKind::Timeout,
                )),
            }
        }
        if state.native_retired.load(Ordering::Acquire) {
            match state.collect_execution().await {
                Ok(None | Some(Ok(()))) => {}
                Ok(Some(Err(error))) | Err(error) => failures.push(error),
            }
            let temporary = matches!(
                state.request.lock().await.history,
                SessionHistory::IsolatedText(_)
            );
            if temporary && !state.history_deleted.load(Ordering::Acquire) {
                match tokio::time::timeout(
                    std::time::Duration::from_secs(10),
                    session.inner.client().delete_session(session.inner.id()),
                )
                .await
                {
                    Ok(Ok(())) => state.history_deleted.store(true, Ordering::Release),
                    Ok(Err(_)) => failures.push(failure(
                        ProviderOperation::SessionClose,
                        ProviderFailureKind::Transport,
                    )),
                    Err(_) => failures.push(ProviderError::Failure(
                        ProviderFailure::new(
                            ProviderOperation::SessionClose,
                            ProviderFailureKind::Timeout,
                        )
                        .at_phase(
                            forge_provider_api::ProviderPhase::Close,
                            forge_provider_api::FailureDisposition::Uncertain,
                        ),
                    )),
                }
            }
        }
        // Native retirement precedes this wait so an in-flight preparation RPC cannot form a close cycle.
        let preparations_collected =
            match tokio::time::timeout(std::time::Duration::from_secs(30), pending_preparation)
                .await
            {
                Ok(()) => true,
                Err(_) => {
                    failures.push(ProviderError::Failure(
                        ProviderFailure::new(
                            ProviderOperation::SessionClose,
                            ProviderFailureKind::Timeout,
                        )
                        .at_phase(
                            forge_provider_api::ProviderPhase::Collection,
                            forge_provider_api::FailureDisposition::Invalidated,
                        ),
                    ));
                    false
                }
            };
        let foreground_collected = preparations_collected
            && state.active.lock().await.is_none()
            && !state.turn.is_active()
            && state.callbacks.running_callbacks.load(Ordering::Acquire) == 0;
        if state.native_retired.load(Ordering::Acquire)
            && state.terminals_collected.load(Ordering::Acquire)
            && foreground_collected
        {
            if let Some(resources) = state.retained_resources.lock().await.as_ref()
                && let Err(error) = resources._profile.close(ProviderOperation::SessionClose)
            {
                failures.push(error);
            }
            if let Err(error) = state
                .collect_retired_profiles(ProviderOperation::SessionClose)
                .await
            {
                failures.push(error);
            }
        } else if failures.is_empty() {
            failures.push(failure(
                ProviderOperation::SessionClose,
                ProviderFailureKind::ResourceConflict,
            ));
        }
        if !failures.is_empty() {
            let first = failures.remove(0);
            let primary = state
                .close_failure
                .lock()
                .await
                .get_or_insert(first)
                .clone();
            return Err(if failures.is_empty() {
                primary
            } else {
                ProviderError::Combined {
                    primary: Box::new(primary),
                    secondary: failures,
                }
            });
        }
        state.retained_resources.lock().await.take();
        self.closed_bindings
            .lock()
            .expect("closed binding registry is not poisoned")
            .insert(binding.clone());
        self.bindings.lock().await.remove(binding);
        self.owners.lock().await.remove(&state.owner);
        Ok(())
    }

    async fn interrupt_invocation(
        &self,
        binding: &BindingId,
        request: CancelRequest,
    ) -> Result<InvocationSettlement, ProviderError> {
        // Fence even an absent scope, and wait for queued preparation outside native admission.
        self.pending_inputs.cancel(binding, &request.scope).await;
        let state = self.binding(binding, ProviderOperation::Cancel).await?;
        // No queued candidate can register now. Do not wait for an unrelated binding mutation
        // merely to report the proven absence of this exact invocation's native owner.
        if !state
            .active
            .lock()
            .await
            .as_ref()
            .is_some_and(|active| active.scope == request.scope)
        {
            if let Some(result) = state
                .cancellations
                .lock()
                .await
                .get(&request.scope)
                .cloned()
            {
                return result;
            }
            if let Some(result) = state.settlements.lock().await.get(&request.scope).cloned() {
                return result;
            }
            return Err(failure(
                ProviderOperation::Cancel,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        let _admission = state.admission.lock().await;
        if let Some(result) = state
            .cancellations
            .lock()
            .await
            .get(&request.scope)
            .cloned()
        {
            return result;
        }
        if let Some(settlement) = state.settlements.lock().await.get(&request.scope).cloned() {
            state
                .cancellations
                .lock()
                .await
                .insert(request.scope, settlement.clone());
            return settlement;
        }
        if !state
            .active
            .lock()
            .await
            .as_ref()
            .is_some_and(|active| active.scope == request.scope)
        {
            return Err(failure(
                ProviderOperation::Cancel,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        let trace = state.request.lock().await.trace.clone();
        trace.record(forge_provider_api::trace::ProviderTraceRecord {
            binding_id: binding.clone(),
            event: forge_provider_api::trace::ProviderTraceEvent::CancellationStarted,
            scope: Some(request.scope.clone()),
            submission_id: None,
            failure: None,
        });
        super::operation::dispatch_for(ProviderOperation::Cancel)?;
        let stop_failure = state.interrupt().await.err();
        trace.record(forge_provider_api::trace::ProviderTraceRecord {
            binding_id: binding.clone(),
            event: forge_provider_api::trace::ProviderTraceEvent::CancellationAcknowledged,
            scope: Some(request.scope.clone()),
            submission_id: None,
            failure: stop_failure.as_ref().map(super::error::trace_failure_kind),
        });
        // Attempt collection even when interruption failed or its acknowledgement timed out.
        trace.record(forge_provider_api::trace::ProviderTraceRecord {
            binding_id: binding.clone(),
            event: forge_provider_api::trace::ProviderTraceEvent::ExecutionCollectionStarted,
            scope: Some(request.scope.clone()),
            submission_id: None,
            failure: None,
        });
        let collection = state.collect_execution().await;
        let collection_failure = match collection {
            Ok(None | Some(Ok(()))) => None,
            Ok(Some(Err(error))) | Err(error) => Some(error),
        };
        trace.record(forge_provider_api::trace::ProviderTraceRecord {
            binding_id: binding.clone(),
            event: forge_provider_api::trace::ProviderTraceEvent::ExecutionCollectionFinished,
            scope: Some(request.scope.clone()),
            submission_id: None,
            failure: collection_failure
                .as_ref()
                .map(super::error::trace_failure_kind),
        });
        let result = match (stop_failure, collection_failure) {
            (Some(primary), Some(secondary)) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![secondary],
            }),
            (Some(error), None) | (None, Some(error)) => Err(error),
            (None, None) => state
                .settlements
                .lock()
                .await
                .get(&request.scope)
                .cloned()
                .unwrap_or_else(|| {
                    Err(failure(
                        ProviderOperation::Cancel,
                        ProviderFailureKind::Protocol,
                    ))
                }),
        };
        state
            .cancellations
            .lock()
            .await
            .insert(request.scope, result.clone());
        result
    }

    async fn fork_history(
        &self,
        request: SessionForkRequest,
    ) -> Result<forge_provider_api::ProviderForkResult, ProviderError> {
        let operation = super::operation::current().ok_or_else(|| {
            failure(
                ProviderOperation::Inspect,
                ProviderFailureKind::OperationConflict,
            )
        })?;
        let context = operation.context.clone();
        let operation_id = context.id().clone();
        let _admission = self.initialization_lock.lock().await;
        if let Some(retained) = self.forks.lock().await.get(&operation_id) {
            if retained.context != context || retained.request != request {
                return Err(failure(
                    ProviderOperation::SessionFork,
                    ProviderFailureKind::HistoryConflict,
                ));
            }
            return retained.result.clone();
        }
        let immutable_request = request.clone();
        if self.closed.load(Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::SessionFork,
                ProviderFailureKind::BindingClosed,
            ));
        }
        let (session_id, checkpoint, workspace, source) = match request.source {
            ForkSource::Binding(binding) => {
                let state = self
                    .binding(&binding, ProviderOperation::SessionFork)
                    .await?;
                let workspace = state.request.lock().await.workspace.clone();
                let id = state.binding.provider_session_id.clone().ok_or_else(|| {
                    failure(
                        ProviderOperation::InputSubmit,
                        ProviderFailureKind::Rejected,
                    )
                })?;
                (id, None, workspace, Some(state))
            }
            ForkSource::Checkpoint {
                provider_session_id,
                checkpoint,
            } => (
                provider_session_id,
                Some(checkpoint.as_str().to_owned()),
                request.target.workspace,
                None,
            ),
        };
        let _source_admission = if let Some(state) = &source {
            Some(state.admission.lock().await)
        } else {
            None
        };
        if let Some(state) = &source {
            if self.closed.load(Ordering::Acquire) || state.closing.load(Ordering::Acquire) {
                return Err(failure(
                    ProviderOperation::SessionFork,
                    ProviderFailureKind::BindingClosed,
                ));
            }
            if state.unsafe_history.load(Ordering::Acquire) {
                return Err(failure(
                    ProviderOperation::SessionFork,
                    ProviderFailureKind::BindingInvalidated,
                ));
            }
            if state
                .active
                .lock()
                .await
                .as_ref()
                .is_some_and(|active| !active.task.is_finished())
            {
                return Err(failure(
                    ProviderOperation::SessionFork,
                    ProviderFailureKind::ExecutionActive,
                ));
            }
        }
        let client = self.runtime.client(&workspace).await?;
        let unknown = ProviderError::Failure(
            ProviderFailure::new(
                ProviderOperation::SessionFork,
                ProviderFailureKind::Transport,
            )
            .at_phase(
                forge_provider_api::ProviderPhase::Observation,
                forge_provider_api::FailureDisposition::Uncertain,
            ),
        );
        self.forks.lock().await.insert(
            operation_id.clone(),
            RetainedFork {
                context: context.clone(),
                request: immutable_request,
                result: Err(unknown),
            },
        );
        let result = client
            .fork(session_id.as_str().to_owned(), checkpoint)
            .await
            .and_then(|id| {
                Ok(forge_provider_api::ProviderForkResult::unprofiled(
                    id.try_into().map_err(|_| {
                        failure(
                            ProviderOperation::SessionFork,
                            ProviderFailureKind::Protocol,
                        )
                    })?,
                ))
            });
        self.forks
            .lock()
            .await
            .get_mut(&operation_id)
            .expect("fork retains its admitted operation")
            .result = result.clone();
        result
    }

    async fn compact_history(
        &self,
        binding: &BindingId,
        request: forge_provider_api::CompactionRequest,
    ) -> Result<CompactionResult, ProviderError> {
        let operation = super::operation::current().ok_or_else(|| {
            failure(
                ProviderOperation::Inspect,
                ProviderFailureKind::OperationConflict,
            )
        })?;
        let context = operation.context.clone();
        let operation_id = context.id().clone();
        request.validate_context(&context)?;
        let state = self
            .binding(binding, ProviderOperation::SessionCompact)
            .await?;
        if !matches!(&request.scope, forge_provider_api::EventScope::SessionOperation { owner, .. } if owner == &state.owner)
        {
            return Err(failure(
                ProviderOperation::SessionCompact,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        let weak_state = Arc::downgrade(&state);
        super::operation::fence(move || {
            if let Some(state) = weak_state.upgrade() {
                state.closing.store(true, Ordering::Release);
                state.callbacks.close_admission();
            }
        });
        let _admission = state.admission.lock().await;
        if self.closed.load(Ordering::Acquire) || state.closing.load(Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::SessionCompact,
                ProviderFailureKind::BindingClosed,
            ));
        }
        if state.unsafe_history.load(Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::SessionCompact,
                ProviderFailureKind::BindingInvalidated,
            ));
        }
        if state
            .active
            .lock()
            .await
            .as_ref()
            .is_some_and(|active| !active.task.is_finished())
        {
            return Err(failure(
                ProviderOperation::SessionCompact,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        if let Some(result) = state.compactions.lock().await.get(&operation_id).cloned() {
            return result;
        }
        state.compactions.lock().await.insert(
            operation_id.clone(),
            Err(ProviderError::Failure(
                ProviderFailure::new(
                    ProviderOperation::SessionCompact,
                    ProviderFailureKind::Transport,
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Observation,
                    forge_provider_api::FailureDisposition::Uncertain,
                ),
            )),
        );
        let result = self
            .sessions
            .compact(&state.runtime_key, operation_id.as_str().to_owned())
            .await
            .map(|compaction| CompactionResult {
                compaction,
                watermark: state.sink.watermark(),
            });
        state
            .compactions
            .lock()
            .await
            .insert(operation_id, result.clone());
        result
    }

    async fn retract_input(
        &self,
        binding: &BindingId,
        request: InputRetractRequest,
    ) -> Result<InputRetractOutcome, ProviderError> {
        let operation = super::operation::current().ok_or_else(|| {
            failure(
                ProviderOperation::Inspect,
                ProviderFailureKind::OperationConflict,
            )
        })?;
        let context = operation.context.clone();
        let operation_id = context.id().clone();
        let state = self
            .binding(binding, ProviderOperation::InputRetract)
            .await?;
        let weak_state = Arc::downgrade(&state);
        super::operation::fence(move || {
            if let Some(state) = weak_state.upgrade() {
                state.closing.store(true, Ordering::Release);
                state.callbacks.close_admission();
            }
        });
        let _admission = state.admission.lock().await;
        if let Some(retained) = state.retractions.lock().await.get(&operation_id) {
            return retained.replay(&context, &request);
        }

        if !state
            .inputs
            .lock()
            .await
            .get(&request.submission_id)
            .is_some_and(|(scope, attempt)| {
                *scope == request.scope && *attempt == request.attempt_id
            })
        {
            return Ok(InputRetractOutcome::Unavailable(
                forge_provider_api::InputRetractUnavailable::WrongInvocation,
            ));
        }
        if let Some(previous) = state
            .retracted_inputs
            .lock()
            .await
            .get(&request.submission_id)
        {
            let outcome = state
                .retractions
                .lock()
                .await
                .get(previous)
                .map(|retained| retained.outcome.clone())
                .unwrap_or(InputRetractOutcome::Unknown);
            return Ok(if outcome == InputRetractOutcome::Unknown {
                outcome
            } else {
                InputRetractOutcome::Unavailable(
                    forge_provider_api::InputRetractUnavailable::HistoryBoundaryChanged,
                )
            });
        }
        if state
            .unconsumed
            .lock()
            .await
            .contains(&request.submission_id)
        {
            state.retractions.lock().await.insert(
                operation_id.clone(),
                RetainedRetraction {
                    context: context.clone(),
                    request: request.clone(),
                    outcome: InputRetractOutcome::NeverConsumed,
                },
            );
            return Ok(InputRetractOutcome::NeverConsumed);
        }
        if self.closed.load(Ordering::Acquire) || state.closing.load(Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::InputRetract,
                ProviderFailureKind::BindingClosed,
            ));
        }
        if state.unsafe_history.load(Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::InputRetract,
                ProviderFailureKind::BindingInvalidated,
            ));
        }
        if state
            .active
            .lock()
            .await
            .as_ref()
            .is_some_and(|active| !active.task.is_finished())
        {
            return Err(failure(
                ProviderOperation::InputRetract,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        if !state
            .settlements
            .lock()
            .await
            .get(&request.scope)
            .is_some_and(|result| {
                result.as_ref().is_ok_and(|settlement| {
                    matches!(
                        settlement.result,
                        forge_provider_api::ExecutionResult::Cancelled { .. }
                    )
                })
            })
        {
            return Ok(InputRetractOutcome::Unavailable(
                forge_provider_api::InputRetractUnavailable::NotCancelled,
            ));
        }
        let target = state
            .turn
            .checked_retraction(&request.submission_id, request.boundary.as_str())
            .map_err(|_| {
                failure(
                    ProviderOperation::InputRetract,
                    ProviderFailureKind::Protocol,
                )
            })?;
        let Some(target) = target else {
            return Ok(InputRetractOutcome::Unavailable(
                forge_provider_api::InputRetractUnavailable::HistoryBoundaryChanged,
            ));
        };
        // Record uncertainty before polling a destructive RPC; a dropped awaiter cannot replay it.
        state.retractions.lock().await.insert(
            operation_id.clone(),
            RetainedRetraction {
                context: context.clone(),
                request: request.clone(),
                outcome: InputRetractOutcome::Unknown,
            },
        );
        state
            .retracted_inputs
            .lock()
            .await
            .insert(request.submission_id.clone(), operation_id.clone());
        state.unsafe_history.store(true, Ordering::Release);
        state.callbacks.revoke();
        let session = state.session.read().await.clone();
        match session.rewind(target).await {
            Ok(()) => {
                let outcome = InputRetractOutcome::Retracted {
                    boundary: request.boundary,
                };
                state
                    .retractions
                    .lock()
                    .await
                    .get_mut(&operation_id)
                    .expect("retraction retains its admitted operation")
                    .outcome = outcome.clone();
                state.unsafe_history.store(false, Ordering::Release);
                Ok(outcome)
            }
            Err(error) => {
                if !operation.dispatched() {
                    state
                        .retracted_inputs
                        .lock()
                        .await
                        .remove(&request.submission_id);
                    state.unsafe_history.store(false, Ordering::Release);
                    return Err(error);
                }
                state.closing.store(true, Ordering::Release);
                state.callbacks.retire_all(binding).await?;
                Ok(InputRetractOutcome::Unknown)
            }
        }
    }

    /// Resolve a read-only catalog context without creating a replacement binding.
    async fn query_workspace(
        &self,
        context: QueryContext,
        operation: ProviderOperation,
    ) -> Result<String, ProviderError> {
        match context {
            QueryContext::Workspace(context) => Ok(context.workspace),
            QueryContext::Binding(binding) => Ok(self
                .binding(&binding, operation)
                .await?
                .request
                .lock()
                .await
                .workspace
                .clone()),
        }
    }

    fn descriptor(&self) -> forge_provider_api::ProviderDescriptor {
        let id = crate::registry::select("copilot").expect("Copilot is registered");
        crate::registry::descriptor(&id).expect("registered Copilot descriptor")
    }

    async fn submit(
        &self,
        binding: &BindingId,
        request: PromptRequest,
        pending: &crate::pending_input::PendingInput,
    ) -> Result<InputAcceptance, ProviderError> {
        let state = self
            .binding(binding, ProviderOperation::InputSubmit)
            .await?;
        pending.check_admission(ProviderOperation::InputSubmit)?;
        super::operation::check()
            .map_err(|cause| ProviderError::PreparationFailed(Box::new(cause)))?;
        let weak_state = Arc::downgrade(&state);
        let target_scope = request.scope.clone();
        super::operation::fence(move || {
            if let Some(state) = weak_state.upgrade() {
                let _ = state.turn.request_stop_for(&target_scope);
            }
        });
        let _admission = tokio::select! {
            biased;
            _ = pending.cancelled() => {
                pending.check_admission(ProviderOperation::InputSubmit)?;
                unreachable!("pending cancellation is sticky");
            }
            admission = state.admission.lock() => admission,
        };
        pending.check_admission(ProviderOperation::InputSubmit)?;
        if self.closed.load(Ordering::Acquire) || state.closing.load(Ordering::Acquire) {
            return Err(ProviderError::Failure(
                ProviderFailure::new(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::BindingClosed,
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Admission,
                    forge_provider_api::FailureDisposition::Invalidated,
                ),
            ));
        }
        if state.unsafe_history.load(Ordering::Acquire) {
            return Err(ProviderError::Failure(
                ProviderFailure::new(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::BindingInvalidated,
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Admission,
                    forge_provider_api::FailureDisposition::Invalidated,
                ),
            ));
        }
        if state.settlements.lock().await.contains_key(&request.scope) {
            return Err(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        let submission_id = request.submission_id.clone();
        let attempt_id = request.attempt_id.clone();
        let isolated_text = matches!(
            state.request.lock().await.history,
            SessionHistory::IsolatedText(_)
        );
        if isolated_text && (!request.tools.is_empty() || request.tool_endpoint.is_some()) {
            return Err(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Policy,
            ));
        }
        let allowed_tools = request
            .tools
            .iter()
            .map(|tool| tool.name.to_owned())
            .collect::<HashSet<_>>();
        if request.scope.owner != state.owner
            || request.receipt.id().as_str() != request.submission_id.as_str()
            || request.receipt.attempt_id().map_err(|_| {
                failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::WrongInvocation,
                )
            })? != request.attempt_id
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
        if !isolated_text {
            self.descriptor()
                .capability
                .validate_access(&request.configuration.access)
                .map_err(|_| {
                    failure(
                        ProviderOperation::InputSubmit,
                        ProviderFailureKind::Rejected,
                    )
                })?;
        }
        if state.stream.lock().await.is_some() {
            return Err(ProviderError::Failure(
                ProviderFailure::new(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::BindingInvalidated,
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Admission,
                    forge_provider_api::FailureDisposition::Invalidated,
                ),
            ));
        }
        let previous = {
            let mut active = state.active.lock().await;
            if active.as_ref().is_some_and(|task| !task.task.is_finished()) {
                return Err(failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::ExecutionActive,
                ));
            }
            active.take()
        };
        if let Some(previous) = previous {
            match previous.task.await {
                Ok(Ok(())) => {}
                Ok(Err(_)) => {}
                Err(_) => {
                    return Err(failure(
                        ProviderOperation::InputSubmit,
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
        let models = tokio::select! {
            biased;
            _ = pending.cancelled() => {
                pending.check_admission(ProviderOperation::InputSubmit)?;
                unreachable!("pending cancellation is sticky");
            }
            models = client.models() => models?,
        };
        let (selected_model, reasoning) = resolve_model_selection(
            &request.configuration.model,
            &models,
            &self.descriptor().capability,
        )
        .map_err(|_| {
            failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Rejected,
            )
        })?;
        pending.check_admission(ProviderOperation::InputSubmit)?;
        session
            .set_model(
                selected_model.as_deref().unwrap_or("auto"),
                reasoning,
                request.configuration.model.context_window.as_deref(),
            )
            .await?;
        pending.check_admission(ProviderOperation::InputSubmit)?;
        let callbacks = InvocationCallbacks {
            scope: request.scope.clone(),
            generation: 0,
            permissions: request.permissions.clone(),
            tools: request.provider_tools.clone(),
            control: request.turn_control.clone(),
            allowed_tools: allowed_tools.clone(),
            _resource_lease: request.resource_lease.clone(),
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
        if let Err(error) = pending.check_admission(ProviderOperation::InputSubmit) {
            state.callbacks.retire_all(&state.binding.id).await?;
            return Err(error);
        }
        let (accepted, acceptance) = tokio::sync::oneshot::channel();
        let turn = state.turn.clone();
        let task_state = state.clone();
        let grant_scope = request.scope.clone();
        let invocation_scope = request.scope.clone();
        let collecting_submission = submission_id.clone();
        state
            .inputs
            .lock()
            .await
            .insert(submission_id.clone(), (request.scope.clone(), attempt_id));
        let options = super::prompt::TurnOptions {
            workspace: session
                .workspace_path()
                .map(|path| path.to_string_lossy().into_owned())
                .unwrap_or_default(),
            model: selected_model,
            capability: self.descriptor().capability,
            input: request.input,
            task: request.task,
            recovered: request.recovered_inputs,
            instructions: request.instructions,
            repair_caution,
            attachments: forge_provider_api::input::MessageInput {
                prompt: String::new(),
                attachments: request.attachments,
            },
            receipt: Some(request.receipt),
            dispatch: Some(request.dispatch),
            submission_id: Some(submission_id.clone()),
            background_compaction_observed: true,
            scope: request.scope.clone(),
            accepted: Some(accepted),
        };
        let control: Arc<CallbackRouter> = state.callbacks.clone();
        let (activated, activation) = tokio::sync::oneshot::channel();
        let (registered, registration) = tokio::sync::oneshot::channel();
        let native_operation = super::operation::current();
        let dispatch_evidence = native_operation.clone();
        let task = tokio::spawn(super::operation::scope(native_operation, async move {
            if registration.await.is_err() {
                return Err(ProviderError::Cancelled);
            }
            let guard = match turn.activate(Some(event_sink.clone()), None) {
                Ok(guard) => {
                    let _ = activated.send(Ok(()));
                    guard
                }
                Err(_) => {
                    let error = failure(
                        ProviderOperation::InputSubmit,
                        ProviderFailureKind::ExecutionActive,
                    );
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
            task_state
                .callbacks
                .finish_foreground_scope(&grant_scope, callback_generation)
                .await;
            drop(guard);
            match result {
                Ok(_) | Err(ProviderError::Cancelled) => Ok(()),
                Err(error) => Err(error),
            }
        }));
        let collecting = state.clone();
        let collect_scope = invocation_scope.clone();
        let collector = tokio::spawn(async move {
            let outcome = task
                .await
                .map_err(|_| failure(ProviderOperation::Cancel, ProviderFailureKind::Runtime))
                .and_then(|result| result);
            let never_dispatched = dispatch_evidence
                .as_ref()
                .is_some_and(|operation| !operation.dispatched());
            if matches!(outcome, Err(ProviderError::NotConsumed)) || never_dispatched {
                collecting
                    .unconsumed
                    .lock()
                    .await
                    .insert(collecting_submission);
            }
            collecting.callbacks.revoke();
            collecting.callbacks.collect_callbacks().await;
            collecting
                .callbacks
                .retire_all(&collecting.binding.id)
                .await?;
            let mut settlement = match &outcome {
                Ok(()) => collecting
                    .turn
                    .settlement_result()
                    .map_err(|_| failure(ProviderOperation::Cancel, ProviderFailureKind::Protocol))
                    .map(|result| InvocationSettlement {
                        binding_id: collecting.binding.id.clone(),
                        scope: collect_scope.clone(),
                        result,
                        watermark: collecting.sink.watermark(),
                    }),
                Err(ProviderError::Retryable(retryable)) => Ok(InvocationSettlement {
                    binding_id: collecting.binding.id.clone(),
                    scope: collect_scope.clone(),
                    result: forge_provider_api::ExecutionResult::Failed {
                        completion: collecting.turn.completion().unwrap_or_default().into(),
                        failure: forge_provider_api::ExecutionFailure::Retryable {
                            address: retryable.address.clone(),
                            category: retryable.category,
                        },
                    },
                    watermark: collecting.sink.watermark(),
                }),
                Err(ProviderError::NotConsumed) => Ok(InvocationSettlement {
                    binding_id: collecting.binding.id.clone(),
                    scope: collect_scope.clone(),
                    result: forge_provider_api::ExecutionResult::Failed {
                        completion: forge_provider_api::completion::ProviderCompletion::default()
                            .into(),
                        failure: forge_provider_api::ExecutionFailure::Rejected,
                    },
                    watermark: collecting.sink.watermark(),
                }),
                Err(_) if never_dispatched => Ok(InvocationSettlement {
                    binding_id: collecting.binding.id.clone(),
                    scope: collect_scope.clone(),
                    result: forge_provider_api::ExecutionResult::Failed {
                        completion: forge_provider_api::completion::ProviderCompletion::default()
                            .into(),
                        failure: forge_provider_api::ExecutionFailure::Rejected,
                    },
                    watermark: collecting.sink.watermark(),
                }),
                Err(error) => Err(error.clone()),
            };
            if let Ok(receipt) = &settlement {
                match collecting
                    .publish_invocation_settlement(receipt.clone())
                    .await
                {
                    Ok(published) => settlement = Ok(published),
                    Err(error) => settlement = Err(error),
                }
            }
            collecting
                .settlements
                .lock()
                .await
                .insert(collect_scope.clone(), settlement.clone());
            match &settlement {
                Ok(_) => {}
                Err(error) => {
                    collecting.closing.store(true, Ordering::Release);
                    let failure = ProviderFailure::new(
                        ProviderOperation::Cancel,
                        super::error::trace_failure_kind(error),
                    )
                    .at_phase(
                        forge_provider_api::ProviderPhase::Collection,
                        forge_provider_api::FailureDisposition::Invalidated,
                    );
                    let _ = collecting
                        .publish_invocation_settlement_failure(&collect_scope, failure)
                        .await;
                }
            }
            collecting.settled.notify_waiters();
            settlement.map(|_| ())
        });
        *state.active.lock().await = Some(ActiveExecution {
            scope: invocation_scope,
            task: collector,
        });
        // Native cancellation now has a stable collector; do not wait for activation or acceptance.
        pending.registered();
        let _ = registered.send(());
        match activation.await {
            Ok(Ok(())) => {}
            Ok(Err(())) => {
                return Err(failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::ExecutionActive,
                ));
            }
            Err(_) => {
                return Err(failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::Runtime,
                ));
            }
        }
        drop(_admission);
        match tokio::time::timeout(std::time::Duration::from_secs(30), acceptance).await {
            Ok(Ok(Ok(()))) => {}
            Ok(Ok(Err(error))) => return Err(error),
            Ok(Err(_)) | Err(_) => return Err(ProviderError::DeliveryUncertain),
        }
        Ok(InputAcceptance { submission_id })
    }
}

/// Register allocation ownership before any fallible binding or native preparation.
fn make_profile(
    profiles: &std::sync::Mutex<Vec<ProfileSlot>>,
    root: &Path,
    request: &SessionRequest,
) -> Result<RetainedResources, ProviderError> {
    let operation = super::operation::current()
        .map_or(ProviderOperation::SessionOpen, |operation| {
            operation.operation
        });
    prepare_profile(
        profiles,
        root,
        request.resource_lease.clone(),
        operation,
        |profile| profile.bind_skills(&request.configuration.resources.skills, "skills"),
    )
}

/// Keep failed preparation and independent cleanup causes without surrendering the allocation.
fn prepare_profile(
    profiles: &std::sync::Mutex<Vec<ProfileSlot>>,
    root: &Path,
    lease: Option<Arc<dyn ResourceLease>>,
    operation: ProviderOperation,
    prepare: impl FnOnce(
        &mut crate::profile::ResourceProfile,
    ) -> Result<(), crate::profile::ProfileError>,
) -> Result<RetainedResources, ProviderError> {
    let profile = crate::profile::ResourceProfile::create(root, "copilot")
        .map_err(|error| error.into_provider_error(operation))?;
    let slot = Arc::new(std::sync::Mutex::new(ProfileContents {
        profile: Some(profile),
        lease,
    }));
    profiles
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .push(slot.clone());
    let owner = RetainedProfile { slot };
    let prepared = {
        let mut retained = owner
            .slot
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        prepare(
            retained
                .profile
                .as_mut()
                .expect("allocated profile is registered before preparation"),
        )
    };
    if let Err(error) = prepared {
        let cause = error.into_provider_error(operation);
        return Err(match owner.close(operation) {
            Ok(()) => cause,
            Err(cleanup) => ProviderError::PreparationCleanup {
                cause: Box::new(cause),
                cleanup: Box::new(cleanup),
            },
        });
    }
    Ok(RetainedResources { _profile: owner })
}

/// Retry released native profiles without closing allocations still leased by a binding.
fn collect_profiles(
    profiles: &std::sync::Mutex<Vec<ProfileSlot>>,
    operation: ProviderOperation,
) -> Result<(), ProviderError> {
    let mut retained = profiles
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let mut failures = Vec::new();
    retained.retain(|slot| {
        if slot
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .profile
            .is_none()
        {
            return false;
        }
        if Arc::strong_count(slot) > 1 {
            failures.push(failure(operation, ProviderFailureKind::ResourceConflict));
            return true;
        }
        match close_profile_slot(slot, operation) {
            Ok(()) => false,
            Err(error) => {
                failures.push(error);
                true
            }
        }
    });
    if failures.is_empty() {
        return Ok(());
    }
    let primary = failures.remove(0);
    Err(if failures.is_empty() {
        primary
    } else {
        ProviderError::Combined {
            primary: Box::new(primary),
            secondary: failures,
        }
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
            ProviderOperation::SessionConfigure,
            ProviderFailureKind::Rejected,
        )
    })?;
    let selected_id = selection.model.as_deref().unwrap_or("auto");
    let model = models
        .iter()
        .find(|model| model.id == selected_id)
        .ok_or_else(|| {
            failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::Rejected,
            )
        })?;
    if selection.service_tier != forge_provider_api::ServiceTier::Standard
        && !model.service_tiers.contains(&selection.service_tier)
    {
        return Err(failure(
            ProviderOperation::SessionConfigure,
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
                ProviderOperation::SessionConfigure,
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

/// Collect prior native message receipts without consuming the currently reconciled submission.
fn accepted_message_ids(
    submission_id: &forge_provider_api::SubmissionId,
    attempt_id: &forge_provider_api::DeliveryAttemptId,
    owner: &forge_provider_api::ExecutionOwner,
    session_id: &str,
    accepted_receipts: &[RecoveryToken],
) -> Result<std::collections::HashSet<String>, ProviderError> {
    let mut known_receipts = std::collections::HashSet::new();
    for accepted in accepted_receipts {
        if let super::prompt::NativeRecoveryEvidence::Accepted {
            version,
            attempt_id: accepted_attempt,
            submission_id: accepted_submission,
            owner: accepted_owner,
            session_id: accepted_session,
            message_id,
        } = super::prompt::decode_recovery_token(accepted)?
            && version == 2
            && (accepted_submission != *submission_id || accepted_attempt != *attempt_id)
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

    pub(super) struct Authority;

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
                allowed_tools: HashSet::from(["fixture_tool".into()]),
                _resource_lease: None,
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
                        id: forge_provider_api::control::ToolCallId::new("fixture-call-1")
                            .expect("fixture call identity is nonempty"),
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
        let generation = install(&router, &scope).await;
        for id in ["first", "second"] {
            router.bind_native_call(id, &scope).await;
            call(&router, id).await;
            router.release_native_call(id).await;
            router.before_send(&scope).await.unwrap();
            router.status(&scope).await.unwrap();
        }
        router.finish_foreground_scope(&scope, generation).await;
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
        let generation = install(&router, &scope).await;
        router.bind_native_call("late", &scope).await;
        router.finish_foreground_scope(&scope, generation).await;
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

    struct PausedBarrier {
        release: Mutex<Option<tokio::sync::oneshot::Receiver<()>>>,
    }

    #[async_trait]
    impl ProviderPermissions for PausedBarrier {
        /// Reject permission prompts while this fixture exercises the pre-effect barrier.
        async fn authorize(
            &self,
            _: &forge_provider_api::InvocationScope,
            _: PermissionRequest,
        ) -> PermissionResponse {
            PermissionResponse::Cancelled
        }
        /// Hold an admitted callback until revocation has crossed its effect boundary.
        async fn before_tool(
            &self,
            _: &forge_provider_api::InvocationScope,
        ) -> Result<(), ProviderError> {
            let release = self
                .release
                .lock()
                .await
                .take()
                .expect("fixture enters its barrier once");
            release.await.map_err(|_| ProviderError::Cancelled)
        }
    }

    /// Revocation is immediate, collects admitted callbacks, and blocks effects after permission waits.
    #[tokio::test]
    async fn callback_revocation_collects_admitted_work_and_blocks_late_effects() {
        let router = Arc::new(CallbackRouter::default());
        let scope = forge_provider_api::InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("revocation"),
            "invocation",
        );
        install(&router, &scope).await;
        let (release, barrier) = tokio::sync::oneshot::channel();
        let mut callbacks = router.callbacks(&scope).await.unwrap();
        callbacks.permissions = Arc::new(PausedBarrier {
            release: Mutex::new(Some(barrier)),
        });
        router.install(callbacks).await.unwrap();
        router.bind_native_call("held", &scope).await;
        let callback = router.call_native(
            "held",
            ToolCall {
                id: forge_provider_api::control::ToolCallId::new("held").unwrap(),
                name: "fixture_tool".into(),
                arguments: json!({}),
            },
        );
        tokio::pin!(callback);
        assert!(futures_util::poll!(callback.as_mut()).is_pending());
        router.revoke();
        assert!(router.begin_callback().is_err());
        let collecting = router.collect_callbacks();
        tokio::pin!(collecting);
        assert!(futures_util::poll!(collecting.as_mut()).is_pending());
        release.send(()).unwrap();
        assert!(
            callback.await.is_err(),
            "revocation allowed a tool effect after its barrier resumed"
        );
        collecting.await;
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
        let current_attempt =
            forge_provider_api::DeliveryAttemptId::new("current-attempt").unwrap();
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
                if message == "current-message" {
                    current_attempt.clone()
                } else {
                    forge_provider_api::DeliveryAttemptId::new("accepted-attempt").unwrap()
                },
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
                current.clone(),
                owner.clone(),
                "native-session",
                "older-current-message",
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
        let known = accepted_message_ids(
            &current,
            &current_attempt,
            &owner,
            "native-session",
            &receipts,
        )
        .unwrap();
        assert_eq!(
            known,
            [
                String::from("claimed-message"),
                String::from("older-current-message")
            ]
            .into_iter()
            .collect()
        );

        let target = super::super::prompt::sending_recovery_token(
            owner,
            current,
            forge_provider_api::DeliveryAttemptId::new("target-attempt").unwrap(),
            super::super::prompt::NativeDeliveryBoundary {
                session_id: "native-session".into(),
                cursor: "cursor".into(),
                anchor: Some("before-input".into()),
            },
            "workspace".into(),
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

#[cfg(test)]
mod retained_recovery_tests {
    use super::*;
    use forge_provider_api::ProviderEvent;
    use forge_provider_api::provider::Provider;
    use sha2::Digest;

    /// Allocate explicit bounded operation authority for one deterministic fixture command.
    fn context(id: &str) -> OperationContext {
        let (_, cancellation) = forge_provider_api::OperationCancellation::channel();
        OperationContext::new(
            forge_provider_api::OperationId::new(id).unwrap(),
            std::time::Duration::from_secs(10),
            cancellation,
        )
        .unwrap()
    }

    struct QueuedReceipt {
        id: forge_provider_api::SubmissionId,
        attempt: forge_provider_api::DeliveryAttemptId,
        sending: AtomicU64,
    }

    #[async_trait]
    impl forge_provider_api::submission::InputReceipt for QueuedReceipt {
        fn id(&self) -> &forge_provider_api::SubmissionId {
            &self.id
        }
        fn attempt_id(&self) -> Result<forge_provider_api::DeliveryAttemptId, ProviderError> {
            Ok(self.attempt.clone())
        }
        async fn record_sending(
            self: Arc<Self>,
            _: Option<RecoveryToken>,
        ) -> Result<(), ProviderError> {
            self.sending.fetch_add(1, Ordering::AcqRel);
            Ok(())
        }
        async fn record_accepted(
            self: Arc<Self>,
            _: Option<RecoveryToken>,
        ) -> Result<(), ProviderError> {
            panic!("cancelled preparation accepted native input")
        }
        async fn record_rejected(self: Arc<Self>, _: ProviderError) -> Result<(), ProviderError> {
            Ok(())
        }
        async fn record_failure(self: Arc<Self>, _: ProviderError) -> Result<(), ProviderError> {
            Ok(())
        }
    }

    struct QueuedHostEffects;
    #[async_trait]
    impl forge_provider_api::session::ProviderHostEffects for QueuedHostEffects {
        async fn settle_foreground(
            &self,
            _: &BindingId,
            _: &forge_provider_api::InvocationScope,
        ) -> Result<(), forge_provider_api::session::HostEffectFailure> {
            panic!("preparation cancellation waited for unrelated host effects")
        }
    }

    fn queued_request(
        binding: &BindingId,
        scope: forge_provider_api::InvocationScope,
        configuration: SessionConfiguration,
        receipt: Arc<QueuedReceipt>,
    ) -> PromptRequest {
        let (dispatch, _) = forge_provider_api::submission::DispatchReporter::channel(
            binding.clone(),
            scope.clone(),
            receipt.id.clone(),
            receipt.attempt.clone(),
        );
        let authority = Arc::new(super::callback_tests::Authority);
        PromptRequest {
            scope,
            mode: forge_provider_api::PromptMode::Chat,
            configuration: forge_provider_api::session::InvocationConfiguration {
                model: configuration.model,
                access: configuration.access,
                approval: configuration.approval,
            },
            task: None,
            tools: Vec::new(),
            tool_endpoint: None,
            resource_lease: None,
            submission_id: receipt.id.clone(),
            attempt_id: receipt.attempt.clone(),
            input: forge_provider_api::PromptInput::Text("never dispatch".into()),
            dispatch,
            recovered_inputs: Vec::new(),
            instructions: String::new(),
            attachments: Vec::new(),
            receipt,
            permissions: authority.clone(),
            provider_tools: authority.clone(),
            turn_control: authority,
            host_effects: Arc::new(QueuedHostEffects),
        }
    }

    fn assert_admission_failure<T>(
        result: Result<T, ProviderError>,
        operation: ProviderOperation,
        kind: ProviderFailureKind,
    ) {
        let error = match result {
            Err(ProviderError::Failure(error)) => error,
            _ => panic!("precondition did not preserve its typed provider failure"),
        };
        assert_eq!(error.operation(), operation);
        assert_eq!(error.kind(), kind);
        assert_eq!(error.phase(), forge_provider_api::ProviderPhase::Admission);
        let expected = if matches!(
            kind,
            ProviderFailureKind::BindingClosed | ProviderFailureKind::BindingInvalidated
        ) {
            forge_provider_api::FailureDisposition::Invalidated
        } else {
            forge_provider_api::FailureDisposition::Unchanged
        };
        assert_eq!(error.disposition(), expected);
    }

    /// Input preconditions preserve exact lifecycle and immutable-target causes without native traffic.
    #[tokio::test]
    async fn input_preconditions_report_exact_admission_causes_before_dispatch() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, opened, _root, peer) = opened_fixture(Arc::new(std::sync::atomic::AtomicBool::new(false)), None, AbortAcknowledgement::Success, None).await;
            let state = provider.binding(&opened.binding.id, ProviderOperation::InputSubmit).await.unwrap();
            let configuration = state.request.lock().await.configuration.clone();
            for index in 0..7 {
                let scope = forge_provider_api::InvocationScope::new(state.owner.clone(), format!("precondition-{index}"));
                let receipt = Arc::new(QueuedReceipt { id: forge_provider_api::SubmissionId::try_from(format!("input-{index}")).unwrap(), attempt: forge_provider_api::DeliveryAttemptId::new(format!("attempt-{index}")).unwrap(), sending: AtomicU64::new(0) });
                let mut request = queued_request(&opened.binding.id, scope.clone(), configuration.clone(), receipt.clone());
                let expected = match index {
                    0 => { state.closing.store(true, Ordering::Release); ProviderFailureKind::BindingClosed }
                    1 => { state.unsafe_history.store(true, Ordering::Release); ProviderFailureKind::BindingInvalidated }
                    2 => { request.scope.owner = forge_provider_api::ExecutionOwner::root("foreign"); ProviderFailureKind::WrongInvocation }
                    3 => { request.submission_id = forge_provider_api::SubmissionId::try_from("different-input").unwrap(); ProviderFailureKind::WrongInvocation }
                    4 => { request.attempt_id = forge_provider_api::DeliveryAttemptId::new("different-attempt").unwrap(); ProviderFailureKind::WrongInvocation }
                    5 => {
                        request.dispatch = forge_provider_api::submission::DispatchReporter::channel(BindingId::new("foreign-binding").unwrap(), scope, receipt.id.clone(), receipt.attempt.clone()).0;
                        ProviderFailureKind::WrongInvocation
                    }
                    6 => {
                        *state.active.lock().await = Some(ActiveExecution { scope, task: tokio::spawn(std::future::pending::<Result<(), ProviderError>>()) });
                        ProviderFailureKind::ExecutionActive
                    }
                    _ => unreachable!("all fixture preconditions are enumerated"),
                };
                assert_admission_failure(provider.input_submit(context(&format!("submit-precondition-{index}")), &opened.binding.id, request).await, ProviderOperation::InputSubmit, expected);
                assert_eq!(receipt.sending.load(Ordering::Acquire), 0);
                state.closing.store(false, Ordering::Release); state.unsafe_history.store(false, Ordering::Release);
                if let Some(active) = state.active.lock().await.take() { active.task.abort(); let _ = active.task.await; }
            }
            let steering = SteeringRequest { scope: forge_provider_api::InvocationScope::new(state.owner.clone(), "inactive"), delivery: forge_provider_api::SteeringDelivery::Context, input: forge_provider_api::PromptInput::Text("never steer".into()), attachments: Vec::new(), task: None };
            assert!(matches!(provider.input_steer(context("inactive-steer"), &opened.binding.id, steering.clone()).await, Err(ProviderError::NotConsumed)));
            state.closing.store(true, Ordering::Release);
            assert_admission_failure(provider.input_steer(context("closed-steer"), &opened.binding.id, steering.clone()).await, ProviderOperation::InputSteer, ProviderFailureKind::BindingClosed);
            state.closing.store(false, Ordering::Release); state.unsafe_history.store(true, Ordering::Release);
            assert_admission_failure(provider.input_steer(context("invalid-steer"), &opened.binding.id, steering.clone()).await, ProviderOperation::InputSteer, ProviderFailureKind::BindingInvalidated);
            state.unsafe_history.store(false, Ordering::Release);
            let mut foreign = steering.clone(); foreign.scope.owner = forge_provider_api::ExecutionOwner::root("foreign");
            assert_admission_failure(provider.input_steer(context("foreign-steer"), &opened.binding.id, foreign).await, ProviderOperation::InputSteer, ProviderFailureKind::WrongInvocation);
            *state.active.lock().await = Some(ActiveExecution { scope: forge_provider_api::InvocationScope::new(state.owner.clone(), "other-active"), task: tokio::spawn(std::future::pending::<Result<(), ProviderError>>()) });
            assert_admission_failure(provider.input_steer(context("wrong-scope-steer"), &opened.binding.id, steering).await, ProviderOperation::InputSteer, ProviderFailureKind::WrongInvocation);
            if let Some(active) = state.active.lock().await.take() { active.task.abort(); let _ = active.task.await; }
            provider.session_close(context("precondition-close"), &opened.binding.id).await.unwrap();
            let closed_scope = forge_provider_api::InvocationScope::new(state.owner.clone(), "collected");
            let closed_receipt = Arc::new(QueuedReceipt { id: forge_provider_api::SubmissionId::try_from("closed-input").unwrap(), attempt: forge_provider_api::DeliveryAttemptId::new("closed-attempt").unwrap(), sending: AtomicU64::new(0) });
            let closed_prompt = queued_request(&opened.binding.id, closed_scope.clone(), configuration.clone(), closed_receipt.clone());
            let closed_steer = SteeringRequest { scope: closed_scope, delivery: forge_provider_api::SteeringDelivery::Context, input: forge_provider_api::PromptInput::Text("cannot reopen".into()), attachments: Vec::new(), task: None };
            let late_submit = provider.input_submit(context("collected-submit"), &opened.binding.id, closed_prompt).await.err().expect("collected binding admitted input");
            let late_steer = provider.input_steer(context("collected-steer"), &opened.binding.id, closed_steer).await.err().expect("collected binding admitted steering");
            for (operation, cause) in [(ProviderOperation::InputSubmit, late_submit), (ProviderOperation::InputSteer, late_steer)] {
                assert!(matches!(cause, ProviderError::Failure(error) if error.operation() == operation
                    && error.kind() == ProviderFailureKind::BindingClosed && error.phase() == forge_provider_api::ProviderPhase::Admission
                    && error.disposition() == forge_provider_api::FailureDisposition::Closed));
            }
            assert_eq!(closed_receipt.sending.load(Ordering::Acquire), 0);
            provider.shutdown(context("precondition-shutdown")).await.unwrap();
            let requests = peer.collected().await;
            assert_eq!(requests.iter().filter(|request| request["method"] == "models.list").count(), 1);
            assert!(!requests.iter().any(|request| matches!(request["method"].as_str(), Some("session.send" | "session.abort"))));
        }).await.expect("typed Copilot precondition fixture timed out");
    }

    /// Exact cancellation joins a queued Store worker despite discarded replay candidates.
    #[tokio::test]
    async fn cancellation_waits_for_queued_input_preparation_and_prevents_late_handoff() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, opened, _root, peer) = opened_fixture(Arc::new(std::sync::atomic::AtomicBool::new(false)), None, AbortAcknowledgement::Success, None).await;
            let state = provider.binding(&opened.binding.id, ProviderOperation::InputSubmit).await.unwrap();
            let configuration = state.request.lock().await.configuration.clone();
            let scope = forge_provider_api::InvocationScope::new(state.owner.clone(), "queued-input");
            let receipt = Arc::new(QueuedReceipt {
                id: forge_provider_api::SubmissionId::try_from("queued-submission").unwrap(),
                attempt: forge_provider_api::DeliveryAttemptId::new("queued-attempt").unwrap(), sending: AtomicU64::new(0),
            });
            let request = queued_request(&opened.binding.id, scope.clone(), configuration, receipt.clone());
            let operation = context("queued-submit");
            let admission = state.admission.lock().await;
            let submitting = provider.input_submit(operation.clone(), &opened.binding.id, request.clone());
            tokio::pin!(submitting);
            assert!(futures_util::poll!(submitting.as_mut()).is_pending());
            let mut replay = Box::pin(provider.input_submit(operation, &opened.binding.id, request));
            assert!(futures_util::poll!(replay.as_mut()).is_pending());
            drop(replay);
            let cancelling = provider.cancel(context("queued-cancel"), &opened.binding.id, CancelRequest { scope: scope.clone() });
            tokio::pin!(cancelling);
            assert!(futures_util::poll!(cancelling.as_mut()).is_pending());
            loop {
                let probe = provider.pending_inputs.register(&opened.binding.id, &scope, context("probe").id());
                let prevented = probe.is_cancelled(); drop(probe);
                if prevented { break; }
                tokio::task::yield_now().await;
            }
            assert!(matches!(cancelling.await, Err(ProviderError::Failure(error)) if error.kind() == ProviderFailureKind::WrongInvocation));
            assert!(matches!(submitting.await, Err(ProviderError::PreparationFailed(cause))
                if matches!(cause.as_ref(), ProviderError::Failure(error) if error.kind() == ProviderFailureKind::Cancelled
                && error.disposition() == forge_provider_api::FailureDisposition::Unchanged)));
            // Cancellation and no-future proof completed without releasing the unrelated lock.
            provider.operations.submit.join_all().await.unwrap();
            drop(admission);
            assert_eq!(receipt.sending.load(Ordering::Acquire), 0);
            assert!(!state.closing.load(Ordering::Acquire));
            let successor = provider.pending_inputs.register(&opened.binding.id, &forge_provider_api::InvocationScope::new(state.owner.clone(), "queued-input"), context("successor").id());
            successor.check_admission(ProviderOperation::InputSubmit).unwrap(); drop(successor);
            provider.session_close(context("queued-close"), &opened.binding.id).await.unwrap();
            provider.shutdown(context("queued-shutdown")).await.unwrap();
            let requests = peer.collected().await;
            assert!(!requests.iter().any(|request| matches!(request["method"].as_str(), Some("session.send" | "session.abort"))));
            assert_eq!(requests.iter().filter(|request| request["method"] == "models.list").count(), 1, "prevented preparation entered native model admission");
        }).await.expect("queued Copilot cancellation fixture timed out");
    }

    /// Failed binding keeps its allocated profile and independent cleanup failure owned until shutdown.
    #[tokio::test]
    async fn failed_profile_preparation_retains_cleanup_for_shutdown_retry() {
        let root = tempfile::tempdir().unwrap();
        let source = root.path().join("source");
        std::fs::create_dir(&source).unwrap();
        let source_file = source.join("SKILL.md");
        std::fs::write(&source_file, "preserve source").unwrap();
        let provider = CopilotProvider::new(Vec::new(), root.path().to_owned()).unwrap();
        let prepared = prepare_profile(
            &provider.profiles,
            root.path(),
            None,
            ProviderOperation::SessionOpen,
            |profile| {
                assert_eq!(
                    provider.profiles.lock().unwrap().len(),
                    1,
                    "allocation was not owned before fallible binding"
                );
                profile.bind(&source, "blocked")?;
                let linked = profile.path().join("blocked");
                #[cfg(windows)]
                {
                    assert_eq!(
                        dunce::canonicalize(junction::get_target(&linked).unwrap()).unwrap(),
                        dunce::canonicalize(&source).unwrap()
                    );
                    junction::delete(&linked).unwrap();
                    // Removing the reparse point leaves an ordinary directory that blocks link cleanup.
                    assert!(std::fs::read_link(&linked).is_err());
                    assert!(linked.is_dir());
                }
                #[cfg(unix)]
                {
                    std::fs::remove_file(&linked).unwrap();
                    std::fs::create_dir(&linked).unwrap();
                }
                Err(crate::profile::ProfileError::MissingSkillSource)
            },
        );
        assert!(
            matches!(prepared, Err(ProviderError::PreparationCleanup { cause, cleanup })
            if matches!(*cause, ProviderError::Failure(error) if error.kind() == ProviderFailureKind::Rejected)
            && matches!(*cleanup, ProviderError::Failure(error) if error.kind() == ProviderFailureKind::Runtime))
        );
        let profile = {
            let profiles = provider.profiles.lock().unwrap();
            let retained = profiles[0].lock().unwrap();
            retained.profile.as_ref().unwrap().path().to_owned()
        };
        assert!(profile.exists());
        std::fs::remove_dir(profile.join("blocked")).unwrap();
        provider
            .shutdown(context("profile-shutdown"))
            .await
            .unwrap();
        assert!(
            !profile.exists(),
            "shutdown did not retry the retained profile cleanup"
        );
        assert!(provider.profiles.lock().unwrap().is_empty());
        assert_eq!(
            std::fs::read_to_string(source_file).unwrap(),
            "preserve source"
        );
    }

    /// Shared text generation's sandbox request is enforced by zero native authority, not tool sandbox support.
    #[tokio::test]
    async fn isolated_text_accepts_shared_access_policy_without_enabling_native_authority() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (reader, writer, peer) = super::super::test_peer::streams(|request| {
                let result = match request["method"].as_str().unwrap() {
                    "status.get" => json!({"version":"1.0.103"}),
                    "models.list" => json!({"models":[{"id":"auto","name":"Auto","capabilities":{}}]}),
                    "mcp.discover" | "session.mcp.listConfigured" => json!({"servers":[]}),
                    "session.skills.list" => json!({"skills":[]}),
                    "session.tasks.refresh" | "session.delete" => json!({}),
                    "session.tasks.list" => json!({"tasks":[]}),
                    "session.create" => {
                        let params = &request["params"];
                        for key in ["enableConfigDiscovery", "enableSkills", "enableFileHooks", "hooks", "enableSessionStore"] {
                            assert_eq!(params[key], false, "isolated admission enabled {key}");
                        }
                        assert_eq!(params["availableTools"], json!([]));
                        assert_eq!(params["tools"], json!([]));
                        assert_eq!(params["mcpServers"], json!({}));
                        assert_eq!(params["systemMessage"]["mode"], "replace");
                        json!({"sessionId":params["sessionId"],"workspacePath":std::env::temp_dir()})
                    }
                    "session.options.update" => {
                        assert_eq!(request["params"]["skipCustomInstructions"], true);
                        json!({"success":true})
                    }
                    "session.detach" => json!({"success":true}),
                    method => panic!("unexpected native authority during isolated open: {method}"),
                };
                vec![super::super::test_peer::response(request, result)]
            });
            let root = tempfile::tempdir().unwrap();
            let provider = CopilotProvider::new(Vec::new(), root.path().to_owned()).unwrap();
            provider.runtime.attach_streams(reader, writer, root.path().to_owned()).await.unwrap();
            let access = forge_provider_api::access::AccessPolicy {
                write_access: forge_provider_api::access::WriteScope::Off,
                ..Default::default()
            };
            assert!(provider.descriptor().capability.validate_access(&access).is_err(), "fixture must exercise the ordinary native-tool sandbox restriction");
            let opened = provider.session_open(context("shared-isolated-open"), SessionRequest {
                owner: forge_provider_api::ExecutionOwner::root("shared-isolated"), mode: SessionMode::Background,
                workspace: root.path().to_string_lossy().into_owned(), history: SessionHistory::IsolatedText(forge_provider_api::IsolatedTextPolicy::new()),
                configuration: SessionConfiguration { model: ModelSelection::default(), access, approval: forge_provider_api::approval::ApprovalMode::Deny, resources: Default::default() },
                resource_lease: None, tools: Vec::new(), tool_endpoint: None,
                system_instructions: "Generate isolated text".into(), trace: Arc::new(NullTrace),
            }).await.unwrap();
            assert!(opened.binding.provider_session_id.is_none());
            provider.session_close(context("shared-isolated-close"), &opened.binding.id).await.unwrap();
            provider.shutdown(context("shared-isolated-shutdown")).await.unwrap();
            let requests = peer.collected().await;
            assert_eq!(requests.iter().filter(|request| request["method"] == "session.create").count(), 1);
            assert_eq!(requests.iter().filter(|request| request["method"] == "session.options.update").count(), 2);
            assert!(requests.iter().all(|request| !matches!(request["method"].as_str(), Some("session.resume" | "session.send"))));
        }).await.expect("shared isolated access fixture timed out");
    }

    /// An unconfirmed native instruction policy never publishes a binding or admits input.
    #[tokio::test]
    async fn failed_isolated_instruction_confirmation_never_publishes_binding() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            for rpc_error in [false, true] {
                let mut patches = 0;
                let (reader, writer, peer) = super::super::test_peer::streams(move |request| {
                    let result = match request["method"].as_str().unwrap() {
                        "status.get" => json!({"version":"1.0.103"}),
                        "models.list" => json!({"models":[{"id":"auto", "name":"Auto", "capabilities":{}}]}),
                        "mcp.discover" => json!({"servers":[]}),
                        "session.create" => {
                            assert_eq!(request["params"]["enableConfigDiscovery"], false);
                            assert_eq!(request["params"]["hooks"], false);
                            json!({"sessionId":request["params"]["sessionId"], "workspacePath":std::env::temp_dir()})
                        }
                        "session.options.update" => {
                            assert_eq!(request["params"]["skipCustomInstructions"], true);
                            patches += 1;
                            if patches == 2 && rpc_error {
                                return vec![json!({"jsonrpc":"2.0", "id":request["id"], "error":{"code":-32601,"message":"instruction policy unsupported"}})];
                            }
                            // The SDK itself ignores this false result; the adapter must confirm it.
                            json!({"success":false})
                        }
                        "session.detach" => json!({"success":true}),
                        method => panic!("unexpected request before confirmed isolation: {method}"),
                    };
                    vec![super::super::test_peer::response(request, result)]
                });
                let root = tempfile::tempdir().unwrap();
                let provider = CopilotProvider::new(Vec::new(), root.path().to_owned()).unwrap();
                provider.runtime.attach_streams(reader, writer, root.path().to_owned()).await.unwrap();
                let access = forge_provider_api::access::AccessPolicy { write_access: forge_provider_api::access::WriteScope::Off, ..Default::default() };
                let result = provider.session_open(context("isolated-open"), SessionRequest {
                    owner: forge_provider_api::ExecutionOwner::root("isolated-confirmation"), mode: SessionMode::Background,
                    workspace: root.path().to_string_lossy().into_owned(), history: SessionHistory::IsolatedText(forge_provider_api::IsolatedTextPolicy::new()),
                    configuration: SessionConfiguration { model: ModelSelection::default(), access, approval: forge_provider_api::approval::ApprovalMode::Deny, resources: forge_provider_api::ResourceSelection::default() },
                    resource_lease: None, tools: Vec::new(), tool_endpoint: None, system_instructions: String::new(), trace: Arc::new(NullTrace),
                }).await;
                assert!(matches!(result, Err(ProviderError::Failure(error)) if error.kind() == ProviderFailureKind::Policy));
                assert!(provider.bindings.lock().await.is_empty());
                assert!(provider.owners.lock().await.is_empty());
                provider.shutdown(context("isolated-shutdown")).await.unwrap();
                let requests = peer.collected().await;
                assert_eq!(requests.iter().filter(|request| request["method"] == "session.options.update").count(), 2);
                assert_eq!(requests.iter().filter(|request| request["method"] == "session.detach").count(), 1);
                assert!(requests.iter().all(|request| request["method"] != "session.send"));
            }
        }).await.expect("isolated confirmation fixture timed out");
    }

    #[derive(Clone, Copy)]
    enum AbortAcknowledgement {
        Success,
        Rejected,
        TransportFailure,
    }

    /// Open the production adapter against an SDK peer that retains shell and abort state.
    async fn opened_fixture(
        shells: Arc<std::sync::atomic::AtomicBool>,
        mut aborted: Option<tokio::sync::oneshot::Sender<()>>,
        abort_acknowledgement: AbortAcknowledgement,
        mut terminal_ack: Option<(
            Arc<std::sync::atomic::AtomicBool>,
            tokio::sync::oneshot::Sender<()>,
        )>,
    ) -> (
        CopilotProvider,
        OpenedSession,
        tempfile::TempDir,
        super::super::test_peer::SdkPeer,
    ) {
        let mut held_terminal = None;
        let (reader, writer, peer) = super::super::test_peer::streams(move |request| {
            let result = match request["method"].as_str().unwrap() {
                "status.get" => json!({"version":"1.0.103"}),
                "models.list" => json!({"models":[{
                    "id":"auto", "name":"Auto",
                    "capabilities":{"limits":{"max_context_window_tokens":128000},"supports":{"vision":false}}
                }]}),
                "mcp.discover" | "session.mcp.listConfigured" => json!({"servers":[]}),
                "session.skills.list" => json!({"skills":[]}),
                "session.create" | "session.resume" => {
                    json!({"sessionId":request["params"]["sessionId"], "workspacePath":std::env::temp_dir()})
                }
                "session.skills.reload" => json!({}),
                "session.options.update" | "session.detach" => json!({"success":true}),
                "session.tasks.refresh" => {
                    if terminal_ack
                        .as_ref()
                        .is_some_and(|(release, _)| release.load(Ordering::Acquire))
                        && let Some(held) = held_terminal.take()
                    {
                        return vec![
                            super::super::test_peer::response(request, json!({})),
                            super::super::test_peer::response(&held, json!({"cancelled":true})),
                        ];
                    }
                    json!({})
                }
                "session.tasks.list" => {
                    if shells.load(Ordering::Acquire) {
                        json!({"tasks":[{"id":"background-shell", "type":"shell", "status":"running", "command":"fixture"}]})
                    } else {
                        json!({"tasks":[]})
                    }
                }
                "session.tasks.cancel" => {
                    shells.store(false, Ordering::Release);
                    if let Some((_, signal)) = terminal_ack.as_mut() {
                        let (replacement, _) = tokio::sync::oneshot::channel();
                        let signal = std::mem::replace(signal, replacement);
                        let _ = signal.send(());
                        held_terminal = Some(request.clone());
                        return Vec::new();
                    }
                    json!({"cancelled":true})
                }
                "session.abort" => {
                    if let Some(aborted) = aborted.take() {
                        let _ = aborted.send(());
                    }
                    match abort_acknowledgement {
                        AbortAcknowledgement::Success => json!({"success":true}),
                        AbortAcknowledgement::Rejected => {
                            json!({"success":false,"error":"fixture rejected abort"})
                        }
                        AbortAcknowledgement::TransportFailure => {
                            return vec![
                                json!({"jsonrpc":"2.0", "id":request["id"], "error":{"code":-32000,"message":"fixture abort acknowledgement lost"}}),
                            ];
                        }
                    }
                }
                method => panic!("unexpected lifecycle fixture method: {method}"),
            };
            vec![super::super::test_peer::response(request, result)]
        });
        let root = tempfile::tempdir().unwrap();
        let provider = CopilotProvider::new(Vec::new(), root.path().to_owned()).unwrap();
        provider
            .runtime
            .attach_streams(reader, writer, root.path().to_owned())
            .await
            .unwrap();
        let access = forge_provider_api::access::AccessPolicy {
            sandbox: false,
            ..Default::default()
        };
        let opened = provider
            .session_open(
                context("open"),
                SessionRequest {
                    owner: forge_provider_api::ExecutionOwner::root("lifecycle-fixture"),
                    mode: SessionMode::Interactive,
                    workspace: root.path().to_string_lossy().into_owned(),
                    history: SessionHistory::Durable { resume: None },
                    configuration: SessionConfiguration {
                        model: ModelSelection::default(),
                        access,
                        approval: forge_provider_api::approval::ApprovalMode::Deny,
                        resources: forge_provider_api::ResourceSelection::default(),
                    },
                    resource_lease: None,
                    tools: Vec::new(),
                    tool_endpoint: None,
                    system_instructions: String::new(),
                    trace: Arc::new(NullTrace),
                },
            )
            .await
            .unwrap();
        (provider, opened, root, peer)
    }

    /// Both invocation collection paths publish on a real retained binding stream without poisoning it.
    #[tokio::test]
    async fn binding_stream_scopes_settlement_and_collection_failure_to_exact_invocations() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, mut opened, _root, peer) = opened_fixture(
                Arc::new(std::sync::atomic::AtomicBool::new(false)),
                None,
                AbortAcknowledgement::Success,
                None,
            ).await;
            let state = provider.binding(&opened.binding.id, ProviderOperation::Cancel).await.unwrap();
            let completed_scope = forge_provider_api::InvocationScope::new(state.owner.clone(), "completed");
            let failed_scope = forge_provider_api::InvocationScope::new(state.owner.clone(), "failed-collection");
            let receipt = state.publish_invocation_settlement(InvocationSettlement {
                binding_id: state.binding.id.clone(),
                scope: completed_scope.clone(),
                result: forge_provider_api::ExecutionResult::Completed {
                    completion: forge_provider_api::completion::ProviderCompletion::default().into(),
                },
                watermark: state.sink.watermark(),
            }).await.unwrap();
            let event = opened.events.recv().await.unwrap().unwrap();
            assert_eq!(event.binding_id, state.binding.id);
            assert_eq!(event.sequence, receipt.watermark);
            assert!(matches!(event.event, ProviderEvent::Settled { settlement }
                if settlement.binding_id == receipt.binding_id && settlement.watermark == receipt.watermark
                && settlement.scope == completed_scope && matches!(settlement.result, forge_provider_api::ExecutionResult::Completed { .. })));
            let failure = ProviderFailure::new(ProviderOperation::Cancel, ProviderFailureKind::Timeout)
                .at_phase(forge_provider_api::ProviderPhase::Collection, forge_provider_api::FailureDisposition::Invalidated);
            state.publish_invocation_settlement_failure(&failed_scope, failure).await.unwrap();
            let event = opened.events.recv().await.unwrap().unwrap();
            assert!(matches!(event.event, ProviderEvent::SettlementFailed { scope, failure: received } if scope == failed_scope && received == failure));
            state.sink.send_wait(ProviderEvent::CapabilitiesChanged {
                scope: forge_provider_api::EventScope::Binding { owner: state.owner.clone() },
                capabilities: forge_provider_api::capability::CapabilityManifest::from_descriptor(provider.descriptor()),
            }).await.unwrap();
            assert!(matches!(opened.events.recv().await.unwrap().unwrap().event, ProviderEvent::CapabilitiesChanged { .. }));
            provider.session_close(context("scoped-settlement-close"), &opened.binding.id).await.unwrap();
            provider.shutdown(context("scoped-settlement-shutdown")).await.unwrap();
            peer.collected().await;
        }).await.expect("binding-scoped settlement fixture timed out");
    }

    /// A failed foreground join cannot skip independent shell termination and native retirement.
    #[tokio::test]
    async fn close_attempts_all_cleanup_after_foreground_collection_failure() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let shells = Arc::new(std::sync::atomic::AtomicBool::new(true));
            let (provider, opened, _root, peer) =
                opened_fixture(shells.clone(), None, AbortAcknowledgement::Success, None).await;
            let state = provider
                .binding(&opened.binding.id, ProviderOperation::SessionClose)
                .await
                .unwrap();
            let task = tokio::spawn(std::future::pending::<Result<(), ProviderError>>());
            task.abort();
            *state.active.lock().await = Some(ActiveExecution {
                scope: forge_provider_api::InvocationScope::new(
                    state.owner.clone(),
                    "failed-collection",
                ),
                task,
            });
            assert!(
                provider
                    .session_close(context("close-first"), &opened.binding.id)
                    .await
                    .is_err()
            );
            assert!(!shells.load(Ordering::Acquire));
            assert!(state.terminals_collected.load(Ordering::Acquire));
            assert!(state.native_retired.load(Ordering::Acquire));
            assert!(
                state.retained_resources.lock().await.is_some(),
                "failed close discarded its profile ownership"
            );
            assert!(
                provider
                    .bindings
                    .lock()
                    .await
                    .contains_key(&opened.binding.id)
            );
            provider
                .session_close(context("close-retry"), &opened.binding.id)
                .await
                .unwrap();
            provider.shutdown(context("shutdown")).await.unwrap();
            let requests = peer.collected().await;
            assert_eq!(
                requests
                    .iter()
                    .filter(|request| request["method"] == "session.tasks.cancel")
                    .count(),
                1
            );
            assert_eq!(
                requests
                    .iter()
                    .filter(|request| request["method"] == "session.detach")
                    .count(),
                1
            );
        })
        .await
        .expect("attempt-all close fixture timed out");
    }

    /// A rejected or lost abort acknowledgement remains the cancellation result after normal completion.
    #[tokio::test]
    async fn repeated_cancel_preserves_first_stop_failure_after_automatic_settlement() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            for abort_acknowledgement in [AbortAcknowledgement::Rejected, AbortAcknowledgement::TransportFailure] {
            let expected_failure = match abort_acknowledgement {
                AbortAcknowledgement::Rejected => ProviderFailureKind::Rejected,
                AbortAcknowledgement::TransportFailure => ProviderFailureKind::Transport,
                AbortAcknowledgement::Success => unreachable!("fixture covers unsuccessful acknowledgements"),
            };
            let (abort, aborted) = tokio::sync::oneshot::channel();
            let (provider, opened, _root, peer) = opened_fixture(Arc::new(std::sync::atomic::AtomicBool::new(false)), Some(abort), abort_acknowledgement, None).await;
            let state = provider.binding(&opened.binding.id, ProviderOperation::Cancel).await.unwrap();
            let scope = forge_provider_api::InvocationScope::new(state.owner.clone(), "racing-completion");
            let task_state = state.clone(); let task_scope = scope.clone();
            let (ready, running) = tokio::sync::oneshot::channel();
            let task = tokio::spawn(async move {
                let guard = task_state.turn.activate(None, None).unwrap(); let _ = ready.send(());
                aborted.await.unwrap();
                task_state.settlements.lock().await.insert(task_scope.clone(), Ok(InvocationSettlement {
                    binding_id: task_state.binding.id.clone(), scope: task_scope,
                    result: forge_provider_api::ExecutionResult::Completed { completion: forge_provider_api::completion::ProviderCompletion::default().into() }, watermark: task_state.sink.watermark(),
                }));
                drop(guard); Ok(())
            });
            running.await.unwrap();
            *state.active.lock().await = Some(ActiveExecution { scope: scope.clone(), task });
            for attempt in 0..2 {
                assert!(matches!(provider.cancel(context(&format!("cancel-{attempt}")), &opened.binding.id, CancelRequest { scope: scope.clone() }).await,
                    Err(ProviderError::Failure(failure)) if failure.kind() == expected_failure));
            }
            assert!(state.settlements.lock().await.get(&scope).is_some_and(Result::is_ok));
            provider.session_close(context("close-retry"), &opened.binding.id).await.unwrap(); provider.shutdown(context("shutdown")).await.unwrap();
            let requests = peer.collected().await;
            assert_eq!(requests.iter().filter(|request| request["method"] == "session.abort").count(), 1);
            }
        }).await.expect("retained cancellation result fixture timed out");
    }

    /// Retained replay keeps cancellation effects after a native replacement commits late.
    #[tokio::test]
    async fn interrupted_configuration_commit_retains_invalidated_disposition() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, opened, _root, peer) = opened_fixture(Arc::new(std::sync::atomic::AtomicBool::new(false)), None, AbortAcknowledgement::Success, None).await;
            let state = provider.binding(&opened.binding.id, ProviderOperation::SessionConfigure).await.unwrap();
            let mut configuration = state.request.lock().await.configuration.clone();
            configuration.approval = forge_provider_api::approval::ApprovalMode::Allow;
            // Hold only the post-native commit slot so cancellation races a proven replacement.
            let resources = state.retained_resources.lock().await;
            let original = resources.as_ref().unwrap().clone();
            let (source, cancellation) = forge_provider_api::OperationCancellation::channel();
            let operation = OperationContext::new(forge_provider_api::OperationId::new("configure-race").unwrap(), std::time::Duration::from_secs(60), cancellation).unwrap();
            let configuring = provider.session_configure(operation.clone(), &opened.binding.id, configuration.clone());
            tokio::pin!(configuring);
            assert!(futures_util::poll!(configuring.as_mut()).is_pending());
            loop {
                if provider.sessions.resources(&state.runtime_key).await.is_some_and(|current| !Arc::ptr_eq(&current, &original)) { break; }
                tokio::task::yield_now().await;
            }
            source.cancel();
            assert!(matches!(configuring.await, Err(ProviderError::Failure(error))
                if error.kind() == ProviderFailureKind::Cancelled && error.disposition() == forge_provider_api::FailureDisposition::Invalidated));
            assert!(state.callbacks.begin_callback().is_err());
            drop(resources);
            provider.operations.configure.join_all().await.unwrap();
            assert_eq!(state.request.lock().await.configuration, configuration, "fixture did not cross the native commit boundary");
            assert!(matches!(provider.session_configure(operation, &opened.binding.id, configuration).await, Err(ProviderError::Failure(error))
                if error.kind() == ProviderFailureKind::Cancelled && error.phase() == forge_provider_api::ProviderPhase::Observation
                && error.disposition() == forge_provider_api::FailureDisposition::Invalidated));
            provider.shutdown(context("configure-race-shutdown")).await.unwrap();
            let requests = peer.collected().await;
            assert_eq!(requests.iter().filter(|request| request["method"] == "session.resume").count(), 1);
        }).await.expect("configuration commit race fixture timed out");
    }

    /// Lost capability delivery after commit immediately revokes the changed binding.
    #[tokio::test]
    async fn committed_configuration_delivery_failure_invalidates_binding() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, opened, _root, peer) = opened_fixture(Arc::new(std::sync::atomic::AtomicBool::new(false)), None, AbortAcknowledgement::Success, None).await;
            let binding = opened.binding.id.clone();
            let state = provider.binding(&binding, ProviderOperation::SessionConfigure).await.unwrap();
            let mut configuration = state.request.lock().await.configuration.clone();
            configuration.approval = forge_provider_api::approval::ApprovalMode::Allow;
            drop(opened.events);
            let result = provider.session_configure(context("configure-delivery"), &binding, configuration.clone()).await;
            assert!(matches!(result, Err(ProviderError::Combined { primary, .. })
                if matches!(*primary, ProviderError::Failure(error) if error.operation() == ProviderOperation::SessionConfigure
                && error.phase() == forge_provider_api::ProviderPhase::Observation
                && error.disposition() == forge_provider_api::FailureDisposition::Invalidated)));
            assert_eq!(state.request.lock().await.configuration, configuration);
            assert!(state.closing.load(Ordering::Acquire));
            assert!(state.callbacks.begin_callback().is_err());
            provider.session_close(context("configure-delivery-close"), &binding).await.unwrap();
            provider.shutdown(context("configure-delivery-shutdown")).await.unwrap();
            let requests = peer.collected().await;
            assert_eq!(requests.iter().filter(|request| request["method"] == "session.resume").count(), 1);
        }).await.expect("configuration delivery fixture timed out");
    }

    /// A live terminal blocks destructive SDK refresh before model, profile, or observer changes.
    #[tokio::test]
    async fn configuration_rejects_live_terminals_before_native_refresh() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, opened, _root, peer) = opened_fixture(Arc::new(std::sync::atomic::AtomicBool::new(true)), None, AbortAcknowledgement::Success, None).await;
            let state = provider.binding(&opened.binding.id, ProviderOperation::SessionConfigure).await.unwrap();
            let original = state.request.lock().await.configuration.clone(); let native = state.session.read().await.id();
            let mut desired = original.clone(); desired.approval = forge_provider_api::approval::ApprovalMode::Allow;
            assert!(matches!(provider.session_configure(context("configure"), &opened.binding.id, desired).await,
                Err(ProviderError::Failure(error)) if error.operation() == ProviderOperation::SessionConfigure && error.kind() == ProviderFailureKind::ResourceConflict));
            assert_eq!(state.request.lock().await.configuration, original);
            assert_eq!(state.session.read().await.id(), native);
            assert!(!state.closing.load(Ordering::Acquire));
            provider.session_close(context("close-retry"), &opened.binding.id).await.unwrap(); provider.shutdown(context("shutdown")).await.unwrap();
            let requests = peer.collected().await;
            assert_eq!(requests.iter().filter(|request| request["method"] == "session.create").count(), 1);
            assert!(requests.iter().all(|request| request["method"] != "session.resume"));
        }).await.expect("terminal configuration preflight fixture timed out");
    }

    /// Shutdown revokes sibling callbacks before waiting for the first binding's mutation gate.
    #[tokio::test]
    async fn shutdown_revokes_all_native_callbacks_before_waiting_for_one_binding() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, first, _root, peer) = opened_fixture(
                Arc::new(std::sync::atomic::AtomicBool::new(false)),
                None,
                AbortAcknowledgement::Success,
                None,
            )
            .await;
            let first_state = provider
                .binding(&first.binding.id, ProviderOperation::Shutdown)
                .await
                .unwrap();
            let mut request = first_state.request.lock().await.clone();
            request.owner = forge_provider_api::ExecutionOwner::root("second-owner");
            let second = provider
                .session_open(context("second-open"), request)
                .await
                .unwrap();
            let second_state = provider
                .binding(&second.binding.id, ProviderOperation::Shutdown)
                .await
                .unwrap();
            let gate = first_state.admission.lock().await;
            let shutdown = provider.shutdown(context("shutdown"));
            tokio::pin!(shutdown);
            assert!(futures_util::poll!(shutdown.as_mut()).is_pending());
            while !provider.closed.load(Ordering::Acquire) {
                tokio::task::yield_now().await;
            }
            assert!(
                second_state.callbacks.begin_callback().is_err(),
                "sibling native admission survived provider shutdown"
            );
            drop(gate);
            shutdown.await.unwrap();
            peer.collected().await;
        })
        .await
        .expect("global native shutdown revocation fixture timed out");
    }

    /// Explicit interruption preserves owned terminal work and later exact replay returns its proven result.
    #[tokio::test]
    async fn interrupted_terminal_operation_retains_late_acknowledgement_and_exact_replay() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let release = Arc::new(std::sync::atomic::AtomicBool::new(false));
            let (requested, request_seen) = tokio::sync::oneshot::channel();
            let (provider, opened, _root, peer) = opened_fixture(Arc::new(std::sync::atomic::AtomicBool::new(true)), None, AbortAcknowledgement::Success, Some((release.clone(), requested))).await;
            let state = provider.binding(&opened.binding.id, ProviderOperation::TerminalTerminate).await.unwrap();
            let mut snapshot = forge_provider_api::terminal::TerminalSnapshot { supported: true, starting: false, terminal: vec![forge_provider_api::terminal::BackgroundTerminal {
                id: forge_provider_api::terminal::TerminalId::new("background-shell").unwrap(), command: "fixture".into(),
            }] };
            state.terminals.lock().await.issue(&mut snapshot).unwrap();
            let terminal = snapshot.terminal[0].id.clone();
            let (source, cancellation) = forge_provider_api::OperationCancellation::channel();
            let operation = OperationContext::new(forge_provider_api::OperationId::new("terminal-operation").unwrap(), std::time::Duration::from_secs(60), cancellation).unwrap();
            let stopping = provider.terminal_terminate(operation.clone(), &opened.binding.id, &terminal);
            tokio::pin!(stopping);
            assert!(futures_util::poll!(stopping.as_mut()).is_pending());
            request_seen.await.unwrap(); source.cancel();
            assert!(matches!(stopping.await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::Cancelled && failure.disposition() == forge_provider_api::FailureDisposition::Uncertain));
            assert!(!state.closing.load(Ordering::Acquire), "terminal cancellation closed unrelated foreground authority");
            release.store(true, Ordering::Release);
            let session = state.session.read().await.clone(); session.terminal_snapshot().await.unwrap();
            provider.operations.terminate.join_all().await.unwrap();
            provider.terminal_terminate(operation, &opened.binding.id, &terminal).await.unwrap();
            let (source, cancellation) = forge_provider_api::OperationCancellation::channel(); source.cancel();
            let rejected = OperationContext::new(forge_provider_api::OperationId::new("pre-cancelled-terminal").unwrap(), std::time::Duration::from_secs(60), cancellation).unwrap();
            assert!(matches!(provider.terminal_terminate(rejected, &opened.binding.id, &terminal).await,
                Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::Cancelled && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged));
            provider.session_close(context("close"), &opened.binding.id).await.unwrap(); provider.shutdown(context("shutdown")).await.unwrap();
            let requests = peer.collected().await;
            assert_eq!(requests.iter().filter(|request| request["method"] == "session.tasks.cancel").count(), 1);
        }).await.expect("interrupted native operation fixture timed out");
    }

    /// A retained uncertain rollback cannot be reused with changed authorization or identity.
    #[test]
    fn retained_retraction_rejects_changed_request_and_preserves_uncertainty() {
        let request = InputRetractRequest {
            scope: forge_provider_api::InvocationScope::new(
                forge_provider_api::ExecutionOwner::root("owner"),
                "invocation",
            ),
            submission_id: forge_provider_api::SubmissionId::try_from("submission").unwrap(),
            attempt_id: forge_provider_api::DeliveryAttemptId::new("attempt").unwrap(),
            boundary: forge_provider_api::CheckpointId::new("boundary").unwrap(),
            authorization: forge_provider_api::RetractionAuthorization::OutputFreeCancellation,
        };
        let operation = context("retract-operation");
        let retained = RetainedRetraction {
            context: operation.clone(),
            request: request.clone(),
            outcome: InputRetractOutcome::Unknown,
        };
        assert_eq!(
            retained.replay(&operation, &request).unwrap(),
            InputRetractOutcome::Unknown
        );
        assert!(
            retained
                .replay(&context("other-operation"), &request)
                .is_err()
        );
        let mut scope = request.clone();
        scope.scope = forge_provider_api::InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("owner"),
            "other-invocation",
        );
        let mut input = request.clone();
        input.submission_id = forge_provider_api::SubmissionId::try_from("other-input").unwrap();
        let mut attempt = request.clone();
        attempt.attempt_id = forge_provider_api::DeliveryAttemptId::new("other-attempt").unwrap();
        let mut boundary = request;
        boundary.boundary = forge_provider_api::CheckpointId::new("other-boundary").unwrap();
        for changed in [scope, input, attempt, boundary] {
            assert!(
                matches!(retained.replay(&operation, &changed), Err(ProviderError::Failure(error)) if error.operation() == ProviderOperation::InputRetract && error.kind() == ProviderFailureKind::HistoryConflict)
            );
        }
    }

    /// Reusing a fork operation cannot replay a different source, target, or configuration.
    #[tokio::test]
    async fn retained_fork_rejects_changed_immutable_request_without_native_work() {
        let root = tempfile::tempdir().unwrap();
        let provider = CopilotProvider::new(Vec::new(), root.path().to_owned()).unwrap();
        let request = SessionForkRequest {
            source: ForkSource::Binding(BindingId::new("source").unwrap()),
            target: forge_provider_api::session::ForkTarget {
                owner: forge_provider_api::ExecutionOwner::root("target"),
                workspace: root.path().to_string_lossy().into_owned(),
                configuration: SessionConfiguration {
                    model: ModelSelection::default(),
                    access: forge_provider_api::access::AccessPolicy::default(),
                    approval: forge_provider_api::approval::ApprovalMode::Deny,
                    resources: forge_provider_api::ResourceSelection::default(),
                },
                system_instructions: "target instructions".into(),
            },
        };
        let operation = context("fork-operation");
        provider.forks.lock().await.insert(
            operation.id().clone(),
            RetainedFork {
                context: operation.clone(),
                request: request.clone(),
                result: Ok(forge_provider_api::ProviderForkResult::unprofiled(
                    "fork-history".to_owned().try_into().unwrap(),
                )),
            },
        );
        provider
            .session_fork(operation.clone(), request.clone())
            .await
            .unwrap();
        let mut source = request.clone();
        source.source = ForkSource::Binding(BindingId::new("different-source").unwrap());
        let mut target = request.clone();
        target.target.owner = forge_provider_api::ExecutionOwner::root("different-target");
        let mut configuration = request;
        configuration.target.configuration.model.model = Some("different-model".into());
        for changed in [source, target, configuration] {
            assert!(
                matches!(provider.session_fork(operation.clone(), changed).await, Err(ProviderError::Failure(error)) if error.operation() == ProviderOperation::SessionFork && error.kind() == ProviderFailureKind::OperationConflict)
            );
        }
        assert_eq!(provider.runtime.start_count(), 0);
        provider.shutdown(context("shutdown")).await.unwrap();
    }

    /// Recovery reads a pinned journal after process loss and never resumes or certifies absence.
    #[tokio::test]
    async fn persisted_recovery_is_observation_only_and_absence_stays_unknown() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            for accepted in [false, true] {
                let (reader, writer, peer) = super::super::test_peer::streams(move |request| {
                    let result = match request["method"].as_str().unwrap() {
                        "status.get" => json!({"version":"1.0.103"}),
                        "sessions.readPersistedEvents" => {
                            let mut events = vec![json!({"id":"before", "type":"assistant.message", "timestamp":"2026-11-04T00:00:00Z", "data":{}})];
                            if accepted { events.push(json!({"id":"input", "type":"user.message", "timestamp":"2026-11-04T00:00:01Z", "data":{"messageId":"message", "content":"recovered request"}})); }
                            json!({"cursor":"snapshot-end", "cursorStatus":"ok", "hasMore":false, "events":events})
                        }
                        method => panic!("recovery mutated native state through {method}"),
                    };
                    vec![super::super::test_peer::response(request, result)]
                });
                let root = tempfile::tempdir().unwrap();
                let provider = CopilotProvider::new(Vec::new(), root.path().to_owned()).unwrap();
                provider.runtime.attach_streams(reader, writer, root.path().to_owned()).await.unwrap();
                let submission = forge_provider_api::SubmissionId::try_from("submission").unwrap();
                let attempt = forge_provider_api::DeliveryAttemptId::new("attempt").unwrap();
                let token = super::super::prompt::sending_recovery_token(
                    forge_provider_api::ExecutionOwner::root("owner"), submission.clone(), attempt.clone(),
                    super::super::prompt::NativeDeliveryBoundary {
                        session_id: "native-session".into(), cursor: "old-process-cursor".into(), anchor: Some("before".into()),
                    },
                    root.path().to_string_lossy().into_owned(), hex::encode(sha2::Sha256::digest(b"recovered request")),
                ).unwrap();
                let request = InputReconcileRequest { submission_id: submission, attempt_id: attempt, provider_session_id: Some("native-session".to_owned().try_into().unwrap()), token: Some(token), accepted_receipts: Vec::new() };
                let evidence = provider.input_reconcile(context("reconcile"), request.clone()).await.unwrap();
                assert_eq!(matches!(evidence, SubmissionEvidence::Accepted { .. }), accepted);
                if !accepted { assert_eq!(evidence, SubmissionEvidence::Unknown); }
                let mut wrong_attempt = request;
                wrong_attempt.attempt_id = forge_provider_api::DeliveryAttemptId::new("another-attempt").unwrap();
                assert_eq!(provider.input_reconcile(context("wrong-attempt"), wrong_attempt).await.unwrap(), SubmissionEvidence::Unknown);
                provider.shutdown(context("shutdown")).await.unwrap();
                let requests = peer.collected().await;
                assert_eq!(requests.iter().filter(|request| request["method"] == "sessions.readPersistedEvents").count(), 1);
                assert!(requests.iter().all(|request| matches!(request["method"].as_str(), Some("status.get" | "sessions.readPersistedEvents"))));
            }
        }).await.expect("persisted submission recovery fixture timed out");
    }
}
