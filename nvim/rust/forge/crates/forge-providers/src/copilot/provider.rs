use super::handle::CopilotSession;
use super::runtime::CopilotRuntime;
use super::sessions::{CopilotSessions, PreparedSession, SessionReuse, SessionSelection};
use super::turn::CopilotTurnEvent;
use async_trait::async_trait;
use forge_provider_api::catalog::{McpQuery, QueryContext};
use forge_provider_api::control::{ProviderTools, ToolCall};
use forge_provider_api::events::{self, ProviderEventSink, ProviderEventStream};
use forge_provider_api::generation::ReasoningSelection;
use forge_provider_api::permissions::{PermissionRequest, PermissionResponse, ProviderPermissions};
use forge_provider_api::session::{
    BindingId, InputAcceptance, ModelSelection, PromptRequest, RecoveryToken, ResourceLease,
    SessionBinding, SessionRequest, SteeringAcceptance, SteeringRequest,
};
use forge_provider_api::turn_control::{HostTurnStatus, TurnControl};
use forge_provider_api::{
    CancelRequest, CompactionResult, ConfiguredSession, ForkSource, InputReconcileRequest,
    InputRetractOutcome, InputRetractRequest, OpenedSession, OperationContext,
    SessionConfiguration, SessionForkRequest, SessionHistory, TurnAddress, TurnInterruptResult,
};
use forge_provider_api::{
    ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation, SubmissionEvidence,
};
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
        TurnInterruptResult,
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
        forge_provider_api::terminal::TerminalInventory,
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
        owner: forge_provider_api::ThreadId,
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
        let _ = self.close(ProviderOperation::ThreadClose);
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
            ProviderOperation::TurnStart,
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
    shell: std::sync::OnceLock<Arc<super::shell_session::ShellSession>>,
    active: RwLock<HashMap<forge_provider_api::InvocationScope, CallbackRegistration>>,
    native_calls: RwLock<HashMap<String, InvocationCallbacks>>,
    retired_native_calls: RwLock<HashSet<String>>,
    observed_call_owners: RwLock<HashMap<String, TurnAddress>>,
    observed_agents: RwLock<HashMap<forge_provider_api::InvocationScope, InvocationCallbacks>>,
    child_resources:
        RwLock<HashMap<forge_provider_api::InvocationScope, super::child_turn::ChildResources>>,
    mcp_tools: RwLock<HashMap<String, HashSet<String>>>,
    /// Native session whose MCP servers back `mcp_tools`; a catalog miss reads one server on demand.
    mcp_session: RwLock<Option<Arc<github_copilot_sdk::session::Session>>>,
    next_generation: AtomicU64,
    revoked: std::sync::Mutex<CallbackAdmission>,
    running_callbacks: AtomicU64,
    foreground_callbacks: AtomicU64,
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
struct NativeCallbackGuard<'a> {
    router: &'a CallbackRouter,
    foreground: bool,
    child: Option<tokio::sync::OwnedRwLockReadGuard<()>>,
}

impl NativeCallbackGuard<'_> {
    /// Transfer a retained callback to child collection once its exact published owner is known.
    fn retain_as_child(&mut self) {
        if self.foreground {
            self.foreground = false;
            self.router
                .foreground_callbacks
                .fetch_sub(1, Ordering::AcqRel);
            self.router.callbacks_changed.notify_waiters();
        }
    }
}

impl Drop for NativeCallbackGuard<'_> {
    fn drop(&mut self) {
        self.router.running_callbacks.fetch_sub(1, Ordering::AcqRel);
        if self.foreground {
            self.router
                .foreground_callbacks
                .fetch_sub(1, Ordering::AcqRel);
        }
        self.router.callbacks_changed.notify_waiters();
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
            .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Runtime))?;
        if self
            .provider_closed
            .as_ref()
            .is_some_and(|closed| closed.load(Ordering::Acquire))
            || !matches!(*revoked, CallbackAdmission::Open)
        {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::BindingClosed,
            ));
        }
        self.running_callbacks.fetch_add(1, Ordering::AcqRel);
        self.foreground_callbacks.fetch_add(1, Ordering::AcqRel);
        Ok(NativeCallbackGuard {
            router: self,
            foreground: true,
            child: None,
        })
    }

    /// Admit known child work independently of foreground retirement, retaining binding revocation.
    async fn begin_native_callback(
        &self,
        call_id: Option<&str>,
    ) -> Result<NativeCallbackGuard<'_>, ProviderError> {
        loop {
            let changed = self.callbacks_changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            let child = match call_id {
                Some(call_id) => self.observed_call_owners.read().await.contains_key(call_id),
                None => false,
            };
            if child {
                break;
            }
            let open = self.permits_effect();
            let retaining_child = !self.observed_agents.read().await.is_empty();
            let closed = matches!(
                *self
                    .revoked
                    .lock()
                    .expect("callback admission lock is not poisoned"),
                CallbackAdmission::Closed
            ) || self
                .provider_closed
                .as_ref()
                .is_some_and(|closed| closed.load(Ordering::Acquire));
            if open || !retaining_child || closed || call_id.is_none() {
                return self.begin_callback();
            }
            changed.await;
        }
        let admission = self
            .revoked
            .lock()
            .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Runtime))?;
        if matches!(*admission, CallbackAdmission::Closed)
            || self
                .provider_closed
                .as_ref()
                .is_some_and(|closed| closed.load(Ordering::Acquire))
        {
            return Err(ProviderError::Cancelled);
        }
        self.running_callbacks.fetch_add(1, Ordering::AcqRel);
        Ok(NativeCallbackGuard {
            router: self,
            foreground: false,
            child: None,
        })
    }

    /// Drain only original foreground callbacks while preserving independently live child authority.
    async fn collect_foreground_callbacks(&self) {
        loop {
            let changed = self.callbacks_changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            if self.foreground_callbacks.load(Ordering::Acquire) == 0 {
                return;
            }
            changed.await;
        }
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
        self.callbacks_changed.notify_waiters();
    }

    /// Permanently revoke a binding so no late invocation installation can restore admission.
    fn close_admission(&self) {
        *self
            .revoked
            .lock()
            .expect("callback admission lock is not poisoned") = CallbackAdmission::Closed;
        self.callbacks_changed.notify_waiters();
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
    /// Retain the session whose MCP servers answer later permission lookups.
    ///
    /// Opening a binding must not wait for MCP startup: a slow or misconfigured server would
    /// otherwise hold `list_tools` until the open deadline and fail the whole binding. A server
    /// can only request permission for a tool once it is running, so the catalog reads that
    /// server's tools when its first permission request arrives.
    async fn register_mcp_tools(
        &self,
        session: &CopilotSession,
        _resources: &forge_provider_api::ResourceSelection,
    ) {
        *self.mcp_session.write().await = Some(session.inner.clone());
        self.mcp_tools.write().await.clear();
    }

    /// Read one server's tool names into the catalog unless a previous lookup already did.
    async fn load_mcp_server_tools(&self, server: &str) {
        if self.mcp_tools.read().await.contains_key(server) {
            return;
        }
        let Some(native) = self.mcp_session.read().await.clone() else {
            return;
        };
        let Ok(Ok(tools)) = super::operation::wait(native.rpc().mcp().list_tools(
            github_copilot_sdk::rpc::McpListToolsRequest {
                server_name: server.to_owned(),
            },
        ))
        .await
        else {
            return;
        };
        self.mcp_tools.write().await.insert(
            server.to_owned(),
            tools.tools.into_iter().map(|tool| tool.name).collect(),
        );
    }

    /// Resolve only one catalog-backed canonical identity; reject unknown or ambiguous aliases.
    async fn canonical_mcp_target(&self, target: &str) -> Option<String> {
        let (server, requested) = target.split_once('/')?;
        self.load_mcp_server_tools(server).await;
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
                    ProviderOperation::TurnStart,
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
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::WrongInvocation,
                )
            })
    }

    async fn retire_all(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let _ = binding;
        self.revoke();
        self.active.write().await.clear();
        let retired = self
            .native_calls
            .read()
            .await
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        self.retired_native_calls.write().await.extend(retired);
        self.native_calls.write().await.clear();
        self.observed_call_owners.write().await.clear();
        self.observed_agents.write().await.clear();
        self.child_resources.write().await.clear();
        Ok(())
    }

    /// Bind a positively observed native call only to its original admitted invocation.
    async fn bind_native_call(
        &self,
        call_id: &str,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        let callbacks = self.callbacks(scope).await?;
        let mut calls = self.native_calls.write().await;
        if calls
            .get(call_id)
            .is_some_and(|known| known.scope != *scope || known.generation != callbacks.generation)
            || self.observed_call_owners.read().await.contains_key(call_id)
        {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::Protocol,
            ));
        }
        self.retired_native_calls.write().await.remove(call_id);
        calls.entry(call_id.to_owned()).or_insert(callbacks);
        drop(calls);
        self.callbacks_changed.notify_waiters();
        Ok(())
    }

    /// Bind a child callback only after its native relationship and item were published.
    async fn bind_observed_call(
        &self,
        call_id: &str,
        parent: &forge_provider_api::InvocationScope,
        child: TurnAddress,
    ) -> Result<(), ProviderError> {
        let callbacks = self
            .observed_agents
            .read()
            .await
            .get(&child.scope)
            .cloned()
            .filter(|callbacks| &callbacks.scope == parent)
            .ok_or_else(|| {
                failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::WrongInvocation,
                )
            })?;
        let mut calls = self.native_calls.write().await;
        let mut owners = self.observed_call_owners.write().await;
        if calls
            .get(call_id)
            .is_some_and(|known| known.scope != *parent)
            || owners.get(call_id).is_some_and(|known| known != &child)
            || (calls.contains_key(call_id) && !owners.contains_key(call_id))
        {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::Protocol,
            ));
        }
        self.retired_native_calls.write().await.remove(call_id);
        owners.insert(call_id.to_owned(), child);
        calls.entry(call_id.to_owned()).or_insert(callbacks);
        drop(owners);
        drop(calls);
        self.callbacks_changed.notify_waiters();
        Ok(())
    }

    /// Attach an exact child callback to its independent collection group before effects.
    async fn retain_child_callback(
        &self,
        callback: &mut NativeCallbackGuard<'_>,
        child: &TurnAddress,
    ) -> Result<super::child_turn::ChildResources, ProviderError> {
        let resources = self
            .child_resources
            .read()
            .await
            .get(&child.scope)
            .cloned()
            .ok_or(ProviderError::Cancelled)?;
        if resources.revoked.is_cancelled() {
            return Err(ProviderError::Cancelled);
        }
        let retained = resources.callbacks.clone().read_owned().await;
        if resources.revoked.is_cancelled() {
            return Err(ProviderError::Cancelled);
        }
        callback.retain_as_child();
        callback.child = Some(retained);
        Ok(resources)
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
        let child_owned = self
            .observed_agents
            .read()
            .await
            .values()
            .any(|callbacks| callbacks.scope == *scope && callbacks.generation == generation);
        if !still_owned && !child_owned {
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
        self.callbacks_changed.notify_waiters();
    }

    async fn callbacks_for_native_call(
        &self,
        call_id: &str,
    ) -> Result<InvocationCallbacks, ProviderError> {
        loop {
            let changed = self.callbacks_changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            if self.retired_native_calls.read().await.contains(call_id) {
                return Err(failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::Rejected,
                ));
            }
            if let Some(callbacks) = self.native_calls.read().await.get(call_id).cloned() {
                return Ok(callbacks);
            }
            let retaining_child = !self.observed_agents.read().await.is_empty();
            let closed = matches!(
                *self
                    .revoked
                    .lock()
                    .expect("callback admission lock is not poisoned"),
                CallbackAdmission::Closed
            ) || self
                .provider_closed
                .as_ref()
                .is_some_and(|closed| closed.load(Ordering::Acquire));
            if closed
                || (!retaining_child
                    && (!self.permits_effect()
                        || !self.active.read().await.values().any(|registration| {
                            matches!(registration.phase, CallbackPhase::Foreground)
                        })))
            {
                return Err(failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::Rejected,
                ));
            }
            changed.await;
        }
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
    fn shell(&self) -> Option<Arc<super::shell_session::ShellSession>> {
        self.shell.get().cloned()
    }

    /// Wait for the exact observed outer call before admitting a helper-side permission or tool.
    async fn before_shell_native(&self, call_id: &str) -> Result<(), ProviderError> {
        let mut cancellation = self.shell().map(|shell| shell.cancellation());
        let child = self.observed_call_owners.read().await.get(call_id).cloned();
        if let Some(child) = child {
            let mut callback = self.begin_native_callback(Some(call_id)).await?;
            let callbacks = self.callbacks_for_native_call(call_id).await?;
            let resources = self.retain_child_callback(&mut callback, &child).await?;
            return tokio::select! {
                result = callbacks.permissions.before_observed_child(&callbacks.scope, &child) => result,
                _ = resources.revoked.cancelled() => Err(ProviderError::Cancelled),
            };
        }
        let original = if let Some(callbacks) = self.native_calls.read().await.get(call_id).cloned()
        {
            callbacks
        } else {
            let active = self.active.read().await;
            let mut foreground = active
                .values()
                .filter(|registration| matches!(registration.phase, CallbackPhase::Foreground));
            let original = foreground
                .next()
                .map(|registration| registration.callbacks.clone());
            if foreground.next().is_some() {
                return Err(failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::ResourceConflict,
                ));
            }
            original.ok_or_else(|| {
                failure(ProviderOperation::TurnStart, ProviderFailureKind::Rejected)
            })?
        };
        loop {
            let changed = self.callbacks_changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            if !self.permits_effect() {
                return Err(failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::BindingClosed,
                ));
            }
            if original.control.status(&original.scope).await?.cancelled {
                return Err(ProviderError::Cancelled);
            }
            if let Some(callbacks) = self.native_calls.read().await.get(call_id).cloned() {
                if callbacks.scope != original.scope || callbacks.generation != original.generation
                {
                    return Err(failure(
                        ProviderOperation::TurnStart,
                        ProviderFailureKind::WrongInvocation,
                    ));
                }
                break;
            }
            if !self
                .active
                .read()
                .await
                .get(&original.scope)
                .is_some_and(|registration| {
                    registration.callbacks.generation == original.generation
                        && matches!(registration.phase, CallbackPhase::Foreground)
                })
            {
                return Err(failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::Rejected,
                ));
            }
            tokio::select! {
                _ = changed => {},
                _ = async {
                    match cancellation.as_mut() {
                        Some(cancellation) => { let _ = cancellation.changed().await; },
                        None => std::future::pending::<()>().await,
                    }
                } => return Err(ProviderError::Cancelled),
            }
        }
        let child = self.observed_call_owners.read().await.get(call_id).cloned();
        if let Some(child) = child {
            return original
                .permissions
                .before_observed_child(&original.scope, &child)
                .await;
        }
        original.permissions.before_tool(&original.scope).await?;
        if original.control.status(&original.scope).await?.cancelled {
            return Err(ProviderError::Cancelled);
        }
        if !self.permits_effect() {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::BindingClosed,
            ));
        }
        Ok(())
    }

    async fn call_native(
        &self,
        call_id: &str,
        mut call: ToolCall,
    ) -> Result<String, ProviderError> {
        let mut callback = self.begin_native_callback(Some(call_id)).await?;
        call.id = forge_provider_api::control::ToolCallId::new(call_id)
            .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?;
        let callbacks = self.callbacks_for_native_call(call_id).await?;
        let child = self.observed_call_owners.read().await.get(call_id).cloned();
        if let Some(child) = child {
            let _resources = self.retain_child_callback(&mut callback, &child).await?;
            return callbacks
                .tools
                .call_observed_child(&callbacks.scope, &child, call)
                .await;
        }
        callbacks.authorize_tool(&call)?;
        callbacks.permissions.before_tool(&callbacks.scope).await?;
        if !self.permits_effect() {
            return Err(failure(
                ProviderOperation::TurnStart,
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
        let Ok(mut callback) = self.begin_native_callback(Some(call_id)).await else {
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
        let callbacks = match self.callbacks_for_native_call(call_id).await {
            Ok(callbacks) => callbacks,
            Err(_) => return PermissionResponse::Cancelled,
        };
        let child = self.observed_call_owners.read().await.get(call_id).cloned();
        if let Some(child) = child {
            let Ok(resources) = self.retain_child_callback(&mut callback, &child).await else {
                return PermissionResponse::Cancelled;
            };
            return tokio::select! {
                response = callbacks.permissions.authorize_observed_child(&callbacks.scope, &child, request) => response,
                _ = resources.revoked.cancelled() => PermissionResponse::Cancelled,
            };
        }
        let response = if callbacks
            .session_directory
            .as_ref()
            .is_some_and(|directory| directory.approves(&request))
        {
            callbacks
                .permissions
                .authorize_session_storage(&callbacks.scope, request)
                .await
        } else {
            callbacks
                .permissions
                .authorize(&callbacks.scope, request)
                .await
        };
        if self.permits_effect() {
            response
        } else {
            PermissionResponse::Cancelled
        }
    }

    async fn before_tool_native(&self, call_id: Option<&str>) -> Result<(), ProviderError> {
        // The SDK hook omits call identity. A live child makes its authority ambiguous.
        if call_id.is_none() && !self.observed_agents.read().await.is_empty() {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::Policy,
            ));
        }
        let mut callback = self.begin_native_callback(call_id).await?;
        if let Some(call_id) = call_id {
            let callbacks = self.callbacks_for_native_call(call_id).await?;
            let child = self.observed_call_owners.read().await.get(call_id).cloned();
            if let Some(child) = child {
                let resources = self.retain_child_callback(&mut callback, &child).await?;
                tokio::select! {
                    result = callbacks.permissions.before_observed_child(&callbacks.scope, &child) => result?,
                    _ = resources.revoked.cancelled() => return Err(ProviderError::Cancelled),
                }
                if matches!(
                    *self
                        .revoked
                        .lock()
                        .expect("callback admission lock is not poisoned"),
                    CallbackAdmission::Closed
                ) {
                    return Err(ProviderError::Cancelled);
                }
                return Ok(());
            }
        }
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
                ProviderOperation::TurnStart,
                ProviderFailureKind::Rejected,
            ));
        }
        for callbacks in callbacks {
            callbacks.permissions.before_tool(&callbacks.scope).await?;
            if callbacks.control.status(&callbacks.scope).await?.cancelled {
                return Err(ProviderError::Cancelled);
            }
        }
        if !self.permits_effect() {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::BindingClosed,
            ));
        }
        Ok(())
    }

    async fn bind_native_call(
        &self,
        call_id: &str,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        CallbackRouter::bind_native_call(self, call_id, scope).await
    }

    async fn observe_native_child(
        &self,
        parent: &forge_provider_api::InvocationScope,
        child: TurnAddress,
        finished: bool,
    ) -> Result<(), ProviderError> {
        if finished {
            let resources = self.child_resources.write().await.remove(&child.scope);
            if let Some(resources) = resources {
                resources.collect().await;
            }
            let callbacks = self.observed_agents.write().await.remove(&child.scope);
            if let Some(callbacks) = callbacks {
                self.prune_settled_scope(&callbacks.scope, callbacks.generation)
                    .await;
            }
        } else {
            let callbacks = self.callbacks(parent).await?;
            self.child_resources
                .write()
                .await
                .entry(child.scope.clone())
                .or_insert_with(super::child_turn::ChildResources::new);
            self.observed_agents
                .write()
                .await
                .entry(child.scope)
                .or_insert(callbacks);
        }
        self.callbacks_changed.notify_waiters();
        Ok(())
    }

    async fn native_child_resources(
        &self,
        child: &TurnAddress,
    ) -> Result<super::child_turn::ChildResources, ProviderError> {
        self.child_resources
            .read()
            .await
            .get(&child.scope)
            .cloned()
            .ok_or(ProviderError::Cancelled)
    }

    async fn collect_native_child(&self, child: &TurnAddress) -> Result<(), ProviderError> {
        let resources = self.child_resources.read().await.get(&child.scope).cloned();
        if let Some(resources) = resources {
            resources.collect().await;
        }
        let callbacks = self.observed_agents.write().await.remove(&child.scope);
        self.child_resources.write().await.remove(&child.scope);
        if let Some(callbacks) = callbacks {
            self.prune_settled_scope(&callbacks.scope, callbacks.generation)
                .await;
        }
        self.callbacks_changed.notify_waiters();
        Ok(())
    }

    async fn bind_observed_native_call(
        &self,
        call_id: &str,
        parent: &forge_provider_api::InvocationScope,
        child: TurnAddress,
    ) -> Result<(), ProviderError> {
        self.bind_observed_call(call_id, parent, child).await
    }

    async fn release_owned_native_call(
        &self,
        call_id: &str,
        scope: &forge_provider_api::InvocationScope,
    ) {
        let mut calls = self.native_calls.write().await;
        let child = self.observed_call_owners.read().await.get(call_id).cloned();
        let original_scope = child
            .as_ref()
            .map(|child| &child.scope)
            .or_else(|| calls.get(call_id).map(|callbacks| &callbacks.scope));
        if original_scope != Some(scope) {
            return;
        }
        let callbacks = calls.remove(call_id);
        self.observed_call_owners.write().await.remove(call_id);
        self.retired_native_calls
            .write()
            .await
            .insert(call_id.to_owned());
        drop(calls);
        if let Some(callbacks) = callbacks {
            self.prune_settled_scope(&callbacks.scope, callbacks.generation)
                .await;
        }
    }

    async fn release_native_call(&self, call_id: &str) {
        self.retired_native_calls
            .write()
            .await
            .insert(call_id.to_owned());
        let callbacks = self.native_calls.write().await.remove(call_id);
        self.observed_call_owners.write().await.remove(call_id);
        if let Some(callbacks) = callbacks {
            self.prune_settled_scope(&callbacks.scope, callbacks.generation)
                .await;
        }
    }
}

struct BindingState {
    binding: SessionBinding,
    open_operation: forge_provider_api::OperationId,
    owner: forge_provider_api::ThreadId,
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
        HashMap<
            forge_provider_api::InvocationScope,
            Result<forge_provider_api::CompletedTurn, ProviderError>,
        >,
    >,
    cancellations: Mutex<HashMap<forge_provider_api::InvocationScope, Result<(), ProviderError>>>,
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
    terminals: Arc<Mutex<super::terminal::TerminalRegistry>>,
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

/// Retain the original native prompt and callback collection behind the shared runner.
struct CopilotTurnDriver {
    native: crate::retained_task::RetainedTask<forge_provider_api::TurnResult>,
    start: Mutex<Option<tokio::sync::oneshot::Sender<forge_provider_api::NativeTurnStart>>>,
    state: Arc<BindingState>,
    sessions: Arc<CopilotSessions<RetainedResources>>,
    session: Arc<CopilotSession>,
    dispatch: Option<super::operation::Admission>,
    submission: forge_provider_api::SubmissionId,
}

#[async_trait]
impl crate::turn_runner::NativeTurnDriver for CopilotTurnDriver {
    /// Observe the same native worker after giving it sole positive-start authority.
    async fn run(
        &self,
        start: forge_provider_api::NativeTurnStart,
    ) -> Result<forge_provider_api::TurnResult, ProviderError> {
        if let Some(sender) = self.start.lock().await.take() {
            sender
                .send(start)
                .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Runtime))?;
        }
        self.native.join().await
    }
    /// Fence sends and interrupt this driver's original native turn.
    async fn interrupt(&self) -> Result<(), ProviderError> {
        self.state.interrupt().await
    }
    /// Collect callbacks and delivery history before the runner may certify completion.
    async fn collect(&self) -> Result<(), ProviderError> {
        let outcome = self.native.join().await;
        let never_dispatched = self
            .dispatch
            .as_ref()
            .is_some_and(|operation| !operation.dispatched());
        let history = if never_dispatched {
            Ok(())
        } else {
            self.sessions
                .mark_submitted(&self.state.owner, &self.session)
                .await
        };
        if never_dispatched
            || matches!(
                outcome,
                Err(ProviderError::NotConsumed | ProviderError::PreparationFailed(_))
            )
        {
            self.state
                .unconsumed
                .lock()
                .await
                .insert(self.submission.clone());
        }
        self.state.callbacks.revoke();
        self.state.callbacks.collect_foreground_callbacks().await;
        let has_children = !self.state.callbacks.observed_agents.read().await.is_empty();
        let callbacks = if !has_children {
            self.state.callbacks.collect_callbacks().await;
            self.state
                .callbacks
                .retire_all(&self.state.binding.id)
                .await
        } else {
            Ok(())
        };
        match (history, callbacks) {
            (Ok(()), result) | (result, Ok(())) => result,
            (Err(primary), Err(secondary)) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![secondary],
            }),
        }
    }
}

impl BindingState {
    /// Reject native context mutation while its foreground owner or an independent observed child lives.
    async fn require_native_idle(&self, operation: ProviderOperation) -> Result<(), ProviderError> {
        let foreground = self
            .active
            .lock()
            .await
            .as_ref()
            .is_some_and(|active| !active.task.is_finished());
        if foreground || !self.callbacks.observed_agents.read().await.is_empty() {
            return Err(failure(operation, ProviderFailureKind::ExecutionActive));
        }
        Ok(())
    }

    /// Fence a committed configuration whose confirmation or original operation authority was lost.
    fn invalidate_configuration(&self, fallback: ProviderFailureKind) -> ProviderError {
        self.closing.store(true, Ordering::Release);
        self.callbacks.close_admission();
        let kind = super::operation::current()
            .and_then(|operation| {
                operation
                    .context
                    .check_admission(ProviderOperation::ThreadConfigure)
                    .err()
            })
            .map(|cause| super::error::trace_failure_kind(&cause))
            .unwrap_or(fallback);
        ProviderError::Failure(
            ProviderFailure::new(ProviderOperation::ThreadConfigure, kind).at_phase(
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
                    .check_admission(ProviderOperation::ThreadConfigure)
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
        if let Some(shell) = self.callbacks.shell() {
            shell.cancel_waits();
        }
        self.turn.request_stop().map_err(|_| {
            failure(
                ProviderOperation::TurnInterrupt,
                ProviderFailureKind::Runtime,
            )
        })?;
        if !self.turn.is_active() {
            return Ok(());
        }
        let session = self.session.read().await.clone();
        // Stop and writer admission share one state lock; every admitted frame
        // precedes abort, while handoffs attempted after stop are rejected.
        let abort = super::operation::wait(session.abort())
            .await
            .unwrap_or_else(Err);
        let pending = super::operation::wait(self.turn.finish_pending_sends())
            .await
            .unwrap_or_else(Err);
        match (pending, abort) {
            (Ok(()), result) | (result, Ok(())) => result,
            (Err(primary), Err(secondary)) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![secondary],
            }),
        }
    }

    /// Join native execution while retaining ownership if the waiter is cancelled or times out.
    async fn collect_execution(&self) -> Result<Option<Result<(), ProviderError>>, ProviderError> {
        let mut active = self.active.lock().await;
        let Some(task) = active.as_mut() else {
            return Ok(None);
        };
        let result = (&mut task.task).await;
        active.take();
        result.map(Some).map_err(|_| {
            failure(
                ProviderOperation::TurnInterrupt,
                ProviderFailureKind::Runtime,
            )
        })
    }
}

#[async_trait]
pub(super) trait NativeCallbackRouter: Send + Sync {
    /// Get the binding-owned native shell helper when this is a live binding router.
    fn shell(&self) -> Option<Arc<super::shell_session::ShellSession>> {
        None
    }
    /// Revalidate the exact invocation immediately before a shell operation.
    async fn before_shell_native(&self, call_id: &str) -> Result<(), ProviderError> {
        self.before_tool_native(Some(call_id)).await
    }
    async fn call_native(&self, call_id: &str, call: ToolCall) -> Result<String, ProviderError>;
    async fn authorize_native(
        &self,
        call_id: &str,
        request: PermissionRequest,
    ) -> PermissionResponse;
    async fn before_tool_native(&self, call_id: Option<&str>) -> Result<(), ProviderError>;
    async fn bind_native_call(
        &self,
        call_id: &str,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError>;
    /// Retain or release the original authority for a positively published child lifetime.
    async fn observe_native_child(
        &self,
        parent: &forge_provider_api::InvocationScope,
        child: TurnAddress,
        finished: bool,
    ) -> Result<(), ProviderError> {
        let _ = (parent, child, finished);
        Ok(())
    }
    /// Get the independently registered original child callback collection group.
    async fn native_child_resources(
        &self,
        child: &TurnAddress,
    ) -> Result<super::child_turn::ChildResources, ProviderError> {
        let _ = child;
        Ok(super::child_turn::ChildResources::new())
    }
    /// Collect only the original child's callbacks after its native terminal boundary.
    async fn collect_native_child(&self, child: &TurnAddress) -> Result<(), ProviderError> {
        let _ = child;
        Ok(())
    }
    /// Retain exact observed-child authority after its causal native publications.
    async fn bind_observed_native_call(
        &self,
        call_id: &str,
        parent: &forge_provider_api::InvocationScope,
        child: TurnAddress,
    ) -> Result<(), ProviderError> {
        let _ = (call_id, parent, child);
        Err(ProviderError::Cancelled)
    }
    /// Release only the positively attributed original invocation when native IDs are reused.
    async fn release_owned_native_call(
        &self,
        call_id: &str,
        scope: &forge_provider_api::InvocationScope,
    ) {
        let _ = scope;
        self.release_native_call(call_id).await;
    }
    async fn release_native_call(&self, call_id: &str);
}

pub(super) struct NoopNativeCallbacks;

#[async_trait]
impl NativeCallbackRouter for NoopNativeCallbacks {
    async fn call_native(&self, _: &str, _: ToolCall) -> Result<String, ProviderError> {
        Err(failure(
            ProviderOperation::TurnStart,
            ProviderFailureKind::Rejected,
        ))
    }
    async fn authorize_native(&self, _: &str, _: PermissionRequest) -> PermissionResponse {
        PermissionResponse::Cancelled
    }
    async fn before_tool_native(&self, _: Option<&str>) -> Result<(), ProviderError> {
        Err(failure(
            ProviderOperation::TurnStart,
            ProviderFailureKind::Rejected,
        ))
    }
    async fn bind_native_call(
        &self,
        _: &str,
        _: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        Ok(())
    }
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
    owners: Arc<Mutex<HashMap<forge_provider_api::ThreadId, BindingId>>>,
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
            super::operation::wait(self.pending_inputs.cancel_binding(&state.binding.id))
                .await
                .map_err(|_| {
                    ProviderError::Failure(
                        ProviderFailure::new(
                            ProviderOperation::ThreadClose,
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
    async fn thread_open(
        &self,
        context: OperationContext,
        request: SessionRequest,
    ) -> Result<OpenedSession, ProviderError> {
        request.validate_thread_owner()?;
        let admission =
            super::operation::Admission::new(context.clone(), ProviderOperation::ThreadOpen);
        let resource = InterruptedResource::Opening {
            owner: request.thread_id.clone(),
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
                ProviderOperation::ThreadOpen,
                payload,
                work,
                move |reason| {
                    let dispatched = boundary.dispatched();
                    let observation = reason.failure(
                        ProviderOperation::ThreadOpen,
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

    async fn thread_configure(
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
            ProviderOperation::ThreadConfigure,
            payload,
            resource,
            async move { provider.configure_binding(&binding, configuration).await },
        )
        .await
    }

    async fn thread_fork(
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
            ProviderOperation::ThreadFork,
            payload,
            resource,
            async move { provider.fork_history(request).await },
        )
        .await
    }

    async fn thread_compact(
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
            ProviderOperation::ThreadCompact,
            payload,
            resource,
            async move { provider.compact_history(&binding, request).await },
        )
        .await
    }

    async fn thread_close(
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
            ProviderOperation::ThreadClose,
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

    async fn turn_interrupt(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: CancelRequest,
    ) -> Result<TurnInterruptResult, ProviderError> {
        let resource = InterruptedResource::RetainNative;
        let payload = (binding.clone(), request.clone());
        let provider = self.clone();
        let binding = binding.clone();
        self.observe(
            &self.operations.cancel,
            context,
            ProviderOperation::TurnInterrupt,
            payload,
            resource,
            async move { provider.interrupt_invocation(&binding, request).await },
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
            ProviderOperation::TurnStart,
            payload,
            resource,
            async move { provider.submit(&binding, request, &pending).await },
        )
        .await
    }

    async fn turn_steer(
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
            ProviderOperation::TurnSteer,
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
    ) -> Result<forge_provider_api::terminal::TerminalInventory, ProviderError> {
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
                    ProviderOperation::TurnStart | ProviderOperation::TurnSteer
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
                        ProviderOperation::ThreadClose | ProviderOperation::Shutdown => {
                            forge_provider_api::ProviderPhase::Close
                        }
                        ProviderOperation::TurnInterrupt => {
                            forge_provider_api::ProviderPhase::Collection
                        }
                        ProviderOperation::ThreadOpen
                        | ProviderOperation::ThreadConfigure
                        | ProviderOperation::ThreadFork
                        | ProviderOperation::ThreadCompact
                        | ProviderOperation::TurnStart
                        | ProviderOperation::TurnSteer
                        | ProviderOperation::InputReconcile
                        | ProviderOperation::InputRetract
                        | ProviderOperation::Inspect
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
        let startup_failures = self.sessions.mcp_failures(&state.owner).await;
        session.mcp_list(definitions, &startup_failures).await
    }

    async fn terminals(
        &self,
        binding: &BindingId,
    ) -> Result<forge_provider_api::terminal::TerminalInventory, ProviderError> {
        super::operation::check()?;
        let state = self
            .binding(binding, ProviderOperation::TerminalList)
            .await?;
        let snapshot = self.sessions.terminals(&state.owner).await.map_err(|_| {
            failure(
                ProviderOperation::TerminalList,
                ProviderFailureKind::Runtime,
            )
        })?;
        if let Some(shell) = state.callbacks.shell() {
            shell.snapshot().await?;
            return Ok(shell.publisher().snapshot());
        }
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
        if let Some(shell) = state.callbacks.shell()
            && shell.owns(terminal).await
        {
            return shell.terminate(terminal).await;
        }
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
            .terminate(&state.owner, &native)
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
                ProviderOperation::ThreadOpen,
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
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::Rejected,
            ));
        }
        if request.thread_id.as_str().trim().is_empty() || request.workspace.trim().is_empty() {
            return Err(failure(
                ProviderOperation::ThreadOpen,
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
                    failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Rejected)
                })?;
        }
        self.collect_failed_runtime().await?;
        if self.owners.lock().await.contains_key(&request.thread_id) {
            return Err(failure(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::ResourceConflict,
            ));
        }
        if let Some(resume) = request.history.resume_id() {
            let bindings = self
                .bindings
                .lock()
                .await
                .values()
                .cloned()
                .collect::<Vec<_>>();
            for state in bindings {
                if state.session.read().await.id() == resume.as_str() {
                    return Err(failure(
                        ProviderOperation::ThreadOpen,
                        ProviderFailureKind::ResourceConflict,
                    ));
                }
            }
        }
        let runtime_key = request.thread_id.clone();
        let binding_id = BindingId::new(uuid::Uuid::new_v4().to_string())
            .map_err(|_| failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Unknown))?;
        let session_scope = forge_provider_api::EventScope::Binding {
            thread_id: request.thread_id.clone(),
        };
        let (sink, stream) = events::channel(binding_id.clone(), session_scope);
        self.sessions
            .subscribe(runtime_key.clone(), sink.clone())
            .await;
        let callbacks = Arc::new(CallbackRouter::new(self.closed.clone()));
        if !matches!(request.history, SessionHistory::IsolatedText(_)) {
            let router: Arc<dyn NativeCallbackRouter> = callbacks.clone();
            let shell = Arc::new(super::shell_session::ShellSession::new(
                Arc::downgrade(&router),
                request.thread_id.clone(),
                sink.clone(),
            ));
            shell.set_cleanup_runtime(self.runtime.cleanup_runtime());
            let _ = callbacks.shell.set(shell);
        }
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
            system_message: request.system_instructions.clone(),
            resources: super::session::SessionResources {
                skill_directories: vec![profile_path.join("skills")],
                selected_mcp,
            },
            excluded_tools: Vec::new(),
            tools: tools.clone(),
            native_callbacks: Some(callbacks.clone()),
        };
        let permissions: Arc<dyn ProviderPermissions> = callbacks.clone();
        let provider_tools: Arc<dyn ProviderTools> = callbacks.clone();
        let selection = SessionSelection {
            execution_owner: request.thread_id.clone(),
            correlation_id: request.thread_id.to_string(),
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
                        ProviderOperation::ThreadOpen,
                        ProviderFailureKind::Initialization,
                    )
                })
            })?;
        callbacks
            .register_mcp_tools(&session, &request.configuration.resources)
            .await;
        let turn = self.sessions.turn(&runtime_key).await;
        let provider_session_id = session
            .id()
            .try_into()
            .map_err(|_| failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Protocol))?;
        let binding = SessionBinding {
            thread_id: request.thread_id.clone(),
            id: binding_id.clone(),
            provider_session_id: (!matches!(request.history, SessionHistory::IsolatedText(_)))
                .then_some(provider_session_id),
        };
        let state = Arc::new(BindingState {
            binding: binding.clone(),
            open_operation: super::operation::id()?,
            owner: request.thread_id.clone(),
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
            terminals: Arc::new(Mutex::new(super::terminal::TerminalRegistry::default())),
            terminals_collected: std::sync::atomic::AtomicBool::new(false),
            native_retired: std::sync::atomic::AtomicBool::new(false),
            history_deleted: std::sync::atomic::AtomicBool::new(false),
        });
        self.bindings
            .lock()
            .await
            .insert(binding_id.clone(), state.clone());
        self.sessions
            .observe_terminals(super::terminal::TerminalPublication {
                owner: state.owner.clone(),
                sink: state.sink.clone(),
                registry: state.terminals.clone(),
                publisher: state.callbacks.shell().map(|shell| shell.publisher()),
            })
            .await
            .map_err(|_| failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Runtime))?;
        self.owners
            .lock()
            .await
            .insert(request.thread_id, binding_id);
        if self.closed.load(Ordering::Acquire) {
            self.close_binding(&binding.id).await?;
            return Err(ProviderError::Failure(
                ProviderFailure::new(
                    ProviderOperation::ThreadOpen,
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
                .check_admission(ProviderOperation::ThreadOpen)
                .is_err()
        {
            let reason = admission.context.interrupted().await;
            self.close_binding(&binding.id).await?;
            return Err(reason.failure(
                ProviderOperation::ThreadOpen,
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
            .binding(binding, ProviderOperation::ThreadConfigure)
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
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::BindingClosed,
            ));
        }
        if state.unsafe_history.load(Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::BindingInvalidated,
            ));
        }
        state
            .require_native_idle(ProviderOperation::ThreadConfigure)
            .await?;
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
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::Policy,
            ));
        }
        let session = state.session.read().await.clone();
        if configuration.access == next_request.configuration.access
            && configuration.approval == next_request.configuration.approval
            && configuration.resources == next_request.configuration.resources
        {
            // Model selection changes neither native resource ownership nor the
            // permission scope of independently running helper terminals.
            let client = self.runtime.client(&next_request.workspace).await?;
            let models = client.models().await?;
            let (model, reasoning) = resolve_model_selection(
                &configuration.model,
                &models,
                &self.descriptor().capability,
            )?;
            state.turn.reset().map_err(|_| {
                failure(
                    ProviderOperation::ThreadConfigure,
                    ProviderFailureKind::Protocol,
                )
            })?;
            super::operation::dispatch_for(ProviderOperation::ThreadConfigure)?;
            if let Err(cause) = session
                .set_model_confirmed(
                    model.as_deref().unwrap_or("auto"),
                    reasoning,
                    configuration.model.context_window.as_deref(),
                )
                .await
            {
                return Err(ProviderError::Combined {
                    primary: Box::new(
                        state.invalidate_configuration(super::error::trace_failure_kind(&cause)),
                    ),
                    secondary: vec![cause],
                });
            }
            next_request.configuration = configuration.clone();
            *state.request.lock().await = next_request;
            state.confirm_configuration_health(self.closed.load(Ordering::Acquire))?;
            let capabilities = forge_provider_api::capability::CapabilityManifest::from_descriptor(
                self.descriptor(),
            );
            if let Err(error) = state.sink.capabilities_changed(capabilities.clone()).await {
                return Err(ProviderError::Combined {
                    primary: Box::new(
                        state.invalidate_configuration(ProviderFailureKind::Transport),
                    ),
                    secondary: vec![ProviderError::from(error)],
                });
            }
            state.confirm_configuration_health(self.closed.load(Ordering::Acquire))?;
            return Ok(ConfiguredSession {
                configuration,
                capabilities,
                watermark: state.sink.watermark(),
            });
        }
        let terminals =
            session
                .terminal_snapshot()
                .await
                .map_err(|cause| ProviderError::Combined {
                    primary: Box::new(ProviderError::Failure(
                        ProviderFailure::new(
                            ProviderOperation::ThreadConfigure,
                            super::error::trace_failure_kind(&cause),
                        )
                        .at_phase(
                            forge_provider_api::ProviderPhase::Observation,
                            forge_provider_api::FailureDisposition::Unchanged,
                        ),
                    )),
                    secondary: vec![cause],
                })?;
        let helper_has_terminals = match state.callbacks.shell() {
            Some(shell) => !shell.snapshot().await?.terminal.is_empty(),
            None => false,
        };
        if !terminals.terminal.is_empty() || helper_has_terminals {
            return Err(failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::ResourceConflict,
            ));
        }
        if let Some(shell) = state.callbacks.shell()
            && shell.started().await
            && !self
                .sessions
                .retains_identity_on_refresh(&state.owner)
                .await
        {
            return Err(failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::ResourceConflict,
            ));
        }
        state.turn.reset().map_err(|_| {
            failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::Protocol,
            )
        })?;

        if matches!(next_request.history, SessionHistory::Durable { .. }) {
            next_request.history = SessionHistory::Durable {
                resume: Some(
                    state
                        .session
                        .read()
                        .await
                        .id()
                        .to_owned()
                        .try_into()
                        .map_err(|_| {
                            failure(
                                ProviderOperation::ThreadConfigure,
                                ProviderFailureKind::Protocol,
                            )
                        })?,
                ),
            };
        }
        next_request.configuration = configuration.clone();
        self.descriptor()
            .capability
            .validate_access(&next_request.configuration.access)
            .map_err(|_| {
                failure(
                    ProviderOperation::ThreadConfigure,
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
            system_message: next_request.system_instructions.clone(),
            resources: super::session::SessionResources {
                skill_directories: vec![profile_path.join("skills")],
                selected_mcp,
            },
            excluded_tools: Vec::new(),
            tools: tools.clone(),
            native_callbacks: Some(state.callbacks.clone()),
        };
        let permissions: Arc<dyn ProviderPermissions> = state.callbacks.clone();
        let provider_tools: Arc<dyn ProviderTools> = state.callbacks.clone();
        let selection = SessionSelection {
            execution_owner: next_request.thread_id.clone(),
            correlation_id: next_request.thread_id.to_string(),
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
                        ProviderOperation::ThreadConfigure,
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
                            ProviderOperation::ThreadConfigure,
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
        let resources = self.sessions.resources(&state.owner).await;
        let native_id = forge_provider_api::ProviderSessionId::try_from(session.id().to_owned())
            .map_err(|_| {
                failure(
                    ProviderOperation::ThreadConfigure,
                    ProviderFailureKind::Protocol,
                )
            })?;
        let identity_changed = state.session.read().await.id() != native_id.as_str();
        *state.session.write().await = session;
        *state.terminals.lock().await = super::terminal::TerminalRegistry::default();
        if matches!(next_request.history, SessionHistory::Durable { .. }) {
            next_request.history = SessionHistory::Durable {
                resume: Some(native_id.clone()),
            };
        }
        self.sessions
            .observe_terminals(super::terminal::TerminalPublication {
                owner: state.owner.clone(),
                sink: state.sink.clone(),
                registry: state.terminals.clone(),
                publisher: state.callbacks.shell().map(|shell| shell.publisher()),
            })
            .await
            .map_err(|_| {
                failure(
                    ProviderOperation::ThreadConfigure,
                    ProviderFailureKind::Runtime,
                )
            })?;
        let replaced = std::mem::replace(&mut *state.retained_resources.lock().await, resources);
        if let Some(replaced) = replaced {
            state.retired_resources.lock().await.push(replaced);
        }
        *state.request.lock().await = next_request;
        if let Err(cause) = state
            .collect_retired_profiles(ProviderOperation::ThreadConfigure)
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
                        ProviderOperation::ThreadConfigure,
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
        if identity_changed
            && let Err(error) = state
                .sink
                .activity_writer()
                .send_wait(forge_provider_api::AdapterActivity::Thread {
                    thread_id: state.owner.clone(),
                    event: forge_provider_api::ThreadEvent::IdentityResolved(native_id),
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
        if let Err(error) = state
            .sink
            .capabilities_changed(
                forge_provider_api::capability::CapabilityManifest::from_descriptor(
                    self.descriptor(),
                ),
            )
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
        let state = self.binding(binding, ProviderOperation::TurnSteer).await?;
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
                    ProviderOperation::TurnSteer,
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
                    ProviderOperation::TurnSteer,
                    ProviderFailureKind::BindingInvalidated,
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Admission,
                    forge_provider_api::FailureDisposition::Invalidated,
                ),
            ));
        }
        if request.scope.thread_id != state.owner {
            return Err(failure(
                ProviderOperation::TurnSteer,
                ProviderFailureKind::WrongInvocation,
            ));
        }
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
                ProviderOperation::TurnSteer,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        if !state.turn.is_active() || active_scope.is_none() {
            return Err(ProviderError::NotConsumed);
        }
        if let Some(input) = covered_input {
            state
                .turn
                .register_input(&request.scope, input)
                .map_err(|_| {
                    failure(
                        ProviderOperation::TurnSteer,
                        ProviderFailureKind::WrongInvocation,
                    )
                })?;
        }
        let callbacks = state
            .callbacks
            .callbacks(&request.scope)
            .await
            .map_err(|cause| cause.for_operation(ProviderOperation::TurnSteer))?;
        let input = forge_provider_api::input::MessageInput {
            prompt: {
                match request.input {
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
                }
            },
            attachments: request.attachments,
        };
        callbacks.control.before_send(&request.scope).await?;
        if let Some(shell) = state.callbacks.shell() {
            shell.interrupt_waits();
        }
        let session = state.session.read().await.clone();
        let workspace = session
            .workspace_path()
            .unwrap_or(Path::new("."))
            .to_string_lossy()
            .into_owned();
        state
            .turn
            .steer_native(
                Some(&request.scope),
                submission_id.clone(),
                |authority| async {
                    session
                        .steer(input, receipt, &request.scope, &workspace, authority)
                        .await
                        .map_err(anyhow::Error::new)
                },
            )
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
            return super::operation::wait(pending_preparation)
                .await
                .map_err(|_| {
                    ProviderError::Failure(
                        ProviderFailure::new(
                            ProviderOperation::ThreadClose,
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
        let shell_cleanup = match state.callbacks.shell() {
            Some(shell) => Some(shell.start_close().await),
            None => None,
        };
        let should_stop = match super::operation::wait(state.admission.lock()).await {
            Ok(_admission) => state
                .active
                .lock()
                .await
                .as_ref()
                .is_some_and(|active| !active.task.is_finished()),
            Err(_) => {
                failures.push(ProviderError::Failure(
                    ProviderFailure::new(
                        ProviderOperation::ThreadClose,
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

        // Attempt independent cleanup even when foreground collection failed. The binding
        // retains its profile, callbacks, native handle, and each unfinished phase for retry.
        let session = state.session.read().await.clone();
        if !state.terminals_collected.load(Ordering::Acquire) {
            let terminals = if session.client_collected() {
                Ok(())
            } else {
                super::operation::wait(session.terminate_all())
                    .await
                    .unwrap_or_else(|_| {
                        Err(failure(
                            ProviderOperation::ThreadClose,
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
            match super::operation::wait(self.sessions.retire(&state.owner)).await {
                Ok(Ok(())) => state.native_retired.store(true, Ordering::Release),
                Ok(Err(error)) => {
                    failures.push(error.downcast::<ProviderError>().unwrap_or_else(|_| {
                        failure(
                            ProviderOperation::ThreadClose,
                            ProviderFailureKind::Transport,
                        )
                    }))
                }
                Err(_) => failures.push(failure(
                    ProviderOperation::ThreadClose,
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
                match super::operation::wait(
                    session.inner.client().delete_session(session.inner.id()),
                )
                .await
                {
                    Ok(Ok(())) => state.history_deleted.store(true, Ordering::Release),
                    Ok(Err(_)) => failures.push(failure(
                        ProviderOperation::ThreadClose,
                        ProviderFailureKind::Transport,
                    )),
                    Err(_) => failures.push(ProviderError::Failure(
                        ProviderFailure::new(
                            ProviderOperation::ThreadClose,
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
        let preparations_collected = match super::operation::wait(pending_preparation).await {
            Ok(()) => true,
            Err(_) => {
                failures.push(ProviderError::Failure(
                    ProviderFailure::new(
                        ProviderOperation::ThreadClose,
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
        if let Some(cleanup) = shell_cleanup
            && let Err(error) = cleanup.await
        {
            failures.push(error);
        }
        let foreground_collected = preparations_collected
            && state.active.lock().await.is_none()
            && !state.turn.is_active()
            && state.callbacks.running_callbacks.load(Ordering::Acquire) == 0;
        if state.native_retired.load(Ordering::Acquire)
            && state.terminals_collected.load(Ordering::Acquire)
            && foreground_collected
        {
            if let Some(resources) = state.retained_resources.lock().await.as_ref()
                && let Err(error) = resources._profile.close(ProviderOperation::ThreadClose)
            {
                failures.push(error);
            }
            if let Err(error) = state
                .collect_retired_profiles(ProviderOperation::ThreadClose)
                .await
            {
                failures.push(error);
            }
        } else if failures.is_empty() {
            failures.push(failure(
                ProviderOperation::ThreadClose,
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

    /// Interrupt and collect exactly the addressed turn without reclassifying its native outcome.
    async fn interrupt_invocation(
        &self,
        binding: &BindingId,
        request: CancelRequest,
    ) -> Result<TurnInterruptResult, ProviderError> {
        self.pending_inputs.cancel(binding, &request.scope).await;
        let state = self
            .binding(binding, ProviderOperation::TurnInterrupt)
            .await?;
        let operation = super::operation::current().ok_or_else(|| {
            failure(
                ProviderOperation::TurnInterrupt,
                ProviderFailureKind::OperationConflict,
            )
        })?;
        let receipt = || -> Result<TurnInterruptResult, ProviderError> {
            Ok(TurnInterruptResult {
                operation_id: operation.context.id().clone(),
                thread_id: request.scope.thread_id.clone(),
                turn_id: request.scope.turn_id().map_err(|_| {
                    failure(
                        ProviderOperation::TurnInterrupt,
                        ProviderFailureKind::WrongInvocation,
                    )
                })?,
                watermark: state.sink.watermark(),
            })
        };
        if let Some(result) = state
            .cancellations
            .lock()
            .await
            .get(&request.scope)
            .cloned()
        {
            result?;
            return receipt();
        }
        if let Some(completed) = state.settlements.lock().await.get(&request.scope).cloned() {
            completed?;
            return receipt();
        }
        if !state
            .active
            .lock()
            .await
            .as_ref()
            .is_some_and(|active| active.scope == request.scope)
        {
            return Err(failure(
                ProviderOperation::TurnInterrupt,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        let _admission = state.admission.lock().await;
        if let Some(completed) = state.settlements.lock().await.get(&request.scope).cloned() {
            completed?;
            return receipt();
        }
        // Revalidate under admission; an old interruption cannot select a successor.
        if !state
            .active
            .lock()
            .await
            .as_ref()
            .is_some_and(|active| active.scope == request.scope)
        {
            return Err(failure(
                ProviderOperation::TurnInterrupt,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        super::operation::dispatch_for(ProviderOperation::TurnInterrupt)?;
        let stop = state.interrupt().await;
        let collected = match state.collect_execution().await {
            Ok(None | Some(Ok(()))) => Ok(()),
            Ok(Some(Err(error))) | Err(error) => Err(error),
        };
        let result = match (stop, collected) {
            (Ok(()), result) | (result, Ok(())) => result,
            (Err(primary), Err(secondary)) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![secondary],
            }),
        };
        state
            .cancellations
            .lock()
            .await
            .insert(request.scope.clone(), result.clone());
        result?;
        receipt()
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
                    ProviderOperation::ThreadFork,
                    ProviderFailureKind::HistoryConflict,
                ));
            }
            return retained.result.clone();
        }
        let immutable_request = request.clone();
        if self.closed.load(Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::ThreadFork,
                ProviderFailureKind::BindingClosed,
            ));
        }
        let (session_id, checkpoint, workspace, source) = match request.source {
            ForkSource::Binding(binding) => {
                let state = self
                    .binding(&binding, ProviderOperation::ThreadFork)
                    .await?;
                let workspace = state.request.lock().await.workspace.clone();
                if state.binding.provider_session_id.is_none() {
                    return Err(failure(
                        ProviderOperation::ThreadFork,
                        ProviderFailureKind::Rejected,
                    ));
                }
                let id = forge_provider_api::ProviderSessionId::try_from(
                    state.session.read().await.id().to_owned(),
                )
                .map_err(|_| {
                    failure(ProviderOperation::ThreadFork, ProviderFailureKind::Protocol)
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
                    ProviderOperation::ThreadFork,
                    ProviderFailureKind::BindingClosed,
                ));
            }
            if state.unsafe_history.load(Ordering::Acquire) {
                return Err(failure(
                    ProviderOperation::ThreadFork,
                    ProviderFailureKind::BindingInvalidated,
                ));
            }
            state
                .require_native_idle(ProviderOperation::ThreadFork)
                .await?;
        }
        let client = self.runtime.client(&workspace).await?;
        let unknown = ProviderError::Failure(
            ProviderFailure::new(
                ProviderOperation::ThreadFork,
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
                        failure(ProviderOperation::ThreadFork, ProviderFailureKind::Protocol)
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
            .binding(binding, ProviderOperation::ThreadCompact)
            .await?;
        if !matches!(&request.scope, forge_provider_api::EventScope::SessionOperation { thread_id, .. } if thread_id == &state.owner)
        {
            return Err(failure(
                ProviderOperation::ThreadCompact,
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
                ProviderOperation::ThreadCompact,
                ProviderFailureKind::BindingClosed,
            ));
        }
        if state.unsafe_history.load(Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::ThreadCompact,
                ProviderFailureKind::BindingInvalidated,
            ));
        }
        state
            .require_native_idle(ProviderOperation::ThreadCompact)
            .await?;
        if let Some(result) = state.compactions.lock().await.get(&operation_id).cloned() {
            return result;
        }
        state.compactions.lock().await.insert(
            operation_id.clone(),
            Err(ProviderError::Failure(
                ProviderFailure::new(
                    ProviderOperation::ThreadCompact,
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
            .compact(&state.owner, operation_id.as_str().to_owned())
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
        state
            .require_native_idle(ProviderOperation::InputRetract)
            .await?;
        if !state
            .settlements
            .lock()
            .await
            .get(&request.scope)
            .is_some_and(|result| {
                result.as_ref().is_ok_and(|settlement| {
                    matches!(
                        settlement.result().outcome(),
                        forge_provider_api::turn::TurnOutcome::Interrupted
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
        let state = self.binding(binding, ProviderOperation::TurnStart).await?;
        pending.check_admission(ProviderOperation::TurnStart)?;
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
                pending.check_admission(ProviderOperation::TurnStart)?;
                unreachable!("pending cancellation is sticky");
            }
            admission = state.admission.lock() => admission,
        };
        pending.check_admission(ProviderOperation::TurnStart)?;
        if self.closed.load(Ordering::Acquire) || state.closing.load(Ordering::Acquire) {
            return Err(ProviderError::Failure(
                ProviderFailure::new(
                    ProviderOperation::TurnStart,
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
                    ProviderOperation::TurnStart,
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
                ProviderOperation::TurnStart,
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
                ProviderOperation::TurnStart,
                ProviderFailureKind::Policy,
            ));
        }
        let allowed_tools = request
            .tools
            .iter()
            .map(|tool| tool.name.to_owned())
            .collect::<HashSet<_>>();
        if request.scope.thread_id != state.owner
            || request.receipt.id().as_str() != request.submission_id.as_str()
            || request.receipt.attempt_id().map_err(|_| {
                failure(
                    ProviderOperation::TurnStart,
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
                ProviderOperation::TurnStart,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        if !isolated_text {
            self.descriptor()
                .capability
                .validate_access(&request.configuration.access)
                .map_err(|_| {
                    failure(ProviderOperation::TurnStart, ProviderFailureKind::Rejected)
                })?;
        }
        if state.stream.lock().await.is_some() {
            return Err(ProviderError::Failure(
                ProviderFailure::new(
                    ProviderOperation::TurnStart,
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
                    ProviderOperation::TurnStart,
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
                        ProviderOperation::TurnStart,
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
                pending.check_admission(ProviderOperation::TurnStart)?;
                unreachable!("pending cancellation is sticky");
            }
            models = client.models() => models?,
        };
        let (selected_model, reasoning) = resolve_model_selection(
            &request.configuration.model,
            &models,
            &self.descriptor().capability,
        )
        .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Rejected))?;
        pending.check_admission(ProviderOperation::TurnStart)?;
        session
            .set_model(
                selected_model.as_deref().unwrap_or("auto"),
                reasoning,
                request.configuration.model.context_window.as_deref(),
            )
            .await?;
        pending.check_admission(ProviderOperation::TurnStart)?;
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
        let repair_caution = self
            .sessions
            .repair(&state.owner)
            .await
            .map(|repair| repair.caution().to_owned());
        if let Err(error) = pending.check_admission(ProviderOperation::TurnStart) {
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
        let (start_sender, start_receiver) = tokio::sync::oneshot::channel();
        let task = tokio::spawn(super::operation::scope(native_operation, async move {
            if registration.await.is_err() {
                return Err(ProviderError::Cancelled);
            }
            let native_start = start_receiver
                .await
                .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Runtime))?;
            let guard = match turn.activate(Some(event_sink.clone()), None) {
                Ok(guard) => {
                    let _ = activated.send(Ok(()));
                    guard
                }
                Err(_) => {
                    let error = failure(
                        ProviderOperation::TurnStart,
                        ProviderFailureKind::ExecutionActive,
                    );
                    let _ = activated.send(Err(()));
                    return Err(error);
                }
            };
            let result = super::prompt::run(
                &session,
                &guard,
                &native_start,
                options,
                &event_sink,
                control.as_ref(),
                task_state.callbacks.clone(),
            )
            .await;
            let native_result = result;
            task_state
                .callbacks
                .finish_foreground_scope(&grant_scope, callback_generation)
                .await;
            drop(guard);
            native_result
        }));
        let collecting = state.clone();
        let history_sessions = self.sessions.clone();
        let history_session = state.session.read().await.clone();
        let collect_scope = invocation_scope.clone();
        let task = crate::retained_task::RetainedTask::new(ProviderOperation::TurnStart, task);
        let driver = Arc::new(CopilotTurnDriver {
            native: task,
            start: Mutex::new(Some(start_sender)),
            state: collecting.clone(),
            sessions: history_sessions,
            session: history_session,
            dispatch: dispatch_evidence,
            submission: collecting_submission.clone(),
        });
        let address = forge_provider_api::TurnAddress {
            scope: collect_scope.clone(),
            thread_id: collect_scope.thread_id.clone(),
            turn_id: collect_scope.turn_id().map_err(|_| {
                failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::WrongInvocation,
                )
            })?,
        };
        let retained = Arc::new(crate::turn_runner::RetainedTurn::spawn(
            driver,
            collecting.sink.clone(),
            address,
        )?);
        let collecting_turn = retained.clone();
        let collector = tokio::spawn(async move {
            let result = collecting_turn.join().await;
            let rejected = result.as_ref().is_err_and(|error| {
                matches!(
                    error,
                    ProviderError::NotConsumed | ProviderError::PreparationFailed(_)
                )
            }) || collecting
                .unconsumed
                .lock()
                .await
                .contains(&collecting_submission);
            collecting
                .settlements
                .lock()
                .await
                .insert(collect_scope.clone(), result.clone());
            collecting.settled.notify_waiters();
            if rejected
                || result
                    .as_ref()
                    .is_err_and(ProviderError::is_clean_cancellation)
                    && collecting
                        .turn
                        .foreground_anchor()
                        .is_ok_and(|anchor| anchor.is_none())
            {
                Ok(())
            } else {
                result.map(|_| ())
            }
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
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::ExecutionActive,
                ));
            }
            Err(_) => {
                return Err(failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::Runtime,
                ));
            }
        }
        drop(_admission);
        match acceptance.await {
            Ok(Ok(())) => {}
            Ok(Err(error)) => return Err(error),
            Err(_) => {
                return Err(retained
                    .join()
                    .await
                    .err()
                    .unwrap_or(ProviderError::DeliveryUncertain));
            }
        }
        retained.started().await?;
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
        .map_or(ProviderOperation::ThreadOpen, |operation| {
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

fn resolve_model_selection(
    selection: &ModelSelection,
    models: &[forge_provider_api::ProviderModel],
    capability: &forge_provider_api::ProviderCapability,
) -> Result<(Option<String>, Option<String>), ProviderError> {
    selection.service_tier.validate(capability).map_err(|_| {
        failure(
            ProviderOperation::ThreadConfigure,
            ProviderFailureKind::Rejected,
        )
    })?;
    let model = match selection.model.as_deref() {
        Some(selected) => models
            .iter()
            .find(|model| model.id == selected)
            .ok_or_else(|| {
                failure(
                    ProviderOperation::ThreadConfigure,
                    ProviderFailureKind::Rejected,
                )
            })?,
        None => models.iter().find(|model| model.is_default).ok_or(
            forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::ModelDefaultReset,
            ),
        )?,
    };
    if selection.service_tier != forge_provider_api::ServiceTier::Standard
        && !model.service_tiers.contains(&selection.service_tier)
    {
        return Err(failure(
            ProviderOperation::ThreadConfigure,
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
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::Rejected,
            ));
        }
    };
    if selection.context_window.as_ref().is_some_and(|selected| {
        !model
            .context_window
            .iter()
            .any(|window| window.id == *selected)
    }) {
        return Err(forge_provider_api::capability::UnsupportedCapability(
            forge_provider_api::capability::ProviderFeature::ContextWindowSelection,
        )
        .into());
    }
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

/// Create a normalized failure for an operation inside the Copilot adapter.
pub(super) fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

/// Collect prior native message receipts without consuming the currently reconciled submission.
fn accepted_message_ids(
    submission_id: &forge_provider_api::SubmissionId,
    attempt_id: &forge_provider_api::DeliveryAttemptId,
    owner: &forge_provider_api::ThreadId,
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

    /// Keep child policy distinct from a parent policy that would otherwise allow the request.
    #[tokio::test]
    async fn native_child_permission_waits_for_exact_published_owner_without_parent_fallback() {
        let router = CallbackRouter::default();
        let parent = forge_provider_api::InvocationScope::new(
            forge_provider_api::testing::root_thread("child-permission"),
            "parent",
        );
        let generation = install(&router, &parent).await;
        let child_scope = forge_provider_api::InvocationScope::new(
            parent
                .thread_id
                .child(&forge_provider_api::agent::AgentId::new("observed-child").unwrap())
                .unwrap(),
            parent.exchange_id.clone(),
        );
        let child = TurnAddress {
            thread_id: child_scope.thread_id.clone(),
            turn_id: child_scope.turn_id().unwrap(),
            scope: child_scope,
        };
        let pending = router.before_tool_native(Some("child-call"));
        tokio::pin!(pending);
        assert!(futures_util::poll!(pending.as_mut()).is_pending());
        router
            .observe_native_child(&parent, child.clone(), false)
            .await
            .unwrap();
        assert!(router.before_tool_native(None).await.is_err());
        router
            .bind_observed_native_call("child-call", &parent, child.clone())
            .await
            .unwrap();
        assert!(pending.await.is_err());
        assert!(
            router
                .call_native(
                    "child-call",
                    ToolCall {
                        id: forge_provider_api::control::ToolCallId::new("child-call").unwrap(),
                        name: "fixture_tool".into(),
                        arguments: json!({}),
                    }
                )
                .await
                .is_err(),
            "child host tools must use the dedicated child executor"
        );
        let request = PermissionRequest {
            id: "child-permission".into(),
            provider: forge_provider_api::ProviderId::new("copilot").unwrap(),
            reason: None,
            target_list: vec![forge_provider_api::permissions::PermissionTarget::Unknown],
        };
        assert!(matches!(
            router.authorize_native("child-call", request.clone()).await,
            PermissionResponse::Deny { .. }
        ));
        router.finish_foreground_scope(&parent, generation).await;
        router.release_native_call("child-call").await;
        assert!(router.callbacks(&parent).await.is_ok());
        router.revoke();
        let later = router.before_tool_native(Some("later-child-call"));
        tokio::pin!(later);
        assert!(futures_util::poll!(later.as_mut()).is_pending());
        router
            .bind_observed_native_call("later-child-call", &parent, child.clone())
            .await
            .unwrap();
        assert!(later.await.is_err());
        assert!(matches!(
            router.authorize_native("later-child-call", request).await,
            PermissionResponse::Deny { .. }
        ));
        router.release_native_call("later-child-call").await;
        router
            .observe_native_child(&parent, child, true)
            .await
            .unwrap();
        assert!(router.callbacks(&parent).await.is_err());
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

    struct AcceptedReceipt {
        id: forge_provider_api::SubmissionId,
        attempt: forge_provider_api::DeliveryAttemptId,
        accepted: AtomicU64,
    }

    #[async_trait]
    impl forge_provider_api::submission::InputReceipt for AcceptedReceipt {
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
            Ok(())
        }
        async fn record_accepted(
            self: Arc<Self>,
            _: Option<RecoveryToken>,
        ) -> Result<(), ProviderError> {
            self.accepted.fetch_add(1, Ordering::AcqRel);
            Ok(())
        }
        async fn record_rejected(
            self: Arc<Self>,
            error: ProviderError,
        ) -> Result<(), ProviderError> {
            Err(error)
        }
        async fn record_failure(
            self: Arc<Self>,
            error: ProviderError,
        ) -> Result<(), ProviderError> {
            Err(error)
        }
    }

    fn queued_request<R: forge_provider_api::submission::InputReceipt + 'static>(
        binding: &BindingId,
        scope: forge_provider_api::InvocationScope,
        configuration: SessionConfiguration,
        receipt: Arc<R>,
    ) -> PromptRequest {
        let (dispatch, _) = forge_provider_api::submission::DispatchReporter::channel(
            binding.clone(),
            scope.clone(),
            receipt.id().clone(),
            receipt.attempt_id().unwrap(),
        );
        let authority = Arc::new(super::callback_tests::Authority);
        PromptRequest {
            thread_id: (scope).thread_id.clone(),
            turn_id: (scope)
                .turn_id()
                .expect("admitted scope has a nonempty host turn identity"),

            scope,
            execution: forge_provider_api::ExecutionMode::Default,
            configuration: forge_provider_api::session::InvocationConfiguration {
                model: configuration.model,
                access: configuration.access,
                approval: configuration.approval,
            },
            tools: Vec::new(),
            tool_endpoint: None,
            resource_lease: None,
            submission_id: receipt.id().clone(),
            attempt_id: receipt.attempt_id().unwrap(),
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

    /// A native terminal result waits for admitted callbacks before any root completion is published.
    #[tokio::test]
    async fn root_completion_waits_for_callback_collection_and_survives_dropped_waiter() {
        async {
            let (provider, opened, _root, peer) = opened_helper_fixture(
                Arc::new(std::sync::atomic::AtomicBool::new(false)),
                None,
                AbortAcknowledgement::Success,
                None,
                HelperBehavior::CompleteTurn,
            )
            .await;
            let binding = opened.binding.id;
            let mut events = opened.events;
            let state = provider
                .binding(&binding, ProviderOperation::TurnStart)
                .await
                .unwrap();
            let admitted_callback = state.callbacks.begin_callback().unwrap();
            let scope =
                forge_provider_api::InvocationScope::new(state.owner.clone(), "collected-root");
            let receipt = Arc::new(AcceptedReceipt {
                id: "collected-input".try_into().unwrap(),
                attempt: forge_provider_api::DeliveryAttemptId::new("collected-attempt").unwrap(),
                accepted: AtomicU64::new(0),
            });
            let request = queued_request(
                &binding,
                scope.clone(),
                state.request.lock().await.configuration.clone(),
                receipt.clone(),
            );
            provider
                .turn_start(context("collected-submit"), &binding, request)
                .await
                .unwrap();
            while !matches!(
                *state.callbacks.revoked.lock().unwrap(),
                CallbackAdmission::Settled
            ) {
                tokio::task::yield_now().await;
            }
            assert!(!state.settlements.lock().await.contains_key(&scope));
            while let Some(event) = events.try_next().unwrap() {
                assert!(!matches!(
                    &event.event,
                    ProviderEvent::Turn {
                        event: forge_provider_api::TurnEvent::Completed(_),
                        ..
                    }
                ));
            }
            let mut abandoned = Box::pin(state.collect_execution());
            assert!(futures_util::poll!(abandoned.as_mut()).is_pending());
            drop(abandoned);
            let mut collecting = Box::pin(state.collect_execution());
            assert!(futures_util::poll!(collecting.as_mut()).is_pending());
            drop(admitted_callback);
            assert!(matches!(collecting.await.unwrap(), Some(Ok(()))));
            let mut turn_finished = 0;
            while let Some(event) = events.try_next().unwrap() {
                if let ProviderEvent::Turn {
                    thread_id,
                    turn_id,
                    event: forge_provider_api::TurnEvent::Completed(completed),
                } = &event.event
                {
                    assert_eq!(turn_id, &scope.turn_id().unwrap());
                    assert_eq!(thread_id, &scope.thread_id.clone());
                    assert!(matches!(
                        completed.result().outcome(),
                        forge_provider_api::turn::TurnOutcome::Completed {
                            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal
                        }
                    ));
                    assert_eq!(
                        completed.result().covered_inputs(),
                        &[forge_provider_api::CoveredInput::new(
                            receipt.id.clone(),
                            receipt.attempt.clone()
                        )]
                    );
                    turn_finished += 1;
                }
            }
            assert_eq!(turn_finished, 1);
            let interruption = provider
                .turn_interrupt(
                    context("collected-late-cancel"),
                    &binding,
                    CancelRequest {
                        scope: scope.clone(),
                    },
                )
                .await
                .unwrap();
            assert_eq!(interruption.turn_id, scope.turn_id().unwrap());
            provider
                .thread_close(context("collected-close"), &binding)
                .await
                .unwrap();
            provider
                .shutdown(context("collected-shutdown"))
                .await
                .unwrap();
            assert!(
                !peer
                    .collected()
                    .await
                    .iter()
                    .any(|request| request["method"] == "session.abort")
            );
        }
        .await;
    }

    #[derive(Clone, Copy)]
    enum AbortAcknowledgement {
        Success,
    }

    #[derive(Clone, Copy)]
    enum HelperBehavior {
        CompleteTurn,
    }

    /// Select whether auxiliary queue admission succeeds while preserving the main lifecycle fixture.
    async fn opened_helper_fixture(
        shells: Arc<std::sync::atomic::AtomicBool>,
        mut aborted: Option<tokio::sync::oneshot::Sender<()>>,
        abort_acknowledgement: AbortAcknowledgement,
        mut terminal_ack: Option<(
            Arc<std::sync::atomic::AtomicBool>,
            tokio::sync::oneshot::Sender<()>,
        )>,
        helper_behavior: HelperBehavior,
    ) -> (
        CopilotProvider,
        OpenedSession,
        tempfile::TempDir,
        super::super::test_peer::SdkPeer,
    ) {
        let mut held_terminal = None;
        let mut helper_ids = HashSet::new();
        let mut helper_tasks = Vec::<Value>::new();
        let (reader, writer, peer) = super::super::test_peer::streams(move |request| {
            let result = match request["method"].as_str().unwrap() {
                "status.get" => json!({"version":"1.0.103"}),
                "models.list" => json!({"models":[{
                    "id":"auto", "name":"Auto",
                    "capabilities":{"limits":{"max_context_window_tokens":128000},"supports":{"vision":false}}
                },{"id":"alternate","name":"Alternate","capabilities":{}}]}),
                "mcp.discover" | "session.mcp.listConfigured" => json!({"servers":[]}),
                "session.skills.list" => json!({"skills":[]}),
                "session.create" | "session.resume" => {
                    if request["params"]["availableTools"]
                        .as_array()
                        .is_some_and(|tools| {
                            tools
                                .iter()
                                .any(|tool| tool == "powershell" || tool == "bash")
                        })
                    {
                        helper_ids
                            .insert(request["params"]["sessionId"].as_str().unwrap().to_owned());
                    }
                    json!({"sessionId":request["params"]["sessionId"], "workspacePath":std::env::temp_dir()})
                }
                "session.send" if matches!(helper_behavior, HelperBehavior::CompleteTurn) => {
                    let notification = |id, parent, kind, data| {
                        json!({"jsonrpc":"2.0","method":"session.event",
                        "params":{"sessionId":request["params"]["sessionId"],"event":{"id":id,"parentId":parent,
                            "type":kind,"timestamp":"2026-11-02T00:00:00Z","data":data}}})
                    };
                    let mut frames = vec![
                        super::super::test_peer::response(
                            request,
                            json!({"messageId":"accepted-input"}),
                        ),
                        notification(
                            "accepted-user",
                            Value::Null,
                            "user.message",
                            json!({"messageId":"accepted-input","turnId":"0"}),
                        ),
                    ];
                    if matches!(helper_behavior, HelperBehavior::CompleteTurn) {
                        frames.extend([
                            notification("native-start", json!("accepted-user"), "assistant.turn_start", json!({"turnId":"0"})),
                            notification("native-answer", json!("native-start"), "assistant.message", json!({"messageId":"answer", "content":"Native answer", "phase":"final_answer"})),
                            notification("native-end", json!("native-answer"), "assistant.turn_end", json!({"turnId":"0"})),
                            notification("native-idle", json!("native-end"), "session.idle", json!({})),
                        ]);
                    } else {
                        frames.push(notification(
                            "aborted-idle",
                            json!("accepted-user"),
                            "session.idle",
                            json!({"aborted":true}),
                        ));
                    }
                    return frames;
                }
                "session.plan.readSqlTodosWithDependencies" => json!({"rows":[],"dependencies":[]}),
                "session.eventLog.tail" => json!({"cursor":"before-input"}),
                "session.eventLog.read" => {
                    json!({"cursor":"anchor", "cursorStatus":"ok", "hasMore":false, "events":[]})
                }
                "session.skills.reload" => json!({}),
                "session.options.update" | "session.detach" => json!({"success":true}),
                "session.delete" => json!({}),
                "session.queue.setDrainPaused" => json!({}),
                "session.tools.execute" => {
                    helper_tasks.push(json!({"id":request["params"]["arguments"]["shellId"],"type":"shell","status":"running","attachmentMode":"attached","executionMode":"background","command":"fixture command"}));
                    json!({"resultType":"success","textResultForLlm":"background fixture started"})
                }
                "session.model.switchTo" => {
                    json!({"status":"applied","deferred":false,"modelId":request["params"]["modelId"]})
                }
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
                    if helper_ids.contains(request["params"]["sessionId"].as_str().unwrap()) {
                        json!({"tasks":helper_tasks})
                    } else if shells.load(Ordering::Acquire) {
                        json!({"tasks":[{"id":"background-shell", "type":"shell", "status":"running", "command":"fixture"}]})
                    } else {
                        json!({"tasks":[]})
                    }
                }
                "session.tasks.cancel" => {
                    if helper_ids.contains(request["params"]["sessionId"].as_str().unwrap()) {
                        helper_tasks.retain(|task| task["id"] != request["params"]["id"]);
                        return vec![super::super::test_peer::response(
                            request,
                            json!({"cancelled":true}),
                        )];
                    }
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
            .thread_open(
                context("open"),
                SessionRequest {
                    thread_id: (forge_provider_api::testing::root_thread("lifecycle-fixture"))
                        .clone(),
                    mode: forge_provider_api::SessionMode::Interactive,
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

    /// Recovery reads a pinned journal after process loss and never resumes or certifies absence.
    #[tokio::test]
    async fn persisted_recovery_is_observation_only_and_absence_stays_unknown() {
        async {
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
                    forge_provider_api::testing::root_thread("owner"), submission.clone(), attempt.clone(),
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
        }.await;
    }
}
