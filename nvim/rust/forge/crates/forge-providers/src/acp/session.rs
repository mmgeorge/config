use super::AcpKind as ProviderKind;
use super::tool_semantics::{ToolKind, normalize_descriptor};
use super::{AcpConnectionError, AcpHandler};
use anyhow::{Context, Result, bail, ensure};
use async_trait::async_trait;
use forge_provider_api::permissions::{PermissionResponse, ScopedPermissions};
use forge_provider_api::*;
use serde_json::{Value, json};
use std::collections::{HashMap, HashSet};
use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};
use tokio::sync::{Mutex, RwLock};

/// Distinguish ACP's native reasoning stream from progressively classified assistant prose.
#[derive(Clone, Copy, Eq, PartialEq)]
enum AcpMessageRole {
    Reasoning,
    Commentary,
    Response,
}

/// Owns native ACP events and conversation state, delegating policy to the host.
pub struct AcpSessionState {
    kind: ProviderKind,
    admission: AtomicBool,
    admission_revocation: tokio_util::sync::CancellationToken,
    collected: AtomicBool,
    provider_admission: Option<Arc<AtomicBool>>,
    first_failure: Mutex<Option<ProviderError>>,
    permission: Option<Arc<dyn ScopedPermissions>>,
    active: RwLock<Option<ActiveTurn>>,
    retained: RwLock<Option<ActiveTurn>>,
    history_replay: AtomicBool,
    callback_work: Arc<RwLock<()>>,
    child_permissions: Mutex<HashMap<String, ChildPermissionAuthority>>,
    child_turns: Mutex<HashMap<TurnAddress, Arc<super::child_turn::AcpChildTurn>>>,
    operation: RwLock<Option<ActiveOperation>>,
    output: Mutex<forge_provider_api::completion::ProviderCompletion>,
    catalog: RwLock<Value>,
    capabilities: RwLock<
        Option<(
            String,
            ProviderEventSink,
            forge_provider_api::capability::CapabilityManifest,
        )>,
    >,
    tool: Mutex<HashMap<(String, String), Value>>,
    tool_owner: Mutex<HashMap<(String, String), TurnAddress>>,
    finished_tool: Mutex<HashSet<(String, String)>>,
    admitted_tool: Mutex<HashMap<(String, String), AcpToolItem>>,
    started_tool: Mutex<HashSet<(String, String)>>,
    compaction: Mutex<HashMap<String, Compaction>>,
    children: Mutex<super::children::AcpChildren>,
    message: Mutex<HashMap<TurnAddress, Vec<AcpMessage>>>,
    usage_owner: Mutex<HashMap<UsageReportId, TurnAddress>>,
    unsupported_updates: Mutex<HashSet<String>>,
    /// Notice text already surfaced on this binding; agents repeat startup notices every turn.
    reported_notices: Mutex<HashSet<String>>,
}

/// Capture immutable callback routing for the admitted provider turn.
#[derive(Clone)]
struct ActiveTurn {
    generation: Arc<()>,
    revocation: tokio_util::sync::CancellationToken,
    sink: ProviderEventSink,
    permissions: Option<Arc<dyn forge_provider_api::permissions::ProviderPermissions>>,
    execution_started: bool,
    native_start: Option<NativeTurnStart>,
}

/// Retain independently observed child permission work after parent foreground collection.
#[derive(Clone)]
struct ChildPermissionAuthority {
    generation: Arc<()>,
    parent_scope: InvocationScope,
    permissions: Arc<dyn forge_provider_api::permissions::ProviderPermissions>,
    revocation: tokio_util::sync::CancellationToken,
    callback_work: Arc<RwLock<()>>,
}

struct ActiveOperation {
    session_id: String,
    sink: ProviderEventSink,
}

/// Retain the immutable normalized item kind after native metadata becomes sufficient.
struct AcpToolItem {
    item_id: ItemId,
    kind: ToolKind,
}

struct AcpMessage {
    address: TurnAddress,
    item_id: ItemId,
    /// The agent's optional ACP `messageId`, which marks where one native message ends.
    native_message_id: Option<String>,
    role: Option<AcpMessageRole>,
    text: String,
    closed: bool,
}

/// Temporarily suppress observations caused only by ACP replaying saved history.
pub(super) struct HistoryReplayGuard<'state> {
    active: &'state AtomicBool,
}

impl Drop for HistoryReplayGuard<'_> {
    fn drop(&mut self) {
        self.active.store(false, Ordering::Release);
    }
}

impl AcpSessionState {
    /// Create session state whose permission authority is installed with each invocation.
    pub fn new_native(kind: ProviderKind) -> Self {
        Self::with_permission(kind, None)
    }

    /// Create native callback state sharing the provider's immediate shutdown admission gate.
    pub(super) fn new_binding(kind: ProviderKind, provider_admission: Arc<AtomicBool>) -> Self {
        let mut state = Self::new_native(kind);
        state.provider_admission = Some(provider_admission);
        state
    }

    fn with_permission(kind: ProviderKind, permission: Option<Arc<dyn ScopedPermissions>>) -> Self {
        Self {
            kind,
            admission: AtomicBool::new(true),
            admission_revocation: tokio_util::sync::CancellationToken::new(),
            collected: AtomicBool::new(false),
            provider_admission: None,
            first_failure: Mutex::new(None),
            permission,
            active: RwLock::new(None),
            retained: RwLock::new(None),
            history_replay: AtomicBool::new(false),
            callback_work: Arc::new(RwLock::new(())),
            child_permissions: Mutex::new(HashMap::new()),
            child_turns: Mutex::new(HashMap::new()),
            operation: RwLock::new(None),
            output: Mutex::new(forge_provider_api::completion::ProviderCompletion::default()),
            catalog: RwLock::new(json!({})),
            capabilities: RwLock::new(None),
            tool: Mutex::new(HashMap::new()),
            tool_owner: Mutex::new(HashMap::new()),
            finished_tool: Mutex::new(HashSet::new()),
            admitted_tool: Mutex::new(HashMap::new()),
            started_tool: Mutex::new(HashSet::new()),
            compaction: Mutex::new(HashMap::new()),
            children: Mutex::new(super::children::AcpChildren::default()),
            message: Mutex::new(HashMap::new()),
            usage_owner: Mutex::new(HashMap::new()),
            unsupported_updates: Mutex::new(HashSet::new()),
            reported_notices: Mutex::new(HashSet::new()),
        }
    }

    /// Reset only turn-scoped output, retaining discovered session metadata.
    pub async fn begin(
        &self,
        session_id: &str,
        sink: ProviderEventSink,
    ) -> Result<(), ProviderError> {
        if self.operation.read().await.is_some() {
            return Err(provider_failure(ProviderFailureKind::Rejected));
        }
        let scope = sink.invocation_scope()?.clone();
        *self.retained.write().await = None;
        *self.output.lock().await = forge_provider_api::completion::ProviderCompletion::default();
        self.compaction.lock().await.clear();
        self.tool
            .lock()
            .await
            .retain(|(session, _), _| session != session_id);
        self.tool_owner
            .lock()
            .await
            .retain(|(session, _), _| session != session_id);
        self.finished_tool
            .lock()
            .await
            .retain(|(session, _)| session != session_id);
        self.admitted_tool
            .lock()
            .await
            .retain(|(session, _), _| session != session_id);
        self.started_tool
            .lock()
            .await
            .retain(|(session, _)| session != session_id);
        self.children.lock().await.begin(session_id.into(), scope);
        let mut active = self.active.write().await;
        self.ensure_admission(ProviderOperation::TurnStart)?;
        *active = Some(ActiveTurn {
            generation: Arc::new(()),
            revocation: tokio_util::sync::CancellationToken::new(),
            sink,
            permissions: None,
            execution_started: false,
            native_start: None,
        });
        Ok(())
    }

    /// Begin an invocation with its normalized host permission callback.
    pub async fn begin_native(
        &self,
        session_id: &str,
        sink: ProviderEventSink,
        permissions: Arc<dyn forge_provider_api::permissions::ProviderPermissions>,
    ) -> Result<(), ProviderError> {
        self.begin(session_id, sink).await?;
        if let Some(active) = self.active.write().await.as_mut() {
            self.ensure_admission(ProviderOperation::TurnStart)?;
            if active.revocation.is_cancelled() {
                return Err(ProviderError::Cancelled);
            }
            active.permissions = Some(permissions);
        }
        Ok(())
    }

    /// Suppress late history replay while a retained ACP conversation is loaded again.
    pub(super) fn suppress_history_replay(&self) -> Result<HistoryReplayGuard<'_>, ProviderError> {
        self.history_replay
            .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
            .map_err(|_| provider_failure(ProviderFailureKind::Rejected))?;
        Ok(HistoryReplayGuard {
            active: &self.history_replay,
        })
    }

    /// Install the shared runner's positive-start authority before prompt dispatch.
    pub(super) async fn install_native_start(&self, start: NativeTurnStart) {
        if let Some(active) = self.active.write().await.as_mut() {
            active.native_start = Some(start);
        }
    }

    /// Record positive processing evidence independently of input acceptance.
    pub(super) async fn observe_native_start(
        &self,
        observed_at_ms: i64,
    ) -> Result<(), ProviderError> {
        let (start, first) = {
            let mut active = self.active.write().await;
            let active = active
                .as_mut()
                .ok_or_else(|| provider_failure(ProviderFailureKind::WrongInvocation))?;
            let first = !active.execution_started;
            active.execution_started = true;
            (active.native_start.clone(), first)
        };
        if let Some(start) = start {
            start.observed(observed_at_ms).await?;
        }
        if first {
            self.publish_resolved_model(observed_at_ms).await?;
        }
        Ok(())
    }

    /// Confirm the session's selected native model once the root turn has started.
    ///
    /// ACP reports no per-turn model, so the session's current model selection is the model
    /// that runs the turn. Without this confirmation Harness keeps the model "unconfirmed".
    async fn publish_resolved_model(&self, observed_at_ms: i64) -> Result<(), ProviderError> {
        let Some(root) = self.children.lock().await.root_address() else {
            return Ok(());
        };
        let catalog = self.catalog().await;
        let Some(model_id) = super::conversation::current_model_id(&catalog)
            .and_then(|model| ModelId::new(model.to_owned()).ok())
        else {
            return Ok(());
        };
        // `emit` can observe the native start, which publishes this confirmation.
        Box::pin(self.emit(AdapterActivity::Turn {
            thread_id: root.thread_id,
            turn_id: root.turn_id,
            event: TurnMetadataEvent::ModelResolved(ResolvedModel {
                model_id,
                observed_at_ms,
            }),
        }))
        .await
    }

    /// Publish activity through the callback's original invocation ownership.
    pub async fn emit(&self, event: AdapterActivity) -> Result<(), ProviderError> {
        let root = self.children.lock().await.root_address();
        let starts_root = match &event {
            AdapterActivity::Item {
                thread_id, turn_id, ..
            }
            | AdapterActivity::Turn {
                thread_id,
                turn_id,
                event: TurnMetadataEvent::ChecklistUpdated(_) | TurnMetadataEvent::Usage(_),
                ..
            } => {
                self.active.read().await.is_some()
                    && root.as_ref().is_some_and(|root| {
                        &root.thread_id == thread_id && &root.turn_id == turn_id
                    })
            }
            AdapterActivity::Thread { .. }
            | AdapterActivity::Turn { .. }
            | AdapterActivity::Agent { .. }
            | AdapterActivity::Warning(_) => false,
        };
        if starts_root {
            self.observe_native_start(super::children::timestamp())
                .await?;
        }
        if let AdapterActivity::Item {
            thread_id, turn_id, ..
        }
        | AdapterActivity::Turn {
            thread_id,
            turn_id,
            event: TurnMetadataEvent::Usage(_) | TurnMetadataEvent::ChecklistUpdated(_),
            ..
        } = &event
        {
            let child = self
                .child_turns
                .lock()
                .await
                .iter()
                .find(|(address, _)| &address.thread_id == thread_id && &address.turn_id == turn_id)
                .map(|(_, child)| child.clone());
            if let Some(child) = child {
                child.observe(super::children::timestamp()).await?;
            }
        }
        let sink = self
            .publication_sink()
            .await
            .ok_or_else(|| provider_failure(ProviderFailureKind::Rejected))?;
        let writer = sink.activity_writer();
        let address = match &event {
            AdapterActivity::Turn {
                event: TurnMetadataEvent::Usage(report),
                ..
            } => self
                .usage_owner
                .lock()
                .await
                .get(report.report_id())
                .cloned(),
            AdapterActivity::Item {
                thread_id, turn_id, ..
            }
            | AdapterActivity::Turn {
                thread_id, turn_id, ..
            } => self
                .children
                .lock()
                .await
                .observed_address(thread_id, turn_id),
            AdapterActivity::Thread { thread_id, .. } => {
                self.children.lock().await.thread_address(thread_id)
            }
            AdapterActivity::Agent { .. } | AdapterActivity::Warning(_) => None,
        };
        let writer = match address {
            Some(address) => writer.register_observed_turn(address)?,
            None => writer,
        };
        writer.send_wait(event).await?;
        Ok(())
    }

    /// Report whether native observations established a turn for the active prompt.
    pub async fn execution_started(&self) -> bool {
        self.active
            .read()
            .await
            .as_ref()
            .is_some_and(|active| active.execution_started)
            || self
                .retained
                .read()
                .await
                .as_ref()
                .is_some_and(|retained| retained.execution_started)
    }

    /// Get whether this native binding still permits new operations.
    pub(super) fn is_admitting(&self) -> bool {
        self.admission.load(Ordering::Acquire)
            && self
                .provider_admission
                .as_ref()
                .is_none_or(|admission| admission.load(Ordering::Acquire))
    }

    /// Close native admission before any asynchronous cleanup boundary.
    pub(super) fn close_admission(&self) {
        self.admission.store(false, Ordering::Release);
        self.admission_revocation.cancel();
    }

    /// Get the original failure after admission was revoked by native observation.
    pub(super) async fn binding_failure(&self) -> Option<ProviderError> {
        self.first_failure.lock().await.clone()
    }

    /// Retain the original typed binding failure across cleanup retries.
    pub(super) async fn retain_failure(&self, cause: ProviderError) -> ProviderError {
        self.first_failure.lock().await.get_or_insert(cause).clone()
    }

    /// Record positive full-resource collection separately from admission revocation.
    pub(super) fn mark_collected(&self) {
        self.collected.store(true, Ordering::Release);
    }

    /// Reject revoked work as invalidated until native owners prove full collection.
    pub(super) fn ensure_admission(
        &self,
        operation: ProviderOperation,
    ) -> Result<(), ProviderError> {
        if self.is_admitting() {
            return Ok(());
        }
        let (kind, disposition) = if self.collected.load(Ordering::Acquire) {
            (
                ProviderFailureKind::BindingClosed,
                FailureDisposition::Closed,
            )
        } else {
            (
                ProviderFailureKind::BindingInvalidated,
                FailureDisposition::Invalidated,
            )
        };
        Err(ProviderError::Failure(
            ProviderFailure::new(operation, kind).at_phase(ProviderPhase::Admission, disposition),
        ))
    }

    /// Revoke a failed native binding without waiting on the callback currently reporting it.
    pub(super) async fn invalidate_binding(&self, cause: ProviderError) {
        self.close_admission();
        self.retain_failure(cause).await;
        self.revoke_authority().await;
    }

    /// Revoke authority locally without waiting for callback collection.
    pub(super) async fn revoke_authority(&self) {
        if let Some(active) = self.active.write().await.as_mut() {
            active.revocation.cancel();
            active.permissions = None;
        }
        if let Some(retained) = self.retained.write().await.as_mut() {
            retained.revocation.cancel();
            retained.permissions = None;
        }
    }

    /// Revoke invocation callback authority before a fenced process is collected.
    pub async fn revoke_callbacks(&self) {
        self.revoke_authority().await;
        let children = self
            .child_permissions
            .lock()
            .await
            .drain()
            .map(|(_, child)| child)
            .collect::<Vec<_>>();
        for child in &children {
            child.revocation.cancel();
        }
        let _collected = self.callback_work.write().await;
        for child in children {
            let _collected = child.callback_work.write().await;
        }
        let turns = self
            .child_turns
            .lock()
            .await
            .values()
            .cloned()
            .collect::<Vec<_>>();
        for child in turns {
            child.close().await;
        }
    }

    /// Revoke only the named invocation when cancellation races with a successor.
    pub(super) async fn revoke_invocation_callbacks(&self, scope: &InvocationScope) {
        let mut revoked = false;
        if let Some(active) = self.active.write().await.as_mut().filter(|active| {
            active
                .sink
                .invocation_scope()
                .is_ok_and(|current| current == scope)
        }) {
            active.revocation.cancel();
            active.permissions = None;
            revoked = true;
        }
        if let Some(retained) = self.retained.write().await.as_mut().filter(|retained| {
            retained
                .sink
                .invocation_scope()
                .is_ok_and(|current| current == scope)
        }) {
            retained.revocation.cancel();
            retained.permissions = None;
            revoked = true;
        }
        if revoked {
            let _collected = self.callback_work.write().await;
        }
    }

    /// Get the active operation's publisher before falling back to a settled invocation.
    async fn publication_sink(&self) -> Option<ProviderEventSink> {
        if let Some(active) = self.active.read().await.as_ref() {
            return Some(active.sink.clone());
        }
        if let Some(operation) = self.operation.read().await.as_ref() {
            return Some(operation.sink.clone());
        }
        if let Some(retained) = self.retained.read().await.as_ref() {
            return Some(retained.sink.clone());
        }
        self.capabilities
            .read()
            .await
            .as_ref()
            .map(|(_, sink, _)| sink.clone())
    }

    /// Recognize observation context without granting permission to start new work.
    async fn has_observation_context(&self) -> bool {
        self.active.read().await.is_some()
            || self.retained.read().await.is_some()
            || self.capabilities.read().await.is_some()
    }

    pub async fn finish_compaction(&self) {
        *self.operation.write().await = None;
    }

    pub async fn has_active_work(&self) -> bool {
        self.active.read().await.is_some()
            || self.operation.read().await.is_some()
            || self
                .child_turns
                .lock()
                .await
                .values()
                .any(|child| !child.is_finished())
    }

    async fn event_scope(&self) -> Result<EventScope> {
        Ok(self
            .publication_sink()
            .await
            .context("ACP update has no observation owner")?
            .scope()
            .clone())
    }

    async fn is_root(&self, session_id: &str) -> bool {
        if let Some(operation) = self.operation.read().await.as_ref() {
            return operation.session_id == session_id;
        }
        if let Some((root, _, _)) = self.capabilities.read().await.as_ref() {
            return root == session_id;
        }
        self.children.lock().await.is_root(session_id)
    }

    pub async fn address(&self, session_id: &str) -> Option<TurnAddress> {
        self.children.lock().await.address(session_id, false)
    }

    /// Append text to the identified native stream without inventing empty items.
    async fn message_chunk(
        &self,
        session_id: &str,
        role: Option<AcpMessageRole>,
        native_message_id: Option<&str>,
        text: &str,
    ) -> Result<()> {
        if text.is_empty() {
            return Ok(());
        }
        // Text from a session no `subagent_spawned` announced, or whose child already finished, has
        // no owner to attribute it to. Dropping it keeps one stray chunk from failing the root turn.
        let Some(turn) = self.address(session_id).await else {
            return Ok(());
        };
        let event = {
            let mut messages = self.message.lock().await;
            let messages = messages.entry(turn.clone()).or_default();
            // Chunks extend the open message only while they keep its role and native message ID.
            // Agents that omit `messageId` match on role alone, as ACP v1 allows.
            let (item_id, event) = if let Some(message) = messages.last_mut().filter(|message| {
                !message.closed
                    && message.role == role
                    && message.native_message_id.as_deref() == native_message_id
            }) {
                message.text.push_str(text);
                (
                    message.item_id.clone(),
                    ItemEvent::Message {
                        event: MessageItemEvent::Delta(text.into()),
                    },
                )
            } else {
                let item_id = ItemId::new(uuid::Uuid::new_v4().to_string())?;
                messages.push(AcpMessage {
                    address: turn.clone(),
                    item_id: item_id.clone(),
                    native_message_id: native_message_id.map(str::to_owned),
                    role,
                    text: text.into(),
                    closed: false,
                });
                let start = if role == Some(AcpMessageRole::Reasoning) {
                    ItemStart::Reasoning(ReasoningStart {
                        text: text.into(),
                        kind: ReasoningKind::Full,
                    })
                } else {
                    ItemStart::AgentMessage(AgentMessageStart {
                        text: text.into(),
                        classification: message_classification(role),
                    })
                };
                (item_id, ItemEvent::Started(start))
            };
            AdapterActivity::Item {
                thread_id: turn.thread_id.clone(),
                turn_id: turn.turn_id.clone(),
                item_id,
                event,
            }
        };
        Ok(self.emit(event).await?)
    }

    /// Classify prose at native tool boundaries while preserving reasoning identity.
    async fn classify_messages(
        &self,
        address: &TurnAddress,
        last_role: AcpMessageRole,
    ) -> Result<()> {
        let updates = {
            let mut messages = self.message.lock().await;
            let pending = messages.entry(address.clone()).or_default();
            let count = pending
                .iter()
                .filter(|message| message.role.is_none())
                .count();
            pending
                .iter_mut()
                .filter(|message| message.role.is_none())
                .enumerate()
                .map(|(index, message)| {
                    let role = if index + 1 == count {
                        last_role
                    } else {
                        AcpMessageRole::Commentary
                    };
                    message.role = Some(role);
                    message.closed = true;
                    AdapterActivity::Item {
                        thread_id: address.thread_id.clone(),
                        turn_id: address.turn_id.clone(),
                        item_id: message.item_id.clone(),
                        event: ItemEvent::Message {
                            event: MessageItemEvent::Classified(message_classification(Some(role))),
                        },
                    }
                })
                .collect::<Vec<_>>()
        };
        for update in updates {
            self.emit(update).await?;
        }
        Ok(())
    }

    /// Close observed children and release callback state even when delivery fails.
    pub async fn finish(
        &self,
        outcome: forge_provider_api::turn::TurnOutcome,
    ) -> Result<(), ProviderError> {
        if self.active.read().await.is_none() {
            self.finish_compaction().await;
            return Ok(());
        }
        let result: Result<(), ProviderError> = async {
            let root = self.children.lock().await.root_address();
            if let Some(address) = root {
                self.finish_messages(&address, outcome)
                    .await
                    .map_err(|_| provider_failure(ProviderFailureKind::Protocol))?;
                self.finish_tools(&address).await?;
            }
            Ok(())
        }
        .await;
        *self.retained.write().await = self.active.write().await.take();
        result
    }

    async fn finish_messages(
        &self,
        address: &TurnAddress,
        outcome: forge_provider_api::turn::TurnOutcome,
    ) -> Result<(), ProviderError> {
        if matches!(
            outcome,
            forge_provider_api::turn::TurnOutcome::Completed { .. }
        ) {
            self.classify_messages(address, AcpMessageRole::Response)
                .await
                .map_err(|_| provider_failure(ProviderFailureKind::Protocol))?;
        }
        let updates = {
            let mut messages = self.message.lock().await;
            messages
                .remove(address)
                .unwrap_or_default()
                .into_iter()
                .map(|message| {
                    let completion = MessageCompletion {
                        text: message.text,
                        outcome: if matches!(
                            outcome,
                            forge_provider_api::turn::TurnOutcome::Completed { .. }
                        ) {
                            MessageOutcome::Completed
                        } else {
                            MessageOutcome::Interrupted
                        },
                    };
                    let completion = if message.role == Some(AcpMessageRole::Reasoning) {
                        ItemCompletion::Reasoning(completion)
                    } else {
                        ItemCompletion::AgentMessage(completion)
                    };
                    AdapterActivity::Item {
                        thread_id: message.address.thread_id,
                        turn_id: message.address.turn_id,
                        item_id: message.item_id,
                        event: ItemEvent::Completed(completion),
                    }
                })
                .collect::<Vec<_>>()
        };
        for update in updates {
            self.emit(update).await?;
        }
        Ok(())
    }

    /// Close unfinished foreground operations at native end before provider collection waits.
    async fn finish_tools(&self, address: &TurnAddress) -> Result<(), ProviderError> {
        let keys = self
            .tool_owner
            .lock()
            .await
            .iter()
            .filter(|(_, owner)| *owner == address)
            .map(|(key, _)| key.clone())
            .collect::<Vec<_>>();
        for key in keys {
            if !self.finished_tool.lock().await.insert(key.clone()) {
                continue;
            }
            let snapshot = self
                .tool
                .lock()
                .await
                .get(&key)
                .cloned()
                .ok_or_else(|| provider_failure(ProviderFailureKind::Protocol))?;
            let (kind, descriptor) = normalize_tool_descriptor(&snapshot);
            let (item_id, admitted) = {
                let mut items = self.admitted_tool.lock().await;
                if let Some(item) = items.get(&key) {
                    if item.kind != kind {
                        return Err(provider_failure(ProviderFailureKind::Protocol));
                    }
                    (item.item_id.clone(), false)
                } else {
                    let id = ItemId::new(uuid::Uuid::new_v4().to_string())
                        .map_err(|_| provider_failure(ProviderFailureKind::Protocol))?;
                    items.insert(
                        key,
                        AcpToolItem {
                            item_id: id.clone(),
                            kind,
                        },
                    );
                    (id, true)
                }
            };
            if admitted {
                let start = match kind {
                    ToolKind::Command => ItemStart::CommandExecution(descriptor.clone()),
                    ToolKind::FileChange => ItemStart::FileChange(descriptor.clone()),
                    ToolKind::ToolCall => ItemStart::ToolCall(descriptor.clone()),
                };
                self.emit(item_activity(address, &item_id, ItemEvent::Started(start)))
                    .await?;
            }
            let completion = ToolCompletion {
                outcome: ToolOutcome::Interrupted,
                descriptor: Some(descriptor),
                output: None,
                changes: Default::default(),
            };
            let completion = match kind {
                ToolKind::Command => ItemCompletion::CommandExecution(completion),
                ToolKind::FileChange => ItemCompletion::FileChange(completion),
                ToolKind::ToolCall => ItemCompletion::ToolCall(completion),
            };
            self.emit(item_activity(
                address,
                &item_id,
                ItemEvent::Completed(completion),
            ))
            .await?;
        }
        Ok(())
    }

    async fn authorize(&self, parameter: Value) -> Result<Value> {
        if !self.is_admitting() {
            return Ok(json!({"outcome":{"outcome":"cancelled"}}));
        }
        let session_id = parameter["sessionId"].as_str().unwrap_or_default();
        let child_authority = self.child_permissions.lock().await.get(session_id).cloned();
        let active = self.active.read().await.clone();
        let (parent_scope, permissions, revocation, callback_work, generation) =
            if let Some(child) = &child_authority {
                (
                    child.parent_scope.clone(),
                    Some(child.permissions.clone()),
                    child.revocation.clone(),
                    child.callback_work.clone(),
                    child.generation.clone(),
                )
            } else {
                ensure!(
                    self.is_root(session_id).await,
                    "ACP child has no registered permission authority"
                );
                let active = active
                    .as_ref()
                    .context("ACP permission request has no active root turn")?;
                (
                    active.sink.invocation_scope()?.clone(),
                    active.permissions.clone(),
                    active.revocation.clone(),
                    self.callback_work.clone(),
                    active.generation.clone(),
                )
            };
        let _callback = callback_work.read().await;
        let authorizing = async {
            let session_id = parameter["sessionId"].as_str().unwrap_or_default();
            let address = if let Some(id) = parameter["toolCall"]["toolCallId"].as_str() {
                self.tool_owner
                    .lock()
                    .await
                    .get(&(session_id.to_owned(), id.to_owned()))
                    .cloned()
            } else {
                None
            };
            if address.is_none()
                && parameter["toolCall"]["toolCallId"].is_string()
                && !["title", "kind", "rawInput", "locations", "content"]
                    .iter()
                    .any(|field| {
                        parameter["toolCall"]
                            .get(*field)
                            .is_some_and(|value| !value.is_null())
                    })
            {
                return Err(ProviderError::Observation(
                    forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
                )
                .into());
            }
            let address = match address {
                Some(address) => address,
                None => self
                    .address(session_id)
                    .await
                    .context("ACP permission request has no active session owner")?,
            };
            let child = child_authority.is_some();
            if !child {
                self.observe_native_start(super::children::timestamp())
                    .await?;
            } else {
                let turn = self
                    .child_turns
                    .lock()
                    .await
                    .get(&address)
                    .cloned()
                    .context("ACP child permission has no retained native turn owner")?;
                turn.observe(super::children::timestamp()).await?;
            }
            ensure!(
                child || parent_scope == address.scope,
                "ACP permission request changed its original root owner"
            );
            if let Some(permissions) = &permissions {
                if child {
                    permissions
                        .before_observed_child(&parent_scope, &address)
                        .await?;
                } else {
                    permissions.before_tool(&address.scope).await?;
                }
            } else if child {
                return Ok(json!({"outcome":{"outcome":"cancelled"}}));
            }
            let supplied = &parameter["toolCall"];
            let item_id = supplied["toolCallId"].as_str();
            let session_id = parameter["sessionId"].as_str().unwrap_or_default();
            let observed_tool = if let Some(item_id) = item_id {
                self.tool
                    .lock()
                    .await
                    .get(&(session_id.to_owned(), item_id.to_owned()))
                    .cloned()
            } else {
                None
            };
            let mut tool = observed_tool.clone().unwrap_or_else(|| json!({}));
            if let Some(fields) = supplied.as_object() {
                for (field, value) in fields {
                    if !value.is_null() {
                        tool[field] = value.clone();
                    }
                }
            }
            let request = super::permission::permission_request(self.kind, &tool)?;
            let resolution = if let Some(permissions) = &permissions {
                let decision = if child {
                    permissions
                        .authorize_observed_child(&parent_scope, &address, request)
                        .await
                } else {
                    permissions.authorize(&address.scope, request).await
                };
                match decision {
                    forge_provider_api::permissions::PermissionResponse::Allow => {
                        PermissionResponse::Allow
                    }
                    forge_provider_api::permissions::PermissionResponse::Deny { reason } => {
                        PermissionResponse::Deny { reason }
                    }
                    forge_provider_api::permissions::PermissionResponse::Cancelled => {
                        PermissionResponse::Cancelled
                    }
                }
            } else if let Some(permission) = &self.permission {
                permission.authorize(address, request).await?
            } else {
                return Err(
                    ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::TurnStart,
                        forge_provider_api::ProviderFailureKind::Rejected,
                    ))
                    .into(),
                );
            };
            let current = if child {
                self.child_permissions
                    .lock()
                    .await
                    .get(session_id)
                    .is_some_and(|current| {
                        Arc::ptr_eq(&current.generation, &generation)
                            && !current.revocation.is_cancelled()
                    })
            } else {
                self.active.read().await.as_ref().is_some_and(|current| {
                    Arc::ptr_eq(&current.generation, &generation)
                        && !current.revocation.is_cancelled()
                })
            };
            if !current || !self.is_admitting() {
                return Ok(json!({"outcome":{"outcome":"cancelled"}}));
            }
            if matches!(resolution, PermissionResponse::Allow)
                && let (Some(item_id), Some(observed)) = (item_id, &observed_tool)
            {
                let current = self.tool.lock().await;
                ensure!(
                    current
                        .get(&(session_id.to_owned(), item_id.to_owned()))
                        .is_some_and(|current| current == observed),
                    "ACP tool changed while approval was pending"
                );
            }
            if matches!(resolution, PermissionResponse::Cancelled) {
                return Ok(json!({"outcome":{"outcome":"cancelled"}}));
            }
            let allow = matches!(resolution, PermissionResponse::Allow);
            let kind = if allow { "allow_once" } else { "reject_once" };
            let option = parameter["options"]
                .as_array()
                .and_then(|options| options.iter().find(|option| option["kind"] == kind))
                .context("ACP agent did not advertise the required once-only permission choice")?;
            Ok(json!({"outcome":{"outcome":"selected","optionId":option["optionId"]}}))
        };
        tokio::select! {
            biased;
            _ = self.admission_revocation.cancelled() => Ok(json!({"outcome":{"outcome":"cancelled"}})),
            _ = revocation.cancelled() => Ok(json!({"outcome":{"outcome":"cancelled"}})),
            result = authorizing => result,
        }
    }
}

impl AcpSessionState {
    async fn handle_notification(&self, method: &str, parameter: Value) -> Result<()> {
        if method != "session/update" {
            return Ok(());
        }
        let update = &parameter["update"];
        if self.history_replay.load(Ordering::Acquire)
            && !matches!(
                update["sessionUpdate"].as_str(),
                Some("available_commands_update" | "config_option_update" | "current_mode_update")
            )
        {
            return Ok(());
        }
        let session_id = parameter["sessionId"].as_str().unwrap_or_default();
        if !self.has_observation_context().await
            && !matches!(
                update["sessionUpdate"].as_str(),
                Some("available_commands_update" | "config_option_update" | "current_mode_update")
            )
            && !(self.operation.read().await.is_some()
                && matches!(
                    update["sessionUpdate"].as_str(),
                    Some("compaction_update" | "compaction_summary_chunk" | "usage_update")
                ))
        {
            return Ok(());
        }
        let idle_root = self.active.read().await.is_none() && self.is_root(session_id).await;
        let starts_unowned_work = idle_root
            && match update["sessionUpdate"].as_str() {
                Some("agent_message_chunk" | "agent_thought_chunk" | "plan") => true,
                Some("subagent_spawned") => true,
                Some("tool_call" | "tool_call_update") => match update["toolCallId"].as_str() {
                    Some(id) => !self
                        .tool
                        .lock()
                        .await
                        .contains_key(&(session_id.to_owned(), id.to_owned())),
                    None => true,
                },
                _ => false,
            };
        if starts_unowned_work {
            return Err(ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::LifecycleUnavailable,
            )
            .into());
        }
        let lifecycle = self.children.lock().await.lifecycle(session_id, update)?;
        if let Some(observation) = lifecycle {
            let completed_at_ms = super::children::timestamp();
            let child_id = update["subagentSessionId"]
                .as_str()
                .context("ACP child has no native identity")?;
            let sink = self
                .publication_sink()
                .await
                .context("ACP child has no observation owner")?;
            let child_turn = {
                let mut turns = self.child_turns.lock().await;
                match turns.entry(observation.address.clone()) {
                    std::collections::hash_map::Entry::Occupied(entry) => entry.get().clone(),
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        let child = Arc::new(super::child_turn::AcpChildTurn::new(
                            sink.clone(),
                            observation.address.clone(),
                            super::child_turn::ChildResources::new(),
                        )?);
                        entry.insert(child.clone());
                        child
                    }
                }
            };
            let resources = child_turn.resources();
            if observation.outcome.is_none()
                && !self.child_permissions.lock().await.contains_key(child_id)
            {
                let parent_scope = self
                    .children
                    .lock()
                    .await
                    .address(session_id, true)
                    .context("ACP child has no original delegating turn")?
                    .scope;
                let permissions = if self.is_root(session_id).await {
                    self.active
                        .read()
                        .await
                        .as_ref()
                        .and_then(|active| active.permissions.clone())
                } else {
                    self.child_permissions
                        .lock()
                        .await
                        .get(session_id)
                        .map(|parent| parent.permissions.clone())
                };
                if let Some(permissions) = permissions {
                    self.child_permissions.lock().await.insert(
                        child_id.into(),
                        ChildPermissionAuthority {
                            generation: Arc::new(()),
                            parent_scope,
                            permissions,
                            revocation: resources.revocation(),
                            callback_work: resources.callbacks(),
                        },
                    );
                }
            }
            let delegation = if let AdapterActivity::Agent {
                event: AgentEvent::Lifecycle { event },
                ..
            } = &observation.activity
            {
                let agent = match event {
                    AgentLifecycleEvent::Started { agent, .. }
                    | AgentLifecycleEvent::Updated { agent, .. }
                    | AgentLifecycleEvent::Finished { agent, .. } => agent,
                };
                agent.delegation.as_ref()
            } else {
                None
            };
            let original_root = self
                .children
                .lock()
                .await
                .root_address()
                .is_some_and(|root| {
                    delegation.is_some_and(|delegation| {
                        delegation.parent_thread_id == root.thread_id
                            && delegation.parent_turn_id == root.turn_id
                    })
                });
            if self.active.read().await.is_some() && self.is_root(session_id).await && original_root
            {
                self.observe_native_start(super::children::timestamp())
                    .await?;
            }
            let writer = sink
                .activity_writer()
                .register_observed_turn(observation.address.clone())?;
            if let Some(outcome) = observation.outcome {
                if !matches!(outcome, turn::TurnOutcome::Interrupted) {
                    child_turn.observe(completed_at_ms).await?;
                }
                self.child_permissions.lock().await.remove(child_id);
                self.finish_messages(&observation.address, outcome).await?;
                self.finish_tools(&observation.address).await?;
            }
            let starts_child = matches!(
                &observation.activity,
                AdapterActivity::Agent {
                    event: AgentEvent::Lifecycle {
                        event: AgentLifecycleEvent::Started { .. }
                    },
                    ..
                }
            );
            writer.send_wait(observation.activity).await?;
            if starts_child {
                child_turn.relationship_published();
            }
            if let Some(outcome) = observation.outcome {
                child_turn.complete(outcome, completed_at_ms).await?;
            }
            return Ok(());
        }
        match update["sessionUpdate"].as_str().unwrap_or_default() {
            "available_commands_update" => {
                if !self.is_root(session_id).await {
                    return Ok(());
                }
                self.update_catalog(json!({"availableCommands": update["availableCommands"]}))
                    .await;
            }
            "config_option_update" => {
                if !self.is_root(session_id).await {
                    return Ok(());
                }
                self.update_catalog(json!({"configOptions": update["configOptions"]}))
                    .await;
            }
            "plan" => {
                use agent_client_protocol::schema::v1::{
                    PlanEntry, PlanEntryPriority, PlanEntryStatus,
                };
                let invalid = || {
                    ProviderError::Observation(
                        forge_provider_api::lifecycle::ObservationFailure::Protocol,
                    )
                };
                let entries: Vec<PlanEntry> =
                    serde_json::from_value(update.get("entries").cloned().ok_or_else(invalid)?)
                        .map_err(|_| invalid())?;
                let address = self.address(session_id).await.ok_or_else(invalid)?;
                let mut entry_list = Vec::new();
                for (provider_ordinal, entry) in entries.into_iter().enumerate() {
                    let status = match entry.status {
                        PlanEntryStatus::Pending => forge_provider_api::ChecklistStatus::Pending,
                        PlanEntryStatus::InProgress => {
                            forge_provider_api::ChecklistStatus::InProgress
                        }
                        PlanEntryStatus::Completed => {
                            forge_provider_api::ChecklistStatus::Completed
                        }
                        _ => return Err(invalid().into()),
                    };
                    let priority = match entry.priority {
                        PlanEntryPriority::High => "high",
                        PlanEntryPriority::Medium => "medium",
                        PlanEntryPriority::Low => "low",
                        _ => return Err(invalid().into()),
                    };
                    entry_list.push(forge_provider_api::ProviderChecklistEntry {
                        provider_id: None,
                        content: entry.content,
                        priority: Some(priority.into()),
                        status,
                        provider_ordinal,
                    });
                }
                self.emit(AdapterActivity::Turn {
                    event: TurnMetadataEvent::ChecklistUpdated(
                        forge_provider_api::ProviderChecklistUpdate {
                            scope_id: format!("acp:plan:{}", address.turn_id),
                            name: None,
                            complete: entry_list.iter().all(|entry| {
                                entry.status == forge_provider_api::ChecklistStatus::Completed
                            }),
                            replace_entries: true,
                            entry_list,
                        },
                    ),
                    thread_id: address.thread_id,
                    turn_id: address.turn_id,
                })
                .await?;
            }
            "agent_message_chunk" | "agent_thought_chunk" => {
                let thought = update["sessionUpdate"] == "agent_thought_chunk";
                let text = update
                    .pointer("/content/text")
                    .and_then(Value::as_str)
                    .unwrap_or_default();
                self.message_chunk(
                    session_id,
                    thought.then_some(AcpMessageRole::Reasoning),
                    update["messageId"].as_str().filter(|id| !id.is_empty()),
                    text,
                )
                .await?;
            }
            "tool_call" | "tool_call_update" => {
                let Some(id) = update["toolCallId"].as_str().filter(|id| !id.is_empty()) else {
                    return Ok(());
                };
                let turn = self.address(session_id).await.ok_or_else(|| {
                    ProviderError::Observation(
                        forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
                    )
                })?;
                {
                    let mut owners = self.tool_owner.lock().await;
                    let owner = owners
                        .entry((session_id.to_owned(), id.to_owned()))
                        .or_insert_with(|| turn.clone());
                    if owner != &turn {
                        return Err(ProviderError::Observation(
                            forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
                        )
                        .into());
                    }
                }
                let mut tool_map = self.tool.lock().await;
                let tool_key = (session_id.to_owned(), id.to_owned());
                let invocation_started = !tool_map.contains_key(&tool_key);
                let snapshot = tool_map.entry(tool_key).or_insert_with(|| json!({}));
                let finished = self
                    .finished_tool
                    .lock()
                    .await
                    .contains(&(session_id.to_owned(), id.to_owned()));
                if finished
                    && ["title", "kind", "rawInput", "status"]
                        .into_iter()
                        .any(|field| {
                            update.get(field).is_some_and(|value| {
                                !value.is_null() && snapshot.get(field) != Some(value)
                            })
                        })
                {
                    return Err(ProviderError::Observation(
                        forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
                    )
                    .into());
                }
                let invocation_changed = merge_tool_snapshot(snapshot, update)?;
                let snapshot = snapshot.clone();
                drop(tool_map);
                let update = &snapshot;
                // Only a newly started invocation shows the agent kept working after its prose.
                // Claude can report a terminal status for an earlier, abandoned tool after the
                // final answer, and that update must leave the answer for turn completion.
                if invocation_started {
                    self.classify_messages(&turn, AcpMessageRole::Commentary)
                        .await?;
                }
                let native_key = (session_id.to_owned(), id.to_owned());
                let status = update["status"].as_str().unwrap_or("in_progress");
                let status = match status {
                    "pending" | "in_progress" => ToolStatus::Running,
                    "completed" => ToolStatus::Succeeded,
                    "failed" => ToolStatus::Failed,
                    "denied" => ToolStatus::Denied,
                    "cancelled" => ToolStatus::Cancelled,
                    "interrupted" => ToolStatus::Interrupted,
                    _ => return Err(provider_failure(ProviderFailureKind::Protocol).into()),
                };
                // Delay immutable type admission until native metadata establishes its category.
                if update.get("kind").is_none_or(Value::is_null) && status == ToolStatus::Running {
                    return Ok(());
                }
                let (kind, descriptor) = normalize_tool_descriptor(update);
                let (item_id, admitted) = {
                    let mut items = self.admitted_tool.lock().await;
                    match items.entry(native_key.clone()) {
                        std::collections::hash_map::Entry::Occupied(entry) => {
                            if entry.get().kind != kind {
                                return Err(ProviderError::Observation(forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict).into());
                            }
                            (entry.get().item_id.clone(), false)
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            let id = ItemId::new(uuid::Uuid::new_v4().to_string())?;
                            entry.insert(AcpToolItem {
                                item_id: id.clone(),
                                kind,
                            });
                            (id, true)
                        }
                    }
                };
                if admitted {
                    let start = match kind {
                        ToolKind::Command => ItemStart::CommandExecution(descriptor.clone()),
                        ToolKind::FileChange => ItemStart::FileChange(descriptor.clone()),
                        ToolKind::ToolCall => ItemStart::ToolCall(descriptor.clone()),
                    };
                    self.emit(item_activity(&turn, &item_id, ItemEvent::Started(start)))
                        .await?;
                } else if invocation_changed {
                    self.emit(item_activity(
                        &turn,
                        &item_id,
                        ItemEvent::Tool {
                            event: ToolItemEvent::DescriptorUpdated(descriptor.clone()),
                        },
                    ))
                    .await?;
                }
                // A tool that succeeded or failed executed even when it skipped `in_progress`
                // (Claude reports its own tools as `pending` then `completed`); without a start,
                // Harness cannot measure it. Denied and cancelled tools never ran.
                let executed = update["status"] == "in_progress"
                    || matches!(status, ToolStatus::Succeeded | ToolStatus::Failed);
                if executed && self.started_tool.lock().await.insert(native_key.clone()) {
                    self.emit(item_activity(
                        &turn,
                        &item_id,
                        ItemEvent::Tool {
                            event: ToolItemEvent::ExecutionStarted,
                        },
                    ))
                    .await?;
                    self.output.lock().await.evidence.tool_called = true;
                }
                let output = tool_output(update)?;
                if let Some(output) = &output {
                    self.emit(item_activity(
                        &turn,
                        &item_id,
                        ItemEvent::Tool {
                            event: ToolItemEvent::Output(TextUpdate::Replace(output.clone())),
                        },
                    ))
                    .await?;
                }
                let changes = file_changes(update);
                if !changes.is_empty() {
                    if status == ToolStatus::Succeeded {
                        self.output.lock().await.evidence.workspace_changed = true;
                    }
                    self.emit(item_activity(
                        &turn,
                        &item_id,
                        ItemEvent::Tool {
                            event: ToolItemEvent::Changes(changes.clone()),
                        },
                    ))
                    .await?;
                }
                let outcome = match status {
                    ToolStatus::Running => None,
                    ToolStatus::Succeeded => Some(ToolOutcome::Succeeded),
                    ToolStatus::Failed => Some(ToolOutcome::Failed {
                        diagnostic: ToolDiagnostic {
                            kind: forge_provider_api::ProviderFailureKind::Unknown,
                            message: forge_provider_api::diagnostic::SanitizedMessage::new(
                                "Provider tool execution failed",
                            )
                            .expect("static diagnostic is valid"),
                        },
                    }),
                    ToolStatus::Denied => Some(ToolOutcome::Denied {
                        reason: forge_provider_api::diagnostic::SanitizedMessage::new(
                            "Tool execution was denied by the provider",
                        )
                        .expect("static denial is nonempty"),
                    }),
                    ToolStatus::Cancelled => Some(ToolOutcome::Cancelled),
                    ToolStatus::Interrupted => Some(ToolOutcome::Interrupted),
                };
                if let Some(outcome) = outcome
                    && self
                        .finished_tool
                        .lock()
                        .await
                        .insert((session_id.to_owned(), id.to_owned()))
                {
                    let completion = ToolCompletion {
                        outcome,
                        descriptor: Some(descriptor),
                        output,
                        changes,
                    };
                    let completion = match kind {
                        ToolKind::Command => ItemCompletion::CommandExecution(completion),
                        ToolKind::FileChange => ItemCompletion::FileChange(completion),
                        ToolKind::ToolCall => ItemCompletion::ToolCall(completion),
                    };
                    self.emit(item_activity(
                        &turn,
                        &item_id,
                        ItemEvent::Completed(completion),
                    ))
                    .await?;
                }
            }
            "compaction_update" => {
                if !self.is_root(session_id).await {
                    return Ok(());
                }
                let thread_id = self.event_scope().await?.thread_id().clone();
                if let Some(mut compaction) = normalize_compaction(update, thread_id.as_str()) {
                    compaction.manual |= self.operation.read().await.is_some();
                    let mut compaction_map = self.compaction.lock().await;
                    if let Some(previous) = compaction_map.get(&compaction.id) {
                        compaction.manual |= previous.manual;
                        if compaction.result.summary_content.is_none() {
                            compaction.result.summary_content =
                                previous.result.summary_content.clone();
                        }
                    }
                    compaction_map.insert(compaction.id.clone(), compaction.clone());
                    drop(compaction_map);
                    self.emit(AdapterActivity::Thread {
                        thread_id: self.event_scope().await?.thread_id().clone(),
                        event: ThreadEvent::CompactionUpdated(Box::new(compaction)),
                    })
                    .await?;
                } else {
                    self.emit(AdapterActivity::Warning(ProviderWarning {
                        scope: WarningScope::Thread { thread_id },
                        kind: ProviderWarningKind::IncompleteCompactionMetadata,
                        message: SanitizedMessage::new(
                            "Provider omitted usable compaction metadata",
                        )
                        .expect("static compaction warning is sanitized"),
                    }))
                    .await?;
                }
            }
            "compaction_summary_chunk" => {
                if !self.is_root(session_id).await {
                    return Ok(());
                }
                if let (Some(id), Some(text)) = (
                    update["compactionId"].as_str(),
                    update.pointer("/content/text").and_then(Value::as_str),
                ) {
                    let mut compaction_map = self.compaction.lock().await;
                    if let Some(compaction) = compaction_map.get_mut(id) {
                        compaction
                            .result
                            .summary_content
                            .get_or_insert_with(String::new)
                            .push_str(text);
                        let compaction = compaction.clone();
                        drop(compaction_map);
                        self.emit(AdapterActivity::Thread {
                            thread_id: self.event_scope().await?.thread_id().clone(),
                            event: ThreadEvent::CompactionUpdated(Box::new(compaction)),
                        })
                        .await?;
                    }
                }
            }
            "usage_update" => {
                let root = self.is_root(session_id).await;
                let thread_id = if root {
                    self.event_scope().await?.thread_id().clone()
                } else {
                    self.children
                        .lock()
                        .await
                        .address(session_id, true)
                        .context("ACP context usage has no known child thread")?
                        .thread_id
                };
                if let Some(context) = update["used"]
                    .as_u64()
                    .zip(update["size"].as_u64())
                    .and_then(|(used, size)| {
                        forge_provider_api::context::ContextUsage::reported(used, size)
                    })
                {
                    if root {
                        self.output.lock().await.metrics.context_usage = Some(context.clone());
                    }
                    self.emit(AdapterActivity::Thread {
                        thread_id,
                        event: ThreadEvent::ContextUsage(context),
                    })
                    .await?;
                }
            }
            // Mode and session-title metadata have no normalized event. Outside history replay,
            // Harness already retains the accepted user input, so an echoed user chunk must not
            // create a second native user item.
            "current_mode_update" | "session_info_update" | "user_message_chunk" => {}
            // Advertising `session.notices` makes agents send status such as MCP startup
            // failures as notices instead of bold text inside the assistant's answer.
            "notice" => self.notice(session_id, update).await?,
            other => self.unsupported_update(session_id, other).await?,
        }
        Ok(())
    }

    /// Resolve the thread that owns a warning from one native session, defaulting to the root.
    async fn warning_thread(&self, session_id: &str) -> Result<ThreadId> {
        let root_thread = self.event_scope().await?.thread_id().clone();
        Ok(if self.is_root(session_id).await {
            root_thread
        } else {
            self.children
                .lock()
                .await
                .address(session_id, true)
                .map_or(root_thread, |address| address.thread_id)
        })
    }

    /// Surface an agent notice as a nonfatal warning at its originating thread.
    async fn notice(&self, session_id: &str, update: &Value) -> Result<()> {
        let title = update["title"].as_str().unwrap_or_default().trim();
        let description = update["description"].as_str().unwrap_or_default().trim();
        let text = match (title.is_empty(), description.is_empty()) {
            (false, false) => format!("{title}: {description}"),
            (false, true) => title.to_owned(),
            (true, false) => description.to_owned(),
            (true, true) => return Ok(()),
        };
        // Claude re-sends an unchanged MCP startup failure with every turn.
        if !self.reported_notices.lock().await.insert(text.clone()) {
            return Ok(());
        }
        let thread_id = self.warning_thread(session_id).await?;
        self.emit(AdapterActivity::Warning(ProviderWarning {
            scope: WarningScope::Thread { thread_id },
            kind: ProviderWarningKind::NativeNotice {
                category: forge_provider_api::ProviderFailureKind::Unknown,
            },
            message: SanitizedMessage::new(text).context("ACP notice message is empty")?,
        }))
        .await?;
        Ok(())
    }

    /// Warn once per binding about a v1 session update kind this adapter does not apply.
    async fn unsupported_update(&self, session_id: &str, update_kind: &str) -> Result<()> {
        let update_kind = if update_kind.trim().is_empty() {
            "(missing sessionUpdate)"
        } else {
            update_kind
        };
        if !self
            .unsupported_updates
            .lock()
            .await
            .insert(update_kind.to_owned())
        {
            return Ok(());
        }
        let thread_id = self.warning_thread(session_id).await?;
        self.emit(AdapterActivity::Warning(ProviderWarning {
            scope: WarningScope::Thread { thread_id },
            kind: ProviderWarningKind::UnsupportedNativeEvent,
            message: SanitizedMessage::new(format!(
                "ACP agent reported session update `{update_kind}`, which Forge does not apply"
            ))
            .context("unsupported ACP update message is empty")?,
        }))
        .await?;
        Ok(())
    }

    async fn handle_request(&self, method: &str, parameter: Value) -> Result<Value> {
        if self.history_replay.load(Ordering::Acquire) {
            return match method {
                "session/request_permission" => Ok(json!({"outcome":{"outcome":"cancelled"}})),
                _ => bail!("unsupported ACP client request during history reload"),
            };
        }
        match method {
            "session/request_permission" => self.authorize(parameter).await,
            _ => bail!("unsupported ACP client request: {method}"),
        }
    }
}

fn provider_failure(kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(forge_provider_api::ProviderFailure::new(
        forge_provider_api::ProviderOperation::TurnStart,
        kind,
    ))
}

fn tool_output(update: &Value) -> Result<Option<String>> {
    let text = update["content"]
        .as_array()
        .into_iter()
        .flatten()
        .filter_map(|block| block.pointer("/content/text").and_then(Value::as_str))
        .collect::<Vec<_>>();
    if !text.is_empty() {
        return Ok(Some(text.join("\n")));
    }
    let Some(raw) = update.get("rawOutput").filter(|raw| !raw.is_null()) else {
        return Ok(None);
    };
    if let Some(text) = raw.as_str() {
        return Ok(Some(text.into()));
    }
    if let Some(text) = raw["combinedOutput"].as_str() {
        let exit = raw["exitCode"]
            .as_i64()
            .map(|code| format!("\nExit code: {code}"))
            .unwrap_or_default();
        return Ok(Some(format!("{text}{exit}")));
    }
    Ok(Some(serde_json::to_string_pretty(raw)?))
}

fn file_changes(update: &Value) -> ProviderChangeSet {
    let deadline = std::time::Instant::now() + std::time::Duration::from_millis(250);
    let file = update["content"]
        .as_array()
        .into_iter()
        .flatten()
        .filter_map(|content| {
            if content["type"] != "diff" {
                return None;
            }
            let path = content["path"].as_str().filter(|path| !path.is_empty())?;
            let after = content["newText"].as_str()?;
            let before = match content.get("oldText")? {
                Value::Null => None,
                Value::String(text) => Some(text.as_str()),
                _ => return None,
            };
            let kind = if before.is_none() {
                ProviderChangeKind::Add
            } else if update["kind"] == "delete" {
                ProviderChangeKind::Delete
            } else {
                ProviderChangeKind::Update
            };
            let diff = similar::TextDiff::configure()
                .deadline(deadline)
                .diff_lines(before.unwrap_or_default(), after)
                .unified_diff()
                .context_radius(3)
                .header(
                    if before.is_none() { "/dev/null" } else { path },
                    if kind == ProviderChangeKind::Delete {
                        "/dev/null"
                    } else {
                        path
                    },
                )
                .to_string();
            Some(ProviderFileChange {
                path: path.into(),
                move_path: None,
                kind,
                diff,
            })
        })
        .collect();
    ProviderChangeSet { file }
}

/// Normalize optional compaction evidence without exposing native error text.
fn normalize_compaction(update: &Value, thread_id: &str) -> Option<Compaction> {
    let id = update["compactionId"]
        .as_str()
        .filter(|id| !id.trim().is_empty())?;
    let state = match update["status"].as_str()? {
        "in_progress" | "started" | "running" => CompactionState::Running,
        "completed" => CompactionState::Completed,
        "failed" => CompactionState::Failed,
        "cancelled" => CompactionState::Unconfirmed,
        _ => return None,
    };
    let summary_content = update["summary"].as_array().map(|contents| {
        contents
            .iter()
            .filter_map(|content| content["text"].as_str())
            .collect::<String>()
    });
    let error = (state == CompactionState::Failed).then(|| {
        ProviderFailure::new(
            ProviderOperation::ThreadCompact,
            ProviderFailureKind::Unknown,
        )
        .at_phase(ProviderPhase::Observation, FailureDisposition::Uncertain)
    });
    let now = super::children::timestamp();
    Some(Compaction {
        id: id.into(),
        thread_id: thread_id.into(),
        state,
        manual: update.pointer("/_meta/trigger").and_then(Value::as_str) == Some("manual")
            || update["trigger"].as_str() == Some("manual"),
        started_at_ms: (state == CompactionState::Running).then_some(now),
        completed_at_ms: (state != CompactionState::Running).then_some(now),
        result: compaction::CompactionResult {
            context_tokens_before: None,
            context_tokens_after: None,
            model: None,
            before_context: None,
            custom_instructions: None,
            summary_content,
            error,
            pre_compaction_tokens: None,
            post_compaction_tokens: None,
            tokens_removed: None,
            pre_compaction_messages_length: None,
            messages_removed: None,
            compaction_tokens_used: None,
            context_window: None,
            conversation_tokens: None,
            system_tokens: None,
            tool_definitions_tokens: None,
        },
    })
}

#[async_trait]
impl AcpHandler for AcpSessionState {
    async fn notification(&self, method: &str, parameter: Value) -> Result<(), AcpConnectionError> {
        match self.handle_notification(method, parameter).await {
            Ok(()) => Ok(()),
            Err(error) => {
                let observation = match error.downcast_ref::<ProviderError>() {
                    Some(ProviderError::Observation(failure)) => failure.clone(),
                    _ => forge_provider_api::lifecycle::ObservationFailure::Protocol,
                };
                let cause = provider_failure(ProviderFailureKind::Protocol);
                self.invalidate_binding(cause.clone()).await;
                if let Some(sink) = self.publication_sink().await {
                    sink.fail(events::EventDeliveryError::Observation(observation));
                }
                Err(AcpConnectionError::Callback(Box::new(cause)))
            }
        }
    }
    async fn request(&self, method: &str, parameter: Value) -> Result<Value, AcpConnectionError> {
        match self.handle_request(method, parameter).await {
            Ok(response) => Ok(response),
            Err(_) => {
                let cause = provider_failure(ProviderFailureKind::Protocol);
                self.invalidate_binding(cause.clone()).await;
                Err(AcpConnectionError::Callback(Box::new(cause)))
            }
        }
    }
}

impl AcpSessionState {
    /// Get the immutable registry identity for this native protocol owner.
    pub(super) fn kind_id(&self) -> ProviderId {
        self.kind.id()
    }

    /// Install the binding publisher independently of any foreground invocation.
    pub(super) async fn install_capabilities(
        &self,
        session_id: &str,
        sink: ProviderEventSink,
        manifest: forge_provider_api::capability::CapabilityManifest,
    ) {
        *self.capabilities.write().await = Some((session_id.to_owned(), sink, manifest));
    }

    /// Get support already published on this binding's ordered stream.
    pub(super) async fn effective_capabilities(
        &self,
    ) -> Option<forge_provider_api::capability::CapabilityManifest> {
        self.capabilities
            .read()
            .await
            .as_ref()
            .map(|(_, _, manifest)| manifest.clone())
    }

    /// Publish support changes at the same boundary that replaces the native catalog.
    async fn publish_capabilities(&self) {
        if !self.is_admitting() {
            return;
        }
        let catalog = self.catalog.read().await;
        let effort = catalog["configOptions"].as_array().is_some_and(|options| {
            options.iter().any(|option| {
                matches!(
                    option["category"].as_str(),
                    Some("thought_level" | "effort")
                ) || matches!(option["id"].as_str(), Some("thought_level" | "effort"))
            })
        });
        drop(catalog);
        let publication = {
            let mut capabilities = self.capabilities.write().await;
            if !self.is_admitting() {
                return;
            }
            let Some((_, sink, manifest)) = capabilities.as_mut() else {
                return;
            };
            let Some(capability) = manifest.capability.iter_mut().find(|capability| {
                capability.id == forge_provider_api::capability::CapabilityId::EffortSelection
            }) else {
                return;
            };
            let support = if effort {
                forge_provider_api::capability::Support::Supported
            } else {
                forge_provider_api::capability::Support::Unsupported
            };
            if capability.support == support {
                return;
            }
            capability.support = support;
            manifest.effective_revision = manifest.effective_revision.saturating_add(1);
            (sink.clone(), manifest.clone())
        };
        let (sink, capabilities) = publication;
        if !self.is_admitting() {
            return;
        }
        let event = capabilities;
        let delivery = tokio::select! {
            biased;
            _ = self.admission_revocation.cancelled() => return,
            delivery = sink.capabilities_changed(event) => delivery,
        };
        if let Err(error) = delivery {
            sink.fail(error.clone());
            self.invalidate_binding(error.into()).await;
        }
    }

    pub async fn catalog(&self) -> Value {
        self.catalog.read().await.clone()
    }
    pub async fn update_catalog(&self, fields: Value) {
        if let Some(fields) = fields.as_object() {
            let mut catalog = self.catalog.write().await;
            for (field, value) in fields {
                catalog[field] = value.clone();
            }
        }
        self.publish_capabilities().await;
    }
    pub async fn take_completion(&self) -> forge_provider_api::completion::ProviderCompletion {
        std::mem::take(&mut *self.output.lock().await)
    }
}

/// Merge a native descriptor only after checking every immutable category constraint.
fn merge_tool_snapshot(snapshot: &mut Value, update: &Value) -> Result<bool, ProviderError> {
    let present = |value: &Value| !value.is_null();
    if let Some(previous) = snapshot.get("kind").filter(|kind| present(kind))
        && let Some(next) = update.get("kind").filter(|kind| present(kind))
        && normalized_tool_kind(previous) != normalized_tool_kind(next)
    {
        return Err(ProviderError::Observation(
            forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
        ));
    }
    let invocation_changed = ["title", "kind", "rawInput"].into_iter().any(|field| {
        update
            .get(field)
            .is_some_and(|value| present(value) && snapshot.get(field) != Some(value))
    });
    if let Some(fields) = update.as_object() {
        for (field, value) in fields {
            if present(value) {
                snapshot[field] = value.clone();
            }
        }
    }
    Ok(invocation_changed)
}

/// Normalize one complete native snapshot before publishing its immutable item type and descriptor.
fn normalize_tool_descriptor(snapshot: &Value) -> (ToolKind, ToolDescriptor) {
    normalize_descriptor(
        normalized_tool_kind(&snapshot["kind"]),
        ToolDescriptor {
            semantics: Default::default(),
            name: snapshot["title"].as_str().unwrap_or("Tool").into(),
            title: snapshot["title"].as_str().unwrap_or("Tool").into(),
            arguments: InspectableArguments::new(
                snapshot.get("rawInput").cloned().unwrap_or(Value::Null),
            ),
        },
    )
}

/// Preserve the normalized item category across native descriptor revisions.
fn normalized_tool_kind(kind: &Value) -> ToolKind {
    match kind.as_str() {
        Some("edit" | "delete" | "move") => ToolKind::FileChange,
        Some("execute") => ToolKind::Command,
        Some(_) | None => ToolKind::ToolCall,
    }
}

/// Preserve prose classification independently from reasoning streams.
fn message_classification(role: Option<AcpMessageRole>) -> MessageClassification {
    match role {
        Some(AcpMessageRole::Commentary) => {
            MessageClassification::Known(AgentMessageRole::Commentary)
        }
        Some(AcpMessageRole::Response) => MessageClassification::Known(AgentMessageRole::Response),
        Some(AcpMessageRole::Reasoning) | None => MessageClassification::Unclassified,
    }
}

/// Attribute one typed item event to its retained original turn.
fn item_activity(turn: &TurnAddress, item_id: &ItemId, event: ItemEvent) -> AdapterActivity {
    AdapterActivity::Item {
        thread_id: turn.thread_id.clone(),
        turn_id: turn.turn_id.clone(),
        item_id: item_id.clone(),
        event,
    }
}
