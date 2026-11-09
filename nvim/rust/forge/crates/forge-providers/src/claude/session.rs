//! Binding state, frame normalization, and callback authority for one Claude Code conversation.
//!
//! The decoder turns stream-json frames into `AdapterActivity`. It never publishes turn start or
//! completion; the shared `RetainedTurn` runner owns those. A Forge turn begins with the user
//! input the adapter writes and ends at the CLI `result` that leaves none of the turn's inputs
//! queued: steering that the CLI queues behind a finishing CLI turn starts the next CLI turn, and
//! the decoder absorbs that turn into the same Forge turn so steering is never lost or orphaned.

use super::tool_targets::{self, HOST_TOOL_SERVER, ToolKind};
use super::transport::{FrameHandler, TransportFailure};
use super::wire::content::{ContentBlock, ToolResultBlock, ToolUseBlock, UserContent};
use super::wire::control::{CanUseTool, InboundControl, InboundControlRequest, PermissionDecision};
use super::wire::stdout::{
    AssistantFrame, CommandState, ResultFrame, StdoutFrame, StreamDelta, StreamEvent, SystemFrame,
    UserFrame,
};
use async_trait::async_trait;
use forge_provider_api::permissions::{PermissionRequest, PermissionResponse};
use forge_provider_api::*;
use serde_json::{Value, json};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use tokio::sync::{Mutex, RwLock, oneshot};
use tokio_util::sync::CancellationToken;

/// The native end of one Forge turn.
#[derive(Clone, Debug)]
pub(crate) struct TurnEnd {
    pub outcome: turn::TurnOutcome,
    pub completed_at_ms: i64,
    pub duration_ms: Option<u64>,
    /// The last transcript chain entry of the turn, which is its rewind checkpoint.
    pub checkpoint: Option<String>,
    /// Inputs the turn consumed, in delivery order.
    pub commands: Vec<String>,
}

/// The native end of one `/compact` operation.
#[derive(Clone, Debug)]
pub(crate) struct CompactionEnd {
    pub compaction: Option<Compaction>,
}

/// Capture immutable callback routing for the admitted provider turn.
#[derive(Clone)]
struct ActiveTurn {
    generation: Arc<()>,
    revocation: CancellationToken,
    sink: ProviderEventSink,
    address: TurnAddress,
    permissions: Option<Arc<dyn permissions::ProviderPermissions>>,
    provider_tools: Option<Arc<dyn control::ProviderTools>>,
    execution_started: bool,
    native_start: Option<NativeTurnStart>,
    /// Inputs written for this turn: the prompt first, then steering in write order.
    commands: Vec<String>,
    interrupting: bool,
}

/// A compaction requested through `/compact`.
struct ActiveOperation {
    sink: ProviderEventSink,
    compaction: Compaction,
    ended: Option<oneshot::Sender<CompactionEnd>>,
}

/// One streamed message or reasoning item.
struct MessageItem {
    item_id: ItemId,
    api_message_id: String,
    block_index: u64,
    reasoning: bool,
    text: String,
    role: Option<AgentMessageRole>,
}

/// One tool invocation item.
struct ToolItem {
    item_id: ItemId,
    kind: ToolKind,
    descriptor: ToolDescriptor,
    started: bool,
    finished: bool,
    denied: Option<String>,
}

/// Per-turn normalization state.
#[derive(Default)]
struct TurnDecoder {
    current_api_message: Option<String>,
    /// Block kinds by API message and stream index, from `content_block_start`.
    block_kind: HashMap<(String, u64), String>,
    /// Complete `assistant` frames received per API message, which locate each frame's block.
    assistant_blocks: HashMap<String, u64>,
    messages: Vec<MessageItem>,
    tools: HashMap<String, ToolItem>,
    tool_order: Vec<String>,
    checkpoint: Option<String>,
    model_resolved: bool,
    last_result: Option<ResultFrame>,
}

/// Session catalog learned from `initialize` and `system/init`.
#[derive(Clone, Debug, Default)]
pub(crate) struct SessionCatalog {
    pub models: Vec<super::wire::control::ModelInfo>,
    pub selected_model: Option<String>,
    pub mcp_status: HashMap<String, String>,
    pub version: Option<String>,
}

/// Own native observations and callback authority for one binding.
pub(crate) struct ClaudeSessionState {
    admission: AtomicBool,
    admission_revocation: CancellationToken,
    collected: AtomicBool,
    provider_admission: Arc<AtomicBool>,
    first_failure: Mutex<Option<ProviderError>>,
    active: RwLock<Option<ActiveTurn>>,
    retained: RwLock<Option<ActiveTurn>>,
    callback_work: Arc<RwLock<()>>,
    operation: Mutex<Option<ActiveOperation>>,
    capabilities: RwLock<Option<(ProviderEventSink, capability::CapabilityManifest)>>,
    host_tools: std::sync::Mutex<Vec<control::ControlToolDefinition>>,
    decoder: Mutex<TurnDecoder>,
    commands: std::sync::Mutex<HashMap<String, tokio::sync::watch::Sender<CommandState>>>,
    turn_end: std::sync::Mutex<Option<oneshot::Sender<Result<TurnEnd, ProviderError>>>>,
    output: Mutex<completion::ProviderCompletion>,
    catalog: RwLock<SessionCatalog>,
    reported_notices: Mutex<HashSet<String>>,
    unsupported_events: Mutex<HashSet<String>>,
}

impl ClaudeSessionState {
    /// Create native callback state sharing the provider's immediate shutdown admission gate.
    pub(crate) fn new(provider_admission: Arc<AtomicBool>) -> Self {
        Self {
            admission: AtomicBool::new(true),
            admission_revocation: CancellationToken::new(),
            collected: AtomicBool::new(false),
            provider_admission,
            first_failure: Mutex::new(None),
            active: RwLock::new(None),
            retained: RwLock::new(None),
            callback_work: Arc::new(RwLock::new(())),
            operation: Mutex::new(None),
            capabilities: RwLock::new(None),
            host_tools: std::sync::Mutex::new(Vec::new()),
            decoder: Mutex::new(TurnDecoder::default()),
            commands: std::sync::Mutex::new(HashMap::new()),
            turn_end: std::sync::Mutex::new(None),
            output: Mutex::new(completion::ProviderCompletion::default()),
            catalog: RwLock::new(SessionCatalog::default()),
            reported_notices: Mutex::new(HashSet::new()),
            unsupported_events: Mutex::new(HashSet::new()),
        }
    }

    // ----- Admission -----------------------------------------------------------------------

    /// Get whether this native binding still permits new operations.
    pub(crate) fn is_admitting(&self) -> bool {
        self.admission.load(Ordering::Acquire) && self.provider_admission.load(Ordering::Acquire)
    }

    /// Close native admission before any asynchronous cleanup boundary.
    pub(crate) fn close_admission(&self) {
        self.admission.store(false, Ordering::Release);
        self.admission_revocation.cancel();
    }

    /// Get the original failure after admission was revoked by native observation.
    pub(crate) async fn binding_failure(&self) -> Option<ProviderError> {
        self.first_failure.lock().await.clone()
    }

    /// Retain the original typed binding failure across cleanup retries.
    pub(crate) async fn retain_failure(&self, cause: ProviderError) -> ProviderError {
        self.first_failure.lock().await.get_or_insert(cause).clone()
    }

    /// Record positive full-resource collection separately from admission revocation.
    pub(crate) fn mark_collected(&self) {
        self.collected.store(true, Ordering::Release);
    }

    /// Reject revoked work as invalidated until native owners prove full collection.
    pub(crate) fn ensure_admission(
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
    pub(crate) async fn invalidate_binding(&self, cause: ProviderError) {
        self.close_admission();
        self.retain_failure(cause.clone()).await;
        self.revoke_authority().await;
        self.end_turn(Err(cause.clone()));
        if let Some(operation) = self.operation.lock().await.as_mut()
            && let Some(ended) = operation.ended.take()
        {
            let _ = ended.send(CompactionEnd { compaction: None });
        }
    }

    /// Revoke authority locally without waiting for callback collection.
    pub(crate) async fn revoke_authority(&self) {
        if let Some(active) = self.active.write().await.as_mut() {
            active.revocation.cancel();
            active.permissions = None;
            active.provider_tools = None;
        }
        if let Some(retained) = self.retained.write().await.as_mut() {
            retained.revocation.cancel();
            retained.permissions = None;
            retained.provider_tools = None;
        }
    }

    /// Revoke invocation callback authority, then wait for running callbacks to finish.
    pub(crate) async fn revoke_callbacks(&self) {
        self.revoke_authority().await;
        let _collected = self.callback_work.write().await;
    }

    /// Revoke only the named invocation when cancellation races with a successor.
    pub(crate) async fn revoke_invocation_callbacks(&self, scope: &InvocationScope) {
        let mut revoked = false;
        for slot in [&self.active, &self.retained] {
            if let Some(turn) = slot
                .write()
                .await
                .as_mut()
                .filter(|turn| &turn.address.scope == scope)
            {
                turn.revocation.cancel();
                turn.permissions = None;
                turn.provider_tools = None;
                revoked = true;
            }
        }
        if revoked {
            let _collected = self.callback_work.write().await;
        }
    }

    /// Report whether a foreground turn or compaction is active.
    pub(crate) async fn has_active_work(&self) -> bool {
        self.active.read().await.is_some() || self.operation.lock().await.is_some()
    }

    // ----- Turn lifecycle ------------------------------------------------------------------

    /// Install the invocation's callback authority before its prompt is written.
    pub(crate) async fn begin_turn(
        &self,
        sink: ProviderEventSink,
        address: TurnAddress,
        permissions: Arc<dyn permissions::ProviderPermissions>,
        provider_tools: Arc<dyn control::ProviderTools>,
        start: NativeTurnStart,
    ) -> Result<oneshot::Receiver<Result<TurnEnd, ProviderError>>, ProviderError> {
        if self.operation.lock().await.is_some() {
            return Err(provider_failure(ProviderFailureKind::ExecutionActive));
        }
        let mut active = self.active.write().await;
        self.ensure_admission(ProviderOperation::TurnStart)?;
        if active.is_some() {
            return Err(provider_failure(ProviderFailureKind::ExecutionActive));
        }
        *self.retained.write().await = None;
        *self.output.lock().await = completion::ProviderCompletion::default();
        *self.decoder.lock().await = TurnDecoder::default();
        let (ended, receiver) = oneshot::channel();
        *self
            .turn_end
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(ended);
        *active = Some(ActiveTurn {
            generation: Arc::new(()),
            revocation: CancellationToken::new(),
            sink,
            address,
            permissions: Some(permissions),
            provider_tools: Some(provider_tools),
            execution_started: false,
            native_start: Some(start),
            commands: Vec::new(),
            interrupting: false,
        });
        Ok(receiver)
    }

    /// Register an input before it is written and return its delivery observer.
    ///
    /// An input that joins the active turn keeps the turn open until the CLI starts it. Joining
    /// fails once the turn's end was decided, so steering never reaches a turn nobody owns.
    pub(crate) async fn track_command(
        &self,
        uuid: &str,
        joins_active_turn: bool,
    ) -> Result<tokio::sync::watch::Receiver<CommandState>, ProviderError> {
        let mut active = self.active.write().await;
        if joins_active_turn {
            let ending = self
                .turn_end
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .is_none();
            let active = active
                .as_mut()
                .filter(|_| !ending)
                .ok_or_else(|| provider_failure(ProviderFailureKind::WrongInvocation))?;
            active.commands.push(uuid.to_owned());
        }
        let (sender, receiver) = tokio::sync::watch::channel(CommandState::Other);
        self.commands
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(uuid.to_owned(), sender);
        Ok(receiver)
    }

    /// Forget a command the CLI never received.
    pub(crate) async fn forget_command(&self, uuid: &str) {
        self.commands
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .remove(uuid);
        if let Some(active) = self.active.write().await.as_mut() {
            active.commands.retain(|command| command != uuid);
        }
    }

    /// Mark the active turn as interrupted so its next result ends it even with queued input.
    pub(crate) async fn mark_interrupting(&self, scope: &InvocationScope) -> bool {
        let mut active = self.active.write().await;
        match active
            .as_mut()
            .filter(|active| &active.address.scope == scope)
        {
            Some(active) => {
                active.interrupting = true;
                true
            }
            None => false,
        }
    }

    /// Report whether native observations established the active turn.
    pub(crate) async fn execution_started(&self) -> bool {
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

    /// Record positive processing evidence independently of input acceptance.
    pub(crate) async fn observe_native_start(
        &self,
        observed_at_ms: i64,
    ) -> Result<(), ProviderError> {
        let (start, address, first) = {
            let mut active = self.active.write().await;
            let Some(active) = active.as_mut() else {
                return Ok(());
            };
            let first = !active.execution_started;
            active.execution_started = true;
            (active.native_start.clone(), active.address.clone(), first)
        };
        if let Some(start) = start {
            start.observed(observed_at_ms).await?;
        }
        if first {
            let model = self.catalog.read().await.selected_model.clone();
            if let Some(model_id) = model.and_then(|model| ModelId::new(model).ok()) {
                self.decoder.lock().await.model_resolved = true;
                self.publish(AdapterActivity::Turn {
                    thread_id: address.thread_id,
                    turn_id: address.turn_id,
                    event: TurnMetadataEvent::ModelResolved(ResolvedModel {
                        model_id,
                        observed_at_ms,
                    }),
                })
                .await?;
            }
        }
        Ok(())
    }

    /// Close unfinished items, then retain the turn's routing for late collection.
    pub(crate) async fn finish(&self, outcome: turn::TurnOutcome) -> Result<(), ProviderError> {
        let Some(active) = self.active.read().await.clone() else {
            return Ok(());
        };
        let completed = matches!(outcome, turn::TurnOutcome::Completed { .. });
        let mut events = Vec::new();
        {
            let mut decoder = self.decoder.lock().await;
            if completed {
                events.extend(classify_pending(
                    &mut decoder,
                    &active.address,
                    Some(AgentMessageRole::Response),
                ));
            }
            for message in std::mem::take(&mut decoder.messages) {
                let completion = MessageCompletion {
                    text: message.text,
                    outcome: if completed {
                        MessageOutcome::Completed
                    } else {
                        MessageOutcome::Interrupted
                    },
                };
                events.push(item(
                    &active.address,
                    &message.item_id,
                    ItemEvent::Completed(if message.reasoning {
                        ItemCompletion::Reasoning(completion)
                    } else {
                        ItemCompletion::AgentMessage(completion)
                    }),
                ));
            }
            for tool_use_id in std::mem::take(&mut decoder.tool_order) {
                let Some(tool) = decoder.tools.get_mut(&tool_use_id) else {
                    continue;
                };
                if tool.finished {
                    continue;
                }
                tool.finished = true;
                events.push(item(
                    &active.address,
                    &tool.item_id,
                    ItemEvent::Completed(tool_completion(
                        tool.kind,
                        ToolCompletion {
                            outcome: ToolOutcome::Interrupted,
                            descriptor: Some(tool.descriptor.clone()),
                            output: None,
                            changes: Default::default(),
                        },
                    )),
                ));
            }
        }
        let mut result = Ok(());
        for event in events {
            if let Err(error) = self.publish(event).await {
                result = Err(error);
                break;
            }
        }
        *self.retained.write().await = self.active.write().await.take();
        result
    }

    /// Take the completion evidence collected for the finished turn.
    pub(crate) async fn take_completion(&self) -> completion::ProviderCompletion {
        std::mem::take(&mut *self.output.lock().await)
    }

    /// Record context usage for the finished turn and publish it on the thread.
    pub(crate) async fn publish_context_usage(
        &self,
        usage: context::ContextUsage,
    ) -> Result<(), ProviderError> {
        self.output.lock().await.metrics.context_usage = Some(usage.clone());
        let thread_id = match self.active.read().await.as_ref() {
            Some(active) => active.address.thread_id.clone(),
            None => return Ok(()),
        };
        self.publish(AdapterActivity::Thread {
            thread_id,
            event: ThreadEvent::ContextUsage(usage),
        })
        .await
    }

    /// Deliver the turn's native end to its waiting runner.
    fn end_turn(&self, end: Result<TurnEnd, ProviderError>) {
        if let Some(ended) = self
            .turn_end
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take()
        {
            let _ = ended.send(end);
        }
    }

    // ----- Compaction ----------------------------------------------------------------------

    /// Begin a `/compact` operation published under its own operation scope.
    pub(crate) async fn begin_compaction(
        &self,
        sink: ProviderEventSink,
        thread_id: &ThreadId,
        command: &str,
    ) -> Result<oneshot::Receiver<CompactionEnd>, ProviderError> {
        if self.active.read().await.is_some() {
            return Err(provider_failure(ProviderFailureKind::ExecutionActive));
        }
        let mut operation = self.operation.lock().await;
        if operation.is_some() {
            return Err(provider_failure(ProviderFailureKind::ExecutionActive));
        }
        let (ended, receiver) = oneshot::channel();
        *operation = Some(ActiveOperation {
            sink,
            compaction: Compaction {
                id: command.to_owned(),
                thread_id: thread_id.as_str().into(),
                state: CompactionState::Running,
                manual: true,
                started_at_ms: Some(timestamp()),
                completed_at_ms: None,
                result: compaction::CompactionResult {
                    context_tokens_before: None,
                    context_tokens_after: None,
                    model: None,
                    before_context: None,
                    custom_instructions: None,
                    summary_content: None,
                    error: None,
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
            },
            ended: Some(ended),
        });
        Ok(receiver)
    }

    /// Release the compaction operation after its result was collected.
    pub(crate) async fn finish_compaction(&self) {
        *self.operation.lock().await = None;
    }

    // ----- Catalog and capabilities -------------------------------------------------------

    /// Install the binding publisher independently of any foreground invocation.
    pub(crate) async fn install_capabilities(
        &self,
        sink: ProviderEventSink,
        manifest: capability::CapabilityManifest,
    ) {
        *self.capabilities.write().await = Some((sink, manifest));
    }

    /// Get support already published on this binding's ordered stream.
    pub(crate) async fn effective_capabilities(&self) -> Option<capability::CapabilityManifest> {
        self.capabilities
            .read()
            .await
            .as_ref()
            .map(|(_, manifest)| manifest.clone())
    }

    pub(crate) async fn catalog(&self) -> SessionCatalog {
        self.catalog.read().await.clone()
    }

    pub(crate) async fn update_catalog(&self, update: impl FnOnce(&mut SessionCatalog)) {
        update(&mut *self.catalog.write().await);
    }

    /// Replace the host tools served over the SDK MCP server and report whether they changed.
    pub(crate) fn replace_host_tools(&self, tools: Vec<control::ControlToolDefinition>) -> bool {
        let mut current = self
            .host_tools
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if *current == tools {
            return false;
        }
        *current = tools;
        true
    }

    // ----- Publication ---------------------------------------------------------------------

    /// Get the active operation's publisher before falling back to a settled invocation.
    async fn publication_sink(&self) -> Option<ProviderEventSink> {
        if let Some(active) = self.active.read().await.as_ref() {
            return Some(active.sink.clone());
        }
        if let Some(operation) = self.operation.lock().await.as_ref() {
            return Some(operation.sink.clone());
        }
        if let Some(retained) = self.retained.read().await.as_ref() {
            return Some(retained.sink.clone());
        }
        self.capabilities
            .read()
            .await
            .as_ref()
            .map(|(sink, _)| sink.clone())
    }

    /// Publish root-turn activity after recording native start evidence.
    async fn emit(&self, event: AdapterActivity) -> Result<(), ProviderError> {
        if matches!(
            event,
            AdapterActivity::Item { .. }
                | AdapterActivity::Turn {
                    event: TurnMetadataEvent::Usage(_) | TurnMetadataEvent::ChecklistUpdated(_),
                    ..
                }
        ) {
            self.observe_native_start(timestamp()).await?;
        }
        self.publish(event).await
    }

    /// Publish activity through the current owner without recording start evidence.
    async fn publish(&self, event: AdapterActivity) -> Result<(), ProviderError> {
        let sink = self
            .publication_sink()
            .await
            .ok_or_else(|| provider_failure(ProviderFailureKind::Rejected))?;
        sink.activity_writer().send_wait(event).await?;
        Ok(())
    }

    /// Get the active turn's address.
    async fn active_address(&self) -> Option<TurnAddress> {
        self.active
            .read()
            .await
            .as_ref()
            .map(|active| active.address.clone())
    }

    /// Get the thread that owns binding-scoped warnings.
    async fn warning_thread(&self) -> Option<ThreadId> {
        let sink = self.publication_sink().await?;
        Some(sink.scope().thread_id().clone())
    }

    /// Surface a native notice once per binding.
    async fn notice(&self, text: &str) -> Result<(), ProviderError> {
        let text = text.trim();
        if text.is_empty() || !self.reported_notices.lock().await.insert(text.to_owned()) {
            return Ok(());
        }
        let Some(thread_id) = self.warning_thread().await else {
            return Ok(());
        };
        let Ok(message) = SanitizedMessage::new(text) else {
            return Ok(());
        };
        self.publish(AdapterActivity::Warning(ProviderWarning {
            scope: WarningScope::Thread { thread_id },
            kind: ProviderWarningKind::NativeNotice {
                category: ProviderFailureKind::Unknown,
            },
            message,
        }))
        .await
    }

    /// Warn once per binding about a frame kind this adapter does not apply.
    async fn unsupported(&self, kind: &str) -> Result<(), ProviderError> {
        if !self.unsupported_events.lock().await.insert(kind.to_owned()) {
            return Ok(());
        }
        let Some(thread_id) = self.warning_thread().await else {
            return Ok(());
        };
        let message = SanitizedMessage::new(format!(
            "Claude Code reported `{kind}`, which Forge does not apply"
        ))
        .map_err(|_| provider_failure(ProviderFailureKind::Protocol))?;
        self.publish(AdapterActivity::Warning(ProviderWarning {
            scope: WarningScope::Thread { thread_id },
            kind: ProviderWarningKind::UnsupportedNativeEvent,
            message,
        }))
        .await
    }

    // ----- Frame decoding ------------------------------------------------------------------

    async fn handle_frame(&self, frame: StdoutFrame) -> Result<(), ProviderError> {
        match frame {
            StdoutFrame::StreamEvent(frame) if frame.parent_tool_use_id.is_none() => {
                self.stream_event(frame.event).await
            }
            StdoutFrame::Assistant(frame) if frame.parent_tool_use_id.is_none() => {
                self.assistant(frame).await
            }
            StdoutFrame::User(frame) if frame.parent_tool_use_id.is_none() => {
                self.user(frame).await
            }
            // Native subagents are disallowed, so nested frames carry nothing Harness owns.
            StdoutFrame::StreamEvent(_) | StdoutFrame::Assistant(_) | StdoutFrame::User(_) => {
                Ok(())
            }
            StdoutFrame::Result(result) => self.result(result).await,
            StdoutFrame::System(system) => self.system(system).await,
            StdoutFrame::CommandLifecycle(lifecycle) => {
                let mut commands = self
                    .commands
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                if let Some(record) = commands.get(&lifecycle.command_uuid) {
                    record.send_replace(lifecycle.state);
                }
                if lifecycle.state == CommandState::Completed {
                    commands.remove(&lifecycle.command_uuid);
                }
                Ok(())
            }
            StdoutFrame::Recognized { .. } => Ok(()),
            StdoutFrame::Unknown { kind, .. } => self.unsupported(&kind).await,
            StdoutFrame::ControlRequest(_)
            | StdoutFrame::ControlResponse(_)
            | StdoutFrame::ControlCancelRequest { .. } => Ok(()),
        }
    }

    async fn stream_event(&self, event: StreamEvent) -> Result<(), ProviderError> {
        let Some(address) = self.active_address().await else {
            return Ok(());
        };
        let events = {
            let mut decoder = self.decoder.lock().await;
            match event {
                StreamEvent::MessageStart { message_id } => {
                    decoder.current_api_message = Some(message_id);
                    Vec::new()
                }
                StreamEvent::ContentBlockStart { index, block_kind } => {
                    if let Some(message) = decoder.current_api_message.clone() {
                        decoder.block_kind.insert((message, index), block_kind);
                    }
                    Vec::new()
                }
                StreamEvent::ContentBlockDelta { index, delta } => {
                    let Some(message) = decoder.current_api_message.clone() else {
                        return Ok(());
                    };
                    match delta {
                        StreamDelta::Text(text) => {
                            append_text(&mut decoder, &address, &message, index, false, &text)?
                        }
                        StreamDelta::Thinking(text) => {
                            append_text(&mut decoder, &address, &message, index, true, &text)?
                        }
                        StreamDelta::Other => Vec::new(),
                    }
                }
                StreamEvent::MessageDelta { stop_reason, .. } => {
                    // Text in an API message that stopped to call a tool precedes that call.
                    if stop_reason.as_deref() == Some("tool_use") {
                        classify_pending(&mut decoder, &address, Some(AgentMessageRole::Commentary))
                    } else {
                        Vec::new()
                    }
                }
                StreamEvent::ContentBlockStop { .. }
                | StreamEvent::MessageStop
                | StreamEvent::Other { .. } => Vec::new(),
            }
        };
        for event in events {
            self.emit(event).await?;
        }
        Ok(())
    }

    async fn assistant(&self, frame: AssistantFrame) -> Result<(), ProviderError> {
        let Some(address) = self.active_address().await else {
            return Ok(());
        };
        if let Some(error) = frame.error.as_deref() {
            self.decoder.lock().await.last_result = None;
            self.record_assistant_error(error).await;
        }
        let message_id = frame.message.id.clone();
        let mut events = Vec::new();
        let mut checklist = None;
        {
            let mut decoder = self.decoder.lock().await;
            if let Some(uuid) = &frame.uuid {
                decoder.checkpoint = Some(uuid.clone());
            }
            if decoder.current_api_message.is_none() {
                decoder.current_api_message = Some(message_id.clone());
            }
            for block in frame.message.content {
                let index = {
                    let counter = decoder
                        .assistant_blocks
                        .entry(message_id.clone())
                        .or_insert(0);
                    let index = *counter;
                    *counter += 1;
                    index
                };
                match block {
                    ContentBlock::Text(text) => events.extend(complete_text(
                        &mut decoder,
                        &address,
                        &message_id,
                        index,
                        false,
                        &text.text,
                    )?),
                    ContentBlock::Thinking(thinking) => events.extend(complete_text(
                        &mut decoder,
                        &address,
                        &message_id,
                        index,
                        true,
                        &thinking.thinking,
                    )?),
                    ContentBlock::ToolUse(tool) => {
                        // Every earlier prose block precedes this tool call.
                        events.extend(classify_pending(
                            &mut decoder,
                            &address,
                            Some(AgentMessageRole::Commentary),
                        ));
                        if tool.name == "TodoWrite" {
                            checklist = todo_checklist(&tool.input, &address);
                        }
                        events.extend(start_tool(&mut decoder, &address, tool)?);
                    }
                    ContentBlock::RedactedThinking
                    | ContentBlock::ToolResult(_)
                    | ContentBlock::Image
                    | ContentBlock::Other { .. } => {}
                }
            }
        }
        for event in events {
            self.emit(event).await?;
        }
        if let Some(checklist) = checklist {
            self.emit(AdapterActivity::Turn {
                thread_id: address.thread_id.clone(),
                turn_id: address.turn_id.clone(),
                event: TurnMetadataEvent::ChecklistUpdated(checklist),
            })
            .await?;
        }
        Ok(())
    }

    async fn record_assistant_error(&self, error: &str) {
        let _ = self
            .notice(&format!("Claude Code reported an API error: {error}"))
            .await;
    }

    async fn user(&self, frame: UserFrame) -> Result<(), ProviderError> {
        if let Some(operation) = self.operation.lock().await.as_mut()
            && let UserContent::Text(text) = &frame.message.content
            && operation.compaction.state == CompactionState::Running
            && !text.starts_with("<local-command")
        {
            // The CLI reports the compaction summary as the first message of the new context.
            operation.compaction.result.summary_content = Some(text.clone());
        }
        let Some(address) = self.active_address().await else {
            return Ok(());
        };
        let UserContent::Blocks(blocks) = frame.message.content else {
            return Ok(());
        };
        let mut events = Vec::new();
        let mut workspace_changed = false;
        let mut tool_called = false;
        {
            let mut decoder = self.decoder.lock().await;
            for block in blocks {
                let ContentBlock::ToolResult(result) = block else {
                    continue;
                };
                if let Some(uuid) = &frame.uuid {
                    decoder.checkpoint = Some(uuid.clone());
                }
                let (completion_events, changed) = finish_tool(
                    &mut decoder,
                    &address,
                    &result,
                    frame.tool_use_result.as_ref(),
                );
                tool_called |= !completion_events.is_empty();
                workspace_changed |= changed;
                events.extend(completion_events);
            }
        }
        if tool_called || workspace_changed {
            let mut output = self.output.lock().await;
            output.evidence.tool_called |= tool_called;
            output.evidence.workspace_changed |= workspace_changed;
        }
        for event in events {
            self.emit(event).await?;
        }
        Ok(())
    }

    async fn system(&self, system: SystemFrame) -> Result<(), ProviderError> {
        match system {
            SystemFrame::Init(init) => {
                let mut catalog = self.catalog.write().await;
                catalog.mcp_status = init
                    .mcp_servers
                    .iter()
                    .map(|server| (server.name.clone(), server.status.clone()))
                    .collect();
                if init.claude_code_version.is_some() {
                    catalog.version = init.claude_code_version;
                }
                Ok(())
            }
            SystemFrame::Status(status) => {
                let mut operation = self.operation.lock().await;
                let Some(operation) = operation.as_mut() else {
                    return Ok(());
                };
                let update = match (status.status.as_deref(), status.compact_result.as_deref()) {
                    (Some("compacting"), _) => true,
                    (_, Some("success")) => {
                        operation.compaction.state = CompactionState::Completed;
                        operation.compaction.completed_at_ms = Some(timestamp());
                        true
                    }
                    (_, Some(_)) => {
                        operation.compaction.state = CompactionState::Failed;
                        operation.compaction.completed_at_ms = Some(timestamp());
                        operation.compaction.result.error = Some(
                            ProviderFailure::new(
                                ProviderOperation::ThreadCompact,
                                ProviderFailureKind::Unknown,
                            )
                            .at_phase(ProviderPhase::Observation, FailureDisposition::Unchanged),
                        );
                        true
                    }
                    _ => false,
                };
                if update {
                    let event = AdapterActivity::Thread {
                        thread_id: operation.sink.scope().thread_id().clone(),
                        event: ThreadEvent::CompactionUpdated(Box::new(
                            operation.compaction.clone(),
                        )),
                    };
                    operation.sink.activity_writer().send_wait(event).await?;
                }
                Ok(())
            }
            SystemFrame::CompactBoundary(boundary) => {
                let mut operation = self.operation.lock().await;
                if let Some(operation) = operation.as_mut() {
                    let metadata = boundary.compact_metadata;
                    let result = &mut operation.compaction.result;
                    result.pre_compaction_tokens = metadata.pre_tokens;
                    result.post_compaction_tokens = metadata.post_tokens;
                    result.context_tokens_before = metadata.pre_tokens;
                    result.context_tokens_after = metadata.post_tokens;
                    result.tokens_removed = metadata
                        .pre_tokens
                        .zip(metadata.post_tokens)
                        .and_then(|(before, after)| before.checked_sub(after));
                    operation.compaction.manual = metadata.trigger.as_deref() != Some("auto");
                }
                Ok(())
            }
            SystemFrame::TaskStarted(task) if task.is_backgrounded == Some(true) => {
                self.notice("Claude Code started a background task, which Forge does not track")
                    .await
            }
            SystemFrame::ApiRetry(retry) => {
                let attempt = match (retry.attempt, retry.max_retries) {
                    (Some(attempt), Some(maximum)) => format!(" (attempt {attempt} of {maximum})"),
                    _ => String::new(),
                };
                let text = format!(
                    "Claude Code is retrying an API request{attempt}: {}",
                    retry.error.as_deref().unwrap_or("unknown error")
                );
                // Retries repeat with new attempt numbers; report each one.
                self.reported_notices.lock().await.remove(&text);
                self.notice(&text).await
            }
            SystemFrame::Informational(info) => {
                if matches!(info.level.as_deref(), Some("warning" | "notice")) {
                    self.notice(&info.content).await
                } else {
                    Ok(())
                }
            }
            SystemFrame::Notification(notification) => self.notice(&notification.text).await,
            SystemFrame::Unknown { subtype, .. } => {
                self.unsupported(&format!("system/{subtype}")).await
            }
            SystemFrame::TaskStarted(_)
            | SystemFrame::TaskNotification(_)
            | SystemFrame::ThinkingTokens
            | SystemFrame::SessionStateChanged { .. }
            | SystemFrame::Recognized { .. } => Ok(()),
        }
    }

    async fn result(&self, result: ResultFrame) -> Result<(), ProviderError> {
        if let Some(operation) = self.operation.lock().await.as_mut()
            && result.local_command.as_deref() == Some("compact")
        {
            if operation.compaction.state == CompactionState::Running {
                operation.compaction.state = if result.is_error {
                    CompactionState::Failed
                } else {
                    CompactionState::Completed
                };
                operation.compaction.completed_at_ms = Some(timestamp());
            }
            if let Some(ended) = operation.ended.take() {
                let _ = ended.send(CompactionEnd {
                    compaction: Some(operation.compaction.clone()),
                });
            }
            return Ok(());
        }
        let Some(active) = self.active.read().await.clone() else {
            if result.unsolicited() {
                return Err(ProviderError::Observation(
                    lifecycle::ObservationFailure::LifecycleUnavailable,
                ));
            }
            return Ok(());
        };
        if let Some(usage) = result.usage {
            let report = TurnUsageReport::accounting(
                identity::UsageReportId::new(result.uuid.clone())
                    .map_err(|_| provider_failure(ProviderFailureKind::Protocol))?,
                timestamp(),
                Some(usage::TokenUsage {
                    input: usage.total_input(),
                    cached_input: usage.cache_read_input_tokens,
                    reasoning: None,
                    output: usage.output_tokens,
                }),
                result.duration_ms,
            )
            .map_err(|_| provider_failure(ProviderFailureKind::Protocol))?;
            self.emit(AdapterActivity::Turn {
                thread_id: active.address.thread_id.clone(),
                turn_id: active.address.turn_id.clone(),
                event: TurnMetadataEvent::Usage(report),
            })
            .await?;
        }
        let checkpoint = {
            let mut decoder = self.decoder.lock().await;
            decoder.last_result = Some(result.clone());
            decoder.current_api_message = None;
            decoder.checkpoint.clone()
        };
        // Decide under the turn lock so concurrently written steering either joins this decision
        // or is rejected. Steering the CLI has not started yet starts the next CLI turn, which
        // continues this Forge turn. An interrupt discards queued input, so it always ends the turn.
        let active = self.active.write().await;
        let Some(active) = active.as_ref() else {
            return Ok(());
        };
        let pending = !active.interrupting && {
            let commands = self
                .commands
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            active.commands.iter().any(|command| {
                commands.get(command).is_some_and(|record| {
                    matches!(*record.borrow(), CommandState::Queued | CommandState::Other)
                })
            })
        };
        if pending {
            return Ok(());
        }
        let outcome = if active.interrupting || result.interrupted() {
            turn::TurnOutcome::Interrupted
        } else {
            result_outcome(&result)
        };
        self.end_turn(Ok(TurnEnd {
            outcome,
            completed_at_ms: timestamp(),
            duration_ms: result.duration_ms,
            checkpoint,
            commands: active.commands.clone(),
        }));
        Ok(())
    }

    // ----- Inbound control requests ------------------------------------------------------

    async fn handle_control(
        &self,
        request: InboundControlRequest,
        cancelled: CancellationToken,
    ) -> Result<Value, String> {
        match request.request {
            InboundControl::CanUseTool(permission) => {
                let tool_use_id = permission.tool_use_id.clone();
                let decision = self.authorize(*permission, cancelled).await;
                Ok(
                    serde_json::to_value(decision.unwrap_or(PermissionDecision::Deny {
                        message: "Forge could not resolve this permission request".into(),
                        interrupt: true,
                        tool_use_id,
                    }))
                    .map_err(|error| error.to_string())?,
                )
            }
            InboundControl::McpMessage {
                server_name,
                message,
            } if server_name == HOST_TOOL_SERVER => Ok(self.host_tool(message).await),
            InboundControl::McpMessage { server_name, .. } => Err(format!(
                "Forge serves no SDK MCP server named {server_name}"
            )),
            InboundControl::HookCallback { .. } => Ok(json!({"continue": true})),
            InboundControl::Other { subtype } => {
                Err(format!("Unsupported control request subtype: {subtype}"))
            }
        }
    }

    /// Resolve one `can_use_tool` request through Harness permission policy.
    async fn authorize(
        &self,
        permission: CanUseTool,
        cancelled: CancellationToken,
    ) -> Option<PermissionDecision> {
        let tool_use_id = permission.tool_use_id.clone();
        let cancel = || PermissionDecision::Deny {
            message: "The permission request was cancelled".into(),
            interrupt: true,
            tool_use_id: tool_use_id.clone(),
        };
        if !self.is_admitting() {
            return Some(cancel());
        }
        let active = self.active.read().await.clone()?;
        let permissions = active.permissions.clone()?;
        let _callback = self.callback_work.read().await;
        let authorizing = async {
            self.observe_native_start(timestamp()).await.ok()?;
            permissions.before_tool(&active.address.scope).await.ok()?;
            let request = PermissionRequest {
                id: uuid::Uuid::new_v4().to_string(),
                provider: ProviderId::new("claude").ok()?,
                reason: permission
                    .decision_reason
                    .clone()
                    .or_else(|| permission.description.clone()),
                target_list: tool_targets::permission_targets(
                    &permission.tool_name,
                    &permission.input,
                    permission.blocked_path.as_deref(),
                ),
            };
            let response = permissions.authorize(&active.address.scope, request).await;
            let current = self.active.read().await.as_ref().is_some_and(|current| {
                Arc::ptr_eq(&current.generation, &active.generation)
                    && !current.revocation.is_cancelled()
            });
            if !current || !self.is_admitting() {
                return Some(cancel());
            }
            Some(match response {
                PermissionResponse::Allow => {
                    self.tool_execution_started(&active.address, &tool_use_id)
                        .await
                        .ok()?;
                    PermissionDecision::Allow {
                        updated_input: permission.input.clone(),
                        tool_use_id: tool_use_id.clone(),
                    }
                }
                PermissionResponse::Deny { reason } => {
                    let message =
                        reason.unwrap_or_else(|| "Forge denied this tool call".to_owned());
                    if let Some(tool) = self.decoder.lock().await.tools.get_mut(&tool_use_id) {
                        tool.denied = Some(message.clone());
                    }
                    PermissionDecision::Deny {
                        message,
                        interrupt: false,
                        tool_use_id: tool_use_id.clone(),
                    }
                }
                PermissionResponse::Cancelled => cancel(),
            })
        };
        tokio::select! {
            biased;
            _ = self.admission_revocation.cancelled() => Some(cancel()),
            _ = active.revocation.cancelled() => Some(cancel()),
            _ = cancelled.cancelled() => Some(cancel()),
            decision = authorizing => decision,
        }
    }

    /// Publish a tool's execution start once its permission was granted.
    async fn tool_execution_started(
        &self,
        address: &TurnAddress,
        tool_use_id: &str,
    ) -> Result<(), ProviderError> {
        let item_id = {
            let mut decoder = self.decoder.lock().await;
            match decoder.tools.get_mut(tool_use_id) {
                Some(tool) if !tool.started => {
                    tool.started = true;
                    Some(tool.item_id.clone())
                }
                _ => None,
            }
        };
        if let Some(item_id) = item_id {
            self.emit(item(
                address,
                &item_id,
                ItemEvent::Tool {
                    event: ToolItemEvent::ExecutionStarted,
                },
            ))
            .await?;
        }
        Ok(())
    }

    /// Answer one MCP message for the Harness control tool server.
    async fn host_tool(&self, message: Value) -> Value {
        let tools = self
            .host_tools
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone();
        match super::host_tools::handle(&message, &tools) {
            super::host_tools::HostToolMessage::Immediate(response) => response,
            super::host_tools::HostToolMessage::Call {
                id,
                name,
                arguments,
                tool_use_id,
            } => {
                let outcome = self
                    .call_host_tool(&tools, name, arguments, tool_use_id)
                    .await;
                super::host_tools::call_result(id, outcome)
            }
        }
    }

    async fn call_host_tool(
        &self,
        tools: &[control::ControlToolDefinition],
        name: String,
        arguments: Value,
        tool_use_id: Option<String>,
    ) -> Result<String, String> {
        if !tools.iter().any(|tool| tool.name == name) {
            return Err(format!("Tool {name} is not available in this turn"));
        }
        let active = self
            .active
            .read()
            .await
            .clone()
            .ok_or_else(|| "No Forge turn is active".to_owned())?;
        let provider_tools = active
            .provider_tools
            .clone()
            .ok_or_else(|| "The Forge turn no longer accepts tool calls".to_owned())?;
        let _callback = self.callback_work.read().await;
        let id = control::ToolCallId::new(
            tool_use_id.unwrap_or_else(|| uuid::Uuid::new_v4().to_string()),
        )
        .map_err(|error| error.to_string())?;
        let call = control::ToolCall {
            id,
            name,
            arguments,
        };
        tokio::select! {
            biased;
            _ = active.revocation.cancelled() => Err("The Forge turn ended".to_owned()),
            outcome = provider_tools.call(&active.address.scope, call) => {
                outcome.map_err(|error| error.to_string())
            }
        }
    }
}

#[async_trait]
impl FrameHandler for ClaudeSessionState {
    async fn frame(&self, frame: StdoutFrame) {
        if let Err(error) = self.handle_frame(frame).await {
            let observation = match &error {
                ProviderError::Observation(failure) => failure.clone(),
                _ => lifecycle::ObservationFailure::Protocol,
            };
            if let Some(sink) = self.publication_sink().await {
                sink.fail(events::EventDeliveryError::Observation(observation));
            }
            self.invalidate_binding(provider_failure(ProviderFailureKind::Protocol))
                .await;
        }
    }

    async fn control(
        &self,
        request: InboundControlRequest,
        cancelled: CancellationToken,
    ) -> Result<Value, String> {
        self.handle_control(request, cancelled).await
    }

    async fn closed(&self, failure: Option<TransportFailure>) {
        self.commands
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clear();
        let kind = match failure {
            Some(TransportFailure::Decode(_)) => ProviderFailureKind::Protocol,
            _ => ProviderFailureKind::Transport,
        };
        let cause = ProviderError::Failure(
            ProviderFailure::new(ProviderOperation::TurnStart, kind)
                .at_phase(ProviderPhase::Observation, FailureDisposition::Invalidated),
        );
        self.invalidate_binding(cause).await;
    }
}

// ----- Decoder helpers ---------------------------------------------------------------------

/// Append streamed text to its block's item, creating the item at its first nonempty delta.
fn append_text(
    decoder: &mut TurnDecoder,
    address: &TurnAddress,
    message_id: &str,
    index: u64,
    reasoning: bool,
    text: &str,
) -> Result<Vec<AdapterActivity>, ProviderError> {
    if text.is_empty() {
        return Ok(Vec::new());
    }
    if let Some(message) = decoder
        .messages
        .iter_mut()
        .find(|message| message.api_message_id == message_id && message.block_index == index)
    {
        message.text.push_str(text);
        return Ok(vec![item(
            address,
            &message.item_id,
            ItemEvent::Message {
                event: MessageItemEvent::Delta(text.into()),
            },
        )]);
    }
    let item_id = ItemId::new(uuid::Uuid::new_v4().to_string())
        .map_err(|_| provider_failure(ProviderFailureKind::Protocol))?;
    decoder.messages.push(MessageItem {
        item_id: item_id.clone(),
        api_message_id: message_id.to_owned(),
        block_index: index,
        reasoning,
        text: text.into(),
        role: None,
    });
    let start = if reasoning {
        ItemStart::Reasoning(ReasoningStart {
            text: text.into(),
            kind: ReasoningKind::Summary,
        })
    } else {
        ItemStart::AgentMessage(AgentMessageStart {
            text: text.into(),
            classification: MessageClassification::Unclassified,
        })
    };
    Ok(vec![item(address, &item_id, ItemEvent::Started(start))])
}

/// Reconcile a block's streamed text with its complete text from the `assistant` frame.
fn complete_text(
    decoder: &mut TurnDecoder,
    address: &TurnAddress,
    message_id: &str,
    index: u64,
    reasoning: bool,
    complete: &str,
) -> Result<Vec<AdapterActivity>, ProviderError> {
    let existing = decoder
        .messages
        .iter_mut()
        .find(|message| message.api_message_id == message_id && message.block_index == index);
    match existing {
        Some(message) => {
            if complete.len() > message.text.len() && complete.starts_with(&message.text) {
                let suffix = complete[message.text.len()..].to_owned();
                message.text = complete.to_owned();
                Ok(vec![item(
                    address,
                    &message.item_id,
                    ItemEvent::Message {
                        event: MessageItemEvent::Delta(suffix),
                    },
                )])
            } else {
                // A lost or reordered delta cannot corrupt the completed item.
                message.text = complete.to_owned();
                Ok(Vec::new())
            }
        }
        None => append_text(decoder, address, message_id, index, reasoning, complete),
    }
}

/// Classify every unclassified prose item of the turn.
///
/// `Some(Commentary)` marks prose that preceded a tool call. `Some(Response)` runs at
/// completion: the last unclassified prose item is the answer and earlier ones are commentary.
fn classify_pending(
    decoder: &mut TurnDecoder,
    address: &TurnAddress,
    role: Option<AgentMessageRole>,
) -> Vec<AdapterActivity> {
    let Some(role) = role else {
        return Vec::new();
    };
    let pending = decoder
        .messages
        .iter()
        .enumerate()
        .filter(|(_, message)| !message.reasoning && message.role.is_none())
        .map(|(position, _)| position)
        .collect::<Vec<_>>();
    let last = pending.last().copied();
    pending
        .into_iter()
        .map(|position| {
            let message = &mut decoder.messages[position];
            let assigned = if role == AgentMessageRole::Response && Some(position) != last {
                AgentMessageRole::Commentary
            } else {
                role
            };
            message.role = Some(assigned);
            item(
                address,
                &message.item_id,
                ItemEvent::Message {
                    event: MessageItemEvent::Classified(MessageClassification::Known(assigned)),
                },
            )
        })
        .collect()
}

/// Publish a tool item when its complete `tool_use` block arrives.
fn start_tool(
    decoder: &mut TurnDecoder,
    address: &TurnAddress,
    tool: ToolUseBlock,
) -> Result<Vec<AdapterActivity>, ProviderError> {
    if decoder.tools.contains_key(&tool.id) {
        return Ok(Vec::new());
    }
    let kind = tool_targets::tool_kind(&tool.name);
    let (title, semantics) = tool_targets::semantics(&tool.name, &tool.input);
    let descriptor = ToolDescriptor {
        name: tool_targets::display_name(&tool.name).to_owned(),
        title,
        semantics,
        arguments: InspectableArguments::new(tool.input.clone()),
    };
    let item_id = ItemId::new(uuid::Uuid::new_v4().to_string())
        .map_err(|_| provider_failure(ProviderFailureKind::Protocol))?;
    let start = match kind {
        ToolKind::Command => ItemStart::CommandExecution(descriptor.clone()),
        ToolKind::FileChange => ItemStart::FileChange(descriptor.clone()),
        ToolKind::ToolCall => ItemStart::ToolCall(descriptor.clone()),
    };
    decoder.tool_order.push(tool.id.clone());
    decoder.tools.insert(
        tool.id,
        ToolItem {
            item_id: item_id.clone(),
            kind,
            descriptor,
            started: false,
            finished: false,
            denied: None,
        },
    );
    Ok(vec![item(address, &item_id, ItemEvent::Started(start))])
}

/// Complete a tool item from its `tool_result`, returning whether it changed the workspace.
fn finish_tool(
    decoder: &mut TurnDecoder,
    address: &TurnAddress,
    result: &ToolResultBlock,
    tool_use_result: Option<&Value>,
) -> (Vec<AdapterActivity>, bool) {
    let Some(tool) = decoder.tools.get_mut(&result.tool_use_id) else {
        return (Vec::new(), false);
    };
    if tool.finished {
        return (Vec::new(), false);
    }
    tool.finished = true;
    let mut events = Vec::new();
    let output = if tool.kind == ToolKind::Command {
        without_cwd_reset(result.text())
    } else {
        result.text()
    };
    let failed = result.is_error == Some(true);
    let outcome = if let Some(reason) = tool.denied.clone() {
        ToolOutcome::Denied {
            reason: SanitizedMessage::new(reason).unwrap_or_else(|_| {
                SanitizedMessage::new("Tool execution was denied")
                    .expect("static denial is nonempty")
            }),
        }
    } else if failed {
        ToolOutcome::Failed {
            diagnostic: tool::ToolDiagnostic {
                kind: ProviderFailureKind::Unknown,
                message: SanitizedMessage::new("Claude Code tool execution failed")
                    .expect("static diagnostic is nonempty"),
            },
        }
    } else {
        ToolOutcome::Succeeded
    };
    let executed = matches!(outcome, ToolOutcome::Succeeded | ToolOutcome::Failed { .. });
    if executed && !tool.started {
        tool.started = true;
        events.push(item(
            address,
            &tool.item_id,
            ItemEvent::Tool {
                event: ToolItemEvent::ExecutionStarted,
            },
        ));
    }
    let changes = if tool.kind == ToolKind::FileChange && !failed && tool.denied.is_none() {
        tool_use_result.map(file_changes).unwrap_or_default()
    } else {
        Default::default()
    };
    let changed = !changes.file.is_empty();
    if changed {
        events.push(item(
            address,
            &tool.item_id,
            ItemEvent::Tool {
                event: ToolItemEvent::Changes(changes.clone()),
            },
        ));
    }
    events.push(item(
        address,
        &tool.item_id,
        ItemEvent::Completed(tool_completion(
            tool.kind,
            ToolCompletion {
                outcome,
                descriptor: Some(tool.descriptor.clone()),
                output: (!output.is_empty()).then_some(output),
                changes,
            },
        )),
    ));
    (events, changed)
}

/// Remove the footer the CLI appends after a shell command changes directory.
///
/// Claude Code restores its working directory after every shell call and reports that in the
/// command's stderr. The line describes the CLI's own bookkeeping, not command output.
fn without_cwd_reset(output: String) -> String {
    match output.rsplit_once('\n') {
        Some((body, footer)) if footer.starts_with("Shell cwd was reset to ") => body.to_owned(),
        None if output.starts_with("Shell cwd was reset to ") => String::new(),
        _ => output,
    }
}

/// Convert a `Write` or `Edit` result into a unified diff.
fn file_changes(result: &Value) -> ProviderChangeSet {
    let Some(path) = result["filePath"].as_str().filter(|path| !path.is_empty()) else {
        return ProviderChangeSet::default();
    };
    let before = result["originalFile"].as_str();
    let after = if let Some(content) = result["content"].as_str() {
        content.to_owned()
    } else if let (Some(original), Some(old), Some(new)) = (
        before,
        result["oldString"].as_str(),
        result["newString"].as_str(),
    ) {
        if result["replaceAll"].as_bool() == Some(true) {
            original.replace(old, new)
        } else {
            original.replacen(old, new, 1)
        }
    } else {
        return ProviderChangeSet::default();
    };
    let kind = if before.is_none() {
        ProviderChangeKind::Add
    } else {
        ProviderChangeKind::Update
    };
    let deadline = std::time::Instant::now() + std::time::Duration::from_millis(250);
    let diff = similar::TextDiff::configure()
        .deadline(deadline)
        .diff_lines(before.unwrap_or_default(), &after)
        .unified_diff()
        .context_radius(3)
        .header(if before.is_none() { "/dev/null" } else { path }, path)
        .to_string();
    ProviderChangeSet {
        file: vec![ProviderFileChange {
            path: path.into(),
            move_path: None,
            kind,
            diff,
        }],
    }
}

/// Publish a `TodoWrite` call as the turn's checklist.
fn todo_checklist(input: &Value, address: &TurnAddress) -> Option<ProviderChecklistUpdate> {
    let todos = input["todos"].as_array()?;
    let entry_list = todos
        .iter()
        .enumerate()
        .map(|(provider_ordinal, todo)| {
            let status = match todo["status"].as_str() {
                Some("in_progress") => ChecklistStatus::InProgress,
                Some("completed") => ChecklistStatus::Completed,
                _ => ChecklistStatus::Pending,
            };
            ProviderChecklistEntry {
                provider_id: None,
                content: todo["content"].as_str().unwrap_or_default().to_owned(),
                priority: None,
                status,
                provider_ordinal,
            }
        })
        .collect::<Vec<_>>();
    Some(ProviderChecklistUpdate {
        scope_id: format!("claude:todo:{}", address.turn_id),
        name: None,
        complete: entry_list
            .iter()
            .all(|entry| entry.status == ChecklistStatus::Completed),
        replace_entries: true,
        entry_list,
    })
}

/// Map a CLI result to the Forge turn outcome.
fn result_outcome(result: &ResultFrame) -> turn::TurnOutcome {
    use turn::{RetryDecision, TurnCompletionReason, TurnOutcome};
    let failed = |kind: ProviderFailureKind, retry: RetryDecision| TurnOutcome::Failed {
        error: ProviderFailure::new(ProviderOperation::TurnStart, kind)
            .at_phase(ProviderPhase::Observation, FailureDisposition::Unchanged),
        retry,
    };
    match result.subtype.as_str() {
        "success" if result.is_error => match result.api_error_status {
            Some(401 | 403) => failed(
                ProviderFailureKind::Authentication,
                RetryDecision::NotAllowed,
            ),
            Some(429) => failed(
                ProviderFailureKind::RateLimited,
                RetryDecision::Allowed { retry_after: None },
            ),
            Some(400 | 404) => failed(ProviderFailureKind::Rejected, RetryDecision::NotAllowed),
            Some(_) => failed(
                ProviderFailureKind::Runtime,
                RetryDecision::Allowed { retry_after: None },
            ),
            None => failed(ProviderFailureKind::Unknown, RetryDecision::NotAllowed),
        },
        "success" => TurnOutcome::Completed {
            stop_reason: match result.stop_reason.as_deref() {
                Some("max_tokens" | "model_context_window_exceeded") => {
                    TurnCompletionReason::TokenLimit
                }
                Some("refusal") => TurnCompletionReason::Refusal,
                _ if result.terminal_reason.as_deref() == Some("prompt_too_long") => {
                    TurnCompletionReason::TokenLimit
                }
                _ => TurnCompletionReason::Normal,
            },
        },
        "error_max_turns" | "error_max_budget_usd" => TurnOutcome::Completed {
            stop_reason: TurnCompletionReason::RequestLimit,
        },
        _ => failed(
            match result.terminal_reason.as_deref() {
                Some("prompt_too_long" | "blocking_limit") => ProviderFailureKind::ContextLimit,
                _ => ProviderFailureKind::Runtime,
            },
            RetryDecision::NotAllowed,
        ),
    }
}

fn tool_completion(kind: ToolKind, completion: ToolCompletion) -> ItemCompletion {
    match kind {
        ToolKind::Command => ItemCompletion::CommandExecution(completion),
        ToolKind::FileChange => ItemCompletion::FileChange(completion),
        ToolKind::ToolCall => ItemCompletion::ToolCall(completion),
    }
}

fn item(address: &TurnAddress, item_id: &ItemId, event: ItemEvent) -> AdapterActivity {
    AdapterActivity::Item {
        thread_id: address.thread_id.clone(),
        turn_id: address.turn_id.clone(),
        item_id: item_id.clone(),
        event,
    }
}

pub(crate) fn provider_failure(kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(ProviderOperation::TurnStart, kind))
}

/// Read the observation clock without a narrowing integer cast.
pub(crate) fn timestamp() -> i64 {
    i64::try_from(
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis(),
    )
    .unwrap_or(i64::MAX)
}

#[cfg(test)]
mod tests {
    use super::without_cwd_reset;

    /// The CLI's directory-restore footer is dropped, and ordinary output stays intact.
    #[test]
    fn shell_output_drops_the_cwd_reset_footer() {
        assert_eq!(
            without_cwd_reset("hi\nShell cwd was reset to C:\\work".into()),
            "hi"
        );
        assert_eq!(without_cwd_reset("Shell cwd was reset to /work".into()), "");
        assert_eq!(
            without_cwd_reset("line one\nline two".into()),
            "line one\nline two"
        );
    }
}
