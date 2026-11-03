use super::AcpKind as ProviderKind;
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

/// Owns native ACP events and conversation state, delegating policy to the host.
pub struct AcpSessionState {
    kind: ProviderKind,
    permission: Option<Arc<dyn ScopedPermissions>>,
    active: RwLock<Option<ActiveTurn>>,
    retained: RwLock<Option<ActiveTurn>>,
    history_replay: AtomicBool,
    operation: RwLock<Option<ActiveOperation>>,
    output: Mutex<forge_provider_api::completion::ProviderCompletion>,
    catalog: RwLock<Value>,
    tool: Mutex<HashMap<(String, String), Value>>,
    tool_owner: Mutex<HashMap<(String, String), TurnAddress>>,
    finished_tool: Mutex<HashSet<(String, String)>>,
    compaction: Mutex<HashMap<String, Compaction>>,
    children: Mutex<super::children::AcpChildren>,
    message: Mutex<HashMap<TurnAddress, Vec<AcpMessage>>>,
}

#[derive(Clone)]
/// Captures immutable callback routing for the admitted provider turn.
struct ActiveTurn {
    generation: Arc<()>,
    sink: ProviderEventSink,
    permissions: Option<Arc<dyn forge_provider_api::permissions::ProviderPermissions>>,
    execution_started: bool,
}

struct ActiveOperation {
    session_id: String,
    sink: ProviderEventSink,
}

struct AcpMessage {
    address: MessageAddress,
    role: Option<MessageRole>,
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
    #[cfg(test)]
    pub fn new(kind: ProviderKind, permission: Arc<dyn ScopedPermissions>) -> Self {
        Self::with_permission(kind, Some(permission))
    }

    /// Create session state whose permission authority is installed with each invocation.
    pub fn new_native(kind: ProviderKind) -> Self {
        Self::with_permission(kind, None)
    }

    fn with_permission(kind: ProviderKind, permission: Option<Arc<dyn ScopedPermissions>>) -> Self {
        Self {
            kind,
            permission,
            active: RwLock::new(None),
            retained: RwLock::new(None),
            history_replay: AtomicBool::new(false),
            operation: RwLock::new(None),
            output: Mutex::new(forge_provider_api::completion::ProviderCompletion::default()),
            catalog: RwLock::new(json!({})),
            tool: Mutex::new(HashMap::new()),
            tool_owner: Mutex::new(HashMap::new()),
            finished_tool: Mutex::new(HashSet::new()),
            compaction: Mutex::new(HashMap::new()),
            children: Mutex::new(super::children::AcpChildren::default()),
            message: Mutex::new(HashMap::new()),
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
        self.tool.lock().await.clear();
        self.finished_tool.lock().await.clear();
        self.message.lock().await.clear();
        *self.children.lock().await = super::children::AcpChildren::new(session_id.into(), scope);
        *self.active.write().await = Some(ActiveTurn {
            generation: Arc::new(()),
            sink,
            permissions: None,
            execution_started: false,
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

    /// Publish live observations through active or retained original invocation ownership.
    pub async fn emit(&self, event: ProviderEvent) -> Result<(), ProviderError> {
        let sink = self
            .publication_sink()
            .await
            .ok_or_else(|| provider_failure(ProviderFailureKind::Rejected))?;
        let address = match event.turn_address().cloned() {
            Some(address) => Some(address),
            None => self.children.lock().await.root_address(),
        };
        let started = {
            let mut active = self.active.write().await;
            if let (Some(active), Some(address)) = (active.as_mut(), address) {
                if !active.execution_started {
                    active.execution_started = true;
                    Some((active.sink.clone(), address))
                } else {
                    None
                }
            } else {
                None
            }
        };
        if let Some((sink, address)) = started {
            let observation = forge_provider_api::lifecycle::ExecutionObservation::new(
                address.scope.clone(),
                forge_provider_api::lifecycle::ExecutionUpdate::Started { address },
            )?;
            sink.send_wait(ProviderEvent::Execution { observation })
                .await?;
        }
        sink.send_wait(event).await?;
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

    /// Revoke invocation callback authority before a fenced process is collected.
    pub async fn revoke_callbacks(&self) {
        if let Some(active) = self.active.write().await.as_mut() {
            active.permissions = None;
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
        self.retained
            .read()
            .await
            .as_ref()
            .map(|retained| retained.sink.clone())
    }

    /// Recognize observation context without granting permission to start new work.
    async fn has_observation_context(&self) -> bool {
        self.active.read().await.is_some() || self.retained.read().await.is_some()
    }

    pub async fn begin_compaction(
        &self,
        session_id: &str,
        sink: ProviderEventSink,
    ) -> Result<(), ProviderError> {
        if sink.scope().invocation().is_some() {
            return Err(provider_failure(ProviderFailureKind::Rejected));
        }
        if self.active.read().await.is_some() {
            return Err(provider_failure(ProviderFailureKind::Rejected));
        }
        *self.output.lock().await = forge_provider_api::completion::ProviderCompletion::default();
        self.compaction.lock().await.clear();
        *self.operation.write().await = Some(ActiveOperation {
            session_id: session_id.into(),
            sink,
        });
        Ok(())
    }

    pub async fn finish_compaction(&self) {
        *self.operation.write().await = None;
    }

    /// Get the most recently observed compaction result for the active operation.
    pub async fn latest_compaction(&self) -> Option<Compaction> {
        self.compaction
            .lock()
            .await
            .values()
            .max_by_key(|compaction| compaction.started_at_ms.or(compaction.completed_at_ms))
            .cloned()
    }

    pub async fn has_active_work(&self) -> bool {
        self.active.read().await.is_some() || self.operation.read().await.is_some()
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
        self.children.lock().await.is_root(session_id)
    }

    pub async fn address(&self, session_id: &str) -> Option<TurnAddress> {
        self.children.lock().await.address(session_id, false)
    }

    pub async fn scope(&self) -> Result<InvocationScope, ProviderError> {
        Ok(self
            .publication_sink()
            .await
            .ok_or_else(|| provider_failure(ProviderFailureKind::Rejected))?
            .invocation_scope()?
            .clone())
    }

    async fn message_chunk(
        &self,
        session_id: &str,
        role: Option<MessageRole>,
        text: &str,
    ) -> Result<()> {
        if text.is_empty() {
            return Ok(());
        }
        let Some(turn) = self.address(session_id).await else {
            return Ok(());
        };
        let event = {
            let mut messages = self.message.lock().await;
            let messages = messages.entry(turn.clone()).or_default();
            if let Some(message) = messages
                .last_mut()
                .filter(|message| !message.closed && message.role == role)
            {
                message.text.push_str(text);
                ProviderEvent::Message {
                    address: message.address.clone(),
                    update: MessageUpdate::Delta { text: text.into() },
                }
            } else {
                let address = MessageAddress {
                    turn,
                    message_id: MessageId::Synthetic(uuid::Uuid::new_v4().to_string()),
                };
                messages.push(AcpMessage {
                    address: address.clone(),
                    role,
                    text: text.into(),
                    closed: false,
                });
                ProviderEvent::Message {
                    address,
                    update: MessageUpdate::Started {
                        classification: role
                            .map(MessageClassification::Known)
                            .unwrap_or(MessageClassification::Unclassified),
                        text: text.into(),
                    },
                }
            }
        };
        Ok(self.emit(event).await?)
    }

    async fn classify_messages(&self, address: &TurnAddress, last_role: MessageRole) -> Result<()> {
        let updates = {
            let mut messages = self.message.lock().await;
            let pending = messages.entry(address.clone()).or_default();
            let count = pending
                .iter()
                .filter(|message| message.role.is_none())
                .count();
            let updates = pending
                .iter_mut()
                .filter(|message| message.role.is_none())
                .enumerate()
                .map(|(index, message)| {
                    let role = if index + 1 == count {
                        last_role
                    } else {
                        MessageRole::Commentary
                    };
                    message.role = Some(role);
                    message.closed = true;
                    ProviderEvent::Message {
                        address: message.address.clone(),
                        update: MessageUpdate::Classified { role },
                    }
                })
                .collect::<Vec<_>>();
            for message in pending {
                message.closed = true;
            }
            updates
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
            let addresses = {
                self.message
                    .lock()
                    .await
                    .keys()
                    .cloned()
                    .collect::<Vec<_>>()
            };
            for address in addresses {
                self.finish_messages(&address, outcome)
                    .await
                    .map_err(|_| provider_failure(ProviderFailureKind::Protocol))?;
            }
            let events = self
                .children
                .lock()
                .await
                .finish()
                .map_err(|_| provider_failure(ProviderFailureKind::Protocol))?;
            for event in events {
                self.emit(event).await?;
            }
            let root = self.children.lock().await.root_address();
            let started = self
                .active
                .read()
                .await
                .as_ref()
                .is_some_and(|active| active.execution_started);
            if started && let Some(address) = root {
                self.emit(ProviderEvent::Turn {
                    address,
                    update: TurnUpdate::Finished {
                        outcome,
                        completed_at_ms: super::children::timestamp(),
                    },
                })
                .await?;
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
        if outcome == forge_provider_api::turn::TurnOutcome::Completed {
            self.classify_messages(address, MessageRole::Response)
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
                    let update = if outcome == forge_provider_api::turn::TurnOutcome::Completed {
                        MessageUpdate::Completed { text: message.text }
                    } else {
                        MessageUpdate::Interrupted
                    };
                    ProviderEvent::Message {
                        address: message.address,
                        update,
                    }
                })
                .collect::<Vec<_>>()
        };
        for update in updates {
            self.emit(update).await?;
        }
        Ok(())
    }

    async fn authorize(&self, parameter: Value) -> Result<Value> {
        let active = self
            .active
            .read()
            .await
            .clone()
            .context("ACP permission request has no active turn")?;
        if let Some(permissions) = &active.permissions {
            permissions
                .before_tool(active.sink.invocation_scope()?)
                .await?;
        }
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
        let address = match address {
            Some(address) => address,
            None => self
                .address(session_id)
                .await
                .context("ACP permission request has no active session owner")?,
        };
        if active.sink.invocation_scope()? != &address.scope {
            return Err(ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
            )
            .into());
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
        let resolution = if let Some(permissions) = &active.permissions {
            match permissions.authorize(&address.scope, request).await {
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
                    forge_provider_api::ProviderOperation::AdmitInput,
                    forge_provider_api::ProviderFailureKind::Rejected,
                ))
                .into(),
            );
        };
        if !self
            .active
            .read()
            .await
            .as_ref()
            .is_some_and(|current| Arc::ptr_eq(&current.generation, &active.generation))
        {
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
    }
}

impl AcpSessionState {
    async fn handle_notification(&self, method: &str, parameter: Value) -> Result<()> {
        if self.history_replay.load(Ordering::Acquire) && method == "_claude/sdkMessage" {
            return Ok(());
        }
        if method == "_claude/sdkMessage" && self.kind == ProviderKind::Claude {
            if !self.has_observation_context().await
                || !self
                    .is_root(parameter["sessionId"].as_str().unwrap_or_default())
                    .await
            {
                return Ok(());
            }
            let message = &parameter["message"];
            if message["type"] == "result" {
                let usage = &message["usage"];
                let input = usage["input_tokens"]
                    .as_u64()
                    .zip(usage["cache_read_input_tokens"].as_u64())
                    .zip(usage["cache_creation_input_tokens"].as_u64())
                    .and_then(|((input, read), created)| {
                        input.checked_add(read)?.checked_add(created)
                    });
                let usage = forge_provider_api::usage::TokenUsage {
                    input,
                    cached_input: usage["cache_read_input_tokens"].as_u64(),
                    reasoning: None,
                    output: usage["output_tokens"].as_u64(),
                };
                if let Some(id) = message["uuid"].as_str() {
                    let update = forge_provider_api::usage::UsageUpdate {
                        context_adjustment: false,
                        id: id.into(),
                        usage: usage.clone(),
                        cumulative: None,
                        cumulative_total: None,
                    };
                    if let Some(address) = self
                        .address(parameter["sessionId"].as_str().unwrap_or_default())
                        .await
                    {
                        self.emit(ProviderEvent::Usage { address, update }).await?;
                    }
                }
                if let Some(duration_ms) = message["duration_ms"].as_u64()
                    && let Some(address) = self
                        .address(parameter["sessionId"].as_str().unwrap_or_default())
                        .await
                {
                    self.emit(ProviderEvent::Turn {
                        address,
                        update: TurnUpdate::Summary {
                            summary: TurnSummary {
                                duration_ms,
                                usage: Some(usage),
                            },
                        },
                    })
                    .await?;
                }
            }
            return Ok(());
        }
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
                Some("subagent_spawned") => true,
                Some("tool_call") => match update["toolCallId"].as_str() {
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
            let failure = forge_provider_api::lifecycle::ObservationFailure::LifecycleUnavailable;
            let observation = forge_provider_api::lifecycle::ExecutionObservation::new(
                self.scope().await?,
                forge_provider_api::lifecycle::ExecutionUpdate::ObservationLost {
                    failure: failure.clone(),
                    completion: None,
                },
            )?;
            self.emit(ProviderEvent::Execution { observation }).await?;
            return Err(ProviderError::Observation(failure).into());
        }
        let lifecycle = self.children.lock().await.lifecycle(session_id, update)?;
        if let Some(events) = lifecycle {
            for event in events {
                if let ProviderEvent::Turn {
                    address,
                    update: TurnUpdate::Finished { outcome, .. },
                } = &event
                {
                    self.finish_messages(address, *outcome).await?;
                }
                self.emit(event).await?;
            }
            return Ok(());
        }
        match update["sessionUpdate"].as_str().unwrap_or_default() {
            "available_commands_update" => {
                if !self.is_root(session_id).await {
                    return Ok(());
                }
                self.catalog.write().await["availableCommands"] =
                    update["availableCommands"].clone();
            }
            "config_option_update" => {
                if !self.is_root(session_id).await {
                    return Ok(());
                }
                self.catalog.write().await["configOptions"] = update["configOptions"].clone();
            }
            "agent_message_chunk" | "agent_thought_chunk" => {
                let thought = update["sessionUpdate"] == "agent_thought_chunk";
                let text = update
                    .pointer("/content/text")
                    .and_then(Value::as_str)
                    .unwrap_or_default();
                self.message_chunk(session_id, thought.then_some(MessageRole::Thought), text)
                    .await?;
            }
            "tool_call" | "tool_call_update" => {
                let Some(id) = update["toolCallId"].as_str().filter(|id| !id.is_empty()) else {
                    return Ok(());
                };
                if let Some(address) = self.address(session_id).await {
                    let mut owners = self.tool_owner.lock().await;
                    let owner = owners
                        .entry((session_id.to_owned(), id.to_owned()))
                        .or_insert_with(|| address.clone());
                    if owner != &address {
                        return Err(ProviderError::Observation(
                            forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
                        )
                        .into());
                    }
                }
                let mut tool_map = self.tool.lock().await;
                let first = !tool_map.contains_key(&(session_id.to_owned(), id.to_owned()));
                let snapshot = tool_map
                    .entry((session_id.to_owned(), id.to_owned()))
                    .or_insert_with(|| json!({}));
                let invocation_changed = ["title", "kind", "rawInput"].into_iter().any(|field| {
                    update
                        .get(field)
                        .is_some_and(|value| !value.is_null() && snapshot.get(field) != Some(value))
                });
                if let Some(field_map) = update.as_object() {
                    for (field, value) in field_map {
                        if !value.is_null() {
                            snapshot[field] = value.clone();
                        }
                    }
                }
                let snapshot = snapshot.clone();
                drop(tool_map);
                let update = &snapshot;
                let Some(turn) = self.address(session_id).await else {
                    return Ok(());
                };
                self.classify_messages(&turn, MessageRole::Commentary)
                    .await?;
                let address = ToolAddress {
                    turn,
                    tool_id: id.into(),
                };
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
                let invocation = crate::acp::normalize_invocation(ToolInvocation {
                    semantics: Default::default(),
                    kind: match update["kind"].as_str() {
                        Some("edit" | "delete" | "move") => ToolActivityKind::FileChange,
                        Some("execute") => ToolActivityKind::Command,
                        _ => ToolActivityKind::ToolCall,
                    },
                    title: update["title"].as_str().unwrap_or("Tool").into(),
                    name: update["title"].as_str().unwrap_or("Tool").into(),
                    arguments: update.get("rawInput").cloned().unwrap_or(Value::Null),
                });
                if first || invocation_changed {
                    self.emit(ProviderEvent::Tool {
                        address: address.clone(),
                        update: ToolUpdate::Requested { invocation },
                    })
                    .await?;
                }
                if first {
                    self.emit(ProviderEvent::Tool {
                        address: address.clone(),
                        update: ToolUpdate::Started,
                    })
                    .await?;
                    self.output.lock().await.evidence.tool_called = true;
                }
                if let Some(output) = tool_output(update)? {
                    self.emit(ProviderEvent::Tool {
                        address: address.clone(),
                        update: ToolUpdate::Output {
                            update: TextUpdate::Replace(output),
                        },
                    })
                    .await?;
                }
                let changes = file_changes(update);
                if !changes.is_empty() {
                    if status == ToolStatus::Succeeded {
                        self.output.lock().await.evidence.workspace_changed = true;
                    }
                    self.emit(ProviderEvent::Tool {
                        address: address.clone(),
                        update: ToolUpdate::Changes { changes },
                    })
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
                    self.emit(ProviderEvent::Tool {
                        address,
                        update: ToolUpdate::Finished { outcome },
                    })
                    .await?;
                }
            }
            "compaction_update" => {
                if !self.is_root(session_id).await {
                    return Ok(());
                }
                if let Some(mut compaction) = normalize_compaction(
                    update,
                    parameter["sessionId"].as_str().unwrap_or_default(),
                ) {
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
                    self.emit(ProviderEvent::Compaction {
                        scope: self.event_scope().await?,
                        update: Box::new(compaction),
                    })
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
                        self.emit(ProviderEvent::Compaction {
                            scope: self.event_scope().await?,
                            update: Box::new(compaction),
                        })
                        .await?;
                    }
                }
            }
            "usage_update" => {
                if !self.is_root(session_id).await {
                    return Ok(());
                }
                if let Some(context) = update["used"]
                    .as_u64()
                    .zip(update["size"].as_u64())
                    .and_then(|(used, size)| {
                        forge_provider_api::context::ContextUsage::reported(used, size)
                    })
                {
                    self.output.lock().await.metrics.context_usage = Some(context.clone());
                    if self.has_observation_context().await {
                        self.emit(ProviderEvent::Runtime {
                            scope: self.scope().await?,
                            update: RuntimeUpdate::ContextUsage { usage: context },
                        })
                        .await?;
                    }
                }
            }
            _ => {}
        }
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
        forge_provider_api::ProviderOperation::NativeTransport,
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

fn normalize_compaction(update: &Value, session_id: &str) -> Option<Compaction> {
    let mut value = update.clone();
    value["id"] = value.get("compactionId")?.clone();
    value["state"] = json!(match update["status"].as_str()? {
        "in_progress" | "started" | "running" => "running",
        "completed" => "completed",
        "failed" => "failed",
        "cancelled" => "unconfirmed",
        _ => return None,
    });
    value["thread_id"] = json!(session_id);
    value["manual"] = json!(
        update.pointer("/_meta/trigger").and_then(Value::as_str) == Some("manual")
            || update["trigger"].as_str() == Some("manual")
    );
    value["result"] = json!({});
    if let Some(error) = update["error"].as_str() {
        value["result"]["error"] = json!(error);
    }
    if let Some(summary) = update["summary"].as_array() {
        let text = summary
            .iter()
            .filter_map(|content| content["text"].as_str())
            .collect::<Vec<_>>()
            .join("");
        value["result"]["summaryContent"] = json!(text);
    }
    serde_json::from_value(value).ok()
}

#[async_trait]
impl AcpHandler for AcpSessionState {
    async fn notification(&self, method: &str, parameter: Value) -> Result<(), AcpConnectionError> {
        self.handle_notification(method, parameter)
            .await
            .map_err(|_| {
                AcpConnectionError::Callback(Box::new(provider_failure(
                    ProviderFailureKind::Protocol,
                )))
            })
    }
    async fn request(&self, method: &str, parameter: Value) -> Result<Value, AcpConnectionError> {
        self.handle_request(method, parameter).await.map_err(|_| {
            AcpConnectionError::Callback(Box::new(provider_failure(ProviderFailureKind::Protocol)))
        })
    }
}

impl AcpSessionState {
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
    }
    pub async fn take_completion(&self) -> forge_provider_api::completion::ProviderCompletion {
        std::mem::take(&mut *self.output.lock().await)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Deny;

    #[async_trait]
    impl ScopedPermissions for Deny {
        async fn authorize(
            &self,
            _: TurnAddress,
            _: forge_provider_api::permissions::PermissionRequest,
        ) -> Result<PermissionResponse, ProviderError> {
            Ok(PermissionResponse::Deny { reason: None })
        }
    }

    #[tokio::test]
    async fn tool_snapshots_keep_original_scope_until_the_next_invocation() -> Result<()> {
        let state = AcpSessionState::new(ProviderKind::Claude, Arc::new(Deny));
        let (sink, _stream) = events::channel(
            BindingId::new("binding")?,
            InvocationScope::new(ExecutionOwner::root("host"), "exchange"),
        );
        state.begin("native", sink).await?;
        state.notification("session/update", json!({"sessionId":"native","update":{
            "sessionUpdate":"tool_call","toolCallId":"edit","kind":"edit","locations":[{"path":"one.rs"}]
        }})).await?;
        {
            let tools = state.tool.lock().await;
            assert!(tools.contains_key(&("native".into(), "edit".into())));
            assert!(!tools.contains_key(&("other".into(), "edit".into())));
        }
        state
            .finish(forge_provider_api::turn::TurnOutcome::Completed)
            .await?;
        assert!(
            state
                .tool
                .lock()
                .await
                .contains_key(&("native".into(), "edit".into()))
        );
        let (next_sink, _next_stream) = events::channel(
            BindingId::new("next-binding")?,
            InvocationScope::new(ExecutionOwner::root("host"), "next"),
        );
        state.begin("native", next_sink).await?;
        assert!(state.tool.lock().await.is_empty());
        let error = state
            .authorize(json!({"sessionId":"native","toolCall":{"toolCallId":"edit"},"options":[]}))
            .await
            .unwrap_err();
        assert!(matches!(
            error.downcast_ref::<ProviderError>(),
            Some(ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict
            ))
        ));
        Ok(())
    }

    #[tokio::test]
    async fn retained_history_reload_does_not_republish_old_transcript_or_tools() -> Result<()> {
        let state = AcpSessionState::new(ProviderKind::Claude, Arc::new(Deny));
        let binding = BindingId::new("history-reload-binding".to_owned())?;
        let scope = InvocationScope::new(ExecutionOwner::root("host"), "original-exchange");
        let (sink, mut stream) = events::channel(binding, scope);
        state.begin("native", sink).await?;
        state
            .finish(forge_provider_api::turn::TurnOutcome::Completed)
            .await?;
        while stream.try_next()?.is_some() {}

        {
            let _replay = state.suppress_history_replay()?;
            state
                .handle_notification("session/update", json!({"sessionId":"native","update":{
                    "sessionUpdate":"agent_message_chunk","content":{"text":"replayed old answer"}
                }}))
                .await?;
            state
                .handle_notification(
                    "session/update",
                    json!({"sessionId":"native","update":{
                        "sessionUpdate":"tool_call","toolCallId":"old-tool","kind":"edit"
                    }}),
                )
                .await?;
            state
                .handle_notification(
                    "session/update",
                    json!({"sessionId":"native","update":{
                        "sessionUpdate":"config_option_update","configOptions":[{"id":"model"}]
                    }}),
                )
                .await?;
        }

        assert!(stream.try_next()?.is_none());
        assert!(state.tool.lock().await.is_empty());
        assert!(!state.execution_started().await);
        assert_eq!(state.catalog().await["configOptions"][0]["id"], "model");
        Ok(())
    }

    #[tokio::test]
    async fn native_tool_output_is_not_reused_as_denial_or_failure_diagnostic() -> Result<()> {
        let state = AcpSessionState::new(ProviderKind::Claude, Arc::new(Deny));
        let (sink, mut stream) = events::channel(
            BindingId::new("tool-diagnostic-binding")?,
            InvocationScope::new(ExecutionOwner::root("host"), "tool-diagnostic-exchange"),
        );
        state.begin("native", sink).await?;

        for (tool_id, status) in [("denied-tool", "denied"), ("failed-tool", "failed")] {
            state
                .notification(
                    "session/update",
                    json!({"sessionId":"native","update":{
                        "sessionUpdate":"tool_call_update",
                        "toolCallId":tool_id,
                        "status":status,
                        "rawOutput":"native diagnostic sentinel"
                    }}),
                )
                .await?;
        }

        let events = std::iter::from_fn(|| stream.try_next().transpose())
            .map(|event| event.map(|event| event.event).map_err(anyhow::Error::from))
            .collect::<Result<Vec<_>>>()?;
        let outputs = events
            .iter()
            .filter_map(|event| match event {
                ProviderEvent::Tool {
                    address,
                    update:
                        ToolUpdate::Output {
                            update: TextUpdate::Replace(text),
                        },
                } if address.tool_id == forge_provider_api::ToolId::from("denied-tool")
                    || address.tool_id == forge_provider_api::ToolId::from("failed-tool") =>
                {
                    Some(text.as_str())
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(
            outputs,
            ["native diagnostic sentinel", "native diagnostic sentinel"]
        );

        let outcomes = events
            .iter()
            .filter_map(|event| match event {
                ProviderEvent::Tool {
                    address,
                    update: ToolUpdate::Finished { outcome },
                } if address.tool_id == forge_provider_api::ToolId::from("denied-tool")
                    || address.tool_id == forge_provider_api::ToolId::from("failed-tool") =>
                {
                    Some((address.tool_id.as_str(), outcome))
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(outcomes.len(), 2);
        match outcomes[0] {
            ("denied-tool", ToolOutcome::Denied { reason }) => {
                assert_eq!(reason.as_str(), "Tool execution was denied by the provider");
            }
            _ => panic!("expected a normalized denied outcome"),
        }
        match outcomes[1] {
            ("failed-tool", ToolOutcome::Failed { diagnostic }) => {
                assert_eq!(
                    diagnostic.message.as_str(),
                    "Provider tool execution failed"
                );
            }
            _ => panic!("expected a normalized failed outcome"),
        }
        assert!(outcomes.iter().all(|(_, outcome)| {
            match outcome {
                ToolOutcome::Denied { reason } => {
                    !reason.as_str().contains("native diagnostic sentinel")
                }
                ToolOutcome::Failed { diagnostic } => !diagnostic
                    .message
                    .as_str()
                    .contains("native diagnostic sentinel"),
                _ => true,
            }
        }));
        Ok(())
    }
}
