use super::transport::AcpHandler;
use crate::backend::approval::{ApprovalResolution, PermissionCoordinator};
use crate::backend::*;
use crate::control_tools::{ControlToolExecutor, ControlToolInvocation, ControlTurnContext};
use crate::permissions::matcher::PermissionTarget;
use crate::session::ApprovalMode;
use anyhow::{Context, Result, bail, ensure};
use async_trait::async_trait;
use serde_json::{Value, json};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use tokio::sync::{Mutex, RwLock};

/// Keeps callback policy and control state separate from outgoing request ownership.
pub(super) struct AcpSessionState {
    pub(super) kind: BackendKind,
    pub(super) permission: Arc<PermissionCoordinator>,
    pub(super) active: RwLock<Option<ActiveTurn>>,
    operation: RwLock<Option<ActiveOperation>>,
    pub(super) output: Mutex<BackendCompletion>,
    pub(super) control: Mutex<Option<Arc<ControlToolExecutor>>>,
    pub(super) catalog: RwLock<Value>,
    pub(super) tool_definition: RwLock<Vec<crate::control_tools::ControlToolDefinition>>,
    tool: Mutex<HashMap<(String, String), Value>>,
    finished_tool: Mutex<HashSet<(String, String)>>,
    compaction: Mutex<HashMap<String, Compaction>>,
    children: Mutex<super::children::AcpChildren>,
    message: Mutex<HashMap<TurnAddress, Vec<AcpMessage>>>,
}

#[derive(Clone)]
/// Captures immutable callback routing for the admitted provider turn.
pub(super) struct ActiveTurn {
    task_authority: Option<crate::control_tools::TaskAuthority>,
    pub(super) mode: ApprovalMode,
    pub(super) sink: BackendEventSink,
}

struct ActiveOperation {
    session_id: String,
    sink: BackendEventSink,
}

struct AcpMessage {
    address: MessageAddress,
    role: Option<MessageRole>,
    text: String,
    closed: bool,
}

impl AcpSessionState {
    pub(super) fn new(kind: BackendKind, permission: Arc<PermissionCoordinator>) -> Self {
        Self {
            kind,
            permission,
            active: RwLock::new(None),
            operation: RwLock::new(None),
            output: Mutex::new(BackendCompletion::default()),
            control: Mutex::new(None),
            catalog: RwLock::new(json!({})),
            tool_definition: RwLock::new(Vec::new()),
            tool: Mutex::new(HashMap::new()),
            finished_tool: Mutex::new(HashSet::new()),
            compaction: Mutex::new(HashMap::new()),
            children: Mutex::new(super::children::AcpChildren::default()),
            message: Mutex::new(HashMap::new()),
        }
    }

    /// Reset only turn-scoped output, retaining discovered session metadata.
    pub(super) async fn begin(
        &self,
        request: &BackendRequest,
        session_id: &str,
        sink: BackendEventSink,
    ) -> Result<()> {
        ensure!(
            self.operation.read().await.is_none(),
            "ACP invocation cannot replace an active session operation"
        );
        *self.output.lock().await = BackendCompletion::default();
        self.compaction.lock().await.clear();
        self.tool.lock().await.clear();
        self.finished_tool.lock().await.clear();
        self.message.lock().await.clear();
        *self.children.lock().await =
            super::children::AcpChildren::new(session_id.into(), sink.invocation_scope()?.clone());
        *self.control.lock().await = Some(Arc::new(ControlToolExecutor::new(
            request
                .control_context
                .clone()
                .unwrap_or_else(|| ControlTurnContext::inactive(request.mode)),
        )));
        *self.active.write().await = Some(ActiveTurn {
            task_authority: request
                .control_context
                .as_ref()
                .and_then(crate::control_tools::TaskAuthority::capture),
            mode: request.approval_mode,
            sink,
        });
        Ok(())
    }

    pub(super) async fn emit(&self, event: BackendEvent) -> Result<()> {
        let sink = if let Some(active) = self.active.read().await.as_ref() {
            Some(active.sink.clone())
        } else {
            self.operation
                .read()
                .await
                .as_ref()
                .map(|operation| operation.sink.clone())
        };
        if let Some(sink) = sink {
            sink.send_wait(event).await?;
        }
        Ok(())
    }

    pub(super) async fn begin_compaction(
        &self,
        session_id: &str,
        sink: BackendEventSink,
    ) -> Result<()> {
        ensure!(
            sink.scope().invocation().is_none(),
            "ACP compaction requires a session-operation scope"
        );
        ensure!(
            self.active.read().await.is_none(),
            "ACP compaction cannot replace an active invocation"
        );
        *self.output.lock().await = BackendCompletion::default();
        self.compaction.lock().await.clear();
        *self.operation.write().await = Some(ActiveOperation {
            session_id: session_id.into(),
            sink,
        });
        Ok(())
    }

    pub(super) async fn finish_compaction(&self) {
        *self.operation.write().await = None;
    }

    pub(super) async fn has_active_work(&self) -> bool {
        self.active.read().await.is_some() || self.operation.read().await.is_some()
    }

    async fn event_scope(&self) -> Result<EventScope> {
        if let Some(active) = self.active.read().await.as_ref() {
            return Ok(active.sink.scope().clone());
        }
        Ok(self
            .operation
            .read()
            .await
            .as_ref()
            .context("ACP update has no active operation")?
            .sink
            .scope()
            .clone())
    }

    async fn is_root(&self, session_id: &str) -> bool {
        if let Some(operation) = self.operation.read().await.as_ref() {
            return operation.session_id == session_id;
        }
        self.children.lock().await.is_root(session_id)
    }

    pub(super) async fn address(&self, session_id: &str) -> Option<TurnAddress> {
        self.children.lock().await.address(session_id, false)
    }

    pub(super) async fn scope(&self) -> Result<InvocationScope> {
        Ok(self
            .active
            .read()
            .await
            .as_ref()
            .context("ACP update has no active invocation")?
            .sink
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
                BackendEvent::Message {
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
                BackendEvent::Message {
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
        self.emit(event).await
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
                    BackendEvent::Message {
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
    pub(super) async fn finish(&self, outcome: crate::turn::TurnOutcome) -> Result<()> {
        if self.active.read().await.is_none() {
            self.close_control().await;
            self.finish_compaction().await;
            return Ok(());
        }
        let result = async {
            let addresses = {
                self.message
                    .lock()
                    .await
                    .keys()
                    .cloned()
                    .collect::<Vec<_>>()
            };
            for address in addresses {
                self.finish_messages(&address, outcome).await?;
            }
            let events = self.children.lock().await.finish()?;
            for event in events {
                self.emit(event).await?;
            }
            let root = self.children.lock().await.root_address();
            if let Some(address) = root {
                self.emit(BackendEvent::Turn {
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
        self.close_control().await;
        *self.active.write().await = None;
        self.tool.lock().await.clear();
        self.finished_tool.lock().await.clear();
        result
    }

    async fn finish_messages(
        &self,
        address: &TurnAddress,
        outcome: crate::turn::TurnOutcome,
    ) -> Result<()> {
        if outcome == crate::turn::TurnOutcome::Completed {
            self.classify_messages(address, MessageRole::Response)
                .await?;
        }
        let updates = {
            let mut messages = self.message.lock().await;
            messages
                .remove(address)
                .unwrap_or_default()
                .into_iter()
                .map(|message| {
                    let update = if outcome == crate::turn::TurnOutcome::Completed {
                        MessageUpdate::Completed { text: message.text }
                    } else {
                        MessageUpdate::Interrupted
                    };
                    BackendEvent::Message {
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

    /// Revoke callback authority before collecting an active tool without its runtime lock.
    pub(super) async fn close_control(&self) {
        let executor = self.cancel_control().await;
        if let Some(executor) = executor {
            executor.close().await;
            let mut current = self.control.lock().await;
            if current
                .as_ref()
                .is_some_and(|current| Arc::ptr_eq(current, &executor))
            {
                *current = None;
            }
        }
    }

    /// Removes callback authority without waiting for an in-flight tool or transport operation.
    pub(super) async fn cancel_control(&self) -> Option<Arc<ControlToolExecutor>> {
        let executor = self.control.lock().await.clone();
        if let Some(executor) = &executor {
            executor.cancel();
        }
        executor
    }

    async fn authorize(&self, parameter: Value) -> Result<Value> {
        let active = self
            .active
            .read()
            .await
            .clone()
            .context("ACP permission request has no active turn")?;
        let address = self
            .address(parameter["sessionId"].as_str().unwrap_or_default())
            .await
            .context("ACP permission request has no active session owner")?;
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
        let request = super::approval::permission_request(self.kind, &tool)?;
        let protected = {
            let store = self.permission.store();
            let store = store
                .read()
                .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?;
            crate::backend::approval::protected_target(&request, &store)
        };
        if protected {
            let option = parameter["options"]
                .as_array()
                .and_then(|options| {
                    options
                        .iter()
                        .find(|option| option["kind"] == "reject_once")
                })
                .context("ACP agent did not advertise protected-target rejection")?;
            return Ok(json!({"outcome":{"outcome":"selected","optionId":option["optionId"]}}));
        }
        let authority = if address.scope.owner.is_root() {
            active.task_authority.clone()
        } else {
            None
        };
        if let Some(authority) = &authority {
            authority.validate()?;
        }
        let control = request.target_list.as_slice();
        let internal = matches!(control, [PermissionTarget::Mcp { target }]
            if target.strip_prefix("forge-harness-control/").is_some_and(|name|
                crate::control_tools::ControlToolRegistry.definition_list().iter().any(|definition|definition.name == name)));
        let resolution = if internal {
            ApprovalResolution::AllowOnce
        } else {
            self.permission
                .authorize(active.mode, request, Some(address), Some(&active.sink))
                .await?
        };
        if matches!(
            resolution,
            ApprovalResolution::AllowOnce
                | ApprovalResolution::AllowExact
                | ApprovalResolution::AllowBroad
        ) && let (Some(item_id), Some(observed)) = (item_id, &observed_tool)
        {
            let current = self.tool.lock().await;
            ensure!(
                current
                    .get(&(session_id.to_owned(), item_id.to_owned()))
                    .is_some_and(|current| current == observed),
                "ACP tool changed while approval was pending"
            );
        }
        if matches!(
            resolution,
            ApprovalResolution::AllowOnce
                | ApprovalResolution::AllowExact
                | ApprovalResolution::AllowBroad
        ) && let Some(authority) = &authority
        {
            authority.validate()?;
        }
        if resolution == ApprovalResolution::Cancel {
            return Ok(json!({"outcome":{"outcome":"cancelled"}}));
        }
        let allow = matches!(
            resolution,
            ApprovalResolution::AllowOnce
                | ApprovalResolution::AllowExact
                | ApprovalResolution::AllowBroad
        );
        let kind = if allow { "allow_once" } else { "reject_once" };
        let option = parameter["options"]
            .as_array()
            .and_then(|options| options.iter().find(|option| option["kind"] == kind))
            .context("ACP agent did not advertise the required once-only permission choice")?;
        Ok(json!({"outcome":{"outcome":"selected","optionId":option["optionId"]}}))
    }
}

#[async_trait]
impl AcpHandler for AcpSessionState {
    async fn notification(&self, method: &str, parameter: Value) -> Result<()> {
        if method == "_claude/sdkMessage" && self.kind == BackendKind::Claude {
            if self.active.read().await.is_none()
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
                let usage = crate::backend::usage::TokenUsage {
                    input,
                    cached_input: usage["cache_read_input_tokens"].as_u64(),
                    reasoning: None,
                    output: usage["output_tokens"].as_u64(),
                };
                if let Some(id) = message["uuid"].as_str() {
                    let update = crate::backend::usage::UsageUpdate {
                        id: id.into(),
                        usage: usage.clone(),
                        cumulative: None,
                        cumulative_total: None,
                    };
                    if let Some(address) = self
                        .address(parameter["sessionId"].as_str().unwrap_or_default())
                        .await
                    {
                        self.emit(BackendEvent::Usage { address, update }).await?;
                    }
                }
                if let Some(duration_ms) = message["duration_ms"].as_u64()
                    && let Some(address) = self
                        .address(parameter["sessionId"].as_str().unwrap_or_default())
                        .await
                {
                    self.emit(BackendEvent::Turn {
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
        let session_id = parameter["sessionId"].as_str().unwrap_or_default();
        if self.active.read().await.is_none()
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
        let lifecycle = self.children.lock().await.lifecycle(session_id, update)?;
        if let Some(events) = lifecycle {
            for event in events {
                if let BackendEvent::Turn {
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
                let status = ToolStatus::parse(status).map_err(|error| BackendError::Protocol {
                    message: format!("{error:#}"),
                })?;
                let invocation = ToolInvocation {
                    kind: match update["kind"].as_str() {
                        Some("edit" | "delete" | "move") => ToolActivityKind::FileChange,
                        Some("execute") => ToolActivityKind::Command,
                        _ => ToolActivityKind::ToolCall,
                    },
                    title: update["title"].as_str().unwrap_or("Tool").into(),
                    name: update["title"].as_str().unwrap_or("Tool").into(),
                    arguments: update.get("rawInput").cloned().unwrap_or(Value::Null),
                };
                if first || invocation_changed {
                    self.emit(BackendEvent::Tool {
                        address: address.clone(),
                        update: ToolUpdate::Requested { invocation },
                    })
                    .await?;
                }
                if first {
                    self.emit(BackendEvent::Tool {
                        address: address.clone(),
                        update: ToolUpdate::Started,
                    })
                    .await?;
                    self.output.lock().await.evidence.tool_called = true;
                }
                if let Some(output) = tool_output(update)? {
                    self.emit(BackendEvent::Tool {
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
                    self.emit(BackendEvent::Tool {
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
                            code: None,
                            message: tool_output(update)?
                                .unwrap_or_else(|| "ACP tool failed".into()),
                        },
                    }),
                    ToolStatus::Denied => Some(ToolOutcome::Denied {
                        reason: tool_output(update)?.unwrap_or_else(|| "ACP tool denied".into()),
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
                    self.emit(BackendEvent::Tool {
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
                    self.emit(BackendEvent::Compaction {
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
                        self.emit(BackendEvent::Compaction {
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
                    .and_then(|(used, size)| crate::session::ContextUsage::reported(used, size))
                {
                    self.output.lock().await.metrics.context_usage = Some(context.clone());
                    if self.active.read().await.is_some() {
                        self.emit(BackendEvent::Runtime {
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

    async fn request(&self, method: &str, parameter: Value) -> Result<Value> {
        match method {
            "forge/tools" => Ok(
                json!({"tools":self.tool_definition.read().await.iter().map(|definition|definition.mcp_value()).collect::<Vec<_>>()}),
            ),
            "session/request_permission" => self.authorize(parameter).await,
            "forge/control" => {
                ensure_active(&self.active).await?;
                let invocation = ControlToolInvocation {
                    task_permit: None,
                    name: parameter["name"]
                        .as_str()
                        .context("control tool name missing")?
                        .into(),
                    arguments: parameter.get("arguments").cloned().unwrap_or(json!({})),
                };
                let executor = self
                    .control
                    .lock()
                    .await
                    .clone()
                    .context("Harness control called outside an active turn")?;
                let name = invocation.name.clone();
                let outcome = executor.execute(invocation.clone()).await?;
                let result = outcome.result;
                let control = self.control.lock().await;
                ensure!(
                    control
                        .as_ref()
                        .is_some_and(|current| Arc::ptr_eq(current, &executor)),
                    "Harness control result belongs to a collected turn"
                );
                let result = match result {
                    Ok(result) => result,
                    Err(error) => {
                        drop(control);
                        let failure = crate::control_tools::control_tool_failure_json(
                            &invocation,
                            &error,
                            outcome.plan_document.as_ref(),
                        );
                        self.emit(BackendEvent::Diagnostic {
                            scope: self.scope().await?.into(),
                            diagnostic: ProviderDiagnostic {
                                code: None,
                                message: format!("Harness tool {name} failed: {failure}"),
                                fatal: false,
                            },
                        })
                        .await?;
                        return Err(anyhow::anyhow!("{failure}"));
                    }
                };
                if let Some(invocation) = result.invocation {
                    crate::control_tools::apply_invocation(
                        &invocation,
                        &mut *self.output.lock().await,
                    )?;
                }
                Ok(json!({"message":result.message}))
            }
            _ => bail!("unsupported ACP client request: {method}"),
        }
    }
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

async fn ensure_active(active: &RwLock<Option<ActiveTurn>>) -> Result<()> {
    if active.read().await.is_none() {
        bail!("Harness control called outside an active turn");
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    async fn activate(state: &AcpSessionState, session: &str) -> events::BackendEventStream {
        let owner = ExecutionOwner::root(session);
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let (sink, stream) = events::channel(scope.clone());
        *state.children.lock().await =
            super::super::children::AcpChildren::new(String::new(), scope);
        *state.active.write().await = Some(ActiveTurn {
            task_authority: None,
            mode: ApprovalMode::Ask,
            sink,
        });
        stream
    }

    fn drain(stream: &mut events::BackendEventStream) -> Vec<BackendEvent> {
        std::iter::from_fn(|| stream.try_next().transpose())
            .collect::<std::result::Result<Vec<_>, _>>()
            .unwrap()
    }

    fn tool_events(
        events: &[BackendEvent],
        id: &str,
    ) -> (
        ToolInvocation,
        Option<String>,
        Option<ToolOutcome>,
        ProviderChangeSet,
    ) {
        let mut invocation = None;
        let mut output = None;
        let mut outcome = None;
        let mut changes = ProviderChangeSet::default();
        for event in events {
            if let BackendEvent::Tool { address, update } = event {
                if address.tool_id.as_str() != id {
                    continue;
                }
                match update {
                    ToolUpdate::Requested { invocation: value } => invocation = Some(value.clone()),
                    ToolUpdate::Output {
                        update: TextUpdate::Replace(text),
                    } => output = Some(text.clone()),
                    ToolUpdate::Output {
                        update: TextUpdate::Append(text),
                    } => output.get_or_insert_default().push_str(text),
                    ToolUpdate::Changes { changes: value } => changes = value.clone(),
                    ToolUpdate::Finished { outcome: value } => outcome = Some(value.clone()),
                    _ => {}
                }
            }
        }
        (
            invocation.expect("requested ACP tool"),
            output,
            outcome,
            changes,
        )
    }

    #[tokio::test]
    async fn manual_compaction_retains_operation_scope_without_a_provider_turn() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let state = AcpSessionState::new(
            BackendKind::Claude,
            PermissionCoordinator::transient(fixture.path())?,
        );
        let scope = EventScope::SessionOperation {
            owner: ExecutionOwner::root("session"),
            operation_id: "compact".into(),
        };
        let (sink, mut stream) = events::channel(scope.clone());
        state.begin_compaction("native", sink).await?;
        assert!(state.has_active_work().await);
        for update in [
            json!({"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"Not a response"}}),
            json!({"sessionUpdate":"compaction_update","compactionId":"native-compact","status":"in_progress"}),
            json!({"sessionUpdate":"compaction_summary_chunk","compactionId":"native-compact","content":{"type":"text","text":"Retained"}}),
            json!({"sessionUpdate":"compaction_update","compactionId":"native-compact","status":"completed"}),
            json!({"sessionUpdate":"usage_update","used":20,"size":100}),
        ] {
            state
                .notification(
                    "session/update",
                    json!({"sessionId":"native","update":update}),
                )
                .await?;
        }
        state.finish_compaction().await;
        assert!(!state.has_active_work().await);
        let events = drain(&mut stream);
        assert_eq!(events.len(), 3);
        assert!(events.iter().all(
            |event| matches!(event, BackendEvent::Compaction { update, .. } if update.manual)
                && event.scope() == scope
        ));
        assert!(state.active.read().await.is_none());
        assert_eq!(
            state
                .output
                .lock()
                .await
                .metrics
                .context_usage
                .as_ref()
                .unwrap()
                .used,
            20
        );
        Ok(())
    }

    #[tokio::test]
    async fn message_chunks_complete_with_one_identity_and_preserve_interruption() -> Result<()> {
        for outcome in [
            crate::turn::TurnOutcome::Completed,
            crate::turn::TurnOutcome::Cancelled,
        ] {
            let fixture = tempfile::tempdir()?;
            let state = AcpSessionState::new(
                BackendKind::Claude,
                PermissionCoordinator::transient(fixture.path())?,
            );
            let mut stream = activate(&state, "test").await;
            for text in ["First ", "answer"] {
                state.notification("session/update", json!({"update":{
                    "sessionUpdate":"agent_message_chunk","content":{"type":"text","text":text}
                }})).await?;
            }
            state.finish(outcome).await?;
            let mut messages = Vec::new();
            while let Some(update) = stream.try_next()? {
                if let BackendEvent::Message { address, update } = update {
                    messages.push((address, update));
                }
            }
            assert!(messages.iter().all(|message| message.0 == messages[0].0));
            let last = messages.last().unwrap();
            if outcome == crate::turn::TurnOutcome::Completed {
                assert!(messages.iter().any(|message| matches!(
                    message.1,
                    MessageUpdate::Classified {
                        role: MessageRole::Response
                    }
                )));
                assert!(
                    matches!(&last.1, MessageUpdate::Completed { text } if text == "First answer")
                );
            } else {
                assert!(
                    !messages
                        .iter()
                        .any(|message| matches!(message.1, MessageUpdate::Classified { .. }))
                );
                assert!(matches!(last.1, MessageUpdate::Interrupted));
            }
        }
        Ok(())
    }

    #[tokio::test]
    async fn cleanup_cancels_tool_waits_for_both_acp_providers() -> Result<()> {
        for kind in [BackendKind::Claude, BackendKind::Antigravity] {
            let fixture = tempfile::tempdir()?;
            let state =
                AcpSessionState::new(kind, PermissionCoordinator::transient(fixture.path())?);
            let _stream = activate(&state, "root").await;
            let mut context = ControlTurnContext::inactive(PromptMode::Chat);
            let (_task_root, _task_store, task_controller) =
                crate::task::SessionTaskController::test_goal();
            context.task_controller = Some(task_controller);
            context.checkpoint = Some(crate::checkpoint::ToolGate::new()?);
            *state.control.lock().await = Some(Arc::new(ControlToolExecutor::new(context)));
            let mut pending = Box::pin(state.request(
                "forge/control",
                json!({
                    "name":"goal_complete", "arguments":{"summary":"old turn"}
                }),
            ));
            assert!(futures_util::poll!(pending.as_mut()).is_pending());
            let (outcome, ()) = tokio::time::timeout(std::time::Duration::from_secs(1), async {
                tokio::join!(pending, state.close_control())
            })
            .await?;
            assert!(outcome.unwrap_err().is::<crate::broker::TurnCancelled>());
            assert!(!state.output.lock().await.evidence.structured_complete);
            let mut context = ControlTurnContext::inactive(PromptMode::Chat);
            let (_task_root, _task_store, task_controller) =
                crate::task::SessionTaskController::test_goal();
            context.task_controller = Some(task_controller);
            *state.control.lock().await = Some(Arc::new(ControlToolExecutor::new(context)));
            state
                .request(
                    "forge/control",
                    json!({
                        "name":"goal_complete", "arguments":{"summary":"replacement turn"}
                    }),
                )
                .await?;
            assert!(state.output.lock().await.evidence.structured_complete);
            assert!(state.output.lock().await.goal_control.is_some());
        }
        Ok(())
    }

    #[tokio::test]
    async fn malformed_agent_tool_returns_field_diagnostics_to_the_model_and_timeline() -> Result<()>
    {
        let fixture = tempfile::tempdir()?;
        let state = AcpSessionState::new(
            BackendKind::Antigravity,
            PermissionCoordinator::transient(fixture.path())?,
        );
        let mut stream = activate(&state, "root").await;
        *state.control.lock().await = Some(Arc::new(ControlToolExecutor::new(
            ControlTurnContext::inactive(crate::backend::PromptMode::Chat),
        )));
        let failure = state
            .request(
                "forge/control",
                json!({"name":"agent_spawn",
            "arguments":{"provider":"incorrect"}}),
            )
            .await
            .unwrap_err();
        let diagnostic: Value = serde_json::from_str(&failure.to_string())?;
        assert_eq!(diagnostic["code"], "invalid_arguments");
        let violation = diagnostic["violation"].as_array().unwrap();
        assert!(violation.iter().any(|item| item["code"] == "missing_field"
            && item["message"].as_str().unwrap().contains("text")));
        assert!(
            violation
                .iter()
                .any(|item| item["path"] == "provider" && item["code"] == "unknown_field")
        );
        assert!(
            matches!(drain(&mut stream).last(), Some(BackendEvent::Diagnostic { diagnostic, .. }) if diagnostic.message.contains("text"))
        );
        Ok(())
    }

    #[tokio::test]
    async fn child_callbacks_preserve_approval_ownership_and_root_metadata() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let permission = PermissionCoordinator::transient(fixture.path())?;
        permission
            .store()
            .write()
            .unwrap()
            .save(r#"{"permission":{"edit":{"*":"ask"}}}"#)?;
        let state = Arc::new(AcpSessionState::new(
            BackendKind::Claude,
            permission.clone(),
        ));
        let mut stream = activate(&state, "test").await;
        *state.children.lock().await =
            super::super::children::AcpChildren::new("root".into(), state.scope().await?);
        state
            .notification(
                "session/update",
                json!({"sessionId":"root","update":{
            "sessionUpdate":"subagent_spawned","subagentSessionId":"child"}}),
            )
            .await?;
        for update in [
            json!({"sessionUpdate":"config_option_update","configOptions":[{"id":"model","currentValue":"child-model"}]}),
            json!({"sessionUpdate":"available_commands_update","availableCommands":[{"name":"child-command"}]}),
            json!({"sessionUpdate":"usage_update","used":10,"size":100}),
            json!({"sessionUpdate":"compaction_update","compactionId":"child-compaction","status":"completed"}),
        ] {
            state
                .notification(
                    "session/update",
                    json!({"sessionId":"child","update":update}),
                )
                .await?;
        }
        state.notification("_claude/sdkMessage", json!({"sessionId":"child","message":{
            "type":"result","uuid":"child-result","usage":{"input_tokens":10,"cache_read_input_tokens":0,
                "cache_creation_input_tokens":0,"output_tokens":5},"duration_ms":20}})).await?;
        assert_eq!(*state.catalog.read().await, json!({}));
        assert!(state.output.lock().await.metrics.context_usage.is_none());
        assert!(state.compaction.lock().await.is_empty());
        let initial = drain(&mut stream);
        assert_eq!(initial.len(), 2);
        let waiting_state = state.clone();
        let waiting = tokio::spawn(async move {
            waiting_state.authorize(json!({"sessionId":"child",
            "toolCall":{"kind":"edit","rawInput":{"file_path":"child.txt"}},
            "options":[{"kind":"allow_once","optionId":"allow"},{"kind":"reject_once","optionId":"deny"}]
        })).await
        });
        let approval = tokio::time::timeout(std::time::Duration::from_secs(1), stream.recv())
            .await??
            .unwrap();
        let BackendEvent::Approval {
            scope,
            update: ApprovalUpdate::Requested(approval),
        } = approval
        else {
            panic!("expected approval")
        };
        let child = state.address("child").await.unwrap();
        assert_eq!(child.thread_id, "child");
        assert_eq!(scope, child.scope);
        assert_eq!(approval.execution_owner.as_ref(), Some(&child.scope.owner));
        let pending = permission.pending_list()?.remove(0);
        permission
            .resolve(
                &pending.id,
                &[crate::backend::approval::ApprovalAnswer {
                    item_id: pending.item_list[0].id.clone(),
                    choice_id: "deny_once".into(),
                }],
                None,
            )
            .await?;
        assert_eq!(waiting.await??["outcome"]["optionId"], "deny");
        assert!(
            state
                .authorize(json!({"sessionId":"unowned"}))
                .await
                .is_err()
        );
        state.finish(crate::turn::TurnOutcome::Completed).await?;
        assert!(state.active.read().await.is_none());
        assert!(drain(&mut stream).iter().any(|event| matches!(event, BackendEvent::Agent { update: AgentUpdate::Lifecycle { event }, .. } if event.status == crate::agent::AgentExecutionState::Interrupted)));
        Ok(())
    }

    #[tokio::test]
    async fn permission_callbacks_preserve_native_target_namespaces() {
        let fixture = tempfile::tempdir().unwrap();
        let permission = PermissionCoordinator::transient(fixture.path()).unwrap();
        permission
            .store()
            .write()
            .unwrap()
            .save(r#"{"permission":{"edit":{"*":"ask"},"mcp":{"*":"ask"},"webfetch":{"*":"ask"}}}"#)
            .unwrap();
        let state = Arc::new(AcpSessionState::new(
            BackendKind::Claude,
            permission.clone(),
        ));
        let _stream = activate(&state, "test").await;
        for (tool, expected) in [
            (
                json!({"kind":"edit","rawInput":{"TargetFile":"target.txt"}}),
                ("edit", "target.txt"),
            ),
            (
                json!({"kind":"other","_meta":{"claudeCode":{"toolName":"mcp__github__search"}},
                "rawInput":{"command":"remote argument"}}),
                ("mcp", "github/search"),
            ),
            (
                json!({"kind":"other","_meta":{"mcp":{"server":"docs","tool":"lookup"}},
                "rawInput":{"path":"remote argument"}}),
                ("mcp", "docs/lookup"),
            ),
            (
                json!({"kind":"fetch","rawInput":{"url":"https://example.test/docs"}}),
                ("webfetch", "https://example.test/docs"),
            ),
        ] {
            let waiting_state = state.clone();
            let waiting = tokio::spawn(async move {
                waiting_state.authorize(json!({"toolCall":tool,
                "options":[{"kind":"allow_once","optionId":"allow"},{"kind":"reject_once","optionId":"deny"}]
            })).await.unwrap()
            });
            let approval = tokio::time::timeout(std::time::Duration::from_secs(1), async {
                loop {
                    if let Some(approval) = permission.pending_list().unwrap().into_iter().next() {
                        break approval;
                    }
                    tokio::task::yield_now().await;
                }
            })
            .await
            .unwrap();
            assert_eq!(approval.item_list.len(), 1);
            let rule = approval.item_list[0]
                .choice_list
                .iter()
                .find(|choice| choice.id == "allow_exact")
                .unwrap();
            assert_eq!(
                rule.rule_list,
                vec![(expected.0.to_owned(), expected.1.to_owned())]
            );
            permission
                .resolve(
                    &approval.id,
                    &[crate::backend::approval::ApprovalAnswer {
                        item_id: approval.item_list[0].id.clone(),
                        choice_id: "deny_once".into(),
                    }],
                    None,
                )
                .await
                .unwrap();
            assert_eq!(waiting.await.unwrap()["outcome"]["optionId"], "deny");
        }
        let internal = tokio::time::timeout(std::time::Duration::from_secs(1), state.authorize(json!({
            "toolCall":{"kind":"other","_meta":{"mcp":{"server":"forge-harness-control","tool":"goal_complete"}}},
            "options":[{"kind":"allow_once","optionId":"internal-allow"},{"kind":"reject_once","optionId":"deny"}]
        }))).await.unwrap().unwrap();
        assert_eq!(internal["outcome"]["optionId"], "internal-allow");
        assert!(permission.pending_list().unwrap().is_empty());
    }

    #[tokio::test]
    async fn callback_updates_preserve_tool_identity_and_compaction_summary() {
        let fixture = tempfile::tempdir().unwrap();
        let state = AcpSessionState::new(
            BackendKind::Claude,
            PermissionCoordinator::transient(fixture.path()).unwrap(),
        );
        state.notification("session/update", json!({"update":{"sessionUpdate":"available_commands_update","availableCommands":[]}})).await.unwrap();
        assert_eq!(state.catalog.read().await["availableCommands"], json!([]));
        let mut stream = activate(&state, "test").await;
        for update in [
            json!({"sessionUpdate":"tool_call","toolCallId":"command-1","title":"Run check","kind":"execute","status":"in_progress"}),
            json!({"sessionUpdate":"tool_call_update","toolCallId":"command-1","status":"completed","content":[{"type":"content","content":{"type":"text","text":"passed"}}]}),
            json!({"sessionUpdate":"compaction_update","compactionId":"compact-1","status":"in_progress","_meta":{"trigger":"manual"}}),
            json!({"sessionUpdate":"compaction_summary_chunk","compactionId":"compact-1","content":{"type":"text","text":"Retained summary"}}),
            json!({"sessionUpdate":"compaction_update","compactionId":"compact-1","status":"completed"}),
            json!({"sessionUpdate":"usage_update","used":50000,"size":200000}),
        ] {
            state
                .notification(
                    "session/update",
                    json!({"sessionId":"native-1","update":update}),
                )
                .await
                .unwrap();
        }
        let events = drain(&mut stream);
        let (invocation, output, outcome, _) = tool_events(&events, "command-1");
        assert_eq!(invocation.title, "Run check");
        assert!(matches!(invocation.kind, ToolActivityKind::Command));
        assert_eq!(output.as_deref(), Some("passed"));
        assert!(matches!(outcome, Some(ToolOutcome::Succeeded)));
        assert_eq!(
            state
                .output
                .lock()
                .await
                .metrics
                .context_usage
                .as_ref()
                .unwrap()
                .remaining_percent,
            75
        );
        let compaction_map = state.compaction.lock().await;
        let compaction = &compaction_map["compact-1"];
        assert!(compaction.manual);
        assert_eq!(compaction.thread_id, "native-1");
        assert_eq!(
            compaction.result.summary_content.as_deref(),
            Some("Retained summary")
        );
        assert_eq!(compaction.state, CompactionState::Completed);
        state.notification("_claude/sdkMessage", json!({"sessionId":"native-1","message":{
            "type":"result","uuid":"call-1","duration_ms":1234,
            "usage":{"input_tokens":10,"cache_read_input_tokens":20,"cache_creation_input_tokens":30,"output_tokens":40}
        }})).await.unwrap();
        let events = drain(&mut stream);
        let usage = events
            .iter()
            .find_map(|event| match event {
                BackendEvent::Usage { update, .. } => Some(update),
                _ => None,
            })
            .unwrap();
        assert_eq!(usage.usage.input, Some(60));
        assert_eq!(usage.usage.cached_input, Some(20));
        assert_eq!(usage.usage.reasoning, None);
        assert!(events.iter().any(|event| matches!(event, BackendEvent::Turn { update: TurnUpdate::Summary { summary }, .. } if summary.duration_ms == 1234)));
        *state.active.write().await = None;
        assert!(
            state
                .request("forge/control", json!({"name":"plan_read"}))
                .await
                .is_err()
        );
    }

    #[tokio::test]
    async fn sparse_tool_completion_preserves_acp_file_diffs() {
        let fixture = tempfile::tempdir().unwrap();
        let state = AcpSessionState::new(
            BackendKind::Claude,
            PermissionCoordinator::transient(fixture.path()).unwrap(),
        );
        let mut stream = activate(&state, "test").await;
        state.notification("session/update", json!({"update":{
            "sessionUpdate":"tool_call","toolCallId":"edit-1","kind":"edit","title":"Edit files",
            "content":[
                {"type":"diff","path":"/project/config.txt","oldText":"before\n","newText":"after\n"},
                {"type":"diff","path":"/project/new.txt","oldText":null,"newText":"created\n"},
                {"type":"diff","path":"/project/invalid.txt","newText":"unknown baseline"},
                {"type":"content","content":{"type":"text","text":"Edited"}}
            ]
        }})).await.unwrap();
        state
            .notification(
                "session/update",
                json!({"update":{
                    "sessionUpdate":"tool_call_update","toolCallId":"edit-1","status":"completed"
                }}),
            )
            .await
            .unwrap();
        let events = drain(&mut stream);
        let (_, output, outcome, changes) = tool_events(&events, "edit-1");
        assert!(matches!(outcome, Some(ToolOutcome::Succeeded)));
        assert_eq!(output.as_deref(), Some("Edited"));
        assert_eq!(changes.file.len(), 2);
        let changed = &changes.file[0];
        assert_eq!(changed.kind, ProviderChangeKind::Update);
        assert!(changed.diff.contains("-before\n+after\n"));
        let added = &changes.file[1];
        assert_eq!(added.kind, ProviderChangeKind::Add);
        assert!(added.diff.starts_with("--- /dev/null\n"));
        assert!(added.diff.contains("+created\n"));
    }

    #[tokio::test]
    async fn raw_tool_completion_preserves_command_output_and_errors() {
        let fixture = tempfile::tempdir().unwrap();
        let state = AcpSessionState::new(
            BackendKind::Antigravity,
            PermissionCoordinator::transient(fixture.path()).unwrap(),
        );
        let mut stream = activate(&state, "test").await;
        for update in [
            json!({"sessionUpdate":"tool_call","toolCallId":"command","kind":"execute","title":"Check","status":"in_progress"}),
            json!({"sessionUpdate":"tool_call_update","toolCallId":"command","status":"completed","content":[],
                "rawOutput":{"combinedOutput":"COMMAND_RESULT","exitCode":7}}),
            json!({"sessionUpdate":"tool_call_update","toolCallId":"error","status":"failed","rawOutput":"PROVIDER_ERROR"}),
            json!({"sessionUpdate":"tool_call_update","toolCallId":"structured","status":"completed","rawOutput":{"result":[1,2]}}),
            json!({"sessionUpdate":"tool_call_update","toolCallId":"explicit","status":"completed","rawOutput":"DUPLICATED_TEXT",
                "content":[{"type":"content","content":{"type":"text","text":"RENDERED_TEXT"}}]}),
        ] {
            state
                .notification("session/update", json!({"update":update}))
                .await
                .unwrap();
        }
        let events = drain(&mut stream);
        let (command, output, outcome, _) = tool_events(&events, "command");
        assert_eq!(command.title, "Check");
        assert_eq!(output.as_deref(), Some("COMMAND_RESULT\nExit code: 7"));
        assert!(matches!(outcome, Some(ToolOutcome::Succeeded)));
        assert_eq!(
            tool_events(&events, "error").1.as_deref(),
            Some("PROVIDER_ERROR")
        );
        let structured = tool_events(&events, "structured").1.unwrap();
        assert_eq!(
            serde_json::from_str::<Value>(&structured).unwrap(),
            json!({"result":[1,2]})
        );
        assert_eq!(
            tool_events(&events, "explicit").1.as_deref(),
            Some("RENDERED_TEXT")
        );
    }

    #[tokio::test]
    async fn sparse_permission_uses_tool_snapshot_and_isolated_session_identity() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let permission = PermissionCoordinator::transient(fixture.path())?;
        permission.store().write().unwrap().set_rule_list(&[(
            crate::permissions::document::CATEGORY_EDIT.into(),
            "one.rs".into(),
            crate::permissions::document::PermissionDecision::Allow,
        )])?;
        let state = AcpSessionState::new(BackendKind::Claude, permission.clone());
        let _stream = activate(&state, "session").await;
        state.notification("session/update",json!({"sessionId":"native","update":{
            "sessionUpdate":"tool_call","toolCallId":"edit","kind":"edit","locations":[{"path":"one.rs"}]
        }})).await?;
        let approval = json!({"sessionId":"native","toolCall":{"toolCallId":"edit"},
            "options":[{"kind":"allow_once","optionId":"allow"},{"kind":"reject_once","optionId":"deny"}]});
        let result =
            tokio::time::timeout(std::time::Duration::from_secs(1), state.authorize(approval))
                .await??;
        assert_eq!(result["outcome"]["optionId"], "allow");
        assert!(permission.pending_list()?.is_empty());
        assert!(
            !state
                .tool
                .lock()
                .await
                .contains_key(&("other".into(), "edit".into()))
        );
        state.finish(crate::turn::TurnOutcome::Completed).await?;
        assert!(state.tool.lock().await.is_empty());
        Ok(())
    }
}
