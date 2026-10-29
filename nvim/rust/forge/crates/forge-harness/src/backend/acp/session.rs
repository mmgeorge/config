use super::transport::AcpHandler;
use crate::backend::approval::{ApprovalResolution, PermissionCoordinator};
use crate::backend::*;
use crate::control_tools::{ControlToolExecutor, ControlToolInvocation, ControlTurnContext};
use crate::permissions::matcher::PermissionTarget;
use crate::session::ApprovalMode;
use anyhow::{Context, Result, bail, ensure};
use async_trait::async_trait;
use serde_json::{Value, json};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::{Mutex, RwLock};

/// Keeps callback policy and control state separate from outgoing request ownership.
pub(super) struct AcpSessionState {
    pub(super) kind: BackendKind,
    pub(super) permission: Arc<PermissionCoordinator>,
    pub(super) active: RwLock<Option<ActiveTurn>>,
    pub(super) output: Mutex<BackendOutput>,
    pub(super) control: Mutex<Option<Arc<ControlToolExecutor>>>,
    pub(super) catalog: RwLock<Value>,
    pub(super) tool_definition: RwLock<Vec<crate::control_tools::ControlToolDefinition>>,
    tool: Mutex<HashMap<(String, String), Value>>,
    compaction: Mutex<HashMap<String, Compaction>>,
    children: Mutex<super::children::AcpChildren>,
}

#[derive(Clone)]
/// Captures immutable callback routing for the admitted provider turn.
pub(super) struct ActiveTurn {
    task_authority: Option<crate::control_tools::TaskAuthority>,
    pub(super) owner: crate::backend::ExecutionOwner,
    pub(super) mode: ApprovalMode,
    pub(super) sink: Option<BackendEventSink>,
}

impl AcpSessionState {
    pub(super) fn new(kind: BackendKind, permission: Arc<PermissionCoordinator>) -> Self {
        Self {
            kind,
            permission,
            active: RwLock::new(None),
            output: Mutex::new(BackendOutput::default()),
            control: Mutex::new(None),
            catalog: RwLock::new(json!({})),
            tool_definition: RwLock::new(Vec::new()),
            tool: Mutex::new(HashMap::new()),
            compaction: Mutex::new(HashMap::new()),
            children: Mutex::new(super::children::AcpChildren::default()),
        }
    }

    /// Reset only turn-scoped output, retaining discovered session metadata.
    pub(super) async fn begin(
        &self,
        request: &BackendRequest,
        owner: crate::backend::ExecutionOwner,
        session_id: &str,
        sink: Option<BackendEventSink>,
    ) {
        *self.output.lock().await = BackendOutput::default();
        self.tool.lock().await.clear();
        *self.children.lock().await = super::children::AcpChildren::new(session_id.into());
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
            owner,
            mode: request.approval_mode,
            sink,
        });
    }

    pub(super) async fn emit(&self, event: BackendEvent) -> Result<()> {
        let active = self.active.read().await.clone();
        let Some(active) = active else { return Ok(()) };
        self.output.lock().await.event.push(event.clone());
        if let Some(sink) = &active.sink {
            sink.send_wait(event).await?;
        }
        Ok(())
    }

    async fn emit_session(&self, session_id: &str, mut event: BackendEvent) -> Result<()> {
        if !self.children.lock().await.route(session_id, &mut event) {
            return Ok(());
        }
        if matches!(event.kind.as_str(), "assistant_message" | "reasoning") {
            let output = self.output.lock().await;
            let previous = output
                .event
                .iter()
                .rev()
                .find(|previous| previous.address == event.address);
            let id = previous
                .filter(|previous| previous.kind == event.kind)
                .and_then(|previous| previous.message_id())
                .map(str::to_owned)
                .unwrap_or_else(|| format!("{session_id}:message:{}", output.event.len()));
            event.message = Some(ProviderMessage {
                id: MessageId(id),
                role: (event.kind == "reasoning").then_some(MessageRole::Thought),
                update: MessageUpdate::Delta {
                    text: event.text.clone().unwrap_or_default(),
                },
            });
        }
        if event.activity.is_some() {
            self.classify_messages(event.address.as_ref(), MessageRole::Commentary)
                .await?;
        }
        self.emit(event).await
    }

    async fn classify_messages(
        &self,
        address: Option<&ProviderAddress>,
        last_role: MessageRole,
    ) -> Result<()> {
        let updates = {
            let mut output = self.output.lock().await;
            let mut pending = Vec::new();
            for event in &output.event {
                if event.address.as_ref() == address
                    && let Some(message) = event.message.as_ref()
                    && message.role.is_none()
                    && !pending.contains(&message.id)
                {
                    pending.push(message.id.clone());
                }
            }
            let count = pending.len();
            pending
                .into_iter()
                .enumerate()
                .map(|(index, id)| {
                    let role = if index + 1 == count {
                        last_role
                    } else {
                        MessageRole::Commentary
                    };
                    for event in &mut output.event {
                        if event.address.as_ref() == address
                            && let Some(message) = &mut event.message
                            && message.id == id
                        {
                            message.role = Some(role);
                        }
                    }
                    BackendEvent {
                        message: Some(ProviderMessage {
                            id,
                            role: Some(role),
                            update: MessageUpdate::Classified,
                        }),
                        received_at_ms: None,
                        address: address.cloned(),
                        turn_boundary: None,
                        kind: "assistant_message".into(),
                        text: None,
                        data: Value::Null,
                        activity: None,
                        summary: None,
                        checklist_update: None,
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
    pub(super) async fn finish(&self, outcome: crate::turn::TurnOutcome) -> Result<()> {
        let result = async {
            let addresses = {
                let output = self.output.lock().await;
                let mut addresses = Vec::new();
                for event in &output.event {
                    if event.message.is_some() && !addresses.contains(&event.address) {
                        addresses.push(event.address.clone());
                    }
                }
                addresses
            };
            for address in addresses {
                self.finish_messages(address.as_ref(), outcome).await?;
            }
            let events = self.children.lock().await.finish()?;
            for event in events {
                self.emit(event).await?;
            }
            Ok(())
        }
        .await;
        self.close_control().await;
        *self.active.write().await = None;
        self.tool.lock().await.clear();
        result
    }

    async fn finish_messages(
        &self,
        address: Option<&ProviderAddress>,
        outcome: crate::turn::TurnOutcome,
    ) -> Result<()> {
        if outcome == crate::turn::TurnOutcome::Completed {
            self.classify_messages(address, MessageRole::Response)
                .await?;
        }
        let updates = {
            let output = self.output.lock().await;
            let mut messages: Vec<ProviderMessage> = Vec::new();
            for event in &output.event {
                if event.address.as_ref() != address {
                    continue;
                }
                let Some(message) = &event.message else {
                    continue;
                };
                let index = messages
                    .iter()
                    .position(|previous| previous.id == message.id)
                    .unwrap_or_else(|| {
                        messages.push(ProviderMessage {
                            id: message.id.clone(),
                            role: message.role,
                            update: MessageUpdate::Delta {
                                text: String::new(),
                            },
                        });
                        messages.len() - 1
                    });
                let previous = &mut messages[index];
                previous.role = message.role.or(previous.role);
                match (&mut previous.update, &message.update) {
                    (MessageUpdate::Delta { text }, MessageUpdate::Delta { text: delta }) => {
                        text.push_str(delta)
                    }
                    (_, MessageUpdate::Completed { .. } | MessageUpdate::Interrupted) => {
                        previous.update = message.update.clone()
                    }
                    _ => {}
                }
            }
            messages
                .into_iter()
                .filter_map(|mut message| {
                    let MessageUpdate::Delta { text } = message.update else {
                        return None;
                    };
                    message.update = if outcome == crate::turn::TurnOutcome::Completed {
                        MessageUpdate::Completed { text }
                    } else {
                        MessageUpdate::Interrupted
                    };
                    let text = message.text().map(str::to_owned);
                    Some(BackendEvent {
                        message: Some(message),
                        received_at_ms: None,
                        address: address.cloned(),
                        turn_boundary: None,
                        kind: "assistant_message".into(),
                        text,
                        data: Value::Null,
                        activity: None,
                        summary: None,
                        checklist_update: None,
                    })
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
        let mut approval = event("approval_requested", None, Value::Null);
        ensure!(
            self.children.lock().await.route(
                parameter["sessionId"].as_str().unwrap_or_default(),
                &mut approval
            ),
            "ACP permission request has no active session owner"
        );
        if let Some(address) = &mut approval.address {
            address.owner = Some(active.owner.clone());
        } else {
            approval.address = Some(crate::backend::ProviderAddress {
                owner: Some(active.owner.clone()),
                thread_id: parameter["sessionId"].as_str().unwrap_or_default().into(),
                turn_id: String::new(),
            });
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
        let authority = if approval
            .address
            .as_ref()
            .and_then(|address| address.owner.as_ref())
            .is_some_and(|owner| owner.is_root())
        {
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
                .authorize(active.mode, request, approval.address, active.sink.as_ref())
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
            if !self
                .children
                .lock()
                .await
                .is_root(parameter["sessionId"].as_str().unwrap_or_default())
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
                    self.emit(event("usage", None, serde_json::to_value(update)?))
                        .await?;
                }
                if let Some(duration_ms) = message["duration_ms"].as_u64() {
                    let mut summary = event("turn_summary", None, Value::Null);
                    summary.summary = Some(TurnSummary {
                        duration_ms,
                        usage: Some(usage),
                    });
                    self.emit(summary).await?;
                }
            }
            return Ok(());
        }
        if method != "session/update" {
            return Ok(());
        }
        let update = &parameter["update"];
        let session_id = parameter["sessionId"].as_str().unwrap_or_default();
        if matches!(
            update["sessionUpdate"].as_str(),
            Some("subagent_spawned" | "subagent_state_update")
        ) && self.active.read().await.is_none()
        {
            return Ok(());
        }
        let lifecycle = self.children.lock().await.lifecycle(session_id, update)?;
        if let Some(events) = lifecycle {
            for event in events {
                if let Some(TurnBoundary::Finished { outcome }) = event.turn_boundary {
                    self.finish_messages(event.address.as_ref(), outcome)
                        .await?;
                }
                self.emit(event).await?;
            }
            return Ok(());
        }
        match update["sessionUpdate"].as_str().unwrap_or_default() {
            "available_commands_update" => {
                if !self.children.lock().await.is_root(session_id) {
                    return Ok(());
                }
                self.catalog.write().await["availableCommands"] =
                    update["availableCommands"].clone();
            }
            "config_option_update" => {
                if !self.children.lock().await.is_root(session_id) {
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
                let mut event = event("assistant_message", Some(text.into()), update.clone());
                if thought {
                    event.kind = "reasoning".into();
                }
                event.data = update.clone();
                self.emit_session(session_id, event).await?;
            }
            "tool_call" | "tool_call_update" => {
                let Some(id) = update["toolCallId"].as_str().filter(|id| !id.is_empty()) else {
                    return Ok(());
                };
                let mut tool_map = self.tool.lock().await;
                let snapshot = tool_map
                    .entry((session_id.to_owned(), id.to_owned()))
                    .or_insert_with(|| json!({}));
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
                let mut event = event("tool_activity", None, update.clone());
                event.kind = "tool_activity".into();
                event.text = None;
                let status = update["status"].as_str().unwrap_or("in_progress");
                event.activity = Some(ToolActivity {
                    id: update["toolCallId"].as_str().unwrap_or_default().into(),
                    kind: match update["kind"].as_str() {
                        Some("edit" | "delete" | "move") => ToolActivityKind::FileChange,
                        Some("execute") => ToolActivityKind::Command,
                        _ => ToolActivityKind::ToolCall,
                    },
                    title: update["title"].as_str().unwrap_or("Tool").into(),
                    output: tool_output(update)?,
                    status: Some(ToolStatus::parse(status)?),
                    change: file_changes(update),
                    output_delta: false,
                });
                event.data = update.clone();
                self.emit_session(session_id, event).await?;
            }
            "compaction_update" => {
                if !self.children.lock().await.is_root(session_id) {
                    return Ok(());
                }
                if let Some(mut compaction) = normalize_compaction(
                    update,
                    parameter["sessionId"].as_str().unwrap_or_default(),
                ) {
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
                    self.emit(BackendEvent::from_compaction(compaction, None))
                        .await?;
                }
            }
            "compaction_summary_chunk" => {
                if !self.children.lock().await.is_root(session_id) {
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
                        self.emit(BackendEvent::from_compaction(compaction, None))
                            .await?;
                    }
                }
            }
            "usage_update" => {
                if !self.children.lock().await.is_root(session_id) {
                    return Ok(());
                }
                if let Some(context) = update["used"]
                    .as_u64()
                    .zip(update["size"].as_u64())
                    .and_then(|(used, size)| crate::session::ContextUsage::reported(used, size))
                {
                    self.output.lock().await.metrics.context_usage = Some(context.clone());
                    self.emit(event("context_usage", None, serde_json::to_value(context)?))
                        .await?;
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
                        self.emit(event(
                            "error",
                            Some(format!("Harness tool {name} failed: {failure}")),
                            Value::Null,
                        ))
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

fn event(kind: &str, text: Option<String>, data: Value) -> BackendEvent {
    BackendEvent {
        message: None,
        kind: kind.into(),
        text,
        data,
        received_at_ms: None,
        address: None,
        turn_boundary: None,
        activity: None,
        summary: None,
        checklist_update: None,
    }
}

#[cfg(test)]
mod test {
    use super::*;

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
            let (sink, mut stream) = crate::backend::events::channel();
            *state.active.write().await = Some(ActiveTurn {
                task_authority: None,
                owner: ExecutionOwner::root("test"),
                mode: ApprovalMode::Ask,
                sink: Some(sink),
            });
            for text in ["First ", "answer"] {
                state.notification("session/update", json!({"update":{
                    "sessionUpdate":"agent_message_chunk","content":{"type":"text","text":text}
                }})).await?;
            }
            state.finish(outcome).await?;
            let mut messages = Vec::new();
            while let Some(update) = stream.try_next()? {
                if let Some(message) = update.message {
                    messages.push(message);
                }
            }
            assert!(messages.iter().all(|message| message.id == messages[0].id));
            let last = messages.last().unwrap();
            if outcome == crate::turn::TurnOutcome::Completed {
                assert_eq!(last.role, Some(MessageRole::Response));
                assert!(
                    matches!(&last.update, MessageUpdate::Completed { text } if text == "First answer")
                );
            } else {
                assert_eq!(last.role, None);
                assert!(matches!(last.update, MessageUpdate::Interrupted));
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
            *state.active.write().await = Some(ActiveTurn {
                task_authority: None,
                owner: crate::backend::ExecutionOwner::root("root"),
                mode: ApprovalMode::Ask,
                sink: None,
            });
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
        *state.active.write().await = Some(ActiveTurn {
            task_authority: None,
            owner: crate::backend::ExecutionOwner::root("root"),
            mode: ApprovalMode::Ask,
            sink: None,
        });
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
        let output = state.output.lock().await;
        assert_eq!(output.event.last().unwrap().kind, "error");
        assert!(
            output
                .event
                .last()
                .unwrap()
                .text
                .as_ref()
                .unwrap()
                .contains("text")
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
        *state.children.lock().await = super::super::children::AcpChildren::new("root".into());
        *state.active.write().await = Some(ActiveTurn {
            task_authority: None,
            owner: crate::backend::ExecutionOwner::root("test"),
            mode: ApprovalMode::Ask,
            sink: None,
        });
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
        assert_eq!(state.output.lock().await.event.len(), 2);
        let (sink, mut stream) = crate::backend::events::channel();
        state.active.write().await.as_mut().unwrap().sink = Some(sink);
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
        assert_eq!(approval.kind, "approval_requested");
        assert_eq!(approval.address.as_ref().unwrap().thread_id, "child");
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
        let output = state.output.lock().await;
        assert_eq!(output.event.last().unwrap().data["status"], "interrupted");
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
        *state.active.write().await = Some(ActiveTurn {
            task_authority: None,
            owner: crate::backend::ExecutionOwner::root("test"),
            mode: ApprovalMode::Ask,
            sink: None,
        });
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
        *state.active.write().await = Some(ActiveTurn {
            task_authority: None,
            owner: crate::backend::ExecutionOwner::root("test"),
            mode: ApprovalMode::Ask,
            sink: None,
        });
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
        let output = state.output.lock().await;
        let activity = output.event[1].activity.as_ref().unwrap();
        assert_eq!(activity.title, "Run check");
        assert!(matches!(activity.kind, ToolActivityKind::Command));
        assert_eq!(activity.output.as_deref(), Some("passed"));
        assert_eq!(activity.status.as_deref(), Some("completed"));
        assert_eq!(
            output
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
        drop(output);
        state.notification("_claude/sdkMessage", json!({"sessionId":"native-1","message":{
            "type":"result","uuid":"call-1","duration_ms":1234,
            "usage":{"input_tokens":10,"cache_read_input_tokens":20,"cache_creation_input_tokens":30,"output_tokens":40}
        }})).await.unwrap();
        let output = state.output.lock().await;
        let usage: crate::backend::usage::UsageUpdate =
            serde_json::from_value(output.event[output.event.len() - 2].data.clone()).unwrap();
        assert_eq!(usage.usage.input, Some(60));
        assert_eq!(usage.usage.cached_input, Some(20));
        assert_eq!(usage.usage.reasoning, None);
        assert_eq!(
            output
                .event
                .last()
                .unwrap()
                .summary
                .as_ref()
                .unwrap()
                .duration_ms,
            1234
        );
        drop(output);
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
        *state.active.write().await = Some(ActiveTurn {
            task_authority: None,
            owner: crate::backend::ExecutionOwner::root("test"),
            mode: ApprovalMode::Ask,
            sink: None,
        });
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
        let output = state.output.lock().await;
        let activity = output.event.last().unwrap().activity.as_ref().unwrap();
        assert_eq!(activity.status.as_deref(), Some("completed"));
        assert_eq!(activity.output.as_deref(), Some("Edited"));
        assert_eq!(activity.change.file.len(), 2);
        let changed = &activity.change.file[0];
        assert_eq!(changed.kind, ProviderChangeKind::Update);
        assert!(changed.diff.contains("-before\n+after\n"));
        let added = &activity.change.file[1];
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
        *state.active.write().await = Some(ActiveTurn {
            task_authority: None,
            owner: crate::backend::ExecutionOwner::root("test"),
            mode: ApprovalMode::Ask,
            sink: None,
        });
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
        let output = state.output.lock().await;
        let command = output.event[1].activity.as_ref().unwrap();
        assert_eq!(command.title, "Check");
        assert_eq!(
            command.output.as_deref(),
            Some("COMMAND_RESULT\nExit code: 7")
        );
        assert_eq!(command.status.as_deref(), Some("completed"));
        assert_eq!(
            output.event[2].activity.as_ref().unwrap().output.as_deref(),
            Some("PROVIDER_ERROR")
        );
        let structured = output.event[3]
            .activity
            .as_ref()
            .unwrap()
            .output
            .as_ref()
            .unwrap();
        assert_eq!(
            serde_json::from_str::<Value>(structured).unwrap(),
            json!({"result":[1,2]})
        );
        assert_eq!(
            output.event[4].activity.as_ref().unwrap().output.as_deref(),
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
        *state.active.write().await = Some(ActiveTurn {
            task_authority: None,
            owner: crate::backend::ExecutionOwner::root("session"),
            mode: ApprovalMode::Ask,
            sink: None,
        });
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
