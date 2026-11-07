use anyhow::Result;
use forge_provider_api::agent::{AgentActiveState, AgentOutcome};
use forge_provider_api::context::ContextUsage;
use forge_provider_api::{
    ChecklistStatus, FailureDisposition, ProviderChangeKind, ProviderChangeSet,
    ProviderChecklistEntry, ProviderChecklistUpdate, ProviderFailure, ProviderFailureKind,
    ProviderFileChange, ProviderOperation, ProviderPhase,
};
use serde_json::{Value, json};
use std::{path::Path, time::Duration};

#[derive(serde::Deserialize)]
struct DynamicToolCall {
    #[serde(rename = "callId")]
    id: forge_provider_api::control::ToolCallId,
    tool: String,
    arguments: Value,
}

/// Decode the app-server dynamic-tool request into the normalized host call.
pub(super) fn dynamic_tool_call(
    value: &Value,
) -> Result<forge_provider_api::control::ToolCall, serde_json::Error> {
    let call: DynamicToolCall = serde_json::from_value(value.clone())?;
    Ok(forge_provider_api::control::ToolCall {
        id: call.id,
        name: call.tool,
        arguments: call.arguments,
    })
}

/// Encode tool output with the app-server response fields and content discriminator.
pub(super) fn dynamic_tool_response(text: String, success: bool) -> Value {
    json!({"contentItems":[{"type":"inputText","text":text}],"success":success})
}

#[derive(Debug, thiserror::Error)]
#[error("invalid Codex tool status")]
struct InvalidCodexToolStatus;

/// Classify the native item representation before constructing the canonical typed start.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum NativeToolKind {
    Command,
    FileChange,
    ToolCall,
}

pub(super) struct CodexToolSnapshot {
    pub id: String,
    pub kind: NativeToolKind,
    pub name: String,
    pub arguments: Value,
    pub title: String,
    pub output: Option<String>,
    pub status: Option<forge_provider_api::ToolStatus>,
    pub change: ProviderChangeSet,
    pub output_delta: bool,
}

/// Name the collaboration tool recorded by Codex app-server `collabAgentToolCall` items.
#[derive(Clone, Copy, Debug, Eq, PartialEq, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum CollabAgentTool {
    /// Create a new child agent; its receiver thread is the spawned child.
    SpawnAgent,
    /// Send additional input to an existing child.
    SendInput,
    /// Resume a previously stopped child.
    ResumeAgent,
    /// Wait for child results without changing child ownership.
    Wait,
    /// Close a child conversation.
    CloseAgent,
    /// Send a message to an existing child.
    SendMessage,
    /// Assign a follow-up task to an existing child.
    FollowupTask,
    /// Interrupt an existing child.
    InterruptAgent,
    /// List known children without changing their state.
    ListAgents,
}

impl CollabAgentTool {
    /// Report whether the tool restarts work in an already-owned child.
    pub fn resumes_child(self) -> bool {
        matches!(self, Self::SendInput | Self::ResumeAgent | Self::Wait)
    }
}

/// Preserve the pinned collaboration tool-call status.
#[derive(Clone, Copy, Debug, Eq, PartialEq, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
enum CollabAgentToolCallStatus {
    /// The tool call has not finished.
    InProgress,
    /// The tool call finished.
    Completed,
    /// The tool call failed.
    Failed,
    /// The tool call was interrupted.
    Interrupted,
}

/// Preserve the pinned last-known child status reported inside `agentsStates`.
#[derive(Clone, Copy, Debug, Eq, PartialEq, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
enum CollabAgentStatus {
    /// The child is initializing.
    PendingInit,
    /// The child is running.
    Running,
    /// The child was interrupted.
    Interrupted,
    /// The child completed normally.
    Completed,
    /// The child ended with an error.
    Errored,
    /// The child was shut down.
    Shutdown,
    /// The child conversation no longer exists.
    NotFound,
}

/// Preserve the pinned kind of a `subAgentActivity` record.
#[derive(Clone, Copy, Debug, Eq, PartialEq, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
enum SubAgentActivityKind {
    /// The child was created.
    Started,
    /// The parent interacted with an existing child without a reported state change.
    Interacted,
    /// The child was interrupted.
    Interrupted,
    /// The child completed.
    Completed,
}

#[derive(serde::Deserialize)]
struct CollabAgentState {
    status: CollabAgentStatus,
}

/// Decode the two pinned Codex app-server 0.164 collaboration item variants.
#[derive(serde::Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
enum CollaborationItem {
    #[serde(rename_all = "camelCase")]
    CollabAgentToolCall {
        tool: CollabAgentTool,
        status: CollabAgentToolCallStatus,
        sender_thread_id: String,
        receiver_thread_ids: Vec<String>,
        prompt: Option<String>,
        #[serde(default)]
        agents_states: std::collections::HashMap<String, CollabAgentState>,
    },
    #[serde(rename_all = "camelCase")]
    SubAgentActivity {
        kind: SubAgentActivityKind,
        agent_thread_id: String,
    },
}

/// Distinguish live child work from an observed child end without an impossible combination.
#[derive(Clone, Debug)]
pub enum NativeAgentStatus {
    /// The child still has live work or a resumable wait.
    Active(AgentActiveState),
    /// The child reported a terminal outcome.
    Finished(AgentOutcome),
}

impl NativeAgentStatus {
    /// Report whether the child can still receive input or interruption.
    pub fn is_active(&self) -> bool {
        matches!(self, Self::Active(_))
    }
}

/// Retain native ancestry evidence privately until the adapter normalizes it.
#[derive(Clone, Debug)]
pub struct NativeAgentLifecycle {
    /// The collaboration tool, absent for `subAgentActivity` records.
    pub tool: Option<CollabAgentTool>,
    pub starts_child: bool,
    pub parent_thread_id: Option<String>,
    pub provider_thread_id: Option<String>,
    pub task: Option<String>,
    pub status: NativeAgentStatus,
}

/// Translate the pinned last-known child status into the normalized lifecycle vocabulary.
fn agent_status(status: CollabAgentStatus) -> NativeAgentStatus {
    match status {
        CollabAgentStatus::PendingInit => NativeAgentStatus::Active(AgentActiveState::Starting),
        CollabAgentStatus::Running => NativeAgentStatus::Active(AgentActiveState::Running),
        CollabAgentStatus::Interrupted => NativeAgentStatus::Finished(AgentOutcome::Interrupted),
        CollabAgentStatus::Completed => NativeAgentStatus::Finished(AgentOutcome::Completed),
        CollabAgentStatus::Errored => NativeAgentStatus::Finished(AgentOutcome::Failed(
            ProviderFailure::new(ProviderOperation::TurnStart, ProviderFailureKind::Unknown)
                .at_phase(ProviderPhase::Observation, FailureDisposition::Uncertain),
        )),
        CollabAgentStatus::Shutdown | CollabAgentStatus::NotFound => {
            NativeAgentStatus::Finished(AgentOutcome::Closed)
        }
    }
}

/// Decode child lifecycle evidence from one `item/started` or `item/completed` notification.
///
/// Only the exact `collabAgentToolCall` and `subAgentActivity` item shapes carry collaboration
/// state. Other methods and item types return no lifecycle; a malformed collaboration item is a
/// protocol failure rather than an ignored observation.
pub fn normalize_agent_lifecycle(
    method: &str,
    params: &Value,
) -> Result<Vec<NativeAgentLifecycle>> {
    if !matches!(method, "item/started" | "item/completed") {
        return Ok(Vec::new());
    }
    let Some(item) = params.get("item") else {
        return Ok(Vec::new());
    };
    if !matches!(
        item.get("type").and_then(Value::as_str),
        Some("collabAgentToolCall" | "subAgentActivity")
    ) {
        return Ok(Vec::new());
    }
    let item: CollaborationItem = serde_json::from_value(item.clone())
        .map_err(|error| anyhow::anyhow!("invalid Codex collaboration item: {error}"))?;
    match item {
        CollaborationItem::SubAgentActivity {
            kind,
            agent_thread_id,
        } => {
            let status = match kind {
                SubAgentActivityKind::Started => {
                    NativeAgentStatus::Active(AgentActiveState::Running)
                }
                // An interaction reports no child state, so it changes no lifecycle.
                SubAgentActivityKind::Interacted => return Ok(Vec::new()),
                SubAgentActivityKind::Interrupted => {
                    NativeAgentStatus::Finished(AgentOutcome::Interrupted)
                }
                SubAgentActivityKind::Completed => {
                    NativeAgentStatus::Finished(AgentOutcome::Completed)
                }
            };
            Ok(vec![NativeAgentLifecycle {
                tool: None,
                starts_child: kind == SubAgentActivityKind::Started,
                parent_thread_id: pointer_string(params, &["/threadId"]),
                provider_thread_id: Some(agent_thread_id),
                task: None,
                status,
            }])
        }
        CollaborationItem::CollabAgentToolCall {
            tool,
            status,
            sender_thread_id,
            receiver_thread_ids,
            prompt,
            agents_states,
        } => {
            let spawn = tool == CollabAgentTool::SpawnAgent;
            let mut lifecycle_list = Vec::new();
            for receiver_thread_id in receiver_thread_ids {
                let status = match agents_states.get(&receiver_thread_id) {
                    Some(state) => agent_status(state.status),
                    None if spawn => NativeAgentStatus::Active(AgentActiveState::Running),
                    None if tool == CollabAgentTool::CloseAgent
                        && method == "item/completed"
                        && status == CollabAgentToolCallStatus::Completed =>
                    {
                        NativeAgentStatus::Finished(AgentOutcome::Closed)
                    }
                    None => continue,
                };
                lifecycle_list.push(NativeAgentLifecycle {
                    tool: Some(tool),
                    starts_child: spawn,
                    parent_thread_id: Some(sender_thread_id.clone()),
                    provider_thread_id: Some(receiver_thread_id),
                    task: prompt.clone(),
                    status,
                });
            }
            if lifecycle_list.is_empty() && spawn {
                lifecycle_list.push(NativeAgentLifecycle {
                    tool: Some(tool),
                    starts_child: true,
                    parent_thread_id: Some(sender_thread_id),
                    provider_thread_id: None,
                    task: prompt,
                    status: NativeAgentStatus::Active(
                        if status == CollabAgentToolCallStatus::InProgress {
                            AgentActiveState::Starting
                        } else {
                            AgentActiveState::Running
                        },
                    ),
                });
            }
            Ok(lifecycle_list)
        }
    }
}

/// Resolve child-agent lifecycle state from one raw provider message.
pub fn agent_lifecycle_list(message: &Value) -> Result<Vec<NativeAgentLifecycle>> {
    let method = message
        .get("method")
        .and_then(Value::as_str)
        .unwrap_or_default();
    let params = message.get("params").unwrap_or(&Value::Null);
    normalize_agent_lifecycle(method, params)
}

pub fn pointer_string(value: &Value, pointer_list: &[&str]) -> Option<String> {
    pointer_list
        .iter()
        .find_map(|pointer| {
            value
                .pointer(pointer)
                .and_then(Value::as_str)
                .filter(|value| !value.is_empty())
        })
        .map(str::to_owned)
}

/// Tokens Codex reserves inside each model context window for its own instructions and tools.
const CODEX_RESERVED_CONTEXT_TOKENS: u64 = 12_000;

pub fn context_usage(params: &Value) -> Option<ContextUsage> {
    if let Some(token_usage) = params
        .get("tokenUsage")
        .or_else(|| params.get("token_usage"))
    {
        let used = token_usage
            .pointer("/last/totalTokens")
            .or_else(|| token_usage.pointer("/last/total_tokens"))
            .and_then(Value::as_u64)?;
        let size = token_usage
            .get("modelContextWindow")
            .or_else(|| token_usage.get("model_context_window"))
            .and_then(Value::as_u64)?;
        // Codex keeps a fixed instruction and tool baseline inside every model window.
        return ContextUsage::with_reserved(used, size, CODEX_RESERVED_CONTEXT_TOKENS);
    }
    let update = params.get("update").unwrap_or(params);
    let used = update.get("used").and_then(Value::as_u64)?;
    let size = update.get("size").and_then(Value::as_u64)?;
    ContextUsage::reported(used, size)
}

/// Represent a native expenditure observation separately from context correction.
pub(super) enum NativeUsageObservation {
    Expenditure {
        report_key: String,
        usage: forge_provider_api::usage::TokenUsage,
        cumulative: forge_provider_api::usage::TokenUsage,
        cumulative_total: Option<u64>,
    },
    ContextAdjustment,
}

/// Parse native token counts without turning context corrections into expenditure.
pub(super) fn usage_update(params: &Value) -> Option<NativeUsageObservation> {
    use forge_provider_api::usage::TokenUsage;
    fn pointer_u64(value: &Value, paths: &[&str]) -> Option<u64> {
        paths
            .iter()
            .find_map(|path| value.pointer(path).and_then(Value::as_u64))
    }
    fn counts(value: &Value) -> TokenUsage {
        TokenUsage {
            input: pointer_u64(value, &["/inputTokens", "/input_tokens"]),
            cached_input: pointer_u64(value, &["/cachedInputTokens", "/cached_input_tokens"]),
            reasoning: pointer_u64(
                value,
                &["/reasoningOutputTokens", "/reasoning_output_tokens"],
            ),
            output: pointer_u64(value, &["/outputTokens", "/output_tokens"]),
        }
    }
    let token_usage = params
        .get("tokenUsage")
        .or_else(|| params.get("token_usage"))?;
    let last = token_usage.get("last")?;
    let usage = counts(last);
    if usage.input.is_none() && usage.output.is_none() {
        return None;
    }
    let total = token_usage.get("total")?;
    let cumulative = counts(total);
    let cumulative_total = pointer_u64(total, &["/totalTokens", "/total_tokens"]);
    let id = serde_json::to_string(&(cumulative_total, &cumulative)).ok()?;
    if usage.input == Some(0)
        && usage.output == Some(0)
        && cumulative_total.is_some_and(|total| total > 0)
    {
        return Some(NativeUsageObservation::ContextAdjustment);
    }
    Some(NativeUsageObservation::Expenditure {
        report_key: id,
        usage,
        cumulative,
        cumulative_total,
    })
}

pub fn is_tool_lifecycle_event(method_lower: &str, params: &Value) -> bool {
    if matches!(
        method_lower,
        "item/commandexecution/requestapproval"
            | "item/filechange/requestapproval"
            | "item/permissions/requestapproval"
    ) {
        return false;
    }
    let envelope = params.get("update").unwrap_or(params);
    let item = envelope.get("item").unwrap_or(envelope);
    let item_type = item.get("type").and_then(Value::as_str).unwrap_or_default();
    match item_type.to_ascii_lowercase().as_str() {
        "usermessage"
        | "agentmessage"
        | "reasoning"
        | "plan"
        | "contextcompaction"
        | "enteredreviewmode"
        | "exitedreviewmode"
        | "collabagenttoolcall"
        | "subagentactivity"
        | "error" => false,
        "commandexecution" | "command_execution" | "filechange" | "file_change"
        | "dynamictoolcall" | "mcptoolcall" => true,
        _ if !item_type.is_empty() && matches!(method_lower, "item/started" | "item/completed") => {
            item.get("id")
                .and_then(Value::as_str)
                .is_some_and(|id| !id.is_empty())
        }
        _ => {
            method_lower.starts_with("item/")
                && (method_lower.contains("tool")
                    || method_lower.contains("commandexecution")
                    || method_lower.contains("filechange"))
        }
    }
}

pub fn normalize_checklist_update(method: &str, params: &Value) -> Option<ProviderChecklistUpdate> {
    let envelope = params.get("update").unwrap_or(params);
    let entry_list = envelope
        .get("plan")
        .or_else(|| envelope.get("entries"))
        .and_then(Value::as_array);
    let replace_entries = entry_list.is_some();
    let entry_list = entry_list
        .into_iter()
        .flatten()
        .enumerate()
        .filter_map(|(provider_ordinal, entry)| {
            let content = entry
                .get("step")
                .or_else(|| entry.get("content"))
                .and_then(Value::as_str)?;
            let status = match entry
                .get("status")
                .and_then(Value::as_str)
                .unwrap_or("pending")
                .to_ascii_lowercase()
                .as_str()
            {
                "inprogress" | "in_progress" | "active" => ChecklistStatus::InProgress,
                "completed" | "complete" | "done" => ChecklistStatus::Completed,
                _ => ChecklistStatus::Pending,
            };
            Some(ProviderChecklistEntry {
                provider_id: entry
                    .get("id")
                    .or_else(|| entry.get("taskId"))
                    .or_else(|| entry.get("task_id"))
                    .and_then(Value::as_str)
                    .map(str::to_owned),
                content: content.to_owned(),
                priority: entry
                    .get("priority")
                    .and_then(Value::as_str)
                    .map(str::to_owned),
                status,
                provider_ordinal,
            })
        })
        .collect::<Vec<_>>();
    let method_lower = method.to_ascii_lowercase();
    let completed = method_lower.ends_with("/completed")
        || envelope
            .get("status")
            .and_then(Value::as_str)
            .is_some_and(|status| matches!(status, "completed" | "complete"));
    if entry_list.is_empty() && !completed {
        return None;
    }
    let scope_id = envelope
        .get("turnId")
        .or_else(|| envelope.get("turn_id"))
        .or_else(|| envelope.get("threadId"))
        .or_else(|| envelope.get("thread_id"))
        .and_then(Value::as_str)
        .unwrap_or("plan")
        .to_owned();
    let name = envelope
        .get("planName")
        .or_else(|| envelope.get("plan_name"))
        .or_else(|| envelope.get("name"))
        .or_else(|| envelope.get("title"))
        .and_then(Value::as_str)
        .filter(|name| !name.trim().is_empty())
        .map(str::to_owned);
    Some(ProviderChecklistUpdate {
        scope_id,
        name,
        complete: completed,
        replace_entries,
        entry_list,
    })
}

pub(super) fn normalize_tool_activity(
    method: &str,
    params: &Value,
    workspace: &str,
) -> Result<Option<CodexToolSnapshot>> {
    let envelope = params.get("update").unwrap_or(params);
    let item = envelope.get("item").unwrap_or(envelope);
    let Some(id) = [
        item.get("id"),
        envelope.get("itemId"),
        envelope.get("item_id"),
        envelope.get("toolCallId"),
        envelope.get("tool_call_id"),
        envelope.get("callId"),
    ]
    .into_iter()
    .flatten()
    .filter_map(Value::as_str)
    .find(|id| !id.is_empty()) else {
        return Ok(None);
    };
    let id = id.to_owned();
    let item_type = item
        .get("type")
        .and_then(Value::as_str)
        .unwrap_or_default()
        .to_ascii_lowercase();
    let method_lower = method.to_ascii_lowercase();
    let kind = if item_type.contains("commandexecution")
        || item_type.contains("command_execution")
        || method_lower.contains("commandexecution")
    {
        NativeToolKind::Command
    } else if item_type.contains("filechange")
        || item_type.contains("file_change")
        || method_lower.contains("filechange")
    {
        NativeToolKind::FileChange
    } else {
        NativeToolKind::ToolCall
    };
    let generic_item = kind == NativeToolKind::ToolCall
        && !item_type.is_empty()
        && !matches!(item_type.as_str(), "mcptoolcall" | "dynamictoolcall");
    let title = callable_tool_title(item)
        .or_else(|| builtin_tool_title(item))
        .or_else(|| {
            [
                item.get("command"),
                envelope.get("command"),
                item.get("title"),
                envelope.get("title"),
                item.get("tool"),
                envelope.get("tool"),
                item.get("name"),
                envelope.get("name"),
            ]
            .into_iter()
            .flatten()
            .find_map(tool_title)
        })
        .or_else(|| generic_item.then(|| generic_tool_title(item)).flatten())
        .unwrap_or_else(|| match kind {
            NativeToolKind::Command => "command".into(),
            NativeToolKind::FileChange => "file changes".into(),
            NativeToolKind::ToolCall if generic_item => item
                .get("type")
                .and_then(tool_title)
                .unwrap_or_else(|| "tool".into()),
            NativeToolKind::ToolCall => "tool".into(),
        });
    let output = if generic_item {
        method_lower
            .ends_with("/completed")
            .then(|| serde_json::to_string_pretty(item).ok())
            .flatten()
            .map(strip_ansi_escapes::strip_str)
    } else {
        mcp_tool_output(item)
            .or_else(|| {
                item.get("contentItems")
                    .and_then(Value::as_array)
                    .map(|contents| {
                        contents
                            .iter()
                            .filter_map(first_text)
                            .map(strip_ansi_escapes::strip_str)
                            .collect::<Vec<_>>()
                            .join("\n")
                    })
                    .filter(|text| !text.is_empty())
            })
            .or_else(|| {
                item.get("aggregatedOutput")
                    .or_else(|| item.get("aggregated_output"))
                    .or_else(|| item.get("output"))
                    .or_else(|| envelope.get("output"))
                    .or_else(|| envelope.get("delta"))
                    .or_else(|| envelope.get("message"))
                    .and_then(Value::as_str)
                    .map(strip_ansi_escapes::strip_str)
                    .or_else(|| {
                        envelope
                            .get("content")
                            .and_then(first_text)
                            .map(strip_ansi_escapes::strip_str)
                    })
            })
    };
    let status = item
        .get("success")
        .and_then(Value::as_bool)
        .filter(|success| !success)
        .map(|_| "failed".to_owned())
        .or_else(|| {
            item.get("status")
                .or_else(|| envelope.get("status"))
                .and_then(Value::as_str)
                .map(str::to_owned)
                .or_else(|| {
                    method_lower
                        .ends_with("/completed")
                        .then(|| "completed".to_owned())
                })
                .or_else(|| {
                    method_lower
                        .ends_with("/started")
                        .then(|| "inProgress".to_owned())
                })
                .or_else(|| {
                    method_lower
                        .eq("item/mcptoolcall/progress")
                        .then(|| "inProgress".to_owned())
                })
        });
    let status = status.as_deref().map(parse_tool_status).transpose()?;
    Ok(Some(CodexToolSnapshot {
        name: item
            .get("tool")
            .or_else(|| item.get("name"))
            .or_else(|| item.get("toolName"))
            .or_else(|| item.get("tool_name"))
            .or_else(|| item.get("type"))
            .and_then(Value::as_str)
            .unwrap_or("tool")
            .into(),
        arguments: item.get("arguments").cloned().unwrap_or_else(|| {
            if kind == NativeToolKind::Command {
                json!({
                    "command": item.get("command").or_else(|| envelope.get("command")),
                    "cwd": item.get("cwd").or_else(|| envelope.get("cwd")),
                })
            } else {
                Value::Null
            }
        }),
        id,
        kind,
        title,
        output,
        status,
        change: normalize_file_change_set(item, envelope, workspace),
        output_delta: method_lower.ends_with("/outputdelta")
            || method_lower.ends_with("/output_delta"),
    }))
}

/// Decode native Codex lifecycle spellings before exposing the canonical host status.
fn parse_tool_status(
    status: &str,
) -> Result<forge_provider_api::ToolStatus, InvalidCodexToolStatus> {
    use forge_provider_api::ToolStatus;
    match status.to_ascii_lowercase().as_str() {
        "pending" | "running" | "inprogress" | "in_progress" => Ok(ToolStatus::Running),
        "completed" | "complete" | "success" | "succeeded" => Ok(ToolStatus::Succeeded),
        "failed" | "error" => Ok(ToolStatus::Failed),
        "denied" | "declined" | "rejected" => Ok(ToolStatus::Denied),
        "cancelled" | "canceled" => Ok(ToolStatus::Cancelled),
        "interrupted" => Ok(ToolStatus::Interrupted),
        _ => Err(InvalidCodexToolStatus),
    }
}

fn normalize_file_change_set(item: &Value, envelope: &Value, workspace: &str) -> ProviderChangeSet {
    let change_list = item
        .get("changes")
        .or_else(|| item.get("change_list"))
        .or_else(|| envelope.get("changes"))
        .or_else(|| envelope.get("change_list"))
        .and_then(Value::as_array);
    let Some(change_list) = change_list else {
        return ProviderChangeSet::default();
    };
    let file = change_list
        .iter()
        .filter_map(|change| {
            let path = change
                .get("path")
                .or_else(|| change.get("filePath"))
                .or_else(|| change.get("file_path"))
                .and_then(Value::as_str)?;
            let path = provider_change_path(path, workspace);
            let kind = match change
                .get("kind")
                .or_else(|| change.get("operation"))
                .and_then(|kind| {
                    kind.as_str()
                        .or_else(|| kind.get("type").and_then(Value::as_str))
                })
                .unwrap_or("update")
                .to_ascii_lowercase()
                .as_str()
            {
                "add" | "create" | "created" => ProviderChangeKind::Add,
                "delete" | "deleted" | "remove" => ProviderChangeKind::Delete,
                "move" | "rename" => ProviderChangeKind::Move,
                _ => ProviderChangeKind::Update,
            };
            let source = change
                .get("diff")
                .or_else(|| change.get("patch"))
                .and_then(Value::as_str)
                .unwrap_or_default();
            let (diff, reported_move) = split_move_report(kind, source);
            let move_path = change
                .get("movePath")
                .or_else(|| change.get("move_path"))
                .and_then(Value::as_str)
                .or(reported_move)
                .map(|path| provider_change_path(path, workspace));
            let kind = if move_path.is_some() {
                ProviderChangeKind::Move
            } else {
                kind
            };
            Some(ProviderFileChange {
                path,
                move_path,
                kind,
                diff: diff.to_owned(),
            })
        })
        .collect();
    ProviderChangeSet { file }
}

/// Separate a trailing move annotation from complete patch content.
fn split_move_report(kind: ProviderChangeKind, diff: &str) -> (&str, Option<&str>) {
    if matches!(kind, ProviderChangeKind::Add | ProviderChangeKind::Delete) {
        return (diff, None);
    }
    let Some((patch, destination)) = diff.rsplit_once("\nMoved to: ") else {
        return (diff, None);
    };
    let destination = destination.trim_end_matches(['\r', '\n']);
    if destination.is_empty() || destination.contains(['\r', '\n']) {
        return (diff, None);
    }
    (patch, Some(destination))
}

fn provider_change_path(path: &str, workspace: &str) -> String {
    if workspace.is_empty() {
        return path.to_owned();
    }
    let path = path.replace('\\', "/");
    let workspace = workspace.replace('\\', "/");
    Path::new(&path)
        .strip_prefix(Path::new(&workspace))
        .unwrap_or_else(|_| Path::new(&path))
        .to_string_lossy()
        .into_owned()
}

fn tool_title(value: &Value) -> Option<String> {
    match value {
        Value::String(title) => compact_tool_title(title),
        Value::Array(part_list) => {
            let arguments = part_list
                .iter()
                .map(Value::as_str)
                .collect::<Option<Vec<_>>>()?;
            let interpreter = forge_shell::shell::CommandShell::from_executable(arguments.first()?);
            if interpreter != forge_shell::shell::CommandShell::Unknown
                && let Some(marker) = arguments.iter().position(|argument| {
                    matches!(
                        argument.to_ascii_lowercase().as_str(),
                        "-command" | "--commands" | "-c" | "-lc" | "-ic" | "-lic"
                    )
                })
                && let Some(script) = arguments.get(marker + 1)
                && marker + 2 == arguments.len()
            {
                let display = forge_shell::command::display_command(script, interpreter);
                return compact_tool_title(&display.source);
            }
            let title = part_list
                .iter()
                .filter_map(Value::as_str)
                .collect::<Vec<_>>()
                .join(" ");
            compact_tool_title(&title)
        }
        _ => None,
    }
}

fn callable_tool_title(item: &Value) -> Option<String> {
    let item_type = item.get("type").and_then(Value::as_str).unwrap_or_default();
    if !item_type.is_empty()
        && !item_type.eq_ignore_ascii_case("mcpToolCall")
        && !item_type.eq_ignore_ascii_case("dynamicToolCall")
    {
        return None;
    }
    let tool = item.get("tool").and_then(Value::as_str)?;
    let namespace = item
        .get("server")
        .or_else(|| item.get("namespace"))
        .and_then(Value::as_str);
    let title = namespace
        .filter(|namespace| !namespace.is_empty())
        .map_or_else(
            || tool.to_owned(),
            |namespace| format!("{namespace}.{tool}"),
        );
    let arguments = item.get("arguments").cloned().unwrap_or_else(|| json!({}));
    crate::tool_display::call_title(&title, &arguments)
}

fn generic_tool_title(item: &Value) -> Option<String> {
    let title = item.get("type").and_then(Value::as_str)?;
    crate::tool_display::call_title(title, item)
}

fn builtin_tool_title(item: &Value) -> Option<String> {
    match item.get("type")?.as_str()? {
        "webSearch" => web_tool_title(item),
        "sleep" => Some(format!(
            "Wait · {}s",
            Duration::from_millis(item.get("durationMs")?.as_u64()?).as_secs_f64()
        )),
        _ => None,
    }
}

fn web_tool_title(item: &Value) -> Option<String> {
    let Some(action) = item.get("action").filter(|action| !action.is_null()) else {
        return Some(
            item.get("query")
                .and_then(Value::as_str)
                .filter(|query| !query.trim().is_empty())
                .and_then(|query| compact_tool_title(&format!("Web Search · {query}")))
                .unwrap_or_else(|| "Web Search".into()),
        );
    };
    let url = action
        .get("url")
        .and_then(Value::as_str)
        .filter(|url| !url.trim().is_empty())
        .map(|url| {
            url.strip_prefix("https://")
                .or_else(|| url.strip_prefix("http://"))
                .unwrap_or(url)
        });
    match action.get("type")?.as_str()? {
        "search" => {
            let mut queries: Vec<&str> = action
                .get("queries")
                .and_then(Value::as_array)
                .into_iter()
                .flatten()
                .filter_map(Value::as_str)
                .filter(|query| !query.trim().is_empty())
                .collect();
            if queries.is_empty() {
                queries.extend(
                    action
                        .get("query")
                        .and_then(Value::as_str)
                        .or_else(|| item.get("query").and_then(Value::as_str))
                        .filter(|query| !query.trim().is_empty()),
                );
            }
            let first = queries.first()?;
            let remaining = queries.len() - 1;
            let suffix = if remaining == 0 {
                String::new()
            } else {
                format!(
                    " · +{remaining} {}",
                    if remaining == 1 { "query" } else { "queries" }
                )
            };
            compact_tool_title(&format!("Web Search · {first}{suffix}"))
        }
        "openPage" => {
            let url = url.or_else(|| {
                item.get("query")
                    .and_then(Value::as_str)
                    .filter(|url| !url.trim().is_empty())
                    .map(|url| {
                        url.strip_prefix("https://")
                            .or_else(|| url.strip_prefix("http://"))
                            .unwrap_or(url)
                    })
            })?;
            compact_tool_title(&format!("Open web · {url}"))
        }
        "findInPage" => {
            let pattern = action.get("pattern")?.as_str()?;
            if pattern.trim().is_empty() {
                return None;
            }
            let pattern = serde_json::to_string(pattern).ok()?;
            let suffix = url.map_or(String::new(), |url| format!(" · {url}"));
            compact_tool_title(&format!("Find in page · {pattern}{suffix}"))
        }
        _ => None,
    }
}

fn mcp_tool_output(item: &Value) -> Option<String> {
    item.get("type")
        .and_then(Value::as_str)
        .is_some_and(|item_type| item_type.eq_ignore_ascii_case("mcpToolCall"))
        .then_some(())?;
    if item
        .pointer("/error/message")
        .is_some_and(|message| !message.is_null())
    {
        return Some("The MCP tool call failed.".into());
    }
    let result = item.get("result")?;
    let text_list = result
        .get("content")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(|content| content.get("text").and_then(Value::as_str))
        .map(format_mcp_text)
        .collect::<Vec<_>>();
    if !text_list.is_empty() {
        return Some(text_list.join("\n"));
    }
    result
        .get("structuredContent")
        .and_then(|content| serde_json::to_string_pretty(content).ok())
}

fn format_mcp_text(text: &str) -> String {
    let text = strip_ansi_escapes::strip_str(text);
    serde_json::from_str::<Value>(&text)
        .and_then(|mut value| {
            value.sort_all_objects();
            serde_json::to_string_pretty(&value)
        })
        .unwrap_or(text)
}

fn compact_tool_title(title: &str) -> Option<String> {
    let compact = title.split_whitespace().collect::<Vec<_>>().join(" ");
    (!compact.is_empty()).then_some(compact)
}

pub fn first_text(value: &Value) -> Option<String> {
    for pointer in [
        "/delta",
        "/text",
        "/content/text",
        "/update/content/text",
        "/item/content/text",
        "/msg/message",
    ] {
        if let Some(text) = value.pointer(pointer).and_then(Value::as_str) {
            return Some(text.to_owned());
        }
    }
    match value {
        Value::Array(item) => item.iter().find_map(first_text),
        Value::Object(map) => map.values().find_map(first_text),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    /// Accumulated billing and a small reply must not substitute for retained context usage.
    #[test]
    fn context_usage_uses_last_request_including_input_not_cumulative_total() {
        let usage = super::context_usage(&serde_json::json!({"tokenUsage":{
            "last":{"inputTokens":149900,"outputTokens":100,"totalTokens":150000},
            "total":{"totalTokens":9000000},"modelContextWindow":258000
        }}))
        .unwrap();
        assert_eq!(usage.used, 150000);
        assert_eq!(usage.size, 258000);
        let small = super::context_usage(&serde_json::json!({"tokenUsage":{
            "last":{"totalTokens":20000},"total":{"totalTokens":9000000},
            "modelContextWindow":258000
        }}))
        .unwrap();
        assert_eq!(small.used, 20000);
    }

    /// Match the installed app-server schema rather than an MCP response envelope.
    #[test]
    fn dynamic_tool_wire_contract_uses_tool_and_content_items() {
        let call = super::dynamic_tool_call(&serde_json::json!({
            "tool":"repository_status", "arguments":{"offset":0},
            "callId":"call", "threadId":"thread", "turnId":"turn"
        }))
        .unwrap();
        assert_eq!(call.name, "repository_status");
        assert_eq!(call.id.as_str(), "call");
        assert!(
            super::dynamic_tool_call(&serde_json::json!({
                "tool":"repository_status", "arguments":{}, "callId":""
            }))
            .is_err()
        );
        assert_eq!(call.arguments, serde_json::json!({"offset":0}));
        assert!(
            super::dynamic_tool_call(&serde_json::json!({
                "name":"repository_status", "arguments":{}
            }))
            .is_err()
        );
        for success in [true, false] {
            assert_eq!(
                super::dynamic_tool_response("result".into(), success),
                serde_json::json!({"contentItems":[{"type":"inputText","text":"result"}],"success":success})
            );
        }
    }

    use super::*;

    #[test]
    fn prettifies_codex_mcp_json_results() {
        let output = mcp_tool_output(&json!({
            "type": "mcpToolCall",
            "result": {
                "content": [{
                    "type": "text",
                    "text": "{\"login\":\"mmgeorge\",\"details\":{\"followers\":22}}"
                }]
            }
        }));
        assert_eq!(
            output.as_deref(),
            Some("{\n  \"details\": {\n    \"followers\": 22\n  },\n  \"login\": \"mmgeorge\"\n}")
        );
    }

    #[test]
    fn move_report_preserves_destination_outside_patch_content() {
        let patch = "@@ -1 +1 @@\n-before\n+Moved to: source text\n";
        let changes = normalize_file_change_set(
            &json!({"changes":[{
                "path":"D:/workspace/src/original.rs", "kind":"update",
                "diff":format!("{patch}\nMoved to: D:/workspace/src/moved.rs")
            }]}),
            &Value::Null,
            "D:/workspace",
        );
        assert_eq!(changes.file[0].kind, ProviderChangeKind::Move);
        assert_eq!(changes.file[0].move_path.as_deref(), Some("src/moved.rs"));
        assert_eq!(changes.file[0].diff, patch);
        let content = "ordinary content\nMoved to: source text";
        for kind in ["add", "delete"] {
            let changes = normalize_file_change_set(
                &json!({"changes":[{
                    "path":"src/plain.txt", "kind":kind, "diff":content
                }]}),
                &Value::Null,
                "D:/workspace",
            );
            assert_eq!(changes.file[0].diff, content);
            assert!(changes.file[0].move_path.is_none());
        }
    }

    #[test]
    fn argv_command_titles_hide_launchers_without_losing_script_boundaries() {
        for (executable, marker) in [
            ("C:\\Program Files\\PowerShell\\7\\pwsh.exe", "-Command"),
            ("/bin/bash", "-lc"),
            ("/bin/zsh", "-ic"),
            ("/opt/homebrew/bin/nu", "--commands"),
        ] {
            assert_eq!(
                tool_title(&json!([executable, marker, "probe 'a path'; probe λ"])).as_deref(),
                Some("probe 'a path'; probe λ")
            );
        }
    }

    #[test]
    fn compacts_multiline_command_titles_for_one_line_previews() {
        assert_eq!(
            compact_tool_title("pwsh -Command\n  Get-Content README.md\r\n").as_deref(),
            Some("pwsh -Command Get-Content README.md")
        );
    }

    #[test]
    fn builtin_titles_handle_sleep_and_web_fallbacks_without_aliasing_mcp() {
        for (item, expected) in [
            (
                json!({"type":"sleep", "durationMs":20000}),
                Some("Wait · 20s"),
            ),
            (
                json!({"type":"sleep", "durationMs":1500}),
                Some("Wait · 1.5s"),
            ),
            (json!({"type":"sleep"}), None),
            (
                json!({"type":"webSearch", "query":"", "action":null, "results":null}),
                Some("Web Search"),
            ),
            (json!({"type":"webSearch"}), Some("Web Search")),
            (
                json!({"type":"webSearch", "query":"documentation", "action":null}),
                Some("Web Search · documentation"),
            ),
            (
                json!({"type":"webSearch", "action":{"type":"search","query":"documentation"}}),
                Some("Web Search · documentation"),
            ),
            (
                json!({"type":"webSearch", "query":"documentation", "action":{"type":"search","query":null}}),
                Some("Web Search · documentation"),
            ),
            (
                json!({"type":"webSearch", "action":{"type":"search","queries":["one","two","three"]}}),
                Some("Web Search · one · +2 queries"),
            ),
            (
                json!({"type":"webSearch", "query":"https://example.com", "action":{"type":"openPage","url":null}}),
                Some("Open web · example.com"),
            ),
            (json!({"type":"webSearch", "action":{"type":"other"}}), None),
            (
                json!({"type":"webSearch", "action":{"type":"search","queries":[]}}),
                None,
            ),
            (
                json!({"type":"mcpToolCall", "server":"webSearch", "tool":"search","arguments":{"query":"documentation"}}),
                None,
            ),
        ] {
            assert_eq!(builtin_tool_title(&item).as_deref(), expected, "{item}");
        }
    }

    /// Collaboration state comes only from the pinned item shapes, never from payload substrings.
    #[test]
    fn collaboration_lifecycle_uses_exact_item_schemas() {
        use super::{CollabAgentTool, NativeAgentStatus, normalize_agent_lifecycle};
        use forge_provider_api::agent::{AgentActiveState, AgentOutcome};
        use serde_json::json;
        let wait = normalize_agent_lifecycle(
            "item/completed",
            &json!({"threadId":"parent","item":{
                "type":"collabAgentToolCall","id":"wait","tool":"wait","status":"completed",
                "senderThreadId":"parent","receiverThreadIds":["running","errored","gone"],
                "prompt":null,"agentsStates":{
                    "running":{"status":"running","message":null},
                    "errored":{"status":"errored","message":"boom"},
                    "gone":{"status":"notFound","message":null}
                }
            }}),
        )
        .unwrap();
        assert_eq!(wait.len(), 3);
        assert!(
            wait.iter()
                .all(|lifecycle| lifecycle.tool == Some(CollabAgentTool::Wait)
                    && lifecycle.parent_thread_id.as_deref() == Some("parent")
                    && !lifecycle.starts_child)
        );
        assert!(matches!(
            wait[0].status,
            NativeAgentStatus::Active(AgentActiveState::Running)
        ));
        assert!(matches!(
            wait[1].status,
            NativeAgentStatus::Finished(AgentOutcome::Failed(_))
        ));
        assert!(matches!(
            wait[2].status,
            NativeAgentStatus::Finished(AgentOutcome::Closed)
        ));

        let interacted = normalize_agent_lifecycle(
            "item/completed",
            &json!({"threadId":"parent","item":{
                "type":"subAgentActivity","id":"activity","kind":"interacted",
                "agentThreadId":"child","agentPath":"/root/child"
            }}),
        )
        .unwrap();
        assert!(interacted.is_empty());

        let unrelated = normalize_agent_lifecycle(
            "thread/subagentStatus",
            &json!({"item":{"type":"collabAgentToolCall","tool":"spawnAgent"},
                "note":"mentions subAgentActivity"}),
        )
        .unwrap();
        assert!(unrelated.is_empty());

        for item in [
            json!({"type":"collabAgentToolCall","id":"x","tool":"futureTool","status":"completed",
                "senderThreadId":"parent","receiverThreadIds":[]}),
            json!({"type":"collabAgentToolCall","id":"x","tool":"wait","status":"completed",
                "senderThreadId":"parent","receiverThreadIds":["child"],
                "agentsStates":{"child":{"status":"futureStatus"}}}),
            json!({"type":"subAgentActivity","id":"x","kind":"started"}),
        ] {
            assert!(
                normalize_agent_lifecycle("item/started", &json!({"item":item})).is_err(),
                "{item}"
            );
        }
    }
}
