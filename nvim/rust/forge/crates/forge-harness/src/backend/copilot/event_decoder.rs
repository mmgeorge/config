use crate::agent::{AgentExecutionState, AgentLifecycleEvent};
use crate::backend::{
    BackendEvent, BackendEventSink, BackendOutput, ChecklistStatus, ProviderChangeKind,
    ProviderChangeSet, ProviderChecklistEntry, ProviderChecklistUpdate, ProviderFileChange,
    ToolActivity, ToolActivityKind,
};
use crate::session::ContextUsage;
use github_copilot_sdk::SessionEvent;
use serde_json::{Value, json};

/// Normalizes stable Copilot SDK session events into the Harness interaction protocol.
#[derive(Default)]
pub struct CopilotEventDecoder {
    pub(super) workspace: String,
}

impl CopilotEventDecoder {
    /// Decode one SDK event and publish its normalized Harness update.
    pub async fn decode(
        &self,
        provider_event: &SessionEvent,
        output: &mut BackendOutput,
        event_sink: Option<&BackendEventSink>,
    ) {
        if provider_event.agent_id.is_none()
            && !provider_event.event_type.starts_with("session.compaction_")
            && let Some(model) = string_field(&provider_event.data, &["model"])
            && !model.is_empty()
            && output.runtime.model.as_ref() != Some(&model)
        {
            output.runtime.model = Some(model);
            publish(output.runtime.resolved_event(), output, event_sink).await;
        }
        match provider_event.event_type.as_str() {
            "assistant.idle" | "session.idle" if provider_event.agent_id.is_none() => {
                let unfinished = output
                    .event
                    .iter()
                    .enumerate()
                    .filter_map(|(index, event)| {
                        if !matches!(
                            event.turn_boundary,
                            Some(crate::backend::TurnBoundary::Started)
                        ) {
                            return None;
                        }
                        let address = event.address.as_ref()?;
                        if Some(&address.thread_id) != output.backend_session_id.as_ref()
                            || output.event[index + 1..].iter().any(|later| {
                                later.address.as_ref() == Some(address)
                                    && matches!(
                                        later.turn_boundary,
                                        Some(crate::backend::TurnBoundary::Finished { .. })
                                    )
                            })
                        {
                            return None;
                        }
                        Some(address.clone())
                    })
                    .collect::<Vec<_>>();
                for address in unfinished {
                    publish(
                        BackendEvent {
                            received_at_ms: None,
                            address: Some(address),
                            turn_boundary: Some(crate::backend::TurnBoundary::Finished {
                                outcome: crate::turn::TurnOutcome::Completed,
                            }),
                            kind: "turn_completed".into(),
                            text: None,
                            data: serde_json::to_value(provider_event).unwrap_or(Value::Null),
                            activity: None,
                            summary: None,
                            checklist_update: None,
                        },
                        output,
                        event_sink,
                    )
                    .await;
                }
            }
            "session.compaction_start" | "session.compaction_complete" => {
                let thread_id = provider_event
                    .agent_id
                    .clone()
                    .or_else(|| output.backend_session_id.clone());
                let Some(thread_id) = thread_id else {
                    return;
                };
                let complete = provider_event.event_type == "session.compaction_complete";
                let id = if complete {
                    output
                        .event
                        .iter()
                        .rev()
                        .find(|event| {
                            event.kind == "compaction"
                                && event.data.get("provider_event_id").and_then(Value::as_str)
                                    == Some(provider_event.id.as_str())
                        })
                        .and_then(|event| event.compaction().ok().flatten())
                        .map(|compaction| compaction.id)
                        .or_else(|| {
                            output
                                .event
                                .iter()
                                .rev()
                                .filter_map(|event| event.compaction().ok().flatten())
                                .find(|compaction| {
                                    compaction.thread_id == thread_id
                                        && compaction.state
                                            == crate::backend::CompactionState::Running
                                })
                                .map(|compaction| compaction.id)
                        })
                        .unwrap_or_else(|| provider_event.id.clone())
                } else {
                    provider_event.id.clone()
                };
                let result = serde_json::from_value(provider_event.data.clone());
                let mut result: crate::backend::CompactionResult = match result {
                    Ok(result) => result,
                    Err(error) => {
                        self.publish_text(
                            "error",
                            Some(format!("Invalid Copilot compaction result: {error}")),
                            provider_event,
                            output,
                            event_sink,
                        )
                        .await;
                        return;
                    }
                };
                if !complete {
                    result.before_context = Some(crate::backend::CompactionContext {
                        conversation_tokens: result.conversation_tokens.take(),
                        system_tokens: result.system_tokens.take(),
                        tool_definitions_tokens: result.tool_definitions_tokens.take(),
                        ..Default::default()
                    });
                    if let Some(context) = &mut result.before_context {
                        context.current_tokens = context
                            .conversation_tokens
                            .zip(context.system_tokens)
                            .zip(context.tool_definitions_tokens)
                            .and_then(|((conversation, system), tools)| {
                                conversation.checked_add(system)?.checked_add(tools)
                            });
                        result.context_tokens_before = context.current_tokens;
                    }
                }
                let compaction = crate::backend::Compaction {
                    id,
                    thread_id,
                    state: if !complete {
                        crate::backend::CompactionState::Running
                    } else if provider_event.data.get("success").and_then(Value::as_bool)
                        == Some(true)
                    {
                        crate::backend::CompactionState::Completed
                    } else {
                        crate::backend::CompactionState::Failed
                    },
                    manual: provider_event.data.get("customInstructions").is_some(),
                    started_at_ms: None,
                    completed_at_ms: None,
                    result,
                };
                let mut event = BackendEvent::from_compaction(compaction, None);
                event.data["provider_event_id"] = Value::String(provider_event.id.clone());
                publish(event, output, event_sink).await;
            }
            "assistant.turn_start" | "assistant.turn_end" => {
                publish(
                    BackendEvent {
                        received_at_ms: None,
                        address: None,
                        turn_boundary: Some(
                            if provider_event.event_type == "assistant.turn_start" {
                                crate::backend::TurnBoundary::Started
                            } else {
                                crate::backend::TurnBoundary::Finished {
                                    outcome: crate::turn::TurnOutcome::Completed,
                                }
                            },
                        ),
                        kind: if provider_event.event_type == "assistant.turn_start" {
                            "turn_started"
                        } else {
                            "turn_completed"
                        }
                        .into(),
                        text: None,
                        data: serde_json::to_value(provider_event).unwrap_or(Value::Null),
                        activity: None,
                        summary: None,
                        checklist_update: None,
                    },
                    output,
                    event_sink,
                )
                .await;
            }
            "assistant.reasoning_delta" => {
                self.publish_text(
                    "reasoning_summary",
                    string_field(&provider_event.data, &["deltaContent", "delta"]),
                    provider_event,
                    output,
                    event_sink,
                )
                .await;
            }
            "assistant.message_delta" => {
                self.publish_text(
                    "assistant_message",
                    string_field(&provider_event.data, &["deltaContent", "delta"]),
                    provider_event,
                    output,
                    event_sink,
                )
                .await;
            }
            "assistant.reasoning" => {
                self.publish_text(
                    "reasoning_summary",
                    string_field(&provider_event.data, &["content", "text"]),
                    provider_event,
                    output,
                    event_sink,
                )
                .await;
            }
            "assistant.message" => {
                self.publish_text(
                    "assistant_message",
                    string_field(&provider_event.data, &["content", "text"]),
                    provider_event,
                    output,
                    event_sink,
                )
                .await;
            }
            "tool.execution_start"
            | "tool.execution_partial_result"
            | "tool.execution_progress"
            | "tool.execution_complete" => {
                output.evidence.tool_called = true;
                let mut activity = tool_activity(provider_event, output);
                if let Some(activity) = activity.as_mut()
                    && activity.kind == ToolActivityKind::FileChange
                    && activity.change.is_empty()
                    && provider_event.event_type == "tool.execution_complete"
                {
                    match super::file_change::from_output(
                        activity.output.as_deref().unwrap_or_default(),
                        &self.workspace,
                    ) {
                        Ok(change) => activity.change = change,
                        Err(error) => {
                            self.publish_text(
                                "error",
                                Some(format!("Invalid Copilot file changes: {error:#}")),
                                provider_event,
                                output,
                                event_sink,
                            )
                            .await;
                        }
                    }
                }
                if activity.as_ref().is_some_and(successful_file_change) {
                    output.evidence.workspace_changed = true;
                }
                if let Some(activity) = activity {
                    publish(
                        BackendEvent {
                            received_at_ms: None,
                            address: None,
                            turn_boundary: None,
                            kind: "tool".into(),
                            text: None,
                            data: serde_json::to_value(provider_event).unwrap_or(Value::Null),
                            activity: Some(activity),
                            summary: None,
                            checklist_update: None,
                        },
                        output,
                        event_sink,
                    )
                    .await;
                }
            }
            "session.usage_info" => {
                let used = unsigned_field(&provider_event.data, &["currentTokens", "used"]);
                let size = unsigned_field(&provider_event.data, &["tokenLimit", "size"]);
                if let (Some(used), Some(size)) = (used, size)
                    && let Some(context_usage) = ContextUsage::reported(used, size)
                {
                    output.metrics.context_usage = Some(context_usage.clone());
                    publish(
                        BackendEvent {
                            received_at_ms: None,
                            address: None,
                            turn_boundary: None,
                            kind: "context_usage".into(),
                            text: None,
                            data: serde_json::to_value(context_usage).unwrap_or(Value::Null),
                            activity: None,
                            summary: None,
                            checklist_update: None,
                        },
                        output,
                        event_sink,
                    )
                    .await;
                }
            }
            "assistant.usage" => {
                use crate::backend::usage::{TokenUsage, UsageUpdate};
                let update = UsageUpdate {
                    id: string_field(&provider_event.data, &["apiCallId"])
                        .unwrap_or_else(|| provider_event.id.clone()),
                    usage: TokenUsage {
                        input: unsigned_field(&provider_event.data, &["inputTokens"]),
                        cached_input: unsigned_field(&provider_event.data, &["cacheReadTokens"]),
                        reasoning: unsigned_field(&provider_event.data, &["reasoningTokens"]),
                        output: unsigned_field(&provider_event.data, &["outputTokens"]),
                    },
                    cumulative: None,
                    cumulative_total: None,
                };
                let mut event = BackendEvent {
                    received_at_ms: None,
                    address: None,
                    turn_boundary: None,
                    kind: "usage".into(),
                    text: None,
                    data: serde_json::to_value(provider_event)
                        .expect("provider event is JSON serializable"),
                    activity: None,
                    summary: None,
                    checklist_update: None,
                };
                address_event(&mut event, output);
                event.data =
                    serde_json::to_value(update).expect("usage update is JSON serializable");
                publish(event, output, event_sink).await;
            }
            "subagent.started" | "subagent.completed" | "subagent.failed" => {
                publish(
                    BackendEvent {
                        received_at_ms: None,
                        address: None,
                        turn_boundary: None,
                        kind: "agent_lifecycle".into(),
                        text: None,
                        data: serde_json::to_value(agent_lifecycle(provider_event))
                            .unwrap_or(Value::Null),
                        activity: None,
                        summary: None,
                        checklist_update: None,
                    },
                    output,
                    event_sink,
                )
                .await;
            }
            "session.error" if !provider_event.is_transient_error() => {
                if super::retry::transient_code(&provider_event.data).is_some()
                    && crate::backend::retry::replay_safe(output)
                {
                    return;
                }
                self.publish_text(
                    "error",
                    Some(session_error(
                        provider_event,
                        output.backend_session_id.as_deref(),
                    )),
                    provider_event,
                    output,
                    event_sink,
                )
                .await;
            }
            _ => {}
        }
    }

    /// Publish one complete Copilot todo snapshot after its signal-only change event.
    pub async fn publish_task_snapshot(
        &self,
        data: Value,
        scope_id: String,
        output: &mut BackendOutput,
        event_sink: Option<&BackendEventSink>,
    ) {
        let Some(checklist_update) = checklist_update(&data, scope_id) else {
            return;
        };
        publish(
            BackendEvent {
                received_at_ms: None,
                address: None,
                turn_boundary: None,
                kind: "plan".into(),
                text: None,
                data,
                activity: None,
                summary: None,
                checklist_update: Some(checklist_update),
            },
            output,
            event_sink,
        )
        .await;
    }

    async fn publish_text(
        &self,
        kind: &str,
        text: Option<String>,
        provider_event: &SessionEvent,
        output: &mut BackendOutput,
        event_sink: Option<&BackendEventSink>,
    ) {
        let Some(text) = text.filter(|text| !text.is_empty()) else {
            return;
        };
        let mut data = serde_json::to_value(provider_event).unwrap_or(Value::Null);
        if kind == "assistant_message" {
            let final_answer = provider_event.event_type == "assistant.message"
                && provider_event
                    .data
                    .get("toolRequests")
                    .and_then(Value::as_array)
                    .is_none_or(|requests| requests.is_empty());
            data["phase"] = json!(if final_answer { "final" } else { "commentary" });
        }
        if let Some(id) = provider_event
            .data
            .get("reasoningId")
            .and_then(Value::as_str)
        {
            data["provider_message_id"] = json!(id);
        }
        if matches!(
            provider_event.event_type.as_str(),
            "assistant.message" | "assistant.reasoning"
        ) {
            data["message_update"] = json!("snapshot");
        }
        publish(
            BackendEvent {
                received_at_ms: None,
                address: None,
                turn_boundary: None,
                kind: kind.into(),
                text: Some(text),
                data,
                activity: None,
                summary: None,
                checklist_update: None,
            },
            output,
            event_sink,
        )
        .await;
    }
}

fn address_event(event: &mut BackendEvent, output: &BackendOutput) {
    if event.kind == "runtime_resolved" {
        return;
    }
    if event.address.is_none() {
        let thread_id = event
            .data
            .get("agentId")
            .or_else(|| event.data.get("agent_id"))
            .and_then(Value::as_str)
            .map(str::to_owned)
            .or_else(|| output.backend_session_id.clone());
        if let Some(thread_id) = thread_id {
            let native_turn_id = event.data.pointer("/data/turnId").and_then(Value::as_str);
            let turn_id = native_turn_id
                .map(|native| {
                    if let Some(interaction) = event
                        .data
                        .pointer("/data/interactionId")
                        .and_then(Value::as_str)
                    {
                        return format!("{interaction}:{native}");
                    }
                    output
                        .event
                        .iter()
                        .rev()
                        .find(|previous| {
                            previous.kind == "turn_started"
                                && previous
                                    .data
                                    .pointer("/data/turnId")
                                    .and_then(Value::as_str)
                                    == Some(native)
                                && previous
                                    .address
                                    .as_ref()
                                    .is_some_and(|address| address.thread_id == thread_id)
                        })
                        .and_then(|previous| {
                            previous
                                .address
                                .as_ref()
                                .map(|address| address.turn_id.clone())
                        })
                        .unwrap_or_else(|| native.to_owned())
                })
                .or_else(|| {
                    output.event.iter().rev().find_map(|previous| {
                        previous
                            .address
                            .as_ref()
                            .filter(|address| address.thread_id == thread_id)
                            .map(|address| address.turn_id.clone())
                    })
                });
            event.address = turn_id.map(|turn_id| crate::backend::ProviderAddress {
                owner: None,
                thread_id,
                turn_id,
            });
        }
    }
}

pub(super) fn session_error(event: &SessionEvent, session_id: Option<&str>) -> String {
    let detail = string_field(&event.data, &["message", "error"])
        .filter(|detail| !detail.trim().is_empty())
        .unwrap_or_else(|| "provider omitted both message and error details".into());
    let code =
        string_field(&event.data, &["errorType", "code"]).unwrap_or_else(|| "unreported".into());
    format!(
        "Copilot session {} reported {} (event {}, code {code}): {detail}",
        session_id.unwrap_or("unattached"),
        event.event_type,
        event.id,
    )
}

async fn publish(
    mut event: BackendEvent,
    output: &mut BackendOutput,
    event_sink: Option<&BackendEventSink>,
) {
    address_event(&mut event, output);
    if let Some(compaction) = event.compaction().ok().flatten()
        && let Some(previous) = output.event.iter_mut().find(|previous| {
            previous
                .compaction()
                .ok()
                .flatten()
                .is_some_and(|existing| {
                    existing.id == compaction.id && existing.thread_id == compaction.thread_id
                })
        })
    {
        if let Some(event_sink) = event_sink {
            let _ = event_sink.send_wait(event.clone()).await;
        }
        if previous
            .compaction()
            .ok()
            .flatten()
            .is_some_and(|existing| existing.state == crate::backend::CompactionState::Running)
        {
            if let Some(existing) = previous.compaction().ok().flatten() {
                let provider_event_id = event.data.get("provider_event_id").cloned();
                let mut retained = compaction;
                retained.result.context_tokens_before = existing
                    .result
                    .context_tokens_before
                    .or(retained.result.context_tokens_before);
                retained.result.before_context = existing.result.before_context;
                retained.result.model = retained.result.model.or(existing.result.model);
                event.data = serde_json::to_value(retained).unwrap_or(event.data.clone());
                if let Some(provider_event_id) = provider_event_id {
                    event.data["provider_event_id"] = provider_event_id;
                }
            }
            *previous = event;
        }
        return;
    }
    if let Some(activity) = event.activity.as_ref()
        && let Some(previous) = output.event.iter_mut().find(|previous| {
            previous.activity.as_ref().is_some_and(|existing| {
                existing.id == activity.id
                    && previous.address.as_ref().map(|address| &address.thread_id)
                        == event.address.as_ref().map(|address| &address.thread_id)
            })
        })
    {
        merge_activity(previous, event);
        if let Some(event_sink) = event_sink {
            let _ = event_sink.send_wait(previous.clone()).await;
        }
        return;
    }
    if let Some(event_sink) = event_sink {
        let _ = event_sink.send_wait(event.clone()).await;
    }
    if matches!(
        event.kind.as_str(),
        "reasoning_summary" | "assistant_message"
    ) && let Some(previous) = output.event.last_mut()
        && previous.kind == event.kind
        && previous.address == event.address
        && previous.message_id() == event.message_id()
        && previous.activity.is_none()
    {
        if event.message_snapshot() {
            previous.text = event.text;
        } else {
            previous
                .text
                .get_or_insert_with(String::new)
                .push_str(event.text.as_deref().unwrap_or_default());
        }
        previous.data = event.data;
        return;
    }
    output.event.push(event);
}

fn merge_activity(previous: &mut BackendEvent, incoming: BackendEvent) {
    let Some(existing) = previous.activity.as_mut() else {
        return;
    };
    let Some(update) = incoming.activity else {
        return;
    };
    // Background output can arrive after the tool has returned its final result.
    if matches!(
        existing.status.as_deref(),
        Some("completed" | "failed" | "denied")
    ) && update.status.as_deref() == Some("in_progress")
    {
        return;
    }
    if update.title != "Copilot tool"
        && (incoming
            .data
            .pointer("/data/arguments")
            .is_some_and(|arguments| !arguments.is_null())
            || existing.title == "Copilot tool")
    {
        existing.title = update.title;
    }
    existing.status = update.status;
    if update.kind == ToolActivityKind::FileChange {
        existing.kind = ToolActivityKind::FileChange;
    }
    if !update.change.is_empty() {
        existing.change = update.change;
    }
    if let Some(text) = update.output {
        if update.output_delta {
            existing
                .output
                .get_or_insert_with(String::new)
                .push_str(&text);
        } else {
            existing.output = Some(text);
        }
    }
    existing.output_delta = false;
    previous.data = incoming.data;
}

fn tool_activity(provider_event: &SessionEvent, output: &BackendOutput) -> Option<ToolActivity> {
    let data = &provider_event.data;
    let id = string_field(data, &["toolCallId", "id"])?;
    let previous = output.event.iter().rev().find_map(|event| {
        let agent_id = event
            .data
            .get("agentId")
            .or_else(|| event.data.get("agent_id"))
            .and_then(Value::as_str);
        event
            .activity
            .as_ref()
            .filter(|activity| activity.id == id && agent_id == provider_event.agent_id.as_deref())
    });
    let file_tool = previous.is_some_and(|activity| activity.kind == ToolActivityKind::FileChange)
        || match data.get("toolName").and_then(Value::as_str) {
            Some("apply_patch" | "edit" | "create") => true,
            Some("str_replace_editor") => matches!(
                data.pointer("/arguments/command").and_then(Value::as_str),
                Some("str_replace" | "create" | "insert" | "undo_edit"),
            ),
            _ => false,
        };
    let title = string_field(data, &["toolName", "name", "toolDescription"])
        .unwrap_or_else(|| "Copilot tool".into());
    let title = data
        .get("arguments")
        .filter(|arguments| !arguments.is_null())
        .and_then(|arguments| ToolActivity::call_title(&title, arguments))
        .unwrap_or(title);
    let (status, output, output_delta) = match provider_event.event_type.as_str() {
        "tool.execution_start" => (Some("in_progress".into()), None, false),
        "tool.execution_partial_result" => (
            Some("in_progress".into()),
            string_field(data, &["partialOutput"]),
            true,
        ),
        "tool.execution_progress" => (
            Some("in_progress".into()),
            string_field(data, &["progressMessage"]),
            false,
        ),
        "tool.execution_complete" => {
            let success = data
                .get("success")
                .and_then(Value::as_bool)
                .unwrap_or(false);
            let output = data
                .pointer("/result/detailedContent")
                .or_else(|| data.pointer("/result/content"))
                .or_else(|| data.pointer("/error/message"))
                .and_then(Value::as_str)
                .map(strip_ansi_escapes::strip_str);
            (
                Some(
                    if success {
                        "completed"
                    } else if super::rejection::is_denied(data) {
                        "denied"
                    } else {
                        "failed"
                    }
                    .into(),
                ),
                output,
                false,
            )
        }
        _ => return None,
    };
    let mut change = provider_change_set(data);
    if change.is_empty()
        && let Some(previous) = previous
    {
        change = previous.change.clone();
    }
    Some(ToolActivity {
        id,
        kind: if file_tool || !change.is_empty() {
            ToolActivityKind::FileChange
        } else {
            ToolActivityKind::ToolCall
        },
        title,
        output,
        status,
        change,
        output_delta,
    })
}

fn provider_change_set(data: &Value) -> ProviderChangeSet {
    let change_list = data
        .get("changes")
        .or_else(|| data.pointer("/result/changes"))
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(|change| {
            let path = string_field(change, &["path", "filePath"])?;
            let kind = match string_field(change, &["kind"])
                .unwrap_or_else(|| "update".into())
                .as_str()
            {
                "add" | "create" | "new" => ProviderChangeKind::Add,
                "delete" | "remove" => ProviderChangeKind::Delete,
                "move" | "rename" => ProviderChangeKind::Move,
                _ => ProviderChangeKind::Update,
            };
            Some(ProviderFileChange {
                path,
                move_path: string_field(change, &["movePath", "newPath"]),
                kind,
                diff: string_field(change, &["diff", "patch"]).unwrap_or_default(),
            })
        })
        .collect();
    ProviderChangeSet { file: change_list }
}

fn successful_file_change(activity: &ToolActivity) -> bool {
    !activity.change.is_empty()
        && activity.status.as_deref().is_some_and(|status| {
            matches!(
                status.to_ascii_lowercase().as_str(),
                "completed" | "success"
            )
        })
}

fn checklist_update(data: &Value, scope_id: String) -> Option<ProviderChecklistUpdate> {
    let entry = data
        .get("todos")
        .or_else(|| data.get("items"))
        .or_else(|| data.get("plan"))
        .or_else(|| data.get("rows"))
        .and_then(Value::as_array)?;
    let entry_list = entry
        .iter()
        .enumerate()
        .filter_map(|(provider_ordinal, value)| {
            let content = string_field(value, &["title", "content", "description"])?;
            let status = match string_field(value, &["status"])
                .unwrap_or_else(|| "pending".into())
                .to_ascii_lowercase()
                .as_str()
            {
                "in_progress" | "inprogress" | "active" => ChecklistStatus::InProgress,
                "completed" | "complete" | "done" => ChecklistStatus::Completed,
                _ => ChecklistStatus::Pending,
            };
            Some(ProviderChecklistEntry {
                provider_id: string_field(value, &["id"]),
                content,
                priority: string_field(value, &["priority"]),
                status,
                provider_ordinal,
            })
        })
        .collect::<Vec<_>>();
    Some(ProviderChecklistUpdate {
        scope_id,
        name: string_field(data, &["name", "title"]),
        complete: !entry_list.is_empty()
            && entry_list
                .iter()
                .all(|entry| entry.status == ChecklistStatus::Completed),
        replace_entries: true,
        entry_list,
    })
}

fn agent_lifecycle(provider_event: &SessionEvent) -> AgentLifecycleEvent {
    let status = match provider_event.event_type.as_str() {
        "subagent.started" => AgentExecutionState::Running,
        "subagent.completed" => AgentExecutionState::Completed,
        _ => AgentExecutionState::Failed,
    };
    AgentLifecycleEvent {
        operation: provider_event.event_type.clone(),
        starts_child: provider_event.event_type == "subagent.started",
        parent_thread_id: None,
        provider_thread_id: provider_event
            .agent_id
            .clone()
            .or_else(|| string_field(&provider_event.data, &["agentId", "toolCallId"])),
        turn_id: string_field(&provider_event.data, &["turnId"]),
        definition: string_field(&provider_event.data, &["agentName"]),
        nickname: string_field(&provider_event.data, &["agentDisplayName", "displayName"]),
        task: string_field(
            &provider_event.data,
            &["task", "prompt", "agentDescription"],
        ),
        status,
    }
}

fn string_field(value: &Value, field_list: &[&str]) -> Option<String> {
    field_list
        .iter()
        .find_map(|field| value.get(*field).and_then(Value::as_str))
        .map(str::to_owned)
}

fn unsigned_field(value: &Value, field_list: &[&str]) -> Option<u64> {
    field_list
        .iter()
        .find_map(|field| value.get(*field).and_then(Value::as_u64))
}

#[cfg(test)]
mod test {
    #[tokio::test]
    async fn repeated_native_turn_numbers_keep_distinct_interaction_addresses() {
        let mut address = Vec::new();
        for interaction in ["first-prompt", "second-prompt"] {
            let mut output = BackendOutput {
                backend_session_id: Some("same-session".into()),
                ..Default::default()
            };
            CopilotEventDecoder::default()
                .decode(
                    &event(
                        "assistant.turn_start",
                        serde_json::json!({
                            "turnId":"0", "interactionId":interaction
                        }),
                    ),
                    &mut output,
                    None,
                )
                .await;
            CopilotEventDecoder::default()
                .decode(
                    &event(
                        "assistant.message_delta",
                        serde_json::json!({
                            "messageId":interaction, "deltaContent":"visible answer"
                        }),
                    ),
                    &mut output,
                    None,
                )
                .await;
            CopilotEventDecoder::default()
                .decode(
                    &event("assistant.turn_end", serde_json::json!({"turnId":"0"})),
                    &mut output,
                    None,
                )
                .await;
            let started = output
                .event
                .iter()
                .find(|event| event.kind == "turn_started")
                .unwrap()
                .address
                .clone()
                .unwrap();
            assert!(
                output
                    .event
                    .iter()
                    .all(|event| event.address.as_ref() == Some(&started))
            );
            address.push(started);
        }
        assert_eq!(address[0].thread_id, address[1].thread_id);
        assert_ne!(address[0].turn_id, address[1].turn_id);
    }

    use super::*;
    use serde_json::json;

    #[tokio::test]
    async fn session_errors_retain_identity_code_and_missing_detail_diagnostics() {
        for data in [
            json!({"message":"quota exhausted","errorType":"quota"}),
            json!({"error":"connection reset","code":"transport"}),
            json!({"message":" "}),
        ] {
            let provider_event = event("session.error", data.clone());
            let mut output = BackendOutput {
                backend_session_id: Some("native-session".into()),
                ..Default::default()
            };
            CopilotEventDecoder::default()
                .decode(&provider_event, &mut output, None)
                .await;
            let expected = session_error(&provider_event, Some("native-session"));
            assert!(expected.contains("native-session"), "{expected}");
            assert!(expected.contains(&provider_event.id), "{expected}");
            let detail = if data.get("errorType").is_some() {
                "quota exhausted"
            } else if data.get("code").is_some() {
                "connection reset"
            } else {
                "provider omitted both message and error details"
            };
            assert!(expected.contains(detail), "{expected}");
            assert_eq!(
                output.event.last().unwrap().text.as_deref(),
                Some(expected.as_str())
            );
        }
    }

    #[tokio::test]
    async fn concise_reasoning_updates_status_without_becoming_assistant_commentary()
    -> anyhow::Result<()> {
        let decoder = CopilotEventDecoder::default();
        let mut output = BackendOutput {
            backend_session_id: Some("session".into()),
            ..Default::default()
        };
        for provider_event in [
            event("assistant.turn_start", json!({"turnId":"turn"})),
            event(
                "assistant.reasoning_delta",
                json!({"reasoningId":"thought","deltaContent":"Simplifying "}),
            ),
            event(
                "assistant.reasoning_delta",
                json!({"reasoningId":"thought","deltaContent":"note handling"}),
            ),
            event(
                "assistant.reasoning",
                json!({"reasoningId":"thought","content":"Testing Git filter behaviors"}),
            ),
            event(
                "assistant.message",
                json!({"messageId":"commentary","content":"The file uses Git LFS.",
                    "toolRequests":[{"toolCallId":"inspect","name":"read"}]}),
            ),
        ] {
            decoder.decode(&provider_event, &mut output, None).await;
        }
        let thoughts = output
            .event
            .iter()
            .filter(|event| event.kind == "reasoning_summary")
            .collect::<Vec<_>>();
        assert_eq!(thoughts.len(), 1);
        assert_eq!(thoughts[0].message_id(), Some("thought"));
        assert!(thoughts[0].message_snapshot());
        assert_eq!(
            thoughts[0].text.as_deref(),
            Some("Testing Git filter behaviors")
        );
        let mut exchange: crate::exchange::Exchange = serde_json::from_value(json!({
            "id":"exchange","session_id":"session","agent_id":"primary","ordinal":1,
            "prompt":"Inspect filters","kind":"chat","state":"running",
            "created_at_ms":0,"attributed_matches_checkpoint":false,"node_list":[]
        }))?;
        exchange.resume(0)?;
        for event in &output.event {
            exchange.observe_turn(event, 1)?;
        }
        assert_eq!(
            exchange.latest_reasoning_summary(),
            Some("Testing Git filter behaviors")
        );
        let messages = exchange.turn[0].messages();
        assert_eq!(messages.len(), 2);
        assert_eq!(
            messages[0].kind(),
            crate::turn::MessageKind::ReasoningSummary
        );
        assert_eq!(messages[1].kind(), crate::turn::MessageKind::Assistant);
        assert_eq!(
            messages[1].delivery(),
            crate::turn::MessageDelivery::Commentary
        );
        Ok(())
    }

    #[tokio::test]
    async fn native_patch_results_render_as_changed_files_after_streaming_and_reload()
    -> anyhow::Result<()> {
        use crate::buffer::projection::{ProjectionSource, project};
        let decoder = CopilotEventDecoder {
            workspace: "/Users/me/project".into(),
        };
        let mut output = BackendOutput {
            backend_session_id: Some("root".into()),
            ..Default::default()
        };
        let mut exchange: crate::exchange::Exchange = serde_json::from_value(json!({
            "id":"patches", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Fix the file", "kind":"plan_execution", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let diff = "\ndiff --git a/Users/me/project/src/lib.rs b/Users/me/project/src/lib.rs\n\
            index 0000000..0000000 100644\n\
            --- a/Users/me/project/src/lib.rs\n+++ b/Users/me/project/src/lib.rs\n\
            @@ -40 +40 @@\n-old\n+new\n";
        for (index, (kind, data)) in [
            ("assistant.turn_start", json!({"turnId":"one"})),
            ("tool.execution_start", json!({"toolCallId":"patch", "toolName":"apply_patch",
                "arguments":"*** Begin Patch\n*** Update File: /Users/me/project/src/lib.rs\n@@\n-old\n+new\n*** End Patch"})),
            ("tool.execution_progress", json!({"toolCallId":"patch", "progressMessage":"Applying"})),
            ("tool.execution_complete", json!({"toolCallId":"patch", "success":true,
                "result":{"content":"Modified 1 file(s): src/lib.rs", "detailedContent":diff}})),
            ("assistant.turn_end", json!({"turnId":"one"})),
        ].into_iter().enumerate() {
            decoder.decode(&event(kind, data), &mut output, None).await;
            exchange.observe_turn(output.event.last().unwrap(), index as i64 + 1)?;
            if kind == "tool.execution_start" {
                let rendered = project(ProjectionSource::Exchange(&exchange), &Default::default(),
                    false, &Default::default(), &Default::default(),
 &mut crate::buffer::identity::DisplayRegistry::default(),
)?;
                let text = rendered.entry.block.iter().flat_map(|block| block.text.wire_rows()).collect::<Vec<_>>().join("\n");
                assert!(text.contains("Changing files"));
                assert!(!text.contains("Ran 1 tool"));
            }
        }
        assert!(output.evidence.workspace_changed);
        let activity = output
            .event
            .iter()
            .find_map(|event| event.activity.as_ref())
            .unwrap();
        assert_eq!(activity.kind, ToolActivityKind::FileChange);
        assert_eq!(activity.change.file[0].path, "src/lib.rs");
        assert_eq!(activity.change.file[0].diff, "@@ -40 +40 @@\n-old\n+new\n");
        exchange.finish(crate::exchange::ExchangeOutcome::Complete, 10)?;
        let restored: crate::exchange::Exchange =
            serde_json::from_value(serde_json::to_value(exchange)?)?;
        let rendered = project(
            ProjectionSource::Exchange(&restored),
            &Default::default(),
            false,
            &Default::default(),
            &Default::default(),
            &mut crate::buffer::identity::DisplayRegistry::default(),
        )?;
        let text = rendered
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(text.contains("Changed 1 file"));
        assert!(text.contains("lib.rs"));
        assert!(!text.contains("Ran 1 tool"));
        assert!(rendered.tool.is_empty());
        let displayed = rendered
            .action
            .values()
            .find_map(|action| match action {
                crate::buffer::projection::TranscriptAction::Diff { text, .. } => Some(text),
                _ => None,
            })
            .expect("changed files must retain a diff action");
        let patch = forge_diff::patch::UnifiedPatch::parse(displayed)?;
        assert_eq!(patch.file.len(), 1);
        assert_eq!(patch.file[0].new_path.as_deref(), Some("src/lib.rs"));
        assert_eq!(patch.file[0].hunk.len(), 1);
        let hunk = &patch.file[0].hunk[0];
        assert_eq!(hunk.old_lines, 39..40);
        assert_eq!(hunk.new_lines, 39..40);
        assert_eq!(hunk.row.len(), 2);
        assert_eq!(hunk.row[0].kind(), forge_diff::display::RowKind::Removed);
        assert_eq!(hunk.row[0].text, "old");
        assert_eq!(hunk.row[1].kind(), forge_diff::display::RowKind::Added);
        assert_eq!(hunk.row[1].text, "new");
        Ok(())
    }

    #[tokio::test]
    async fn file_tool_failures_and_read_only_diffs_never_claim_workspace_changes() {
        let decoder = CopilotEventDecoder::default();
        let diff = "diff --git a/file b/file\n--- a/file\n+++ b/file\n@@ -1 +1 @@\n-old\n+new\n";
        for (name, arguments, success, expected) in [
            (
                "apply_patch",
                Value::Null,
                false,
                ToolActivityKind::FileChange,
            ),
            (
                "bash",
                json!({"command":"git diff"}),
                true,
                ToolActivityKind::ToolCall,
            ),
            (
                "str_replace_editor",
                json!({"command":"view", "path":"file"}),
                true,
                ToolActivityKind::ToolCall,
            ),
        ] {
            let mut output = BackendOutput::default();
            decoder
                .decode(
                    &event(
                        "tool.execution_start",
                        json!({
                            "toolCallId":"tool", "toolName":name, "arguments":arguments,
                        }),
                    ),
                    &mut output,
                    None,
                )
                .await;
            decoder.decode(&event("tool.execution_complete", json!({
                "toolCallId":"tool", "success":success, "result":{"detailedContent":diff},
            })), &mut output, None).await;
            let activity = output.event[0].activity.as_ref().unwrap();
            assert_eq!(activity.kind, expected);
            assert!(!output.evidence.workspace_changed);
            if expected == ToolActivityKind::FileChange {
                assert_eq!(activity.status.as_deref(), Some("failed"));
            } else {
                assert!(activity.change.is_empty());
            }
        }
    }

    #[tokio::test]
    async fn malformed_file_diffs_report_errors_without_losing_tool_completion() {
        let decoder = CopilotEventDecoder::default();
        let mut output = BackendOutput::default();
        decoder
            .decode(
                &event(
                    "tool.execution_start",
                    json!({"toolCallId":"patch", "toolName":"apply_patch"}),
                ),
                &mut output,
                None,
            )
            .await;
        decoder
            .decode(
                &event(
                    "tool.execution_complete",
                    json!({"toolCallId":"patch", "success":true,
            "result":{"detailedContent":"diff --git a/file b/file\n@@ -1 +1 @@\n-old\n"}}),
                ),
                &mut output,
                None,
            )
            .await;
        assert!(output.event.iter().any(|event| {
            event.kind == "error"
                && event
                    .text
                    .as_deref()
                    .unwrap()
                    .contains("truncated saved patch hunk")
        }));
        assert_eq!(
            output.event[0].activity.as_ref().unwrap().status.as_deref(),
            Some("completed")
        );
        assert!(!output.evidence.workspace_changed);
    }

    #[tokio::test]
    async fn structured_changes_upgrade_generic_tools_and_survive_nameless_completion() {
        let decoder = CopilotEventDecoder::default();
        let mut output = BackendOutput::default();
        let mut tools = crate::turn::ToolStore::default();
        for data in [
            json!({"toolCallId":"tool", "toolName":"custom_edit"}),
            json!({"toolCallId":"tool", "changes":[{"path":"file", "diff":"@@ -1 +1 @@\n-old\n+new\n"}]}),
        ] {
            decoder
                .decode(&event("tool.execution_start", data), &mut output, None)
                .await;
            tools.merge(output.event[0].activity.as_ref().unwrap(), 1);
        }
        decoder
            .decode(
                &event(
                    "tool.execution_complete",
                    json!({"toolCallId":"tool", "success":true,
            "result":{"content":"done"}}),
                ),
                &mut output,
                None,
            )
            .await;
        tools.merge(output.event[0].activity.as_ref().unwrap(), 2);
        assert!(output.evidence.workspace_changed);
        assert_eq!(
            output.event[0].activity.as_ref().unwrap().kind,
            ToolActivityKind::FileChange
        );
        assert_eq!(tools.get("tool").unwrap().kind, "file_change");
        assert_eq!(tools.get("tool").unwrap().change.file.len(), 1);
    }

    #[tokio::test]
    async fn compaction_retains_summary_failure_and_identity_across_replays() {
        let decoder = CopilotEventDecoder::default();
        let mut output = BackendOutput {
            backend_session_id: Some("root".into()),
            ..Default::default()
        };
        let start = event(
            "session.compaction_start",
            json!({"model":"summary-model", "conversationTokens":100}),
        );
        let complete = event(
            "session.compaction_complete",
            json!({
                "success":false, "error":"summary request failed", "summaryContent":"Retain the contract.",
                "preCompactionTokens":100, "postCompactionTokens":40, "tokensRemoved":60,
                "messagesRemoved":0, "compactionTokensUsed":{"inputTokens":100,"outputTokens":20,"cacheReadTokens":0,
                    "copilotUsage":{"totalNanoAiu":1.5,"tokenDetails":[{"tokenType":"input","tokenCount":100}]}}
            }),
        );
        for update in [&start, &complete, &complete, &start] {
            decoder.decode(update, &mut output, None).await;
        }
        assert_eq!(output.event.len(), 1);
        let compaction = output.event[0].compaction().unwrap().unwrap();
        assert_eq!(compaction.id, start.id);
        assert_eq!(compaction.state, crate::backend::CompactionState::Failed);
        assert_eq!(
            compaction.result.summary_content.as_deref(),
            Some("Retain the contract.")
        );
        assert_eq!(
            compaction.result.error.as_deref(),
            Some("summary request failed")
        );
        assert_eq!(compaction.result.messages_removed, Some(0));
        assert_eq!(compaction.result.model.as_deref(), Some("summary-model"));
        assert_eq!(
            compaction
                .result
                .before_context
                .unwrap()
                .conversation_tokens,
            Some(100)
        );
        assert_eq!(output.event[0].data["provider_event_id"], complete.id);
        let usage = compaction.result.compaction_tokens_used.unwrap();
        assert_eq!(usage.cached_input, Some(0));
        assert_eq!(usage.billing.unwrap().total_nano_aiu, Some(1.5));
        let mut orphan = event("session.compaction_complete", json!({"success":true}));
        orphan.id = "other-operation".into();
        decoder.decode(&orphan, &mut output, None).await;
        assert_eq!(output.event.len(), 2);
        assert_eq!(
            output.event[1]
                .compaction()
                .unwrap()
                .unwrap()
                .result
                .pre_compaction_tokens,
            None
        );
    }

    fn event(event_type: &str, data: Value) -> SessionEvent {
        SessionEvent {
            id: format!("{event_type}-id"),
            timestamp: "2026-07-16T00:00:00Z".into(),
            parent_id: None,
            ephemeral: None,
            agent_id: None,
            debug_cli_received_at_ms: None,
            debug_ws_forwarded_at_ms: None,
            event_type: event_type.into(),
            data,
        }
    }

    #[tokio::test]
    async fn publishes_root_model_before_turn_and_ignores_child_model() {
        let decoder = CopilotEventDecoder::default();
        let mut output = BackendOutput {
            runtime: crate::backend::BackendRuntime {
                provider: "Copilot CLI".into(),
                model: None,
            },
            ..BackendOutput::default()
        };
        decoder
            .decode(
                &event("session.start", json!({"model":"root-model"})),
                &mut output,
                None,
            )
            .await;
        assert_eq!(output.event.len(), 1);
        assert_eq!(output.event[0].kind, "runtime_resolved");
        assert_eq!(output.event[0].data["model"], "root-model");
        assert!(output.event[0].address.is_none());

        let mut child = event("session.start", json!({"model":"child-model"}));
        child.agent_id = Some("child".into());
        decoder.decode(&child, &mut output, None).await;
        decoder
            .decode(
                &event("session.start", json!({"model":"root-model"})),
                &mut output,
                None,
            )
            .await;
        assert_eq!(output.runtime.model.as_deref(), Some("root-model"));
        assert_eq!(output.event.len(), 1);
    }

    #[tokio::test]
    async fn reports_every_model_call_and_addresses_late_child_usage() {
        let decoder = CopilotEventDecoder::default();
        let mut output = BackendOutput {
            backend_session_id: Some("parent".into()),
            ..BackendOutput::default()
        };
        decoder
            .decode(
                &event("assistant.turn_start", json!({"turnId":"parent-turn"})),
                &mut output,
                None,
            )
            .await;
        let mut child = event("assistant.turn_start", json!({"turnId":"child-turn"}));
        child.agent_id = Some("child".into());
        decoder.decode(&child, &mut output, None).await;
        for id in ["call-one", "call-two"] {
            let usage = event(
                "assistant.usage",
                json!({
                    "apiCallId":id, "inputTokens":1000, "cacheReadTokens":900,
                    "reasoningTokens":200, "outputTokens":280,
                }),
            );
            decoder.decode(&usage, &mut output, None).await;
        }
        child.event_type = "assistant.turn_end".into();
        decoder.decode(&child, &mut output, None).await;
        child.event_type = "assistant.usage".into();
        child.data = json!({"apiCallId":"child-call", "inputTokens":10,"outputTokens":20});
        decoder.decode(&child, &mut output, None).await;
        let usage = output
            .event
            .iter()
            .filter(|event| event.kind == "usage")
            .collect::<Vec<_>>();
        assert_eq!(usage.len(), 3);
        assert_eq!(usage[0].address.as_ref().unwrap().turn_id, "parent-turn");
        assert_eq!(usage[1].data["id"], "call-two");
        assert_eq!(usage[2].address.as_ref().unwrap().thread_id, "child");
        assert_eq!(usage[2].address.as_ref().unwrap().turn_id, "child-turn");
        assert!(usage[2].data["usage"]["reasoning"].is_null());
        let native: crate::backend::usage::UsageUpdate =
            serde_json::from_value(usage[0].data.clone()).unwrap();
        assert_eq!(native.usage.non_reasoning_output(), Some(80));
    }

    #[tokio::test]
    async fn retains_tool_identity_across_turns_and_ignores_late_background_output() {
        let decoder = CopilotEventDecoder::default();
        let mut output = BackendOutput {
            backend_session_id: Some("session".into()),
            ..BackendOutput::default()
        };
        for (kind, data) in [
            ("assistant.turn_start", json!({"turnId":"one"})),
            (
                "tool.execution_start",
                json!({"toolCallId":"shell", "toolName":"powershell"}),
            ),
            (
                "tool.execution_partial_result",
                json!({"toolCallId":"shell", "partialOutput":"hello"}),
            ),
            (
                "tool.execution_complete",
                json!({"toolCallId":"shell", "success":true, "result":{"content":"hello"}}),
            ),
            ("assistant.turn_start", json!({"turnId":"two"})),
            (
                "tool.execution_partial_result",
                json!({"toolCallId":"shell", "partialOutput":"hello"}),
            ),
        ] {
            decoder.decode(&event(kind, data), &mut output, None).await;
        }
        let tools = output
            .event
            .iter()
            .filter(|event| event.activity.is_some())
            .collect::<Vec<_>>();
        assert_eq!(tools.len(), 1);
        assert_eq!(tools[0].address.as_ref().unwrap().turn_id, "one");
        let activity = tools[0].activity.as_ref().unwrap();
        assert_eq!(activity.title, "powershell");
        assert_eq!(activity.status.as_deref(), Some("completed"));
        assert_eq!(activity.output.as_deref(), Some("hello"));
        assert!(!activity.output_delta);
    }

    #[tokio::test]
    async fn coalesces_streamed_message_and_tool_lifecycle_updates() {
        let decoder = CopilotEventDecoder::default();
        let mut output = BackendOutput::default();
        decoder
            .decode(
                &event(
                    "assistant.message_delta",
                    json!({ "deltaContent": "Hello " }),
                ),
                &mut output,
                None,
            )
            .await;
        decoder
            .decode(
                &event(
                    "assistant.message_delta",
                    json!({ "deltaContent": "world" }),
                ),
                &mut output,
                None,
            )
            .await;
        decoder
            .decode(
                &event(
                    "tool.execution_start",
                    json!({ "toolCallId": "tool-1", "toolName": "grep" }),
                ),
                &mut output,
                None,
            )
            .await;
        decoder.decode(
            &event(
                "tool.execution_complete",
                json!({ "toolCallId": "tool-1", "toolName": "grep", "success": true, "result": { "content": "one" } }),
            ),
            &mut output,
            None,
        ).await;
        assert_eq!(output.event[0].text.as_deref(), Some("Hello world"));
        assert_eq!(output.event.len(), 2);
        assert_eq!(output.event[1].activity.as_ref().unwrap().title, "grep");
        assert_eq!(
            output.event[1].activity.as_ref().unwrap().status.as_deref(),
            Some("completed")
        );
    }

    #[tokio::test]
    async fn normalizes_provider_turn_boundaries_without_counting_context_as_usage() {
        let decoder = CopilotEventDecoder::default();
        let mut output = BackendOutput {
            backend_session_id: Some("session".into()),
            ..BackendOutput::default()
        };
        decoder
            .decode(
                &event("assistant.turn_start", json!({"turnId":"turn-one"})),
                &mut output,
                None,
            )
            .await;
        decoder
            .decode(
                &event("assistant.message_delta", json!({"deltaContent":"working"})),
                &mut output,
                None,
            )
            .await;
        decoder
            .decode(
                &event(
                    "session.usage_info",
                    json!({"currentTokens":20,"tokenLimit":100}),
                ),
                &mut output,
                None,
            )
            .await;
        decoder
            .decode(
                &event("assistant.turn_end", json!({"turnId":"turn-one"})),
                &mut output,
                None,
            )
            .await;
        assert!(!output.event.iter().any(|event| event.kind == "usage"));
        assert!(matches!(
            output.event[0].turn_boundary,
            Some(crate::backend::TurnBoundary::Started)
        ));
        assert_eq!(output.event[1].address, output.event[0].address);
        assert!(matches!(
            output.event.last().unwrap().turn_boundary,
            Some(crate::backend::TurnBoundary::Finished {
                outcome: crate::turn::TurnOutcome::Completed
            })
        ));
        assert_eq!(
            output.event[0].address.as_ref().unwrap().turn_id,
            "turn-one"
        );
    }

    #[tokio::test]
    async fn maps_usage_tasks_and_subagent_events() {
        let decoder = CopilotEventDecoder::default();
        let mut output = BackendOutput::default();
        decoder
            .decode(
                &event(
                    "session.usage_info",
                    json!({ "currentTokens": 20, "tokenLimit": 100 }),
                ),
                &mut output,
                None,
            )
            .await;
        decoder
            .publish_task_snapshot(
                json!({ "rows": [{ "id": "one", "title": "Inspect", "status": "completed" }] }),
                "copilot-root".into(),
                &mut output,
                None,
            )
            .await;
        decoder
            .decode(
                &event(
                    "subagent.started",
                    json!({ "agentId": "child", "agentName": "explorer" }),
                ),
                &mut output,
                None,
            )
            .await;
        assert_eq!(output.metrics.context_usage.unwrap().remaining_percent, 80);
        assert!(output.event[1].checklist_update.as_ref().unwrap().complete);
        let lifecycle: AgentLifecycleEvent =
            serde_json::from_value(output.event[2].data.clone()).unwrap();
        assert_eq!(lifecycle.status, AgentExecutionState::Running);
    }
}
