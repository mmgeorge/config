use crate::agent::{AgentLifecycleEvent, AgentRunStatus};
use crate::backend::{
    BackendEvent, BackendEventSink, BackendOutput, ProviderChangeKind, ProviderChangeSet,
    ProviderFileChange, ProviderTaskEntry, ProviderTaskUpdate, TaskStatus, ToolActivity,
    ToolActivityKind,
};
use crate::session::ContextUsage;
use github_copilot_sdk::SessionEvent;
use serde_json::Value;

/// Normalizes stable Copilot SDK session events into the Harness interaction protocol.
#[derive(Default)]
pub struct CopilotEventDecoder;

impl CopilotEventDecoder {
    /// Decode one SDK event and publish its normalized Harness update.
    pub fn decode(
        &self,
        provider_event: &SessionEvent,
        output: &mut BackendOutput,
        event_sink: Option<&BackendEventSink>,
    ) {
        if let Some(model) = string_field(&provider_event.data, &["model"]) {
            output.runtime.model = Some(model);
        }
        match provider_event.event_type.as_str() {
            "assistant.reasoning_delta" => {
                self.publish_text(
                    "reasoning",
                    string_field(&provider_event.data, &["deltaContent", "delta"]),
                    provider_event,
                    output,
                    event_sink,
                );
            }
            "assistant.message_delta" => {
                self.publish_text(
                    "assistant_message",
                    string_field(&provider_event.data, &["deltaContent", "delta"]),
                    provider_event,
                    output,
                    event_sink,
                );
            }
            "assistant.reasoning" => {
                self.publish_final_text(
                    "reasoning",
                    string_field(&provider_event.data, &["content", "text"]),
                    provider_event,
                    output,
                    event_sink,
                );
            }
            "assistant.message" => {
                self.publish_final_text(
                    "assistant_message",
                    string_field(&provider_event.data, &["content", "text"]),
                    provider_event,
                    output,
                    event_sink,
                );
            }
            "tool.execution_start"
            | "tool.execution_partial_result"
            | "tool.execution_progress"
            | "tool.execution_complete" => {
                output.evidence.tool_called = true;
                let activity = tool_activity(provider_event);
                if activity.as_ref().is_some_and(successful_file_change) {
                    output.evidence.workspace_changed = true;
                }
                if let Some(activity) = activity {
                    publish(
                        BackendEvent {
                            kind: "tool".into(),
                            text: None,
                            data: serde_json::to_value(provider_event).unwrap_or(Value::Null),
                            activity: Some(activity),
                            summary: None,
                            task_update: None,
                        },
                        output,
                        event_sink,
                    );
                }
            }
            "session.usage_info" => {
                let used = unsigned_field(&provider_event.data, &["currentTokens", "used"]);
                let size = unsigned_field(&provider_event.data, &["tokenLimit", "size"]);
                if let (Some(used), Some(size)) = (used, size)
                    && let Some(context_usage) = ContextUsage::reported(used, size)
                {
                    output.metrics.token_count = Some(used);
                    output.metrics.context_usage = Some(context_usage.clone());
                    publish(
                        BackendEvent {
                            kind: "context_usage".into(),
                            text: None,
                            data: serde_json::to_value(context_usage).unwrap_or(Value::Null),
                            activity: None,
                            summary: None,
                            task_update: None,
                        },
                        output,
                        event_sink,
                    );
                }
            }
            "assistant.usage" if output.metrics.token_count.is_none() => {
                output.metrics.token_count = Some(
                    ["inputTokens", "outputTokens", "reasoningTokens"]
                        .iter()
                        .filter_map(|field| unsigned_field(&provider_event.data, &[*field]))
                        .sum(),
                );
            }
            "subagent.started" | "subagent.completed" | "subagent.failed" => {
                publish(
                    BackendEvent {
                        kind: "agent_lifecycle".into(),
                        text: None,
                        data: serde_json::to_value(agent_lifecycle(provider_event))
                            .unwrap_or(Value::Null),
                        activity: None,
                        summary: None,
                        task_update: None,
                    },
                    output,
                    event_sink,
                );
            }
            "session.error" if !provider_event.is_transient_error() => {
                self.publish_text(
                    "error",
                    string_field(&provider_event.data, &["message", "error"]),
                    provider_event,
                    output,
                    event_sink,
                );
            }
            _ => {}
        }
    }

    /// Publish one complete Copilot todo snapshot after its signal-only change event.
    pub fn publish_task_snapshot(
        &self,
        data: Value,
        scope_id: String,
        output: &mut BackendOutput,
        event_sink: Option<&BackendEventSink>,
    ) {
        let Some(task_update) = task_update(&data, scope_id) else {
            return;
        };
        publish(
            BackendEvent {
                kind: "plan".into(),
                text: None,
                data,
                activity: None,
                summary: None,
                task_update: Some(task_update),
            },
            output,
            event_sink,
        );
    }

    fn publish_text(
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
        publish(
            BackendEvent {
                kind: kind.into(),
                text: Some(text),
                data: serde_json::to_value(provider_event).unwrap_or(Value::Null),
                activity: None,
                summary: None,
                task_update: None,
            },
            output,
            event_sink,
        );
    }

    fn publish_final_text(
        &self,
        kind: &str,
        text: Option<String>,
        provider_event: &SessionEvent,
        output: &mut BackendOutput,
        event_sink: Option<&BackendEventSink>,
    ) {
        let message_id = string_field(&provider_event.data, &["messageId"]);
        let already_streamed = output
            .event
            .iter()
            .rev()
            .take_while(|event| event.activity.is_none())
            .any(|event| {
                event.kind == kind
                    && event.text.is_some()
                    && message_id.as_ref().is_none_or(|message_id| {
                        event
                            .data
                            .pointer("/data/messageId")
                            .and_then(Value::as_str)
                            == Some(message_id)
                    })
            });
        if !already_streamed {
            self.publish_text(kind, text, provider_event, output, event_sink);
        }
    }
}

fn publish(event: BackendEvent, output: &mut BackendOutput, event_sink: Option<&BackendEventSink>) {
    if let Some(activity) = event.activity.as_ref()
        && let Some(previous) = output.event.iter_mut().find(|previous| {
            previous
                .activity
                .as_ref()
                .is_some_and(|existing| existing.id == activity.id)
        })
    {
        merge_activity(previous, event);
        if let Some(event_sink) = event_sink {
            let _ = event_sink.send(previous.clone());
        }
        return;
    }
    if let Some(event_sink) = event_sink {
        let _ = event_sink.send(event.clone());
    }
    if matches!(event.kind.as_str(), "reasoning" | "assistant_message")
        && let Some(previous) = output.event.last_mut()
        && previous.kind == event.kind
        && previous.activity.is_none()
    {
        let incoming = event.text.unwrap_or_default();
        previous
            .text
            .get_or_insert_with(String::new)
            .push_str(&incoming);
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
    if update.title != "Copilot tool" {
        existing.title = update.title;
    }
    existing.status = update.status;
    existing.change = update.change;
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

fn tool_activity(provider_event: &SessionEvent) -> Option<ToolActivity> {
    let data = &provider_event.data;
    let id = string_field(data, &["toolCallId", "id"])?;
    let title = string_field(data, &["toolName", "name", "toolDescription"])
        .unwrap_or_else(|| "Copilot tool".into());
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
                Some(if success { "completed" } else { "failed" }.into()),
                output,
                false,
            )
        }
        _ => return None,
    };
    let change = provider_change_set(data);
    Some(ToolActivity {
        id,
        kind: if change.is_empty() {
            ToolActivityKind::ToolCall
        } else {
            ToolActivityKind::FileChange
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

fn task_update(data: &Value, scope_id: String) -> Option<ProviderTaskUpdate> {
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
                "in_progress" | "inprogress" | "active" => TaskStatus::InProgress,
                "completed" | "complete" | "done" => TaskStatus::Completed,
                _ => TaskStatus::Pending,
            };
            Some(ProviderTaskEntry {
                provider_id: string_field(value, &["id"]),
                content,
                priority: string_field(value, &["priority"]),
                status,
                provider_ordinal,
            })
        })
        .collect::<Vec<_>>();
    Some(ProviderTaskUpdate {
        scope_id,
        name: string_field(data, &["name", "title"]),
        complete: !entry_list.is_empty()
            && entry_list
                .iter()
                .all(|entry| entry.status == TaskStatus::Completed),
        replace_entries: true,
        entry_list,
    })
}

fn agent_lifecycle(provider_event: &SessionEvent) -> AgentLifecycleEvent {
    let status = match provider_event.event_type.as_str() {
        "subagent.started" => AgentRunStatus::Running,
        "subagent.completed" => AgentRunStatus::Completed,
        _ => AgentRunStatus::Failed,
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
    use super::*;
    use serde_json::json;

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

    #[test]
    fn coalesces_streamed_message_and_tool_lifecycle_updates() {
        let decoder = CopilotEventDecoder;
        let mut output = BackendOutput::default();
        decoder.decode(
            &event(
                "assistant.message_delta",
                json!({ "deltaContent": "Hello " }),
            ),
            &mut output,
            None,
        );
        decoder.decode(
            &event(
                "assistant.message_delta",
                json!({ "deltaContent": "world" }),
            ),
            &mut output,
            None,
        );
        decoder.decode(
            &event(
                "tool.execution_start",
                json!({ "toolCallId": "tool-1", "toolName": "grep" }),
            ),
            &mut output,
            None,
        );
        decoder.decode(
            &event(
                "tool.execution_complete",
                json!({ "toolCallId": "tool-1", "toolName": "grep", "success": true, "result": { "content": "one" } }),
            ),
            &mut output,
            None,
        );
        assert_eq!(output.event[0].text.as_deref(), Some("Hello world"));
        assert_eq!(output.event.len(), 2);
        assert_eq!(output.event[1].activity.as_ref().unwrap().title, "grep");
        assert_eq!(
            output.event[1].activity.as_ref().unwrap().status.as_deref(),
            Some("completed")
        );
    }

    #[test]
    fn maps_usage_tasks_and_subagent_events() {
        let decoder = CopilotEventDecoder;
        let mut output = BackendOutput::default();
        decoder.decode(
            &event(
                "session.usage_info",
                json!({ "currentTokens": 20, "tokenLimit": 100 }),
            ),
            &mut output,
            None,
        );
        decoder.publish_task_snapshot(
            json!({ "rows": [{ "id": "one", "title": "Inspect", "status": "completed" }] }),
            "copilot-root".into(),
            &mut output,
            None,
        );
        decoder.decode(
            &event(
                "subagent.started",
                json!({ "agentId": "child", "agentName": "explorer" }),
            ),
            &mut output,
            None,
        );
        assert_eq!(output.metrics.context_usage.unwrap().remaining_percent, 80);
        assert!(output.event[1].task_update.as_ref().unwrap().complete);
        let lifecycle: AgentLifecycleEvent =
            serde_json::from_value(output.event[2].data.clone()).unwrap();
        assert_eq!(lifecycle.status, AgentRunStatus::Running);
    }
}
