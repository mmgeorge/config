mod message_state;
mod stream;

pub use stream::CopilotEventDecoder;

use crate::agent::{AgentExecutionState, AgentLifecycleEvent};
use crate::backend::{
    ChecklistStatus, ProviderChangeKind, ProviderChangeSet, ProviderChecklistEntry,
    ProviderChecklistUpdate, ProviderFileChange,
};
use github_copilot_sdk::SessionEvent;
use serde_json::Value;

pub(super) fn decode_compaction(
    event: &SessionEvent,
    thread: &str,
    previous: Option<&crate::backend::Compaction>,
) -> Result<crate::backend::Compaction, crate::backend::BackendError> {
    use crate::backend::{
        BackendError, Compaction, CompactionContext, CompactionResult, CompactionState,
    };
    let complete = event.event_type == "session.compaction_complete";
    let succeeded = if complete {
        event
            .data
            .get("success")
            .and_then(Value::as_bool)
            .ok_or_else(|| BackendError::Protocol {
                message: "Copilot compaction result omitted success".into(),
            })?
    } else {
        false
    };
    let mut result: CompactionResult =
        serde_json::from_value(event.data.clone()).map_err(|error| BackendError::Protocol {
            message: format!("Invalid Copilot compaction: {error}"),
        })?;
    if !complete {
        let mut context = CompactionContext {
            conversation_tokens: result.conversation_tokens.take(),
            system_tokens: result.system_tokens.take(),
            tool_definitions_tokens: result.tool_definitions_tokens.take(),
            ..Default::default()
        };
        context.current_tokens = context
            .conversation_tokens
            .zip(context.system_tokens)
            .zip(context.tool_definitions_tokens)
            .and_then(|((conversation, system), tools)| {
                conversation.checked_add(system)?.checked_add(tools)
            });
        result.context_tokens_before = context.current_tokens;
        result.before_context = Some(context);
    }
    let now = timestamp_ms();
    let update = Compaction {
        id: previous
            .filter(|_| complete)
            .map(|previous| previous.id.clone())
            .unwrap_or_else(|| event.id.clone()),
        thread_id: thread.into(),
        state: if !complete {
            CompactionState::Running
        } else if succeeded {
            CompactionState::Completed
        } else {
            CompactionState::Failed
        },
        manual: event.data.get("customInstructions").is_some(),
        started_at_ms: (!complete).then_some(now),
        completed_at_ms: complete.then_some(now),
        result,
    };
    if let Some(previous) = previous.filter(|previous| previous.id == update.id) {
        let mut retained = previous.clone();
        retained.observe(update, now);
        Ok(retained)
    } else {
        Ok(update)
    }
}

fn timestamp_ms() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .expect("system clock precedes Unix epoch")
        .as_millis() as i64
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

fn provider_change_set(data: &Value) -> ProviderChangeSet {
    let file = data
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
    ProviderChangeSet { file }
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

fn agent_lifecycle(event: &SessionEvent) -> AgentLifecycleEvent {
    AgentLifecycleEvent {
        operation: event.event_type.clone(),
        starts_child: event.event_type == "subagent.started",
        parent_thread_id: None,
        provider_thread_id: event
            .agent_id
            .clone()
            .or_else(|| string_field(&event.data, &["agentId", "toolCallId"])),
        turn_id: string_field(&event.data, &["turnId"]),
        definition: string_field(&event.data, &["agentName"]),
        nickname: string_field(&event.data, &["agentDisplayName", "displayName"]),
        task: string_field(&event.data, &["task", "prompt", "agentDescription"]),
        status: match event.event_type.as_str() {
            "subagent.started" => AgentExecutionState::Running,
            "subagent.completed" => AgentExecutionState::Completed,
            _ => AgentExecutionState::Failed,
        },
    }
}

fn string_field(value: &Value, fields: &[&str]) -> Option<String> {
    fields
        .iter()
        .find_map(|field| value.get(*field).and_then(Value::as_str))
        .map(str::to_owned)
}

fn unsigned_field(value: &Value, fields: &[&str]) -> Option<u64> {
    fields
        .iter()
        .find_map(|field| value.get(*field).and_then(Value::as_u64))
}

#[cfg(test)]
mod tests;
