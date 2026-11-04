use forge_provider_api::agent::{
    AgentExecutionState, AgentId, AgentLifecycleEvent, AgentLifecycleKind,
};
use forge_provider_api::change::{ProviderChangeKind, ProviderChangeSet, ProviderFileChange};
use forge_provider_api::checklist::{
    ChecklistStatus, ProviderChecklistEntry, ProviderChecklistUpdate,
};
use github_copilot_sdk::SessionEvent;
use serde_json::Value;

pub fn decode_compaction(
    event: &SessionEvent,
    thread: &str,
    previous: Option<&forge_provider_api::compaction::Compaction>,
) -> Result<forge_provider_api::compaction::Compaction, forge_provider_api::ProviderError> {
    use forge_provider_api::compaction::{
        Compaction, CompactionContext, CompactionResult, CompactionState,
    };
    let complete = event.event_type == "session.compaction_complete";
    let succeeded = if complete {
        event
            .data
            .get("success")
            .and_then(Value::as_bool)
            .ok_or_else(|| {
                super::error::native_failure(
                    forge_provider_api::ProviderOperation::InputSubmit,
                    forge_provider_api::ProviderFailureKind::Protocol,
                )
            })?
    } else {
        false
    };
    let mut result: CompactionResult = serde_json::from_value::<
        super::compaction_metrics::NativeCompactionResult,
    >(event.data.clone())
    .map(Into::into)
    .map_err(|_error| {
        super::error::native_failure(
            forge_provider_api::ProviderOperation::InputSubmit,
            forge_provider_api::ProviderFailureKind::Protocol,
        )
    })?;
    if succeeded && result.diagnostic.is_some() {
        return Err(super::error::native_failure(
            forge_provider_api::ProviderOperation::InputSubmit,
            forge_provider_api::ProviderFailureKind::Protocol,
        ));
    }
    if complete && !succeeded && result.diagnostic.is_none() {
        result.diagnostic = Some(forge_provider_api::ProviderDiagnostic {
            kind: forge_provider_api::ProviderFailureKind::Unknown,
            message: forge_provider_api::SanitizedMessage::new("Provider compaction failed")
                .expect("static diagnostic is nonempty"),
            fatal: false,
        });
    }
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

pub fn timestamp_ms() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .expect("system clock precedes Unix epoch")
        .as_millis() as i64
}

pub fn provider_change_set(data: &Value) -> ProviderChangeSet {
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

pub fn checklist_update(data: &Value, scope_id: String) -> Option<ProviderChecklistUpdate> {
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

pub fn agent_lifecycle(event: &SessionEvent) -> Option<AgentLifecycleEvent> {
    let agent_id = event
        .agent_id
        .clone()
        .or_else(|| string_field(&event.data, &["agentId", "toolCallId"]))
        .and_then(|id| AgentId::new(id).ok())?;
    let (kind, state) = match event.event_type.as_str() {
        "subagent.started" => (AgentLifecycleKind::Started, AgentExecutionState::Starting),
        "subagent.completed" => (AgentLifecycleKind::Finished, AgentExecutionState::Completed),
        "subagent.failed" => (AgentLifecycleKind::Finished, AgentExecutionState::Failed),
        _ => return None,
    };
    Some(AgentLifecycleEvent {
        agent_id,
        parent_agent_id: string_field(&event.data, &["parentAgentId"])
            .and_then(|id| AgentId::new(id).ok()),
        conversation_id: string_field(&event.data, &["sessionId", "conversationId"])
            .and_then(|id| forge_provider_api::ProviderSessionId::try_from(id).ok()),
        kind,
        definition: string_field(&event.data, &["agentName"]),
        nickname: string_field(&event.data, &["agentDisplayName", "displayName"]),
        task: string_field(&event.data, &["task", "prompt", "agentDescription"]),
        state,
    })
}

pub fn string_field(value: &Value, fields: &[&str]) -> Option<String> {
    fields
        .iter()
        .find_map(|field| value.get(*field).and_then(Value::as_str))
        .map(str::to_owned)
}

pub fn unsigned_field(value: &Value, fields: &[&str]) -> Option<u64> {
    fields
        .iter()
        .find_map(|field| value.get(*field).and_then(Value::as_u64))
}

/// Recognizes both native denial codes without treating execution failures as rejections.
pub fn is_denied(data: &Value) -> bool {
    data.get("success").and_then(Value::as_bool) == Some(false)
        && matches!(
            data.pointer("/error/code").and_then(Value::as_str),
            Some("denied" | "rejected")
        )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::error::Error as _;

    /// Keep malformed Copilot payload text out of every normalized error surface.
    #[test]
    fn malformed_compaction_payload_exposes_only_protocol_classification() {
        let sentinel = "copilot-native-payload-sentinel";
        let event: SessionEvent = serde_json::from_value(serde_json::json!({
            "id":"malformed-compaction",
            "type":"session.compaction_complete",
            "timestamp":"2026-11-02T00:00:00Z",
            "data":{"success":true,"preCompactionTokens":sentinel}
        }))
        .expect("fixture event has a valid SDK envelope");

        let error = decode_compaction(&event, "private-session", None)
            .expect_err("malformed native compaction data is rejected");
        assert!(matches!(
            &error,
            forge_provider_api::ProviderError::Failure(failure)
                if failure.kind() == forge_provider_api::ProviderFailureKind::Protocol
        ));
        let rendered = format!("{error} {error:?}");
        assert!(!rendered.contains(sentinel));
        assert!(!rendered.contains("private-session"));
        let mut source = error.source();
        while let Some(error) = source {
            assert!(!error.to_string().contains(sentinel));
            source = error.source();
        }
    }
}
