//! Validates resumed native history before admitting another model request.

use forge_provider_api::{ProviderError, ProviderFailureKind, ProviderOperation};
use github_copilot_sdk::SessionEvent;
use github_copilot_sdk::rpc::{
    EventLogReadRequest, EventsAgentScope, EventsCursorStatus, HistoryTruncateRequest,
};
use github_copilot_sdk::session_events::{
    AssistantMessageData, SessionEventType, ToolExecutionCompleteData,
};
use std::collections::{HashMap, HashSet};

type Result<T> = std::result::Result<T, ProviderError>;

/// Records a native suffix repair without changing the user's submitted input.
#[derive(Clone)]
pub struct HistoryRepair;

impl HistoryRepair {
    /// Explain why prior side effects must be inspected before repeating work.
    pub fn caution(&self) -> &'static str {
        "Recovery notice: an interrupted native tool-call suffix was removed from provider history. Its actions may already have completed or partially changed the workspace. Inspect the workspace before repeating writes; do not replay the interrupted request automatically."
    }
}

/// Remove only a proven incomplete trailing tool-call suffix, then verify repair.
pub async fn repair(session: &super::handle::CopilotSession) -> Result<Option<HistoryRepair>> {
    let session = session.inner.as_ref();
    let history = read_history(session).await?;
    let Some(event_id) = incomplete_suffix(&history)? else {
        return Ok(None);
    };
    let preserved = history
        .iter()
        .take_while(|event| event.id != event_id)
        .map(|event| event.id.as_str())
        .collect::<Vec<_>>();
    let result = session
        .rpc()
        .history()
        .truncate(HistoryTruncateRequest { event_id })
        .await
        .map_err(|_| failure(ProviderFailureKind::Transport))?;
    if result.events_removed == 0 {
        return Err(failure(ProviderFailureKind::Protocol));
    }
    let repaired = read_history(session).await?;
    if !repaired.iter().map(|event| event.id.as_str()).eq(preserved)
        || incomplete_suffix(&repaired)?.is_some()
    {
        return Err(failure(ProviderFailureKind::Protocol));
    }
    Ok(Some(HistoryRepair))
}

/// Read the entire persisted primary history without imposing a content limit.
async fn read_history(session: &github_copilot_sdk::session::Session) -> Result<Vec<SessionEvent>> {
    let mut cursor = None;
    let mut history = Vec::new();
    let mut seen_cursor = HashSet::new();
    loop {
        let page = session
            .rpc()
            .event_log()
            .read(EventLogReadRequest {
                agent_ids: None,
                direction: None,
                include_ephemeral: Some(false),
                agent_scope: Some(EventsAgentScope::Primary),
                cursor: cursor.clone(),
                max: None,
                types: Some(serde_json::json!("*")),
                wait_ms: Some(0),
            })
            .await
            .map_err(|_| failure(ProviderFailureKind::Transport))?;
        if page.cursor_status != EventsCursorStatus::Ok {
            return Err(failure(ProviderFailureKind::Protocol));
        }
        history.extend(
            page.events
                .into_iter()
                .filter(|event| event.ephemeral != Some(true)),
        );
        if !page.has_more {
            return Ok(history);
        }
        if !seen_cursor.insert(page.cursor.clone()) {
            return Err(failure(ProviderFailureKind::Protocol));
        }
        cursor = Some(page.cursor);
    }
}

/// Find an unmatched declaration only when removing its suffix loses no later completed work.
fn incomplete_suffix(history: &[SessionEvent]) -> Result<Option<String>> {
    let mut declaration = HashMap::new();
    let mut pending = HashMap::new();
    for (index, event) in history
        .iter()
        .enumerate()
        .filter(|(_, event)| event.agent_id.is_none())
    {
        match event.parsed_type() {
            SessionEventType::AssistantMessage => {
                let message = event
                    .typed_data::<AssistantMessageData>()
                    .ok_or_else(|| failure(ProviderFailureKind::Protocol))?;
                for request in message.tool_requests.unwrap_or_default() {
                    if declaration
                        .insert(request.tool_call_id.clone(), index)
                        .is_some()
                    {
                        return Err(failure(ProviderFailureKind::Protocol));
                    }
                    pending.insert(request.tool_call_id, index);
                }
            }
            SessionEventType::ToolExecutionComplete => {
                let completion = event
                    .typed_data::<ToolExecutionCompleteData>()
                    .ok_or_else(|| failure(ProviderFailureKind::Protocol))?;
                pending.remove(&completion.tool_call_id);
            }
            _ => {}
        }
    }
    let Some(boundary) = pending.values().copied().min() else {
        return Ok(None);
    };
    let primary_events = history
        .iter()
        .filter(|event| event.agent_id.is_none())
        .map(|event| (event.id.as_str(), event))
        .collect::<HashMap<_, _>>();
    for event in history
        .iter()
        .skip(boundary + 1)
        .filter(|event| event.agent_id.is_none())
    {
        match event.parsed_type() {
            SessionEventType::AssistantMessage => {
                let message = event
                    .typed_data::<AssistantMessageData>()
                    .ok_or_else(|| failure(ProviderFailureKind::Protocol))?;
                if !message.content.is_empty() {
                    return Err(failure(ProviderFailureKind::Rejected));
                }
            }
            SessionEventType::ToolExecutionComplete => {
                let completion = event
                    .typed_data::<ToolExecutionCompleteData>()
                    .ok_or_else(|| failure(ProviderFailureKind::Protocol))?;
                if !declaration
                    .get(&completion.tool_call_id)
                    .is_some_and(|index| *index == boundary)
                {
                    return Err(failure(ProviderFailureKind::Rejected));
                }
            }
            SessionEventType::AssistantTurnEnd => {
                // Copilot closes failed query turns before recording their linked error.
                let failed = history.iter().any(|candidate| {
                    candidate.agent_id.is_none()
                        && candidate.parsed_type() == SessionEventType::SessionError
                        && candidate.parent_id.as_deref() == Some(event.id.as_str())
                });
                if !failed && !interrupted_turn(&primary_events, event, &history[boundary].id) {
                    return Err(failure(ProviderFailureKind::Rejected));
                }
            }
            SessionEventType::UserMessage => {
                return Err(failure(ProviderFailureKind::Rejected));
            }
            _ => {}
        }
    }
    Ok(Some(history[boundary].id.clone()))
}

/// Require an abort on this turn's ancestry, even when persisted after its terminal event.
fn interrupted_turn(
    events: &HashMap<&str, &SessionEvent>,
    terminal: &SessionEvent,
    boundary: &str,
) -> bool {
    let mut cursor = Some(terminal);
    let mut visited = HashSet::new();
    let mut aborted = false;
    while let Some(event) = cursor {
        if !visited.insert(event.id.as_str()) {
            return false;
        }
        if event.id == boundary {
            return aborted;
        }
        aborted |= event.parsed_type() == SessionEventType::Abort;
        cursor = event
            .parent_id
            .as_deref()
            .and_then(|id| events.get(id).copied());
    }
    false
}

/// Classify a history recovery failure without exposing native history or SDK diagnostics.
fn failure(kind: ProviderFailureKind) -> ProviderError {
    super::error::native_failure(ProviderOperation::InputReconcile, kind)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// Build persisted protocol events with their native identity intact.
    fn event(id: &str, kind: &str, data: serde_json::Value) -> SessionEvent {
        serde_json::from_value(
            json!({"id":id,"type":kind,"timestamp":"2026-10-30T00:00:00Z","data":data}),
        )
        .unwrap()
    }

    #[test]
    fn native_history_preserves_completed_prefix_and_parallel_results() -> Result<()> {
        let mut history = vec![
            event(
                "prefix",
                "assistant.message",
                json!({"messageId":"prefix","content":"done"}),
            ),
            event(
                "boundary",
                "assistant.message",
                json!({"messageId":"request","content":"","toolRequests":[{"name":"shell","toolCallId":"done"},{"name":"shell","toolCallId":"pending"}]}),
            ),
            event(
                "result",
                "tool.execution_complete",
                json!({"toolCallId":"done","success":true}),
            ),
        ];
        assert_eq!(incomplete_suffix(&history)?, Some("boundary".into()));
        history.push(event(
            "result2",
            "tool.execution_complete",
            json!({"toolCallId":"pending","success":false}),
        ));
        assert_eq!(incomplete_suffix(&history)?, None);
        Ok(())
    }

    /// Resume a permission-cancelled tool suffix without mistaking its aborted turn for success.
    #[test]
    fn native_history_repairs_only_correlated_aborted_suffix() -> Result<()> {
        let mut history = vec![
            event(
                "boundary",
                "assistant.message",
                json!({"messageId":"request","content":"","toolRequests":[{"name":"shell","toolCallId":"pending"}]}),
            ),
            event("permission", "permission.requested", json!({})),
            event("permission-complete", "permission.completed", json!({})),
            event("end", "assistant.turn_end", json!({"turnId":"0"})),
            event("abort", "abort", json!({"reason":"remote_command"})),
        ];
        history[1].parent_id = Some("boundary".into());
        history[2].parent_id = Some("abort".into());
        history[3].parent_id = Some("permission-complete".into());
        history[4].parent_id = Some("permission".into());
        assert_eq!(incomplete_suffix(&history)?, Some("boundary".into()));

        history[4].agent_id = Some("child".into());
        assert!(incomplete_suffix(&history).is_err());
        history[4].agent_id = None;
        history[4].parent_id = Some("unrelated".into());
        assert!(incomplete_suffix(&history).is_err());
        history[4].parent_id = Some("permission-complete".into());
        assert!(incomplete_suffix(&history).is_err());
        history[4].parent_id = Some("permission".into());
        history[3].parent_id = Some("boundary".into());
        assert!(incomplete_suffix(&history).is_err());
        Ok(())
    }

    /// Never discard a newer accepted user input while repairing an earlier incomplete tool call.
    #[test]
    fn native_history_refuses_later_user_input() {
        let history = vec![
            event(
                "boundary",
                "assistant.message",
                json!({"messageId":"request","content":"","toolRequests":[{"name":"shell","toolCallId":"pending"}]}),
            ),
            event(
                "later",
                "user.message",
                json!({"content":"keep this input"}),
            ),
        ];
        assert!(
            matches!(incomplete_suffix(&history), Err(ProviderError::Failure(error))
            if error.operation() == ProviderOperation::InputReconcile
                && error.kind() == ProviderFailureKind::Rejected)
        );
    }
}
