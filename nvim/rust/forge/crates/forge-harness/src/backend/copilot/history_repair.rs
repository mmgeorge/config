//! Validates resumed native history before admitting another model request.

use anyhow::{Context, Result, ensure};
use github_copilot_sdk::SessionEvent;
use github_copilot_sdk::rpc::{
    EventLogReadRequest, EventsAgentScope, EventsCursorStatus, HistoryTruncateRequest,
};
use github_copilot_sdk::session_events::{
    AssistantMessageData, SessionEventType, ToolExecutionCompleteData,
};
use std::collections::{HashMap, HashSet};

/// Records a native suffix repair without changing the user's submitted input.
#[derive(Clone)]
pub(super) struct HistoryRepair;

impl HistoryRepair {
    /// Explain why prior side effects must be inspected before repeating work.
    pub(super) fn caution(&self) -> &'static str {
        "Recovery notice: an interrupted native tool-call suffix was removed from provider history. Its actions may already have completed or partially changed the workspace. Inspect the workspace before repeating writes; do not replay the interrupted request automatically."
    }
}

/// Remove only a proven incomplete trailing tool-call suffix, then verify repair.
pub(super) async fn repair(
    session: &github_copilot_sdk::session::Session,
) -> Result<Option<HistoryRepair>> {
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
        .context("repair interrupted Copilot native history")?;
    ensure!(
        result.events_removed > 0,
        "Copilot native history repair removed no events"
    );
    let repaired = read_history(session).await?;
    ensure!(
        repaired.iter().map(|event| event.id.as_str()).eq(preserved),
        "Copilot native history repair did not preserve the completed prefix"
    );
    ensure!(
        incomplete_suffix(&repaired)?.is_none(),
        "Copilot native history remains incomplete after repair"
    );
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
                agent_scope: Some(EventsAgentScope::Primary),
                cursor: cursor.clone(),
                max: None,
                types: Some(serde_json::json!("*")),
                wait_ms: Some(0),
            })
            .await
            .context("validate resumed Copilot native history")?;
        ensure!(
            page.cursor_status == EventsCursorStatus::Ok,
            "Copilot native history cursor is unavailable"
        );
        history.extend(
            page.events
                .into_iter()
                .filter(|event| event.ephemeral != Some(true)),
        );
        if !page.has_more {
            return Ok(history);
        }
        ensure!(
            seen_cursor.insert(page.cursor.clone()),
            "Copilot native history cursor did not advance"
        );
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
                    .context("invalid Copilot assistant history")?;
                for request in message.tool_requests.unwrap_or_default() {
                    ensure!(
                        declaration
                            .insert(request.tool_call_id.clone(), index)
                            .is_none(),
                        "duplicate Copilot native tool declaration"
                    );
                    pending.insert(request.tool_call_id, index);
                }
            }
            SessionEventType::ToolExecutionComplete => {
                let completion = event
                    .typed_data::<ToolExecutionCompleteData>()
                    .context("invalid Copilot tool completion history")?;
                pending.remove(&completion.tool_call_id);
            }
            _ => {}
        }
    }
    let Some(boundary) = pending.values().copied().min() else {
        return Ok(None);
    };
    for event in history
        .iter()
        .skip(boundary + 1)
        .filter(|event| event.agent_id.is_none())
    {
        match event.parsed_type() {
            SessionEventType::AssistantMessage => {
                let message = event
                    .typed_data::<AssistantMessageData>()
                    .context("invalid Copilot assistant history")?;
                ensure!(
                    message.content.is_empty(),
                    "incomplete Copilot tool history precedes later assistant output; refusing destructive repair"
                );
            }
            SessionEventType::ToolExecutionComplete => {
                let completion = event
                    .typed_data::<ToolExecutionCompleteData>()
                    .context("invalid Copilot tool completion history")?;
                ensure!(
                    declaration
                        .get(&completion.tool_call_id)
                        .is_some_and(|index| *index == boundary),
                    "incomplete Copilot tool history precedes later completed work; refusing destructive repair"
                );
            }
            SessionEventType::AssistantTurnEnd => {
                // Copilot closes failed query turns before recording their linked error.
                let failed = history.iter().any(|candidate| {
                    candidate.agent_id.is_none()
                        && candidate.parsed_type() == SessionEventType::SessionError
                        && candidate.parent_id.as_deref() == Some(event.id.as_str())
                });
                ensure!(
                    failed,
                    "incomplete Copilot tool history precedes a completed turn; refusing destructive repair"
                );
            }
            _ => {}
        }
    }
    Ok(Some(history[boundary].id.clone()))
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

    #[test]
    fn native_history_refuses_later_completed_work() {
        let history = vec![
            event(
                "boundary",
                "assistant.message",
                json!({"messageId":"request","content":"","toolRequests":[{"name":"shell","toolCallId":"pending"}]}),
            ),
            event(
                "later",
                "assistant.message",
                json!({"messageId":"later","content":"completed answer"}),
            ),
        ];
        assert!(incomplete_suffix(&history).is_err());
    }

    #[test]
    fn native_history_accepts_linked_failed_query_suffix_only() -> Result<()> {
        let mut history = vec![
            event(
                "boundary",
                "assistant.message",
                json!({"messageId":"request","content":"","toolRequests":[{"name":"shell","toolCallId":"pending"}]}),
            ),
            event("end", "assistant.turn_end", json!({"turnId":"0"})),
        ];
        assert!(incomplete_suffix(&history).is_err());
        let mut failure = event(
            "failure",
            "session.error",
            json!({"errorType":"query","message":"provider rejected request"}),
        );
        failure.parent_id = Some("end".into());
        history.push(failure);
        assert_eq!(incomplete_suffix(&history)?, Some("boundary".into()));
        let mut child = event(
            "child",
            "assistant.message",
            json!({"messageId":"child","content":"child output"}),
        );
        child.agent_id = Some("separate-native-agent".into());
        history.push(child);
        assert_eq!(incomplete_suffix(&history)?, Some("boundary".into()));
        Ok(())
    }

    #[test]
    fn native_history_rejects_completion_crossing_cut_boundary() {
        let history = vec![
            event(
                "earlier",
                "assistant.message",
                json!({"messageId":"earlier","content":"","toolRequests":[{"name":"shell","toolCallId":"earlier-tool"}]}),
            ),
            event(
                "boundary",
                "assistant.message",
                json!({"messageId":"later","content":"","toolRequests":[{"name":"shell","toolCallId":"orphan"}]}),
            ),
            event(
                "completion",
                "tool.execution_complete",
                json!({"toolCallId":"earlier-tool","success":true}),
            ),
        ];
        assert!(incomplete_suffix(&history).is_err());
    }
}
