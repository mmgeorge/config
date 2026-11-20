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
///
/// The next prompt reports it as [`forge_provider_api::forge::Notice::HistoryRepaired`].
#[derive(Clone)]
pub struct HistoryRepair;

/// Remove a proven incomplete trailing suffix in one cut, then verify repair.
pub async fn repair(session: &super::handle::CopilotSession) -> Result<Option<HistoryRepair>> {
    let session = session.inner.as_ref();
    let history = read_history(session).await?;
    let Some(event_id) = repair_cut(&history)? else {
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
        || unanswered_request(&repaired).is_some()
    {
        return Err(failure(ProviderFailureKind::Protocol));
    }
    Ok(Some(HistoryRepair))
}

/// Choose the one event at which truncation leaves a settled history.
///
/// An incomplete tool call is cut at the message that declared it. When the request that turn
/// answered was itself left unanswered before that declaration, removing only the call would
/// leave the request as the history's last word, which the next prompt's turn would carry out
/// and which verification rejects, so the cut moves back to that `user.message`.
fn repair_cut(history: &[SessionEvent]) -> Result<Option<String>> {
    let Some(boundary) = incomplete_suffix(history)? else {
        return Ok(unanswered_request(history));
    };
    let index = history
        .iter()
        .position(|event| event.id == boundary)
        .ok_or_else(|| failure(ProviderFailureKind::Protocol))?;
    Ok(Some(
        unanswered_request(&history[..index]).unwrap_or(boundary),
    ))
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
    let primary_events = history
        .iter()
        .filter(|event| event.agent_id.is_none())
        .map(|event| (event.id.as_str(), event))
        .collect::<HashMap<_, _>>();
    // Copilot never records a completion for a call its abort interrupted, yet the abort closes
    // that call's turn and the session continues after it. Such a call is settled history; only a
    // call that no aborted turn closed can form the incomplete suffix of a lost process.
    pending.retain(|_, index| {
        let declaration = history[*index].id.as_str();
        !history.iter().skip(*index + 1).any(|event| {
            event.agent_id.is_none()
                && event.parsed_type() == SessionEventType::AssistantTurnEnd
                && interrupted_turn(&primary_events, event, declaration)
        })
    });
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

/// Find a trailing user request the model never began answering.
///
/// The host settles a prompt it interrupted before Copilot answered it, but Copilot keeps the
/// accepted `user.message` and its opened turn, and the next prompt's turn then carries out the
/// interrupted request. Only turn opening, hooks, and the model selection a resume records may
/// follow it; any answer, tool call, abort, or turn end means the model acted on the request
/// and its history is kept.
fn unanswered_request(history: &[SessionEvent]) -> Option<String> {
    let primary = history
        .iter()
        .filter(|event| event.agent_id.is_none())
        .collect::<Vec<_>>();
    let request = primary
        .iter()
        .rposition(|event| event.parsed_type() == SessionEventType::UserMessage)?;
    primary[request + 1..]
        .iter()
        .all(|event| {
            matches!(
                event.parsed_type(),
                SessionEventType::AssistantTurnStart
                    | SessionEventType::HookStart
                    | SessionEventType::HookEnd
                    | SessionEventType::SessionModelChange
            )
        })
        .then(|| primary[request].id.clone())
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

    /// Build a persisted event with its native parent, as Copilot chains each record.
    fn child(id: &str, parent: &str, kind: &str, data: serde_json::Value) -> SessionEvent {
        serde_json::from_value(json!({
            "id":id,"parentId":parent,"type":kind,"timestamp":"2026-10-30T00:00:00Z","data":data
        }))
        .unwrap()
    }

    /// Append one turn whose external shell call was interrupted: Copilot records the abort and
    /// closes the turn but never writes a completion for the call.
    fn interrupted_shell_turn(history: &mut Vec<SessionEvent>, turn: &str, parent: &str) {
        let id = |name: &str| format!("{turn}-{name}");
        history.extend([
            child(&id("user"), parent, "user.message", json!({"content":turn})),
            child(
                &id("start"),
                &id("user"),
                "assistant.turn_start",
                json!({"turnId":"0"}),
            ),
            child(
                &id("request"),
                &id("start"),
                "assistant.message",
                json!({"messageId":id("request"),"content":"","toolRequests":[{"name":"powershell","toolCallId":id("call")}]}),
            ),
            child(
                &id("tool"),
                &id("request"),
                "tool.execution_start",
                json!({"toolCallId":id("call"),"toolName":"powershell"}),
            ),
            child(
                &id("external"),
                &id("tool"),
                "external_tool.requested",
                json!({"requestId":id("external")}),
            ),
            child(
                &id("abort"),
                &id("external"),
                "abort",
                json!({"reason":"user_initiated"}),
            ),
            child(
                &id("released"),
                &id("abort"),
                "external_tool.completed",
                json!({"requestId":id("external")}),
            ),
            child(
                &id("end"),
                &id("released"),
                "assistant.turn_end",
                json!({"turnId":"0"}),
            ),
        ]);
    }

    /// A call that an abort closed earlier is settled history, not an incomplete suffix.
    #[test]
    fn interrupted_calls_before_later_turns_need_no_repair() -> Result<()> {
        let mut history = Vec::new();
        interrupted_shell_turn(&mut history, "first", "root");
        interrupted_shell_turn(&mut history, "second", "first-end");
        history.extend([
            child(
                "later-user",
                "second-end",
                "user.message",
                json!({"content":"later"}),
            ),
            child(
                "later-start",
                "later-user",
                "assistant.turn_start",
                json!({"turnId":"0"}),
            ),
            child(
                "later-answer",
                "later-start",
                "assistant.message",
                json!({"messageId":"later-answer","content":"done"}),
            ),
            child(
                "later-end",
                "later-answer",
                "assistant.turn_end",
                json!({"turnId":"0"}),
            ),
        ]);
        assert_eq!(incomplete_suffix(&history)?, None);
        // A trailing call that no abort closed is still repaired, at its own declaration.
        history.extend([
            child("tail-user", "later-end", "user.message", json!({"content":"tail"})),
            child(
                "tail-start",
                "tail-user",
                "assistant.turn_start",
                json!({"turnId":"0"}),
            ),
            child(
                "tail-request",
                "tail-start",
                "assistant.message",
                json!({"messageId":"tail-request","content":"","toolRequests":[{"name":"powershell","toolCallId":"tail-call"}]}),
            ),
        ]);
        assert_eq!(incomplete_suffix(&history)?, Some("tail-request".into()));
        Ok(())
    }

    /// An interrupted request Copilot never answered is truncated at its `user.message`, in the
    /// shape a resume after an access change records.
    #[test]
    fn unanswered_trailing_request_is_truncated() {
        let mut history = vec![
            child(
                "answered",
                "root",
                "user.message",
                json!({"content":"first"}),
            ),
            child(
                "answer",
                "answered",
                "assistant.message",
                json!({"messageId":"m1","content":"done"}),
            ),
            child(
                "answer-end",
                "answer",
                "assistant.turn_end",
                json!({"turnId":"0"}),
            ),
            child(
                "hook",
                "answer-end",
                "hook.start",
                json!({"hookInvocationId":"h","hookType":"userPromptSubmitted","input":{}}),
            ),
            child(
                "hook-end",
                "hook",
                "hook.end",
                json!({"hookInvocationId":"h","hookType":"userPromptSubmitted","success":true}),
            ),
            child(
                "request",
                "hook-end",
                "user.message",
                json!({"content":"Create notes/wave5.txt"}),
            ),
            child(
                "start",
                "request",
                "assistant.turn_start",
                json!({"turnId":"1"}),
            ),
            child(
                "resume",
                "start",
                "session.model_change",
                json!({"newModel":"gpt-5"}),
            ),
        ];
        assert_eq!(unanswered_request(&history), Some("request".into()));
        history.push(child(
            "reply",
            "resume",
            "assistant.message",
            json!({"messageId":"m2","content":"working"}),
        ));
        assert_eq!(unanswered_request(&history), None);
    }

    /// A turn that failed while its delegated call was pending is cut at its request in one
    /// truncation, so the repaired history neither ends in the request nor holds the call.
    #[test]
    fn pending_call_of_an_unanswered_request_is_cut_at_the_request() -> Result<()> {
        let history = vec![
            child(
                "answered",
                "root",
                "user.message",
                json!({"content":"first"}),
            ),
            child(
                "answer",
                "answered",
                "assistant.message",
                json!({"messageId":"m1","content":"done"}),
            ),
            child(
                "answer-end",
                "answer",
                "assistant.turn_end",
                json!({"turnId":"0"}),
            ),
            child(
                "request",
                "answer-end",
                "user.message",
                json!({"content":"Find the parser"}),
            ),
            child(
                "start",
                "request",
                "assistant.turn_start",
                json!({"turnId":"1"}),
            ),
            child(
                "delegate",
                "start",
                "assistant.message",
                json!({"messageId":"m2","content":"","toolRequests":[{"name":"search_code_subagent","toolCallId":"search"}]}),
            ),
            child(
                "delegated",
                "delegate",
                "tool.execution_start",
                json!({"toolCallId":"search","toolName":"search_code_subagent"}),
            ),
        ];
        assert_eq!(repair_cut(&history)?, Some("request".into()));
        let repaired = &history[..3];
        assert_eq!(incomplete_suffix(repaired)?, None);
        assert_eq!(unanswered_request(repaired), None);
        Ok(())
    }

    /// A request the turn had already acted on keeps its completed work; only the pending call
    /// is cut.
    #[test]
    fn pending_call_after_completed_work_keeps_the_request() -> Result<()> {
        let history = vec![
            child(
                "request",
                "root",
                "user.message",
                json!({"content":"Find the parser"}),
            ),
            child(
                "start",
                "request",
                "assistant.turn_start",
                json!({"turnId":"0"}),
            ),
            child(
                "read",
                "start",
                "assistant.message",
                json!({"messageId":"m1","content":"","toolRequests":[{"name":"view","toolCallId":"view"}]}),
            ),
            child(
                "read-done",
                "read",
                "tool.execution_complete",
                json!({"toolCallId":"view","success":true}),
            ),
            child(
                "delegate",
                "read-done",
                "assistant.message",
                json!({"messageId":"m2","content":"","toolRequests":[{"name":"search_code_subagent","toolCallId":"search"}]}),
            ),
        ];
        assert_eq!(repair_cut(&history)?, Some("delegate".into()));
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
