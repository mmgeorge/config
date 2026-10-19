use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use super::{Agent, AgentTask};
use crate::{
    exchange::Exchange,
    turn::{MessageDelivery, MessageKind, ToolCall, ToolState, TurnItem},
};

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
/// Addresses one recorded call independently of a child's current run.
pub(crate) struct AgentToolReference {
    /// Root session that owns the original tool execution.
    pub(crate) session_id: String,
    /// Child identity retained across follow-ups.
    pub(crate) agent_id: String,
    /// Original child run, independent of the currently active run.
    pub(crate) exchange_id: String,
    /// Provider turn that owns this call.
    pub(crate) turn_id: String,
    /// Provider call identity scoped to its turn.
    pub(crate) tool_id: String,
}

impl AgentToolReference {
    fn encode(exchange: &Exchange, turn_id: &str, tool_id: &str) -> Result<String> {
        let reference = Self {
            session_id: exchange.session_id.clone(),
            agent_id: exchange.agent_id.clone(),
            exchange_id: exchange.id.clone(),
            turn_id: turn_id.into(),
            tool_id: tool_id.into(),
        };
        Ok(format!(
            "agent-tool:{}",
            hex::encode(serde_json::to_vec(&reference)?)
        ))
    }

    /// Decode a complete reference without granting authority over its identities.
    pub(crate) fn decode(reference: &str) -> Result<Self> {
        let encoded = reference
            .strip_prefix("agent-tool:")
            .context("invalid agent tool reference")?;
        serde_json::from_slice(&hex::decode(encoded).context("invalid agent tool reference")?)
            .context("invalid agent tool reference")
    }
}

#[derive(Serialize)]
/// Contains a bounded view of one run without copying tool output or final answers.
pub(crate) struct AgentActivity {
    agent: String,
    agent_id: String,
    profile: String,
    task_id: String,
    state: crate::exchange::ExchangeState,
    elapsed_ms: u64,
    last_activity_at_ms: Option<i64>,
    last_activity_age_ms: Option<u64>,
    awaiting_approval: bool,
    awaiting_input: bool,
    entries: Vec<Value>,
    has_more: bool,
}

impl AgentActivity {
    /// Captures the latest entries in timeline order with 4000-character thought bounds.
    pub(crate) fn capture(
        agent: &Agent,
        task: &AgentTask,
        exchange: &Exchange,
        limit: usize,
        now_ms: i64,
        awaiting_approval: bool,
    ) -> Result<Self> {
        validate_limit(limit)?;
        ensure!(
            exchange.agent_id == agent.id && task.agent_id == agent.id && exchange.id == task.id,
            "activity exchange does not belong to the selected child run"
        );
        let mut entries = Vec::with_capacity(limit + 1);
        'turns: for turn in exchange.turn.iter().rev() {
            for item in turn.items().iter().rev() {
                let entry = match item {
                    TurnItem::Message { id } => {
                        let Some(message) =
                            turn.messages().iter().find(|message| message.id() == id)
                        else {
                            continue;
                        };
                        if message.text().trim().is_empty()
                            || message.delivery() == MessageDelivery::Final
                        {
                            continue;
                        }
                        let (text, truncated) = preview(message.text(), 4000);
                        json!({"kind":"thought", "source":match message.kind() {
                            MessageKind::Reasoning | MessageKind::ReasoningSummary => "reasoning_summary",
                            MessageKind::Assistant => "commentary",
                        }, "text":text, "truncated":truncated})
                    }
                    TurnItem::Tool { id } => {
                        let Some(tool) = turn.tools().find(|tool| tool.id == *id) else {
                            continue;
                        };
                        let (category, category_truncated) = preview(&tool.kind, 256);
                        let (name, name_truncated) = preview(&tool.title, 512);
                        json!({"kind":"tool", "name":name, "category":category,
                            "truncated":name_truncated || category_truncated, "status":status(tool),
                            "failed":tool.failed, "duration_ms":tool.elapsed_ms(now_ms),
                            "call_ref":AgentToolReference::encode(exchange,turn.id(),&tool.id)?})
                    }
                };
                entries.push(entry);
                if entries.len() > limit {
                    break 'turns;
                }
            }
        }
        let has_more = entries.len() > limit;
        entries.truncate(limit);
        entries.reverse();
        Ok(Self {
            agent: agent.task_name.clone(),
            agent_id: agent.id.clone(),
            profile: agent.definition.clone(),
            task_id: task.id.clone(),
            state: task.state,
            elapsed_ms: exchange.elapsed(now_ms),
            last_activity_at_ms: task.last_activity_at_ms,
            last_activity_age_ms: task
                .last_activity_at_ms
                .map(|observed| now_ms.saturating_sub(observed).max(0) as u64),
            awaiting_approval,
            awaiting_input: exchange.awaiting_input,
            entries,
            has_more,
        })
    }
}

/// Rejects activity bounds that could hide all progress or exceed the inspection budget.
pub(crate) fn validate_limit(limit: usize) -> Result<()> {
    ensure!(
        (1..=20).contains(&limit),
        "activity limit must be from 1 to 20 entries"
    );
    Ok(())
}

/// Projects provider spellings into the same lifecycle used by the timeline.
pub(crate) fn status(tool: &ToolCall) -> &'static str {
    match tool.state() {
        ToolState::Running => "running",
        ToolState::Completed => "completed",
        ToolState::Failed => "failed",
        ToolState::Cancelled => "cancelled",
        ToolState::Interrupted => "interrupted",
    }
}

fn preview(text: &str, limit: usize) -> (String, bool) {
    let mut characters = text.chars();
    let preview = characters.by_ref().take(limit).collect();
    (preview, characters.next().is_some())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn complete_tool_identity_roundtrips_beyond_the_former_reference_limit() {
        let exchange = Exchange::agent_admission("session-1", 1, "Inspect source".into(), 0);
        let tool_id = "provider-tool-".repeat(2048);
        let encoded = AgentToolReference::encode(&exchange, "turn-1", &tool_id).unwrap();
        assert!(encoded.len() > 16384);
        let decoded = AgentToolReference::decode(&encoded).unwrap();
        assert_eq!(decoded.tool_id, tool_id);
        assert_eq!(decoded.session_id, exchange.session_id);
    }
}
