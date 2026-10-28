use super::*;
use crate::{
    backend::{BackendEvent, ProviderAddress, ToolActivity, ToolEvent},
    exchange::{Exchange, ExchangeNode},
};
use anyhow::Result;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub(crate) struct TestTurn {
    exchange: Exchange,
}

impl TestTurn {
    pub(crate) fn new(id: String, address: ProviderAddress, now_ms: i64) -> Self {
        let mut exchange: Exchange = serde_json::from_value(serde_json::json!({
            "id":"test", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Work", "state":"running", "created_at_ms":now_ms,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        exchange.turn.push(Turn::new(id, address, now_ms));
        Self { exchange }
    }
    fn synchronize(&mut self) -> Result<()> {
        let turn = &self.exchange.turn[0];
        for item in turn.items() {
            let id = match item {
                TurnItem::Message { id } | TurnItem::Tool { id } => id,
            };
            let node_id = format!("{}:content:{id}", turn.id());
            if !self
                .exchange
                .node_list
                .iter()
                .any(|node| node.id() == node_id)
            {
                self.exchange.node_list.push(ExchangeNode::TurnContent {
                    id: node_id,
                    turn_id: turn.id().to_owned(),
                    item: item.clone(),
                });
            }
        }
        self.exchange.reconcile_entries()
    }
    pub(crate) fn tools(&self) -> impl Iterator<Item = &ToolCall> {
        self.exchange.tools().map(|(_, tool)| tool)
    }
    pub(crate) fn record_tool(&mut self, activity: &ToolActivity, now_ms: i64) -> Result<()> {
        let event = BackendEvent {
            received_at_ms: None,
            address: None,
            turn_boundary: None,
            kind: "tool".into(),
            text: None,
            data: serde_json::Value::Null,
            activity: Some(activity.clone()),
            summary: None,
            task_update: None,
        };
        self.record_tool_event(event.tool_event().unwrap(), now_ms)
    }
    pub(crate) fn record_tool_event(&mut self, event: ToolEvent<'_>, now_ms: i64) -> Result<()> {
        self.exchange.turn[0].admit_tool(&event.activity().id)?;
        self.synchronize()?;
        let id = self.exchange.turn[0].id().to_owned();
        self.exchange.record_tool_event(&id, event, now_ms)
    }
    pub(crate) fn record_text(&mut self, kind: MessageKind, text: &str) -> Result<()> {
        self.exchange.turn[0].record_text(kind, text)?;
        self.synchronize()
    }
    pub(crate) fn finish(&mut self, outcome: TurnOutcome, now_ms: i64) -> Result<()> {
        self.exchange.turn[0].finish(outcome, now_ms)?;
        self.exchange.settle_tools(now_ms);
        Ok(())
    }
    pub(crate) fn attribute_tool(&mut self, id: &str, task: &str) -> Result<bool> {
        let turn = self.exchange.turn[0].id().to_owned();
        self.exchange.attribute_tool(&turn, id, task)
    }
    pub(crate) fn into_exchange(self) -> Exchange {
        self.exchange
    }
}
impl std::ops::Deref for TestTurn {
    type Target = Turn;
    fn deref(&self) -> &Turn {
        &self.exchange.turn[0]
    }
}
impl std::ops::DerefMut for TestTurn {
    fn deref_mut(&mut self) -> &mut Turn {
        &mut self.exchange.turn[0]
    }
}

/// Finds a tool in a serialized commentary for tests that mutate provider snapshots.
pub(crate) fn tool_value_mut<'value>(
    value: &'value mut serde_json::Value,
    id: &str,
) -> &'value mut serde_json::Value {
    if value.get("entry").is_none() {
        return &mut value["turn"][0]["tool"]["item"][id];
    }
    let entries = value["entry"].as_array_mut().expect("exchange entries");
    for entry in entries {
        if entry["kind"] != "commentary" {
            continue;
        }
        for group in entry["commentary"]["tool"]
            .as_array_mut()
            .expect("commentary tools")
        {
            if let Some(tool) = group["store"]["item"]
                .as_object_mut()
                .expect("tool store")
                .get_mut(id)
            {
                return tool;
            }
        }
    }
    panic!("serialized commentary has no tool {id}");
}
