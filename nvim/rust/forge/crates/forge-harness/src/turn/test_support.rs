use super::*;
use crate::{
    backend::{
        BackendEvent, ExecutionOwner, InvocationScope, MessageAddress, MessageClassification,
        MessageId, MessageRole, MessageUpdate, ToolInvocation, ToolUpdate, TurnAddress,
    },
    exchange::{Exchange, ExchangeNode},
};
use anyhow::Result;
use serde::{Deserialize, Serialize};

pub(crate) fn address(exchange: &Exchange, turn_id: &str) -> TurnAddress {
    TurnAddress {
        scope: InvocationScope {
            owner: ExecutionOwner {
                session_id: exchange.session_id.clone().into(),
                agent_id: exchange.agent_id.clone().into(),
            },
            exchange_id: exchange.id.clone().into(),
            invocation_id: format!("{}:invocation:{turn_id}", exchange.id).into(),
        },
        thread_id: "thread".into(),
        turn_id: turn_id.into(),
    }
}

pub(crate) fn message(
    turn: &TurnAddress,
    id: impl Into<String>,
    role: Option<MessageRole>,
    text: impl Into<String>,
) -> BackendEvent {
    BackendEvent::Message {
        address: MessageAddress {
            turn: turn.clone(),
            message_id: MessageId::Native(id.into()),
        },
        update: MessageUpdate::Started {
            classification: role.map_or(
                MessageClassification::Unclassified,
                MessageClassification::Known,
            ),
            text: text.into(),
        },
    }
}

pub(crate) fn tool(
    exchange: &mut Exchange,
    turn: &TurnAddress,
    id: &str,
    invocation: ToolInvocation,
    update: impl IntoIterator<Item = ToolUpdate>,
    now_ms: i64,
) -> Result<()> {
    let address = crate::backend::ToolAddress {
        turn: turn.clone(),
        tool_id: id.into(),
    };
    exchange.observe_turn(
        &BackendEvent::Tool {
            address: address.clone(),
            update: ToolUpdate::Requested { invocation },
        },
        now_ms,
    )?;
    for update in update {
        exchange.observe_turn(
            &BackendEvent::Tool {
                address: address.clone(),
                update,
            },
            now_ms,
        )?;
    }
    Ok(())
}

pub(crate) fn tool_updates(
    status: crate::backend::ToolStatus,
    output: Option<String>,
    changes: crate::backend::ProviderChangeSet,
) -> Vec<ToolUpdate> {
    use crate::backend::{TextUpdate, ToolOutcome, ToolStatus};
    let mut update = Vec::new();
    if status == ToolStatus::Running {
        update.push(ToolUpdate::Started);
    }
    if let Some(output) = output {
        update.push(ToolUpdate::Output {
            update: TextUpdate::Replace(output),
        });
    }
    if !changes.is_empty() {
        update.push(ToolUpdate::Changes { changes });
    }
    let outcome = match status {
        ToolStatus::Running => None,
        ToolStatus::Succeeded => Some(ToolOutcome::Succeeded),
        ToolStatus::Failed => Some(ToolOutcome::Failed {
            diagnostic: crate::backend::ToolDiagnostic {
                code: None,
                message: "tool execution failed".into(),
            },
        }),
        ToolStatus::Denied => Some(ToolOutcome::Denied {
            reason: "tool execution denied".into(),
        }),
        ToolStatus::Cancelled => Some(ToolOutcome::Cancelled),
        ToolStatus::Interrupted => Some(ToolOutcome::Interrupted),
    };
    if let Some(outcome) = outcome {
        update.push(ToolUpdate::Finished { outcome });
    }
    update
}

#[derive(Serialize, Deserialize)]
pub(crate) struct TestTurn {
    exchange: Exchange,
}

impl TestTurn {
    pub(crate) fn new(id: String, address: TurnAddress, now_ms: i64) -> Self {
        let mut exchange: Exchange = serde_json::from_value(serde_json::json!({
            "id":address.scope.exchange_id, "session_id":address.scope.owner.session_id,
            "agent_id":address.scope.owner.agent_id, "ordinal":1,
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

    pub(crate) fn record_tool(&mut self, id: &str, update: ToolUpdate, now_ms: i64) -> Result<()> {
        let address = crate::backend::ToolAddress {
            turn: self.exchange.turn[0].provider().clone(),
            tool_id: id.into(),
        };
        self.exchange.turn[0].admit_tool(id)?;
        self.synchronize()?;
        let turn_id = self.exchange.turn[0].id().to_owned();
        if !matches!(update, ToolUpdate::Requested { .. })
            && !self
                .exchange
                .tools_for_turn(&turn_id)
                .any(|tool| tool.id == id)
        {
            self.exchange.record_tool_event(
                &turn_id,
                &address,
                &ToolUpdate::Requested {
                    invocation: ToolInvocation {
                        kind: crate::backend::ToolActivityKind::ToolCall,
                        name: id.into(),
                        title: id.into(),
                        arguments: serde_json::json!({}),
                    },
                },
                now_ms,
            )?;
        }
        self.exchange
            .record_tool_event(&turn_id, &address, &update, now_ms)
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

pub(crate) fn tool_value_mut<'value>(
    value: &'value mut serde_json::Value,
    id: &str,
) -> &'value mut serde_json::Value {
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
