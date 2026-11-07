use super::*;
use crate::provider::ScopeOwner;
use crate::{
    exchange::{Exchange, ExchangeNode},
    provider::{InvocationScope, ProviderEvent, ThreadOwner, TurnAddress},
};
use anyhow::Result;
use forge_provider_api::{
    AgentMessageRole, AgentMessageStart, ItemCompletion, ItemEvent, ItemId, ItemStart,
    MessageClassification, ToolCompletion, ToolItemEvent,
};
use serde::{Deserialize, Serialize};

/// Create a fixture address with distinct checked normalized identities.
pub(crate) fn address(exchange: &Exchange, turn_id: &str) -> TurnAddress {
    let scope = InvocationScope {
        thread_id: forge_provider_api::ThreadId::from(
            ThreadOwner::new(
                exchange.session_id.clone().into(),
                forge_provider_api::agent::AgentId::new(exchange.agent_id.clone()).unwrap(),
            )
            .expect("fixture owner is registered"),
        ),
        exchange_id: exchange.id.clone().into(),
        invocation_id: format!("{}:invocation:{turn_id}", exchange.id).into(),
    };
    TurnAddress {
        thread_id: scope.owner().thread_id(),
        turn_id: scope.turn_id().unwrap(),
        scope,
    }
}

/// Collect one real retained terminal fixture and preserve its original host scope.
pub(crate) fn completed_event(
    address: &TurnAddress,
    outcome: TurnOutcome,
    completed_at_ms: i64,
) -> crate::provider::events::HarnessEvent {
    let result =
        forge_provider_api::TurnResult::new(outcome, completed_at_ms, None, vec![], vec![], None)
            .unwrap();
    let completed =
        crate::provider::test_support::completed_turn(address.scope.clone(), result).unwrap();
    crate::provider::test_support::native_event(
        crate::provider::EventScope::Invocation {
            invocation: address.scope.clone(),
        },
        ProviderEvent::Turn {
            thread_id: address.thread_id.clone(),
            turn_id: address.turn_id.clone(),
            event: forge_provider_api::TurnEvent::Completed(completed),
        },
    )
}

/// Preserve original ownership for a native item fixture.
pub(crate) fn item_event(
    address: &TurnAddress,
    id: impl Into<String>,
    event: ItemEvent,
) -> crate::provider::events::HarnessEvent {
    crate::provider::test_support::native_event(
        crate::provider::EventScope::Invocation {
            invocation: address.scope.clone(),
        },
        ProviderEvent::Item {
            thread_id: address.thread_id.clone(),
            turn_id: address.turn_id.clone(),
            item_id: ItemId::new(id).unwrap(),
            event,
        },
    )
}

/// Preserve identified native accounting under its original invocation scope.
pub(crate) fn usage_event(
    address: &TurnAddress,
    report: forge_provider_api::TurnUsageReport,
) -> crate::provider::events::HarnessEvent {
    crate::provider::test_support::native_event(
        crate::provider::EventScope::Invocation {
            invocation: address.scope.clone(),
        },
        ProviderEvent::Turn {
            thread_id: address.thread_id.clone(),
            turn_id: address.turn_id.clone(),
            event: forge_provider_api::TurnEvent::Usage(report),
        },
    )
}
/// Start an explicitly identified native reasoning representation.
pub(crate) fn reasoning(
    address: &TurnAddress,
    id: impl Into<String>,
    kind: forge_provider_api::ReasoningKind,
    text: impl Into<String>,
) -> crate::provider::events::HarnessEvent {
    item_event(
        address,
        id,
        ItemEvent::Started(ItemStart::Reasoning(forge_provider_api::ReasoningStart {
            text: text.into(),
            kind,
        })),
    )
}
/// Admit a real typed assistant item under its original turn.
pub(crate) fn message(
    turn: &TurnAddress,
    id: impl Into<String>,
    role: Option<AgentMessageRole>,
    text: impl Into<String>,
) -> crate::provider::events::HarnessEvent {
    crate::provider::test_support::native_event(
        crate::provider::EventScope::Invocation {
            invocation: turn.scope.clone(),
        },
        ProviderEvent::Item {
            thread_id: turn.thread_id.clone(),
            turn_id: turn.turn_id.clone(),
            item_id: ItemId::new(id).unwrap(),
            event: ItemEvent::Started(ItemStart::AgentMessage(AgentMessageStart {
                text: text.into(),
                classification: role.map_or(
                    MessageClassification::Unclassified,
                    MessageClassification::Known,
                ),
            })),
        },
    )
}

/// Observe an admitted typed tool item followed by its ordered activity.
pub(crate) fn tool(
    exchange: &mut Exchange,
    turn: &TurnAddress,
    id: &str,
    start: ItemStart,
    events: impl IntoIterator<Item = ItemEvent>,
    now_ms: i64,
) -> Result<()> {
    let item_id = ItemId::new(id)?;
    for event in std::iter::once(ItemEvent::Started(start)).chain(events) {
        exchange.observe_event(
            &crate::provider::test_support::native_event(
                crate::provider::EventScope::Invocation {
                    invocation: turn.scope.clone(),
                },
                ProviderEvent::Item {
                    thread_id: turn.thread_id.clone(),
                    turn_id: turn.turn_id.clone(),
                    item_id: item_id.clone(),
                    event,
                },
            ),
            now_ms,
        )?;
    }
    Ok(())
}

/// Create typed fixture activity with a completion matching the admitted item family.
pub(crate) fn tool_updates(
    start: &ItemStart,
    status: crate::provider::ToolStatus,
    output: Option<String>,
    changes: crate::provider::ProviderChangeSet,
) -> Vec<ItemEvent> {
    use crate::provider::{ToolOutcome, ToolStatus};
    let mut events = Vec::new();
    if status == ToolStatus::Running {
        events.push(ItemEvent::Tool {
            event: ToolItemEvent::ExecutionStarted,
        });
    }
    if let Some(output) = output {
        events.push(ItemEvent::Tool {
            event: ToolItemEvent::Output(forge_provider_api::TextUpdate::Replace(output)),
        });
    }
    if !changes.is_empty() {
        events.push(ItemEvent::Tool {
            event: ToolItemEvent::Changes(changes),
        });
    }
    let outcome = match status {
        ToolStatus::Running => return events,
        ToolStatus::Succeeded => ToolOutcome::Succeeded,
        ToolStatus::Failed => ToolOutcome::Failed {
            diagnostic: crate::provider::ToolDiagnostic {
                kind: forge_provider_api::ProviderFailureKind::Unknown,
                message: forge_provider_api::SanitizedMessage::new("Tool execution failed")
                    .unwrap(),
            },
        },
        ToolStatus::Denied => ToolOutcome::Denied {
            reason: forge_provider_api::SanitizedMessage::new("Tool execution denied").unwrap(),
        },
        ToolStatus::Cancelled => ToolOutcome::Cancelled,
        ToolStatus::Interrupted => ToolOutcome::Interrupted,
    };
    let completion = ToolCompletion {
        outcome,
        descriptor: None,
        output: None,
        changes: Default::default(),
    };
    let completion = match start {
        ItemStart::CommandExecution(_) => ItemCompletion::CommandExecution(completion),
        ItemStart::FileChange(_) => ItemCompletion::FileChange(completion),
        ItemStart::ToolCall(_) => ItemCompletion::ToolCall(completion),
        ItemStart::UserMessage(_) | ItemStart::AgentMessage(_) | ItemStart::Reasoning(_) => {
            panic!("tool fixture requires a tool item start")
        }
    };
    events.push(ItemEvent::Completed(completion));
    events
}

#[derive(Serialize, Deserialize)]
pub(crate) struct TestTurn {
    exchange: Exchange,
}

impl TestTurn {
    pub(crate) fn new(id: String, address: TurnAddress, now_ms: i64) -> Self {
        let mut exchange: Exchange = serde_json::from_value(serde_json::json!({
            "id":address.scope.exchange_id, "session_id":address.scope.owner().session_id(),
            "agent_id":address.scope.owner().agent_id(), "ordinal":1,
            "prompt":"Work", "state":"running", "created_at_ms":now_ms,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        // Production exchanges start their active clock at admission; tool timers read it.
        exchange.clock.resume(now_ms);
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

    /// Apply a native item fixture through the same reducer as production observations.
    pub(crate) fn record_tool(&mut self, id: &str, event: &ItemEvent, now_ms: i64) -> Result<()> {
        let address = self.exchange.turn[0].provider().clone();
        self.exchange.observe_event(
            &crate::provider::test_support::native_event(
                crate::provider::EventScope::Invocation {
                    invocation: address.scope.clone(),
                },
                ProviderEvent::Item {
                    thread_id: address.thread_id,
                    turn_id: address.turn_id,
                    item_id: ItemId::new(id)?,
                    event: event.clone(),
                },
            ),
            now_ms,
        )?;
        Ok(())
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

/// Reopen one serialized tool as running, keeping its display row and native outcome consistent.
pub(crate) fn reopen_tool_value(value: &mut serde_json::Value, id: &str) {
    let entries = value["entry"].as_array_mut().expect("exchange entries");
    for entry in entries {
        if entry["kind"] != "commentary" {
            continue;
        }
        for group in entry["commentary"]["tool"]
            .as_array_mut()
            .expect("commentary tools")
        {
            let store = &mut group["store"];
            let Some(tool) = store["item"]
                .as_object_mut()
                .expect("tool store")
                .get_mut(id)
            else {
                continue;
            };
            tool["status"] = serde_json::json!("running");
            tool["state"] = serde_json::json!("running");
            tool["failed"] = serde_json::json!(false);
            tool["completed_at_ms"] = serde_json::Value::Null;
            tool["completed_active_ms"] = serde_json::Value::Null;
            let execution = &mut store["execution"][id];
            execution["outcome"] = serde_json::Value::Null;
            execution["final_snapshot"] = serde_json::Value::Null;
            return;
        }
    }
    panic!("serialized commentary has no tool {id}");
}
