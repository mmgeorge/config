use super::{Exchange, ExchangeNode, InputIntent};
use crate::turn::{MessageDelivery, MessageKind, ToolCall, ToolStore, TurnItem};
use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};

/// Orders visible exchange content independently of provider execution boundaries.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ExchangeEntry {
    Commentary { commentary: Commentary },
    Steering { id: String },
    Response { id: String },
    Event { id: String },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Owns tool records for one commentary without duplicating provider payloads.
pub struct Commentary {
    pub id: String,
    pub source: CommentarySource,
    pub tool: Vec<CommentaryTools>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum CommentarySource {
    Provider { turn_id: String, message_id: String },
    Initial { anchor: String },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CommentaryTools {
    pub turn_id: String,
    pub store: ToolStore,
}

impl ExchangeEntry {
    pub fn anchor(&self) -> &str {
        match self {
            Self::Commentary {
                commentary:
                    Commentary {
                        source: CommentarySource::Initial { anchor },
                        ..
                    },
            } => anchor,
            _ => self.id(),
        }
    }

    pub fn id(&self) -> &str {
        match self {
            Self::Commentary { commentary } => &commentary.id,
            Self::Steering { id } | Self::Response { id } | Self::Event { id } => id,
        }
    }
}

#[derive(Clone, Debug, Default)]
/// In-memory totals derived from commentary-owned tool state transitions.
pub struct ToolCounter {
    pub total: usize,
    pub completed: usize,
    pub running: usize,
    pub failed: usize,
}

impl Exchange {
    pub(crate) fn rebuild_tool_counter(&mut self) {
        let mut counter = ToolCounter::default();
        for (_, tool) in self.tools() {
            counter.total += 1;
            counter.failed += usize::from(tool.failed);
            counter.completed +=
                usize::from(tool.state() == crate::turn::ToolState::Completed && !tool.failed);
            counter.running += usize::from(tool.state() == crate::turn::ToolState::Running);
        }
        self.tool_counter = counter;
    }

    pub(crate) fn settle_tools(&mut self, now_ms: i64) {
        let finished: Vec<_> = self
            .turn
            .iter()
            .filter_map(|turn| match turn.state() {
                crate::turn::TurnState::Finished { outcome } => {
                    Some((turn.id().to_owned(), outcome))
                }
                _ => None,
            })
            .collect();
        for (turn_id, outcome) in finished {
            self.finish_tools(&turn_id, outcome, now_ms);
        }
        self.rebuild_tool_counter();
    }

    /// Integrates new timeline identities without changing existing tool ownership.
    pub(crate) fn reconcile_entries(&mut self) -> Result<()> {
        let positions: std::collections::HashMap<_, _> = self
            .node_list
            .iter()
            .enumerate()
            .map(|(position, node)| (node.id().to_owned(), position))
            .collect();
        let previous_entry_count = self.entry.len();
        self.entry
            .retain(|entry| positions.contains_key(entry.anchor()));
        if self.entry.len() != previous_entry_count {
            self.rebuild_tool_counter();
        }
        let mut known: std::collections::HashSet<_> = self
            .entry
            .iter()
            .map(|entry| entry.id().to_owned())
            .collect();
        for node in &self.node_list {
            if known.contains(node.id()) {
                continue;
            }
            let entry = match node {
                ExchangeNode::TurnContent {
                    id,
                    turn_id,
                    item: TurnItem::Message { id: message_id },
                } => {
                    let message = self
                        .turn
                        .iter()
                        .find(|turn| turn.id() == turn_id)
                        .and_then(|turn| {
                            turn.messages()
                                .iter()
                                .find(|message| message.id() == message_id)
                        })
                        .context("entry references a missing provider message")?;
                    if message.kind() != MessageKind::Assistant {
                        continue;
                    }
                    if message.delivery() == MessageDelivery::Final {
                        ExchangeEntry::Response { id: id.clone() }
                    } else {
                        ExchangeEntry::Commentary {
                            commentary: Commentary {
                                id: id.clone(),
                                source: CommentarySource::Provider {
                                    turn_id: turn_id.clone(),
                                    message_id: message_id.clone(),
                                },
                                tool: Vec::new(),
                            },
                        }
                    }
                }
                ExchangeNode::TurnContent { .. } => continue,
                ExchangeNode::ExchangeInput { prompt }
                    if prompt.intent == InputIntent::Steering =>
                {
                    ExchangeEntry::Steering {
                        id: prompt.id.clone(),
                    }
                }
                _ => ExchangeEntry::Event {
                    id: node.id().to_owned(),
                },
            };
            known.insert(entry.id().to_owned());
            self.entry.push(entry);
        }
        self.entry
            .sort_by_key(|entry| positions.get(entry.anchor()).copied());
        Ok(())
    }

    pub fn tools(&self) -> impl Iterator<Item = (&crate::turn::Turn, &ToolCall)> {
        self.entry
            .iter()
            .filter_map(|entry| match entry {
                ExchangeEntry::Commentary { commentary } => Some(commentary),
                _ => None,
            })
            .flat_map(|commentary| &commentary.tool)
            .flat_map(|tools| {
                let turn = self.turn.iter().find(|turn| turn.id() == tools.turn_id);
                tools
                    .store
                    .iter()
                    .filter_map(move |tool| turn.map(|turn| (turn, tool)))
            })
    }

    pub(crate) fn omit_tool_output(&mut self) {
        for entry in &mut self.entry {
            if let ExchangeEntry::Commentary { commentary } = entry {
                for tools in &mut commentary.tool {
                    tools.store.omit_output();
                }
            }
        }
    }

    pub(crate) fn attribute_tool(
        &mut self,
        turn_id: &str,
        tool_id: &str,
        checklist_item_id: &str,
    ) -> Result<bool> {
        ensure!(self.turn.iter().any(|turn| turn.id() == turn_id && turn.state() == crate::turn::TurnState::Running),
            "completed turn rejects attribution changes");
        Ok(self
            .tool_store_mut(turn_id, tool_id)?
            .attribute(tool_id, checklist_item_id))
    }

    pub fn tools_for_turn<'exchange>(
        &'exchange self,
        turn_id: &'exchange str,
    ) -> impl Iterator<Item = &'exchange ToolCall> {
        self.entry
            .iter()
            .filter_map(|entry| match entry {
                ExchangeEntry::Commentary { commentary } => Some(commentary),
                _ => None,
            })
            .flat_map(|commentary| &commentary.tool)
            .filter(move |tools| tools.turn_id == turn_id)
            .flat_map(|tools| tools.store.iter())
    }

    pub(crate) fn tool_store_mut(
        &mut self,
        turn_id: &str,
        tool_id: &str,
    ) -> Result<&mut ToolStore> {
        self.reconcile_entries()?;
        let owner = self.entry.iter().position(|entry| match entry {
            ExchangeEntry::Commentary { commentary } => commentary
                .tool
                .iter()
                .any(|tools| tools.turn_id == turn_id && tools.store.get(tool_id).is_some()),
            _ => false,
        });
        let owner = if let Some(owner) = owner {
            owner
        } else {
            let anchor = self
                .node_list
                .iter()
                .find(|node| {
                    matches!(node,
                ExchangeNode::TurnContent { turn_id: owner, item: TurnItem::Tool { id }, .. }
                if owner == turn_id && id == tool_id)
                })
                .context("tool has no timeline position")?
                .id()
                .to_owned();
            let position = self
                .node_list
                .iter()
                .position(|node| node.id() == anchor)
                .unwrap();
            let candidate = self.entry.iter().enumerate().rev().find(|(_, entry)| {
                self.node_list
                    .iter()
                    .position(|node| node.id() == entry.anchor())
                    .is_some_and(|entry_position| entry_position <= position)
                    && match entry {
                        ExchangeEntry::Steering { .. } => false,
                        ExchangeEntry::Event { id } => self.node_list.iter().any(|node| {
                            node.id() == id
                                && matches!(
                                    node,
                                    ExchangeNode::QuestionPresented { .. }
                                        | ExchangeNode::ExchangeInput { .. }
                                )
                        }),
                        _ => true,
                    }
            });
            if let Some((owner, ExchangeEntry::Commentary { .. })) = candidate {
                owner
            } else {
                self.entry.push(ExchangeEntry::Commentary {
                    commentary: Commentary {
                        id: format!("{anchor}:initial"),
                        source: CommentarySource::Initial { anchor },
                        tool: Vec::new(),
                    },
                });
                self.reconcile_entries()?;
                self.entry.iter().position(|entry| matches!(entry,
                    ExchangeEntry::Commentary { commentary } if matches!(commentary.source, CommentarySource::Initial { .. })
                    && entry.anchor() == self.node_list[position].id())).unwrap()
            }
        };
        let ExchangeEntry::Commentary { commentary } = &mut self.entry[owner] else {
            unreachable!()
        };
        let index = commentary
            .tool
            .iter()
            .position(|tools| tools.turn_id == turn_id)
            .unwrap_or_else(|| {
                commentary.tool.push(CommentaryTools {
                    turn_id: turn_id.to_owned(),
                    store: ToolStore::default(),
                });
                commentary.tool.len() - 1
            });
        Ok(&mut commentary.tool[index].store)
    }

    pub(crate) fn record_tool_event(
        &mut self,
        turn_id: &str,
        event: crate::backend::ToolEvent<'_>,
        now_ms: i64,
    ) -> Result<()> {
        ensure!(self.turn.iter().any(|turn| turn.id() == turn_id && turn.state() == crate::turn::TurnState::Running),
            "completed turn rejects tool events");
        let activity = event.activity();
        let store = self.tool_store_mut(turn_id, &activity.id)?;
        let previous = store
            .get(&activity.id)
            .map(|tool| (tool.state(), tool.failed));
        let settled = store
            .get(&activity.id)
            .is_some_and(|tool| tool.state() != crate::turn::ToolState::Running);
        store.merge(activity, event.observed_at_ms(now_ms));
        if !settled {
            store.timing(
                &activity.id,
                event.timestamp("/params/startedAtMs"),
                event.timestamp("/params/completedAtMs"),
                event.timestamp("/params/item/durationMs"),
            );
        }
        let current = store
            .get(&activity.id)
            .map(|tool| (tool.state(), tool.failed))
            .unwrap();
        if let Some((state, failed)) = previous {
            self.tool_counter.failed -= usize::from(failed);
            self.tool_counter.completed -=
                usize::from(state == crate::turn::ToolState::Completed && !failed);
            self.tool_counter.running -= usize::from(state == crate::turn::ToolState::Running);
        } else {
            self.tool_counter.total += 1;
        }
        self.tool_counter.failed += usize::from(current.1);
        self.tool_counter.completed +=
            usize::from(current.0 == crate::turn::ToolState::Completed && !current.1);
        self.tool_counter.running += usize::from(current.0 == crate::turn::ToolState::Running);
        Ok(())
    }

    pub(crate) fn finish_tools(
        &mut self,
        turn_id: &str,
        outcome: crate::turn::TurnOutcome,
        now_ms: i64,
    ) {
        for entry in &mut self.entry {
            if let ExchangeEntry::Commentary { commentary } = entry {
                for tools in &mut commentary.tool {
                    if tools.turn_id == turn_id {
                        tools.store.finish(outcome, now_ms);
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::{BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind};
    use serde_json::json;

    #[test]
    fn initial_commentary_owns_tools_once_across_steering_reload_and_late_updates() -> Result<()> {
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"ownership", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Work", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        let address = ProviderAddress {
            owner: None,
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        exchange.start_turn(address.clone(), 0)?;
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(address),
            turn_boundary: None,
            kind: "tool".into(),
            text: None,
            data: json!(null),
            activity: Some(ToolActivity {
                id: "first".into(),
                title: "inspect".into(),
                kind: ToolActivityKind::Command,
                output: None,
                output_delta: false,
                status: Some("running".into()),
                change: Default::default(),
            }),
            summary: None,
            checklist_update: None,
        };
        exchange.observe_turn(&event, 1)?;
        exchange.append_input(InputIntent::Steering, "more".into(), 2)?;
        event.activity.as_mut().unwrap().id = "second".into();
        exchange.observe_turn(&event, 3)?;
        assert_eq!(exchange.tool_counter.total, 2);
        assert_eq!(exchange.tool_counter.running, 2);
        let initial_id = exchange.entry[0].id().to_owned();
        assert!(
            matches!(&exchange.entry[0], ExchangeEntry::Commentary { commentary }
            if matches!(commentary.source, CommentarySource::Initial { .. }))
        );
        event.kind = "assistant_message".into();
        event.activity = None;
        event.text = Some("Now check results".into());
        event.data = json!({"provider_message_id":"commentary", "phase":"commentary"});
        exchange.observe_turn(&event, 4)?;
        event.kind = "tool".into();
        event.text = None;
        event.data = json!(null);
        event.activity = Some(ToolActivity {
            id: "first".into(),
            title: "inspect".into(),
            kind: ToolActivityKind::Command,
            output: Some("complete".into()),
            output_delta: false,
            status: Some("completed".into()),
            change: Default::default(),
        });
        exchange.observe_turn(&event, 5)?;
        exchange.observe_turn(&event, 6)?;
        assert_eq!(exchange.tool_counter.total, 2);
        assert_eq!(exchange.tool_counter.completed, 1);
        assert_eq!(exchange.tool_counter.running, 1);
        let serialized = serde_json::to_value(&exchange)?;
        assert!(serialized["turn"][0].get("tool").is_none());
        assert!(serialized.get("tool_counter").is_none());
        let restored: Exchange = serde_json::from_value(serialized)?;
        assert_eq!(restored.tool_counter.total, 2);
        assert_eq!(restored.tool_counter.completed, 1);
        let owner = restored
            .entry
            .iter()
            .find(|entry| entry.id() == initial_id)
            .unwrap();
        let ExchangeEntry::Commentary { commentary } = owner else {
            panic!("initial commentary missing")
        };
        assert_eq!(
            commentary
                .tool
                .iter()
                .flat_map(|group| group.store.iter())
                .count(),
            2
        );
        let view = restored.content(&restored.node_list.iter().collect::<Vec<_>>())?;
        assert_eq!(view.commentary[0].tools.call.len(), 2);
        assert!(matches!(
            view.part[1],
            super::super::thought::EntryPart::Event(_)
        ));
        let rendered = crate::buffer::projection::project(
            crate::buffer::projection::ProjectionSource::Exchange(&restored),
            &Default::default(),
            false,
            &Default::default(),
            &Default::default(),
            &mut Default::default(),
        )?;
        let blocks = &rendered.entry.block;
        let heading = blocks
            .iter()
            .find(|block| {
                block
                    .text
                    .wire_rows()
                    .join("")
                    .contains("Considering initial steps")
            })
            .unwrap();
        let steering = blocks
            .iter()
            .find(|block| block.text.wire_rows().join("").contains("Steering: more"))
            .unwrap();
        assert!(
            heading
                .metadata
                .fold
                .iter()
                .all(|fold| fold.end.block != steering.id)
        );
        event.activity.as_mut().unwrap().id = "failed-call".into();
        event.activity.as_mut().unwrap().status = Some("failed".into());
        exchange.observe_turn(&event, 7)?;
        exchange.observe_turn(&event, 8)?;
        assert_eq!(exchange.tool_counter.total, 3);
        assert_eq!(exchange.tool_counter.failed, 1);
        exchange.node_list.clear();
        exchange.reconcile_entries()?;
        assert_eq!(exchange.tool_counter.total, 0);
        Ok(())
    }
}
