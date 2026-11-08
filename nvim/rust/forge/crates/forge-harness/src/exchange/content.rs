use super::{Exchange, ExchangeNode, InputIntent};
use crate::turn::{ToolCall, ToolStore, TurnItem};
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
/// Owns activity records for one commentary without duplicating provider payloads.
pub struct Commentary {
    pub id: String,
    pub source: CommentarySource,
    pub tool: Vec<CommentaryTools>,
    #[serde(default)]
    pub check: Vec<crate::checks::CheckRun>,
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
    pub(crate) fn commentary_is_eligible(&self, commentary: &Commentary) -> bool {
        match &commentary.source {
            CommentarySource::Initial { .. } => true,
            CommentarySource::Provider {
                turn_id,
                message_id,
            } => self
                .turn
                .iter()
                .find(|turn| turn.id() == turn_id)
                .and_then(|turn| {
                    turn.messages()
                        .iter()
                        .find(|message| message.id() == message_id)
                })
                .is_some_and(|message| {
                    message.role() == Some(forge_provider_api::AgentMessageRole::Commentary)
                        && !message.text().trim().is_empty()
                }),
        }
    }

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

    /// Recover orphaned background tools without changing settled foreground execution.
    pub(crate) fn interrupt_detached_background_tools(&mut self, observed_at: i64) -> Result<bool> {
        let observed_at = self.tool_instant(observed_at);
        let mut changed = false;
        for entry in &mut self.entry {
            if let ExchangeEntry::Commentary { commentary } = entry {
                for tools in &mut commentary.tool {
                    changed |= tools.store.interrupt_detached_background(observed_at)?;
                }
            }
        }
        if changed {
            self.rebuild_tool_counter();
        }
        Ok(changed)
    }

    /// Integrates new timeline identities without changing existing tool ownership.
    pub(crate) fn reconcile_entries(&mut self) -> Result<()> {
        let legacy_checks: Vec<_> = self
            .node_list
            .iter()
            .filter_map(|node| match node {
                ExchangeNode::TimelineEvent { event } => match event.as_ref() {
                    crate::timeline::TimelineEvent::Check { run } => Some((**run).clone()),
                    _ => None,
                },
                _ => None,
            })
            .collect();
        self.node_list.retain(|node| !matches!(node,
            ExchangeNode::TimelineEvent { event } if matches!(event.as_ref(), crate::timeline::TimelineEvent::Check { .. })));
        let positions: std::collections::HashMap<_, _> = self
            .node_list
            .iter()
            .enumerate()
            .map(|(position, node)| (node.id().to_owned(), position))
            .collect();
        let previous_entry_count = self.entry.len();
        self.entry
            .retain(|entry| entry.anchor() == self.id || positions.contains_key(entry.anchor()));
        self.entry.retain_mut(|entry| {
            let Some(ExchangeNode::TurnContent {
                turn_id,
                item: TurnItem::Message { id: message_id },
                ..
            }) = self.node_list.iter().find(|node| node.id() == entry.id())
            else {
                return true;
            };
            let message = self
                .turn
                .iter()
                .find(|turn| turn.id() == turn_id)
                .and_then(|turn| {
                    turn.messages()
                        .iter()
                        .find(|message| message.id() == message_id)
                });
            let role = message.and_then(|message| message.role());
            if matches!(entry, ExchangeEntry::Commentary { commentary }
                if !commentary.tool.is_empty() || !commentary.check.is_empty())
            {
                return true;
            }
            if message.is_none_or(|message| message.text().trim().is_empty()) {
                return false;
            }
            match role {
                Some(forge_provider_api::AgentMessageRole::Commentary) => {
                    if !matches!(entry, ExchangeEntry::Commentary { .. }) {
                        *entry = ExchangeEntry::Commentary {
                            commentary: Commentary {
                                id: entry.id().to_owned(),
                                source: CommentarySource::Provider {
                                    turn_id: turn_id.clone(),
                                    message_id: message_id.clone(),
                                },
                                tool: Vec::new(),
                                check: Vec::new(),
                            },
                        };
                    }
                    true
                }
                Some(forge_provider_api::AgentMessageRole::Response) => {
                    *entry = ExchangeEntry::Response {
                        id: entry.id().to_owned(),
                    };
                    true
                }
                _ => false,
            }
        });
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
                    if message.text().trim().is_empty() {
                        continue;
                    }
                    if message.role() == Some(forge_provider_api::AgentMessageRole::Response) {
                        ExchangeEntry::Response { id: id.clone() }
                    } else if message.role()
                        == Some(forge_provider_api::AgentMessageRole::Commentary)
                    {
                        ExchangeEntry::Commentary {
                            commentary: Commentary {
                                id: id.clone(),
                                source: CommentarySource::Provider {
                                    turn_id: turn_id.clone(),
                                    message_id: message_id.clone(),
                                },
                                tool: Vec::new(),
                                check: Vec::new(),
                            },
                        }
                    } else {
                        continue;
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
        for run in legacy_checks {
            self.own_check(run)?;
        }
        Ok(())
    }

    /// Enumerates completed check records through their persisted commentary owners.
    pub fn checks(&self) -> impl Iterator<Item = &crate::checks::CheckRun> {
        self.entry
            .iter()
            .filter_map(|entry| match entry {
                ExchangeEntry::Commentary { commentary } => Some(&commentary.check),
                _ => None,
            })
            .flatten()
    }

    /// Attaches a check to the commentary present at admission, retaining replay identities.
    pub(crate) fn record_check(&mut self, run: crate::checks::CheckRun) -> Result<()> {
        run.owner.validate_exchange(self)?;
        self.reconcile_entries()?;
        self.own_check(run)
    }

    fn own_check(&mut self, run: crate::checks::CheckRun) -> Result<()> {
        run.owner.validate_exchange(self)?;
        for entry in &mut self.entry {
            if let ExchangeEntry::Commentary { commentary } = entry
                && let Some(previous) = commentary.check.iter_mut().find(|check| check.id == run.id)
            {
                ensure!(previous.owner == run.owner, "check ownership cannot change");
                *previous = run;
                return Ok(());
            }
        }
        let crate::timeline::TimelineOwner::Exchange { node_count, .. } = run.owner.as_ref() else {
            unreachable!()
        };
        let anchors: std::collections::HashSet<_> = self
            .node_list
            .iter()
            .filter(|node| crate::timeline::ownership::source_node(node))
            .take(*node_count)
            .map(ExchangeNode::id)
            .collect();
        let owner = self.entry.iter().rposition(|entry| {
            matches!(entry, ExchangeEntry::Commentary { commentary } if self.commentary_is_eligible(commentary))
                && (entry.anchor() == self.id || anchors.contains(entry.anchor()))
        });
        let owner = owner.unwrap_or_else(|| {
            self.entry.insert(
                0,
                ExchangeEntry::Commentary {
                    commentary: Commentary {
                        id: format!("{}:initial", self.id),
                        source: CommentarySource::Initial {
                            anchor: self.id.clone(),
                        },
                        tool: Vec::new(),
                        check: Vec::new(),
                    },
                },
            );
            0
        });
        let ExchangeEntry::Commentary { commentary } = &mut self.entry[owner] else {
            unreachable!()
        };
        commentary.check.push(run);
        commentary.check.sort_by(|left, right| {
            left.started_at_ms
                .cmp(&right.started_at_ms)
                .then_with(|| left.id.cmp(&right.id))
        });
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
                (self
                    .node_list
                    .iter()
                    .position(|node| node.id() == entry.anchor())
                    .is_some_and(|entry_position| entry_position <= position)
                    || entry.anchor() == self.id)
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
                        ExchangeEntry::Commentary { commentary } => self.commentary_is_eligible(commentary)
                            || matches!(&commentary.source, CommentarySource::Provider { turn_id, message_id }
                                if self.turn.iter().find(|turn| turn.id() == turn_id)
                                    .and_then(|turn| turn.messages().iter().find(|message| message.id() == message_id))
                                    .is_some_and(|message| message.role() == Some(forge_provider_api::AgentMessageRole::Response))),
                        ExchangeEntry::Response { .. } => true,
                    }
            });
            if let Some((owner, ExchangeEntry::Commentary { commentary })) = candidate
                && self.commentary_is_eligible(commentary)
            {
                owner
            } else {
                self.entry.push(ExchangeEntry::Commentary {
                    commentary: Commentary {
                        id: format!("{anchor}:initial"),
                        source: CommentarySource::Initial { anchor },
                        tool: Vec::new(),
                        check: Vec::new(),
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

    /// Admit late completion only for a positively certified background command.
    pub(crate) fn accepts_settled_tool_update(
        &self,
        address: &crate::provider::TurnAddress,
        item_id: &forge_provider_api::ItemId,
        event: &forge_provider_api::ItemEvent,
    ) -> bool {
        matches!(
            event,
            forge_provider_api::ItemEvent::Tool { .. }
                | forge_provider_api::ItemEvent::Completed(_)
        ) && self.entry.iter().any(|entry| {
            let ExchangeEntry::Commentary { commentary } = entry else {
                return false;
            };
            commentary
                .tool
                .iter()
                .any(|tools| tools.store.accepts_background_completion(address, item_id))
        })
    }

    pub(crate) fn record_tool_event(
        &mut self,
        turn_id: &str,
        address: &crate::provider::TurnAddress,
        item_id: &forge_provider_api::ItemId,
        event: &forge_provider_api::ItemEvent,
        now_ms: i64,
    ) -> Result<()> {
        let late = self.accepts_settled_tool_update(address, item_id, event);
        ensure!(
            self.turn.iter().any(|turn| turn.id() == turn_id
                && turn.provider() == address
                && (turn.state() == crate::turn::TurnState::Running || late)),
            "completed turn rejects tool events"
        );
        let tool_id = item_id.as_str();
        let now = self.tool_instant(now_ms);
        // Validate the candidate before changing timeline ownership or counters.
        let mut candidate = self.clone();
        let store = candidate.tool_store_mut(turn_id, tool_id)?;
        store.record_item(address, item_id, event, now)?;
        candidate.rebuild_tool_counter();
        *self = candidate;
        Ok(())
    }

    pub(crate) fn finish_tools(
        &mut self,
        turn_id: &str,
        outcome: crate::turn::TurnOutcome,
        now_ms: i64,
    ) {
        let now = self.tool_instant(now_ms);
        for entry in &mut self.entry {
            if let ExchangeEntry::Commentary { commentary } = entry {
                for tools in &mut commentary.tool {
                    if tools.turn_id == turn_id {
                        tools.store.finish(outcome, now);
                    }
                }
            }
        }
    }
}
