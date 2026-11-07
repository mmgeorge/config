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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::provider::{EventScope, HarnessEvent, ProviderEvent, TurnAddress};
    use forge_provider_api::{
        AgentMessageRole, ItemCompletion, ItemEvent, ItemId, ItemStart, MessageClassification,
        MessageCompletion, MessageItemEvent, MessageOutcome, ToolCompletion, ToolDescriptor,
        ToolItemEvent, ToolOutcome,
    };
    use serde_json::json;

    fn exchange(id: &str) -> Exchange {
        serde_json::from_value(json!({"id":id,"session_id":"session","agent_id":"primary","ordinal":1,
            "prompt":"Work","state":"running","created_at_ms":0,"attributed_matches_checkpoint":false,"node_list":[]})).unwrap()
    }
    fn item(address: &TurnAddress, id: &str, event: ItemEvent) -> HarnessEvent {
        HarnessEvent::Native {
            source: crate::provider::events::NativeEventSource {
                binding_id: forge_provider_api::BindingId::new("content-reducer-fixture").unwrap(),
                sequence: forge_provider_api::EventSequence::new(1),
            },
            scope: EventScope::Invocation {
                invocation: address.scope.clone(),
            },
            event: Box::new(ProviderEvent::Item {
                thread_id: address.thread_id.clone(),
                turn_id: address.turn_id.clone(),
                item_id: ItemId::new(id).unwrap(),
                event,
            }),
        }
    }
    fn command(title: &str) -> ItemEvent {
        ItemEvent::Started(ItemStart::CommandExecution(ToolDescriptor {
            semantics: Default::default(),
            name: title.into(),
            title: title.into(),
            arguments: forge_provider_api::InspectableArguments::new(json!({})),
        }))
    }
    fn complete(outcome: ToolOutcome, output: Option<&str>) -> ItemEvent {
        ItemEvent::Completed(ItemCompletion::CommandExecution(ToolCompletion {
            outcome,
            descriptor: None,
            output: output.map(str::to_owned),
            changes: Default::default(),
        }))
    }
    fn message_complete(text: &str) -> ItemEvent {
        ItemEvent::Completed(ItemCompletion::AgentMessage(MessageCompletion {
            text: text.into(),
            outcome: MessageOutcome::Completed,
        }))
    }
    fn classified(role: AgentMessageRole) -> ItemEvent {
        ItemEvent::Message {
            event: MessageItemEvent::Classified(MessageClassification::Known(role)),
        }
    }

    /// Codex starts a command before asking approval; the wait pauses the Working clock,
    /// so the tool's timer must exclude it too, both while running and once settled.
    #[test]
    fn tool_timer_pauses_with_the_exchange_clock_during_approval() -> Result<()> {
        let mut exchange = exchange("approval-timer");
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        exchange.clock.resume(0);
        exchange.observe_event(&item(&address, "call", command("write")), 1_000)?;
        exchange.observe_event(
            &item(
                &address,
                "call",
                ItemEvent::Tool {
                    event: ToolItemEvent::ExecutionStarted,
                },
            ),
            1_000,
        )?;
        exchange.observe_blocker("approval:call".into(), true, 2_000);
        exchange.observe_blocker("approval:call".into(), false, 12_000);
        let running = exchange.tools().next().unwrap().1.clone();
        assert_eq!(
            running.elapsed_ms(exchange.tool_instant(12_500)),
            Some(1_500),
            "the running tool timer counted the approval wait"
        );
        exchange.observe_event(
            &item(&address, "call", complete(ToolOutcome::Succeeded, None)),
            13_000,
        )?;
        let (_, settled) = exchange.tools().next().unwrap();
        assert_eq!(
            settled.elapsed_ms(exchange.tool_instant(90_000)),
            Some(2_000)
        );
        assert_eq!(exchange.elapsed(13_000), 3_000);
        Ok(())
    }

    #[test]
    fn empty_snapshots_remove_text_without_admitting_empty_commentary() -> Result<()> {
        let mut exchange = exchange("empty");
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        let message = crate::turn::test_support::message(
            &address,
            "commentary",
            Some(AgentMessageRole::Commentary),
            "Visible",
        );
        assert!(exchange.entry.is_empty());
        assert!(exchange.turn[0].messages().is_empty());
        exchange.observe_event(&item(&address, "call", command("inspect")), 2)?;
        let owner = exchange.entry[0].id().to_owned();
        exchange.observe_event(&message, 3)?;
        assert_eq!(exchange.entry.len(), 2);
        exchange.observe_event(&item(&address, "commentary", message_complete("")), 4)?;
        let restored: Exchange = serde_json::from_value(serde_json::to_value(exchange)?)?;
        assert_eq!(restored.entry.len(), 1);
        assert_eq!(restored.entry[0].id(), owner);
        assert_eq!(restored.tools().count(), 1);
        assert!(restored.turn[0].messages()[0].text().is_empty());
        Ok(())
    }

    #[test]
    fn typed_classification_preserves_activity_without_duplicate_message_blocks() -> Result<()> {
        let mut exchange = exchange("classification");
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        exchange.observe_event(
            &crate::turn::test_support::message(&address, "answer", None, "Canonical answer"),
            1,
        )?;
        assert!(exchange.entry.is_empty());
        exchange.observe_event(
            &item(&address, "answer", classified(AgentMessageRole::Commentary)),
            2,
        )?;
        exchange.observe_event(&item(&address, "original", command("inspect")), 3)?;
        let provider_owner = exchange.entry[0].id().to_owned();
        let before_conflict = serde_json::to_value(&exchange)?;
        let error = exchange
            .observe_event(
                &item(&address, "answer", classified(AgentMessageRole::Response)),
                4,
            )
            .unwrap_err();
        assert!(
            matches!(error.downcast_ref::<crate::provider::ProviderError>(), Some(crate::provider::ProviderError::Failure(failure)) if failure.kind() == forge_provider_api::ProviderFailureKind::Protocol)
        );
        assert_eq!(serde_json::to_value(&exchange)?, before_conflict);
        let completed = item(
            &address,
            "answer",
            message_complete("Canonical final answer"),
        );
        exchange.observe_event(&completed, 4)?;
        exchange.observe_event(&completed, 5)?;
        let mut restored: Exchange = serde_json::from_value(serde_json::to_value(exchange)?)?;
        assert_eq!(restored.turn[0].messages().len(), 1);
        assert_eq!(
            restored.turn[0].messages()[0].role(),
            Some(AgentMessageRole::Commentary)
        );
        restored.observe_event(
            &crate::turn::test_support::message(
                &address,
                "next-inspection",
                Some(AgentMessageRole::Commentary),
                "Next inspection",
            ),
            6,
        )?;
        restored.observe_event(&item(&address, "later", command("inspect")), 6)?;
        let original = restored
            .entry
            .iter()
            .find(|entry| entry.id() == provider_owner)
            .unwrap();
        assert!(
            matches!(original, ExchangeEntry::Commentary { commentary } if commentary.tool[0].store.get("original").is_some() && commentary.tool[0].store.get("later").is_none())
        );
        assert_eq!(restored.tool_counter.total, 2);
        let rendered = crate::buffer::projection::project(
            crate::buffer::projection::ProjectionSource::Exchange(&restored),
            &Default::default(),
            crate::buffer::projection::FIXTURE_NOW_MS,
            false,
            &Default::default(),
            &Default::default(),
            &mut Default::default(),
        )?;
        assert_eq!(
            rendered
                .entry
                .block
                .iter()
                .filter(|block| block
                    .text
                    .wire_rows()
                    .join("")
                    .contains("Canonical final answer"))
                .count(),
            1
        );
        assert!(!rendered.entry.block.iter().any(|block| {
            block
                .text
                .wire_rows()
                .join("")
                .contains("Noncanonical mirror")
        }));
        let mut block_ids = std::collections::HashSet::new();
        assert!(
            rendered
                .entry
                .block
                .iter()
                .all(|block| block_ids.insert(&block.id))
        );
        Ok(())
    }

    #[test]
    fn initial_commentary_owns_tools_once_across_steering_reload_and_late_updates() -> Result<()> {
        let mut exchange = exchange("ownership");
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        exchange.observe_event(&item(&address, "first", command("inspect")), 1)?;
        exchange.append_input(InputIntent::Steering, "more".into(), 2)?;
        exchange.observe_event(&item(&address, "second", command("inspect")), 3)?;
        assert_eq!(exchange.tool_counter.total, 2);
        assert_eq!(exchange.tool_counter.running, 2);
        let initial_id = exchange.entry[0].id().to_owned();
        assert!(
            matches!(&exchange.entry[0], ExchangeEntry::Commentary { commentary } if matches!(commentary.source, CommentarySource::Initial { .. }))
        );
        exchange.observe_event(
            &crate::turn::test_support::message(
                &address,
                "commentary",
                Some(AgentMessageRole::Commentary),
                "Now check results",
            ),
            4,
        )?;
        for id in ["first", "second"] {
            exchange.observe_event(
                &item(
                    &address,
                    id,
                    ItemEvent::Tool {
                        event: ToolItemEvent::ExecutionStarted,
                    },
                ),
                4,
            )?;
        }
        let finished = item(&address, "first", complete(ToolOutcome::Succeeded, None));
        exchange.observe_event(&finished, 5)?;
        exchange.observe_event(&finished, 6)?;
        exchange.observe_event(
            &item(
                &address,
                "second",
                ItemEvent::Tool {
                    event: ToolItemEvent::Backgrounded,
                },
            ),
            6,
        )?;
        assert_eq!(exchange.tool_counter.completed, 1);
        assert_eq!(exchange.tool_counter.running, 1);
        let serialized = serde_json::to_value(&exchange)?;
        assert!(serialized["turn"][0].get("tool").is_none());
        assert!(serialized.get("tool_counter").is_none());
        let mut restored: Exchange = serde_json::from_value(serialized)?;
        restored.turn[0].finish(
            crate::turn::TurnOutcome::Completed {
                stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
            },
            6,
        )?;
        restored.settle_tools(6);
        // A completed foreground item cannot acquire new output; a certified background command retains its original owner.
        let before = serde_json::to_value(&restored)?;
        assert!(
            restored
                .observe_event(
                    &item(
                        &address,
                        "first",
                        ItemEvent::Tool {
                            event: ToolItemEvent::Output(crate::provider::TextUpdate::Append(
                                "foreign late output".into()
                            ))
                        }
                    ),
                    7
                )
                .is_err()
        );
        assert_eq!(serde_json::to_value(&restored)?, before);
        let node_count = restored.node_list.len();
        restored.observe_event(
            &item(
                &address,
                "second",
                ItemEvent::Tool {
                    event: ToolItemEvent::Output(crate::provider::TextUpdate::Append(
                        "late output".into(),
                    )),
                },
            ),
            7,
        )?;
        restored.observe_event(
            &item(
                &address,
                "second",
                ItemEvent::Tool {
                    event: ToolItemEvent::TitleChanged("Inspect completed results".into()),
                },
            ),
            7,
        )?;
        let terminal = item(
            &address,
            "second",
            complete(ToolOutcome::Succeeded, Some("late output")),
        );
        restored.observe_event(&terminal, 8)?;
        assert_eq!(restored.node_list.len(), node_count);
        let ExchangeEntry::Commentary { commentary } = restored
            .entry
            .iter()
            .find(|entry| entry.id() == initial_id)
            .unwrap()
        else {
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
        let updated = commentary.tool[0].store.get("second").unwrap();
        assert_eq!(updated.output, "late output");
        assert_eq!(updated.title, "Inspect completed results");
        assert_eq!(updated.state(), crate::turn::ToolState::Completed);
        assert_eq!(updated.completed_at_ms, Some(8));
        assert_eq!(restored.tool_counter.completed, 2);
        let view = restored.content(&restored.node_list.iter().collect::<Vec<_>>(), true)?;
        assert_eq!(view.commentary[0].tools.call.len(), 2);
        assert!(matches!(
            view.part[1],
            super::super::thought::EntryPart::Event(_)
        ));
        let rendered = crate::buffer::projection::project(
            crate::buffer::projection::ProjectionSource::Exchange(&restored),
            &Default::default(),
            crate::buffer::projection::FIXTURE_NOW_MS,
            false,
            &Default::default(),
            &Default::default(),
            &mut Default::default(),
        )?;
        let heading = rendered
            .entry
            .block
            .iter()
            .find(|block| {
                block
                    .text
                    .wire_rows()
                    .join("")
                    .contains("Considering initial steps")
            })
            .unwrap();
        let steering = rendered
            .entry
            .block
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
        exchange.observe_event(&item(&address, "failed-call", command("check")), 7)?;
        let failed = item(
            &address,
            "failed-call",
            complete(
                ToolOutcome::Failed {
                    diagnostic: crate::provider::ToolDiagnostic {
                        kind: forge_provider_api::ProviderFailureKind::Unknown,
                        message: forge_provider_api::SanitizedMessage::new("failed").unwrap(),
                    },
                },
                None,
            ),
        );
        exchange.observe_event(&failed, 7)?;
        exchange.observe_event(&failed, 8)?;
        assert_eq!(exchange.tool_counter.total, 3);
        assert_eq!(exchange.tool_counter.failed, 1);
        exchange.node_list.clear();
        exchange.reconcile_entries()?;
        assert_eq!(exchange.tool_counter.total, 0);
        Ok(())
    }
}
