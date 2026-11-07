use super::{CommentarySource, Exchange, ExchangeEntry, ExchangeNode};
use crate::turn::{Message, ToolCall, ToolState, Turn, TurnItem};
use anyhow::{Context, Result};
use std::collections::HashMap;

pub(crate) struct ThoughtCall<'source> {
    pub id: &'source str,
    pub turn: &'source Turn,
    pub tool: &'source ToolCall,
}

pub(crate) struct ThoughtGroup<'source> {
    pub call: Vec<ThoughtCall<'source>>,
    pub live: bool,
}

pub(crate) struct CommentaryView<'source> {
    pub id: String,
    pub message: Option<&'source Message>,
    pub tools: ThoughtGroup<'source>,
    pub writes: ThoughtGroup<'source>,
    pub checks: &'source [crate::checks::CheckRun],
}

impl CommentaryView<'_> {
    pub fn has_activity(&self) -> bool {
        !self.tools.call.is_empty() || !self.writes.call.is_empty() || !self.checks.is_empty()
    }
}

pub(crate) enum EntryPart<'source> {
    Commentary(usize),
    Event(Vec<&'source ExchangeNode>),
}

pub(crate) struct ExchangeContentView<'source> {
    pub commentary: Vec<CommentaryView<'source>>,
    pub part: Vec<EntryPart<'source>>,
}

impl Exchange {
    /// Resolves visible references from persisted commentary ownership.
    /// Missing message, turn, or activity references fail instead of dropping content.
    pub(crate) fn content<'source>(
        &'source self,
        nodes: &[&'source ExchangeNode],
        include_initial: bool,
    ) -> Result<ExchangeContentView<'source>> {
        let visible: HashMap<_, _> = nodes
            .iter()
            .enumerate()
            .map(|(position, node)| (node.id(), (position, *node)))
            .collect();
        let active = self
            .entry
            .iter()
            .rev()
            .find(|entry| match entry {
                ExchangeEntry::Commentary { commentary } => self.commentary_is_eligible(commentary)
                    || matches!(&commentary.source, CommentarySource::Provider { turn_id, message_id }
                        if self.turn.iter().find(|turn| turn.id() == turn_id)
                            .and_then(|turn| turn.messages().iter().find(|message| message.id() == message_id))
                            .is_some_and(|message| message.role() == Some(forge_provider_api::AgentMessageRole::Response))),
                ExchangeEntry::Response { .. } => true,
                ExchangeEntry::Event { id } => self.node_list.iter().any(|node| {
                    node.id() == id
                        && matches!(
                            node,
                            ExchangeNode::QuestionPresented { .. }
                                | ExchangeNode::ExchangeInput { .. }
                        )
                }),
                ExchangeEntry::Steering { .. } => false,
            })
            .filter(|entry| matches!(entry, ExchangeEntry::Commentary { commentary }
                if self.commentary_is_eligible(commentary)))
            .map(ExchangeEntry::id);
        // A parked, finalizing, or finished exchange runs nothing, so only a running execution
        // clock keeps the newest group open; otherwise its label would report stale running tools.
        let executing = !self.is_finished() && self.execution_started_at_ms().is_some();
        let mut content = ExchangeContentView {
            commentary: Vec::new(),
            part: Vec::new(),
        };
        let mut ordered = Vec::new();
        for entry in &self.entry {
            let ExchangeEntry::Commentary { commentary } = entry else {
                continue;
            };
            let position = if entry.anchor() == self.id && include_initial {
                0
            } else if let Some((position, _)) = visible.get(entry.anchor()) {
                *position
            } else {
                continue;
            };
            let message = match &commentary.source {
                CommentarySource::Initial { .. } => None,
                CommentarySource::Provider {
                    turn_id,
                    message_id,
                } => Some(
                    self.turn
                        .iter()
                        .find(|turn| turn.id() == turn_id)
                        .and_then(|turn| {
                            turn.messages()
                                .iter()
                                .find(|message| message.id() == message_id)
                        })
                        .context("commentary references a missing provider message")?,
                )
                .filter(|message| {
                    message.role() == Some(forge_provider_api::AgentMessageRole::Commentary)
                        && !message.text().trim().is_empty()
                }),
            };
            if message.is_none() && commentary.tool.is_empty() && commentary.check.is_empty() {
                continue;
            }
            let live = executing && active == Some(commentary.id.as_str());
            let mut view = CommentaryView {
                id: if matches!(commentary.source, CommentarySource::Provider { .. })
                    && message.is_none()
                {
                    format!("{}:activity", commentary.id)
                } else {
                    commentary.id.clone()
                },
                message,
                checks: &commentary.check,
                tools: ThoughtGroup {
                    call: Vec::new(),
                    live,
                },
                writes: ThoughtGroup {
                    call: Vec::new(),
                    live,
                },
            };
            for tools in &commentary.tool {
                let turn = self
                    .turn
                    .iter()
                    .find(|turn| turn.id() == tools.turn_id)
                    .context("commentary references a missing turn")?;
                for tool in tools.store.iter() {
                    let node = nodes.iter().find(|node| matches!(node, ExchangeNode::TurnContent {
                        turn_id, item: TurnItem::Tool { id }, .. } if turn_id == &tools.turn_id && id == &tool.id));
                    let Some(node) = node else { continue };
                    let hidden_question = tool
                        .title
                        .split('(')
                        .next()
                        .unwrap_or("")
                        .trim()
                        .ends_with("question_ask")
                        && self
                            .node_list
                            .iter()
                            .any(|node| matches!(node, ExchangeNode::QuestionPresented { .. }));
                    let hidden_agent = tool
                        .title
                        .split('(')
                        .next()
                        .unwrap_or("")
                        .trim()
                        .ends_with("agent_spawn")
                        && self.node_list.iter().any(|node| {
                            matches!(node, ExchangeNode::AgentReference { agent }
                            if agent.parent_turn_id.as_deref() == Some(turn.id()))
                        });
                    if tool.state() == ToolState::Completed
                        && !tool.failed
                        && (hidden_question || hidden_agent)
                    {
                        continue;
                    }
                    let group = if tool.kind == "file_change" {
                        &mut view.writes
                    } else {
                        &mut view.tools
                    };
                    group.live |= executing && tool.state() == ToolState::Running;
                    group.call.push(ThoughtCall {
                        id: node.id(),
                        turn,
                        tool,
                    });
                }
            }
            let index = content.commentary.len();
            content.commentary.push(view);
            ordered.push((position, EntryPart::Commentary(index)));
        }
        for (position, node) in nodes.iter().enumerate() {
            if let ExchangeNode::TurnContent { turn_id, item, .. } = node {
                match item {
                    TurnItem::Tool { .. } => continue,
                    TurnItem::Message { id } => {
                        let message = self
                            .turn
                            .iter()
                            .find(|turn| turn.id() == turn_id)
                            .and_then(|turn| {
                                turn.messages().iter().find(|message| message.id() == id)
                            })
                            .context("timeline references a missing provider message")?;
                        // Reasoning has no agent role, so delivery, not role, separates
                        // outer response content from private or commentary text.
                        if !matches!(
                            message.delivery(),
                            crate::turn::MessageDelivery::Final
                                | crate::turn::MessageDelivery::Unknown
                        ) || message.text().trim().is_empty()
                        {
                            continue;
                        }
                    }
                }
            }
            ordered.push((position, EntryPart::Event(vec![*node])));
        }
        ordered.sort_by_key(|(position, _)| *position);
        content.part = ordered.into_iter().map(|(_, part)| part).collect();
        Ok(content)
    }
}

#[cfg(test)]
mod tests {}
