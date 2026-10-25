use std::collections::HashMap;

use anyhow::{Context, Result};

use super::{Exchange, ExchangeNode};
use crate::turn::{Message, MessageKind, ToolCall, ToolState, Turn, TurnItem};

/// Classifies the two activity groups owned by one thought.
#[derive(Clone, Copy)]
pub(crate) enum ThoughtGroupKind {
    Tools,
    Writes,
}

/// Borrows an activity and its original provider-turn identity.
pub(crate) struct ThoughtCall<'source> {
    pub id: &'source str,
    pub node: &'source ExchangeNode,
    pub turn: &'source Turn,
    pub tool: &'source ToolCall,
}

/// Owns ordered activity references while the exchange owns their streamed content.
pub(crate) struct ThoughtGroup<'source> {
    pub kind: ThoughtGroupKind,
    pub call: Vec<ThoughtCall<'source>>,
    pub live: bool,
}

/// Associates one visible assistant thought with its interleaved tools and writes.
/// Activity before the first thought has no message and uses its first activity ID.
pub(crate) struct ExchangeThought<'source> {
    pub id: &'source str,
    pub message: Option<&'source Message>,
    pub events: Vec<&'source ExchangeNode>,
    pub tools: ThoughtGroup<'source>,
    pub writes: ThoughtGroup<'source>,
}

impl ExchangeThought<'_> {
    /// Identify thoughts that own expandable activity independently of rendered blocks.
    pub fn has_activity(&self) -> bool {
        !self.events.is_empty() || !self.tools.call.is_empty() || !self.writes.call.is_empty()
    }

    /// Count successful logical files without counting repeat edits or failed attempts.
    pub fn changed_files(&self) -> usize {
        super::ProviderDiffBuilder::file_count(self.writes.call.iter().map(|call| call.tool))
    }
}

/// Preserves user-input boundaries between owned thought sections.
pub(crate) enum ThoughtPart<'source> {
    Thought(usize),
    Boundary(Vec<&'source ExchangeNode>),
}

/// Resolves thought ownership from the canonical ordered events of one exchange branch.
/// References always expose current streamed content without a second persisted copy.
pub(crate) struct ThoughtContent<'source> {
    pub thought: Vec<ExchangeThought<'source>>,
    pub part: Vec<ThoughtPart<'source>>,
    thought_index: HashMap<&'source str, usize>,
    group_index: HashMap<&'source str, (usize, ThoughtGroupKind)>,
}

impl<'source> ThoughtContent<'source> {
    fn boundary(&mut self, node: &'source ExchangeNode) {
        if let Some(ThoughtPart::Boundary(nodes)) = self.part.last_mut() {
            nodes.push(node);
        } else {
            self.part.push(ThoughtPart::Boundary(vec![node]));
        }
    }

    /// Get the section that begins at this message or initial activity node.
    pub fn thought(&self, id: &str) -> Option<&ExchangeThought<'source>> {
        self.thought_index
            .get(id)
            .map(|index| &self.thought[*index])
    }

    /// Get a group only at its first activity, keeping its identity stable as calls arrive.
    pub fn group(&self, id: &str) -> Option<&ThoughtGroup<'source>> {
        self.group_index.get(id).map(|(index, kind)| match kind {
            ThoughtGroupKind::Tools => &self.thought[*index].tools,
            ThoughtGroupKind::Writes => &self.thought[*index].writes,
        })
    }

    fn begin(&mut self, id: &'source str, message: Option<&'source Message>) -> usize {
        let index = self.thought.len();
        self.thought.push(ExchangeThought {
            id,
            message,
            events: Vec::new(),
            tools: ThoughtGroup {
                kind: ThoughtGroupKind::Tools,
                call: Vec::new(),
                live: true,
            },
            writes: ThoughtGroup {
                kind: ThoughtGroupKind::Writes,
                call: Vec::new(),
                live: true,
            },
        });
        self.part.push(ThoughtPart::Thought(index));
        self.thought_index.insert(self.thought[index].id, index);
        index
    }

    fn close(&mut self, index: usize, complete: bool) {
        let thought = &mut self.thought[index];
        for group in [&mut thought.tools, &mut thought.writes] {
            group.live = !complete
                && group
                    .call
                    .iter()
                    .any(|call| call.tool.state() == ToolState::Running);
        }
    }
}

impl Exchange {
    /// Resolve thought sections for an ordered branch of this exchange.
    /// Messages and user input close sections. Events remain under their current thought.
    /// Missing provider references fail instead of dropping activity.
    pub(crate) fn thoughts<'source>(
        &'source self,
        nodes: &[&'source ExchangeNode],
    ) -> Result<ThoughtContent<'source>> {
        let turns: HashMap<_, _> = self.turn.iter().map(|turn| (turn.id(), turn)).collect();
        let messages: HashMap<_, _> = self
            .turn
            .iter()
            .flat_map(|turn| {
                turn.messages()
                    .iter()
                    .map(move |message| ((turn.id(), message.id()), message))
            })
            .collect();
        let tools: HashMap<_, _> = self
            .turn
            .iter()
            .flat_map(|turn| {
                turn.tools()
                    .map(move |tool| ((turn.id(), tool.id.as_str()), tool))
            })
            .collect();
        let mut content = ThoughtContent {
            thought: Vec::new(),
            part: Vec::new(),
            thought_index: HashMap::new(),
            group_index: HashMap::new(),
        };
        let mut question_turn = std::collections::HashSet::new();
        let mut preceding_turn = None;
        for node in &self.node_list {
            if let ExchangeNode::TurnContent { turn_id, .. } = node {
                preceding_turn = Some(turn_id.as_str());
            }
            let question = match node {
                ExchangeNode::QuestionPresented { .. } => true,
                ExchangeNode::PlanEvent { event } => matches!(&event.content,
                    crate::plan::PlanEventContent::Lifecycle { lifecycle, .. }
                        if lifecycle.kind == crate::plan::PlanLifecycleKind::QuestionAsked),
                _ => false,
            };
            if question && let Some(turn_id) = preceding_turn {
                question_turn.insert(turn_id);
            }
        }
        let mut active = None;
        for node in nodes {
            let input_boundary = matches!(
                node,
                ExchangeNode::ExchangeInput { .. } | ExchangeNode::QuestionPresented { .. }
            ) || matches!(node, ExchangeNode::PlanEvent { event } if matches!(&event.content,
                    crate::plan::PlanEventContent::Lifecycle { lifecycle, .. } if matches!(lifecycle.kind,
                        crate::plan::PlanLifecycleKind::QuestionAsked | crate::plan::PlanLifecycleKind::QuestionAnswered
                            | crate::plan::PlanLifecycleKind::QuestionWithdrawn)));
            if input_boundary {
                if let Some(index) = active.take() {
                    content.close(index, self.completed_at_ms.is_some());
                }
                content.boundary(node);
                continue;
            }
            match node {
                ExchangeNode::TurnContent {
                    id,
                    turn_id,
                    item: TurnItem::Message { id: message_id },
                } => {
                    let message = messages
                        .get(&(turn_id.as_str(), message_id.as_str()))
                        .context("thought references a missing provider message")?;
                    if message.kind() != MessageKind::Assistant || message.text().trim().is_empty()
                    {
                        continue;
                    }
                    if let Some(index) = active.take() {
                        content.close(index, self.completed_at_ms.is_some());
                    }
                    active = Some(content.begin(id, Some(message)));
                }
                ExchangeNode::TurnContent {
                    id,
                    turn_id,
                    item: TurnItem::Tool { id: tool_id },
                } => {
                    let turn = turns
                        .get(turn_id.as_str())
                        .context("thought references a missing provider turn")?;
                    let tool = tools
                        .get(&(turn_id.as_str(), tool_id.as_str()))
                        .context("thought references a missing provider activity")?;
                    if !tool.failed && tool.state() == ToolState::Completed {
                        let name = tool.title.split('(').next().unwrap_or("").trim();
                        if (name.ends_with("question_ask")
                            && question_turn.contains(turn_id.as_str()))
                            || (name.ends_with("agent_spawn")
                                && self.node_list.iter().any(|node| {
                                    matches!(node,
                                ExchangeNode::AgentReference { agent }
                                    if agent.parent_turn_id.as_deref() == Some(turn_id.as_str()))
                                }))
                        {
                            continue;
                        }
                    }
                    let index = *active.get_or_insert_with(|| content.begin(id, None));
                    let group = if tool.kind == "file_change" {
                        &mut content.thought[index].writes
                    } else {
                        &mut content.thought[index].tools
                    };
                    if group.call.is_empty() {
                        content.group_index.insert(id, (index, group.kind));
                    }
                    group.call.push(ThoughtCall {
                        id,
                        node,
                        turn,
                        tool,
                    });
                }
                ExchangeNode::Compaction { compaction, .. }
                    if compaction.state == crate::backend::CompactionState::Running => {}
                _ => {
                    if let Some(index) = active {
                        content.thought[index].events.push(node);
                    } else {
                        content.boundary(node);
                    }
                }
            }
        }
        if self.completed_at_ms.is_some()
            && let Some(index) = active
        {
            content.close(index, true);
        }
        Ok(content)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::{BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind};
    use crate::exchange::InputIntent;
    use serde_json::json;

    #[test]
    fn steering_preserves_late_activity_ownership_across_reload() -> Result<()> {
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"exchange", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Work", "kind":"chat", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
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
            kind: "assistant_message".into(),
            text: Some("Inspect files".into()),
            data: json!({"params":{"itemId":"thought","phase":"commentary"}}),
            activity: None,
            summary: None,
            task_update: None,
        };
        exchange.observe_turn(&event, 1)?;
        event.kind = "tool".into();
        event.text = None;
        event.data = serde_json::Value::Null;
        event.activity = Some(ToolActivity {
            id: "before".into(),
            title: "inspect".into(),
            kind: ToolActivityKind::Command,
            output: None,
            output_delta: false,
            status: Some("running".into()),
            change: Default::default(),
        });
        exchange.observe_turn(&event, 2)?;
        exchange.append_input(InputIntent::Steering, "Include tests".into(), 3)?;
        event.activity.as_mut().unwrap().id = "after".into();
        exchange.observe_turn(&event, 4)?;
        event.activity.as_mut().unwrap().id = "before".into();
        event.activity.as_mut().unwrap().status = Some("completed".into());
        event.activity.as_mut().unwrap().output = Some("Late result".into());
        exchange.observe_turn(&event, 5)?;
        exchange.observe_turn(&event, 6)?;
        let restored: Exchange = serde_json::from_value(serde_json::to_value(&exchange)?)?;
        let content = restored.thoughts(&restored.node_list.iter().collect::<Vec<_>>())?;
        assert_eq!(content.thought.len(), 2);
        assert_eq!(content.thought[0].message.unwrap().text(), "Inspect files");
        assert!(content.thought[1].message.is_none());
        assert_eq!(content.thought[0].tools.call.len(), 1);
        assert_eq!(content.thought[0].tools.call[0].tool.id, "before");
        assert_eq!(
            content.thought[0].tools.call[0].tool.state(),
            ToolState::Completed
        );
        assert!(!content.thought[0].tools.live);
        assert_eq!(content.thought[1].tools.call[0].tool.id, "after");
        assert!(content.thought[1].tools.live);
        assert_eq!(restored.node_list.len(), 4);
        Ok(())
    }
}
