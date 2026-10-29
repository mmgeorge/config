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
    pub id: &'source str,
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
                ExchangeEntry::Commentary { .. } | ExchangeEntry::Response { .. } => true,
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
            .filter(|entry| matches!(entry, ExchangeEntry::Commentary { .. }))
            .map(ExchangeEntry::id);
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
                ),
            };
            let live = !self.is_finished() && active == Some(commentary.id.as_str());
            let mut view = CommentaryView {
                id: &commentary.id,
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
                    group.live |= !self.is_finished() && tool.state() == ToolState::Running;
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
                        if message.kind() != crate::turn::MessageKind::Assistant
                            || message.delivery() != crate::turn::MessageDelivery::Final
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
            checklist_update: None,
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
        let content = restored.content(&restored.node_list.iter().collect::<Vec<_>>(), true)?;
        assert_eq!(content.commentary.len(), 1);
        assert_eq!(
            content.commentary[0].message.unwrap().text(),
            "Inspect files"
        );
        let owner = &content.commentary[0];
        assert_eq!(owner.tools.call.len(), 2);
        assert_eq!(owner.tools.call[0].tool.id, "before");
        assert_eq!(owner.tools.call[0].tool.state(), ToolState::Completed);
        assert_eq!(owner.tools.call[1].tool.id, "after");
        assert!(owner.tools.live);
        assert!(matches!(content.part[1], EntryPart::Event(_)));
        assert_eq!(restored.node_list.len(), 4);
        Ok(())
    }

    #[test]
    fn consecutive_steering_keeps_tools_and_edits_under_the_acknowledgement() -> Result<()> {
        use crate::backend::{ProviderChangeKind, ProviderChangeSet, ProviderFileChange};
        use crate::buffer::projection::{ProjectionSource, project};
        use forge_buffer::node::NodeKind;
        use forge_buffer::width::WidthProfile;

        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"steering", "session_id":"session", "agent_id":"primary", "ordinal":1,
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
            kind: "assistant_message".into(),
            text: Some("Initial work".into()),
            data: json!({"provider_message_id":"first","phase":"commentary"}),
            activity: None,
            summary: None,
            checklist_update: None,
        };
        exchange.observe_turn(&event, 1)?;
        event.kind = "tool".into();
        event.text = None;
        event.activity = Some(ToolActivity {
            id: "earlier".into(),
            kind: ToolActivityKind::Command,
            title: "earlier".into(),
            output: None,
            output_delta: false,
            status: Some("completed".into()),
            change: Default::default(),
        });
        exchange.observe_turn(&event, 2)?;
        exchange.append_input(InputIntent::Steering, "Run checks".into(), 3)?;
        event.kind = "assistant_message".into();
        event.text = Some("I will run the relevant checks".into());
        event.activity = None;
        event.data = json!({"provider_message_id":"ack","phase":"commentary"});
        exchange.observe_turn(&event, 4)?;
        let acknowledgement = exchange.node_list.last().unwrap().id().to_owned();
        exchange.append_input(InputIntent::Steering, "That was temporary".into(), 5)?;
        event.kind = "tool".into();
        event.text = None;
        event.data = serde_json::Value::Null;
        for index in 0..6 {
            event.activity = Some(ToolActivity {
                id: format!("command-{index}"),
                kind: ToolActivityKind::Command,
                title: format!("inspect {index}"),
                output: None,
                output_delta: false,
                status: Some("completed".into()),
                change: Default::default(),
            });
            exchange.observe_turn(&event, index + 6)?;
        }
        event.activity = Some(ToolActivity {
            id: "write".into(),
            kind: ToolActivityKind::FileChange,
            title: "edit".into(),
            output: None,
            output_delta: false,
            status: Some("completed".into()),
            change: ProviderChangeSet {
                file: (0..9)
                    .map(|index| ProviderFileChange {
                        path: format!("file-{index}.rs"),
                        move_path: None,
                        kind: ProviderChangeKind::Add,
                        diff: format!("@@ -0,0 +1 @@\n+file {index}\n"),
                    })
                    .collect(),
            },
        });
        exchange.observe_turn(&event, 12)?;
        let running = exchange.clone();
        event.kind = "assistant_message".into();
        event.text = Some("Admission now reads committed state".into());
        event.activity = None;
        event.data = json!({"provider_message_id":"next","phase":"commentary"});
        exchange.observe_turn(&event, 13)?;
        let settled_group = exchange.clone();
        exchange.finish(super::super::ExchangeOutcome::Cancelled, 14)?;
        let reopened: Exchange = serde_json::from_value(serde_json::to_value(&exchange)?)?;

        for source in [&running, &settled_group, &reopened] {
            let content = source.content(&source.node_list.iter().collect::<Vec<_>>(), true)?;
            let owner = content
                .commentary
                .iter()
                .find(|thought| thought.id == acknowledgement)
                .unwrap();
            assert_eq!(owner.tools.call.len(), 6);
            assert_eq!(owner.writes.call.len(), 1);
            for columns in [100, 160] {
                let projected = project(
                    ProjectionSource::Exchange(source),
                    &WidthProfile {
                        columns,
                        ..Default::default()
                    },
                    false,
                    &Default::default(),
                    &Default::default(),
                    &mut Default::default(),
                )?;
                let blocks = &projected.entry.block;
                let heading = blocks
                    .iter()
                    .find(|block| block.id.0 == acknowledgement)
                    .unwrap();
                assert_eq!(
                    heading.metadata.node.as_ref().unwrap().kind,
                    NodeKind::Thought
                );
                for label in ["6 tools", "Reported edits"] {
                    let group = blocks
                        .iter()
                        .find(|block| block.text.wire_rows().join("").contains(label))
                        .unwrap();
                    assert_eq!(
                        group
                            .metadata
                            .node
                            .as_ref()
                            .unwrap()
                            .parent
                            .as_ref()
                            .unwrap()
                            .0,
                        acknowledgement
                    );
                }
                let position = |label: &str| {
                    blocks
                        .iter()
                        .position(|block| block.text.wire_rows().join("").contains(label))
                        .unwrap()
                };
                assert!(position("Run checks") < position("I will run"));
                assert!(position("I will run") < position("That was temporary"));
                assert!(position("6 tools") < position("That was temporary"));
            }
        }
        Ok(())
    }
    #[test]
    fn only_steering_within_commentary_preserves_the_active_owner() -> Result<()> {
        for intent in [
            InputIntent::Steering,
            InputIntent::Clarification,
            InputIntent::Answer,
        ] {
            for phase in ["commentary", "final_answer"] {
                let mut exchange: Exchange = serde_json::from_value(json!({
                    "id":"boundary", "session_id":"session", "agent_id":"primary", "ordinal":1,
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
                    kind: "assistant_message".into(),
                    text: Some("Existing message".into()),
                    data: json!({"phase":phase}),
                    activity: None,
                    summary: None,
                    checklist_update: None,
                };
                exchange.observe_turn(&event, 1)?;
                exchange.append_input(intent, "New input".into(), 2)?;
                event.kind = "tool".into();
                event.text = None;
                event.activity = Some(ToolActivity {
                    id: "later".into(),
                    kind: ToolActivityKind::Command,
                    title: "later".into(),
                    output: None,
                    output_delta: false,
                    status: Some("completed".into()),
                    change: Default::default(),
                });
                exchange.observe_turn(&event, 3)?;
                let content =
                    exchange.content(&exchange.node_list.iter().collect::<Vec<_>>(), true)?;
                let retained = intent == InputIntent::Steering && phase == "commentary";
                assert_eq!(
                    content.commentary.len(),
                    if phase == "commentary" && !retained {
                        2
                    } else {
                        1
                    }
                );
                if !retained && phase == "commentary" {
                    assert!(content.commentary.last().unwrap().message.is_none());
                    assert!(matches!(content.part[1], EntryPart::Event(_)));
                }
            }
        }
        Ok(())
    }
}
