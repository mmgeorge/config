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
                            .is_some_and(|message| message.role() == Some(crate::backend::MessageRole::Response))),
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
                    message.role() == Some(crate::backend::MessageRole::Commentary)
                        && !message.text().trim().is_empty()
                }),
            };
            if message.is_none() && commentary.tool.is_empty() && commentary.check.is_empty() {
                continue;
            }
            let live = !self.is_finished() && active == Some(commentary.id.as_str());
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
                        if !matches!(
                            message.role(),
                            None | Some(crate::backend::MessageRole::Response)
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
mod tests {
    use super::*;
    use crate::backend::{
        BackendEvent, MessageRole, TextUpdate, ToolActivityKind, ToolInvocation, ToolOutcome,
        ToolUpdate,
    };
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
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        let event = crate::turn::test_support::message(
            &address,
            "thought",
            Some(MessageRole::Commentary),
            "Inspect files",
        );
        exchange.observe_turn(&event, 1)?;
        let mut event = BackendEvent::Tool {
            address: crate::backend::ToolAddress {
                turn: address,
                tool_id: "before".into(),
            },
            update: ToolUpdate::Requested {
                invocation: ToolInvocation {
                    title: "inspect".into(),
                    name: "inspect".into(),
                    kind: ToolActivityKind::Command,
                    arguments: json!({}),
                },
            },
        };
        exchange.observe_turn(&event, 2)?;
        exchange.append_input(InputIntent::Steering, "Include tests".into(), 3)?;
        if let BackendEvent::Tool { address, .. } = &mut event {
            address.tool_id = "after".into();
        }
        exchange.observe_turn(&event, 4)?;
        if let BackendEvent::Tool { address, update } = &mut event {
            address.tool_id = "before".into();
            *update = ToolUpdate::Output {
                update: TextUpdate::Replace("Late result".into()),
            };
        }
        exchange.observe_turn(&event, 5)?;
        if let BackendEvent::Tool { update, .. } = &mut event {
            *update = ToolUpdate::Finished {
                outcome: ToolOutcome::Succeeded,
            };
        }
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
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        let event = crate::turn::test_support::message(
            &address,
            "first",
            Some(MessageRole::Commentary),
            "Initial work",
        );
        exchange.observe_turn(&event, 1)?;
        crate::turn::test_support::tool(
            &mut exchange,
            &address,
            "earlier",
            ToolInvocation {
                kind: ToolActivityKind::Command,
                name: "earlier".into(),
                title: "earlier".into(),
                arguments: json!({}),
            },
            [
                ToolUpdate::Started,
                ToolUpdate::Finished {
                    outcome: ToolOutcome::Succeeded,
                },
            ],
            2,
        )?;
        exchange.append_input(InputIntent::Steering, "Run checks".into(), 3)?;
        let event = crate::turn::test_support::message(
            &address,
            "ack",
            Some(MessageRole::Commentary),
            "I will run the relevant checks",
        );
        exchange.observe_turn(&event, 4)?;
        let acknowledgement = exchange.node_list.last().unwrap().id().to_owned();
        exchange.append_input(InputIntent::Steering, "That was temporary".into(), 5)?;
        for index in 0..6 {
            crate::turn::test_support::tool(
                &mut exchange,
                &address,
                &format!("command-{index}"),
                ToolInvocation {
                    kind: ToolActivityKind::Command,
                    name: "inspect".into(),
                    title: format!("inspect {index}"),
                    arguments: json!({}),
                },
                [
                    ToolUpdate::Started,
                    ToolUpdate::Finished {
                        outcome: ToolOutcome::Succeeded,
                    },
                ],
                index + 6,
            )?;
        }
        crate::turn::test_support::tool(
            &mut exchange,
            &address,
            "write",
            ToolInvocation {
                kind: ToolActivityKind::FileChange,
                name: "edit".into(),
                title: "edit".into(),
                arguments: json!({}),
            },
            [
                ToolUpdate::Started,
                ToolUpdate::Changes {
                    changes: ProviderChangeSet {
                        file: (0..9)
                            .map(|index| ProviderFileChange {
                                path: format!("file-{index}.rs"),
                                move_path: None,
                                kind: ProviderChangeKind::Add,
                                diff: format!("@@ -0,0 +1 @@\n+file {index}\n"),
                            })
                            .collect(),
                    },
                },
                ToolUpdate::Finished {
                    outcome: ToolOutcome::Succeeded,
                },
            ],
            12,
        )?;
        let running = exchange.clone();
        let event = crate::turn::test_support::message(
            &address,
            "next",
            Some(MessageRole::Commentary),
            "Admission now reads committed state",
        );
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
                let address = crate::turn::test_support::address(&exchange, "turn");
                exchange.start_turn(address.clone(), 0)?;
                let event = crate::turn::test_support::message(
                    &address,
                    "existing",
                    Some(if phase == "commentary" {
                        crate::backend::MessageRole::Commentary
                    } else {
                        crate::backend::MessageRole::Response
                    }),
                    "Existing message",
                );
                exchange.observe_turn(&event, 1)?;
                exchange.append_input(intent, "New input".into(), 2)?;
                crate::turn::test_support::tool(
                    &mut exchange,
                    &address,
                    "later",
                    ToolInvocation {
                        kind: ToolActivityKind::Command,
                        name: "later".into(),
                        title: "later".into(),
                        arguments: json!({}),
                    },
                    [
                        ToolUpdate::Started,
                        ToolUpdate::Finished {
                            outcome: ToolOutcome::Succeeded,
                        },
                    ],
                    3,
                )?;
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
