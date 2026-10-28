use std::collections::HashMap;

use anyhow::{Context, Result};

use super::{Exchange, ExchangeNode};
use crate::turn::{Message, MessageDelivery, MessageKind, ToolCall, ToolState, Turn, TurnItem};

/// Borrows an activity and its original provider-turn identity.
pub(crate) struct ThoughtCall<'source> {
    pub id: &'source str,
    pub turn: &'source Turn,
    pub tool: &'source ToolCall,
}

/// Owns ordered activity references while the exchange owns their streamed content.
pub(crate) struct ThoughtGroup<'source> {
    pub call: Vec<ThoughtCall<'source>>,
    pub live: bool,
}

/// Keeps activity on either side of steering in its chronological position.
pub(crate) enum ThoughtBody<'source> {
    Activity(ThoughtActivity<'source>),
    Steering(&'source ExchangeNode),
}

/// Borrows one uninterrupted activity group owned by a commentary.
pub(crate) struct ThoughtActivity<'source> {
    pub events: Vec<&'source ExchangeNode>,
    pub tools: ThoughtGroup<'source>,
    pub writes: ThoughtGroup<'source>,
}

impl ThoughtActivity<'_> {
    fn new() -> Self {
        Self {
            events: Vec::new(),
            tools: ThoughtGroup {
                call: Vec::new(),
                live: true,
            },
            writes: ThoughtGroup {
                call: Vec::new(),
                live: true,
            },
        }
    }
}

/// Associates one visible assistant thought with its interleaved tools and writes.
/// Activity before the first thought has no message and uses its first activity ID.
pub(crate) struct ExchangeThought<'source> {
    pub id: &'source str,
    pub message: Option<&'source Message>,
    pub body: Vec<ThoughtBody<'source>>,
}

impl<'source> ExchangeThought<'source> {
    /// Identify thoughts that own expandable activity independently of rendered blocks.
    pub fn has_activity(&self) -> bool {
        self.body.iter().any(|part| match part {
            ThoughtBody::Activity(activity) => {
                !activity.events.is_empty()
                    || !activity.tools.call.is_empty()
                    || !activity.writes.call.is_empty()
            }
            ThoughtBody::Steering(_) => true,
        })
    }

    pub fn events(&self) -> impl Iterator<Item = &&'source ExchangeNode> {
        self.body.iter().flat_map(|part| match part {
            ThoughtBody::Activity(activity) => activity.events.as_slice(),
            ThoughtBody::Steering(node) => std::slice::from_ref(node),
        })
    }

    fn activity(&mut self) -> &mut ThoughtActivity<'source> {
        if !matches!(self.body.last(), Some(ThoughtBody::Activity(_))) {
            self.body
                .push(ThoughtBody::Activity(ThoughtActivity::new()));
        }
        match self.body.last_mut().expect("thought activity was inserted") {
            ThoughtBody::Activity(activity) => activity,
            ThoughtBody::Steering(_) => unreachable!("last thought body is activity"),
        }
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
}

impl<'source> ThoughtContent<'source> {
    fn boundary(&mut self, node: &'source ExchangeNode) {
        if let Some(ThoughtPart::Boundary(nodes)) = self.part.last_mut() {
            nodes.push(node);
        } else {
            self.part.push(ThoughtPart::Boundary(vec![node]));
        }
    }

    fn begin(&mut self, id: &'source str, message: Option<&'source Message>) -> usize {
        let index = self.thought.len();
        self.thought.push(ExchangeThought {
            id,
            message,
            body: vec![ThoughtBody::Activity(ThoughtActivity::new())],
        });
        self.part.push(ThoughtPart::Thought(index));
        index
    }

    fn close(&mut self, index: usize, complete: bool) {
        let thought = &mut self.thought[index];
        if let Some(ThoughtBody::Activity(activity)) = thought.body.last_mut() {
            for group in [&mut activity.tools, &mut activity.writes] {
                group.live = !complete
                    && group
                        .call
                        .iter()
                        .any(|call| call.tool.state() == ToolState::Running);
            }
        }
    }
}

impl Exchange {
    /// Resolve thought sections for an ordered branch of this exchange.
    /// New messages and question branches close sections. Steering retains the current owner
    /// and separates its activity groups without moving events across the input.
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
        let mut active: Option<usize> = None;
        for node in nodes {
            if matches!(node, ExchangeNode::ExchangeInput { prompt } if prompt.intent == super::InputIntent::Steering)
            {
                if let Some(index) = active.filter(|index| {
                    !content.thought[*index]
                        .message
                        .is_some_and(|message| message.delivery() == MessageDelivery::Final)
                }) {
                    content.close(index, self.is_finished());
                    content.thought[index]
                        .body
                        .push(ThoughtBody::Steering(node));
                } else {
                    if let Some(index) = active.take() {
                        content.close(index, self.is_finished());
                    }
                    content.boundary(node);
                }
                continue;
            }
            let input_boundary = matches!(
                node,
                ExchangeNode::ExchangeInput { .. } | ExchangeNode::QuestionPresented { .. }
            ) || matches!(node, ExchangeNode::PlanEvent { event } if matches!(&event.content,
                    crate::plan::PlanEventContent::Lifecycle { lifecycle, .. } if matches!(lifecycle.kind,
                        crate::plan::PlanLifecycleKind::QuestionAsked | crate::plan::PlanLifecycleKind::QuestionAnswered
                            | crate::plan::PlanLifecycleKind::QuestionWithdrawn)));
            if input_boundary {
                if let Some(index) = active.take() {
                    content.close(index, self.is_finished());
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
                        content.close(index, self.is_finished());
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
                    let activity = content.thought[index].activity();
                    let group = if tool.kind == "file_change" {
                        &mut activity.writes
                    } else {
                        &mut activity.tools
                    };
                    group.call.push(ThoughtCall { id, turn, tool });
                }
                ExchangeNode::Compaction { compaction, .. }
                    if compaction.state == crate::backend::CompactionState::Running => {}
                _ => {
                    if let Some(index) = active {
                        content.thought[index].activity().events.push(node);
                    } else {
                        content.boundary(node);
                    }
                }
            }
        }
        if self.is_finished()
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
        assert_eq!(content.thought.len(), 1);
        assert_eq!(content.thought[0].message.unwrap().text(), "Inspect files");
        let [
            ThoughtBody::Activity(before),
            ThoughtBody::Steering(_),
            ThoughtBody::Activity(after),
        ] = content.thought[0].body.as_slice()
        else {
            panic!("steering must divide one owner's activity into ordered groups")
        };
        assert_eq!(before.tools.call.len(), 1);
        assert_eq!(before.tools.call[0].tool.id, "before");
        assert_eq!(before.tools.call[0].tool.state(), ToolState::Completed);
        assert!(!before.tools.live);
        assert_eq!(after.tools.call[0].tool.id, "after");
        assert!(after.tools.live);
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
            task_update: None,
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
            let content = source.thoughts(&source.node_list.iter().collect::<Vec<_>>())?;
            let owner = content
                .thought
                .iter()
                .find(|thought| thought.id == acknowledgement)
                .unwrap();
            let [
                ThoughtBody::Activity(_),
                ThoughtBody::Steering(_),
                ThoughtBody::Activity(activity),
            ] = owner.body.as_slice()
            else {
                panic!("acknowledgement lost its steering and activity")
            };
            assert_eq!(activity.tools.call.len(), 6);
            assert_eq!(activity.writes.call.len(), 1);
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
                assert!(position("That was temporary") < position("6 tools"));
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
                    task_update: None,
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
                let content = exchange.thoughts(&exchange.node_list.iter().collect::<Vec<_>>())?;
                let retained = intent == InputIntent::Steering && phase == "commentary";
                assert_eq!(content.thought.len(), if retained { 1 } else { 2 });
                if !retained {
                    assert!(content.thought[1].message.is_none());
                    assert!(matches!(content.part[1], ThoughtPart::Boundary(_)));
                }
            }
        }
        Ok(())
    }
}
