use crate::plan::PlanElicitation;
use serde::{Deserialize, Serialize};

mod change;
mod node;
mod task;
mod timeline;

pub use change::ProviderChangeIndex;
pub use node::{
    ActiveWait, AgentReference, InteractionNode, MainSegment, SegmentState, SteeringPrompt,
};
pub use task::{TaskItem, TaskSnapshot, TaskTracker};
pub use timeline::{
    ActiveThought, ActiveThoughtUpdate, CompletedThought, CompletedTool, TimelineReducer,
    TimelineTransition,
};

/// Defines the timeline role of one admitted interaction.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum InteractionKind {
    #[default]
    Chat,
    PlanDraft,
    PlanRevision,
    PlanExecution,
}

/// Represents one admitted user action and every backend turn it caused.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InteractionRecord {
    pub id: String,
    pub session_id: String,
    pub ordinal: u64,
    pub prompt: String,
    #[serde(default)]
    pub kind: InteractionKind,
    #[serde(default)]
    pub plan_id: Option<String>,
    #[serde(default)]
    pub execution_id: Option<String>,
    pub state: InteractionState,
    pub checkpoint_before: Option<String>,
    pub checkpoint_after: Option<String>,
    pub attributed_diff_text: Option<String>,
    pub checkpoint_diff_text: Option<String>,
    pub attributed_matches_checkpoint: bool,
    pub created_at_ms: i64,
    pub completed_at_ms: Option<i64>,
    pub node_list: Vec<InteractionNode>,
    #[serde(default)]
    pub awaiting_input: bool,
    #[serde(default)]
    pub elicitation: Option<PlanElicitation>,
    #[serde(default)]
    pub duration_ms: u64,
    #[serde(default)]
    pub token_count: Option<u64>,
    #[serde(default)]
    pub comment: Vec<InteractionComment>,
    #[serde(default)]
    pub task: Option<TaskSnapshot>,
}

impl InteractionRecord {
    /// Start or return the running main-agent segment for this interaction.
    pub fn ensure_running_segment(&mut self, now_ms: i64) -> &mut MainSegment {
        let running = self.node_list.last().is_some_and(|node| {
            matches!(
                node,
                InteractionNode::MainSegment { segment }
                    if segment.state == SegmentState::Running
            )
        });
        if !running {
            let segment_ordinal = self
                .node_list
                .iter()
                .filter(|node| matches!(node, InteractionNode::MainSegment { .. }))
                .count()
                + 1;
            self.node_list.push(InteractionNode::MainSegment {
                segment: Box::new(MainSegment::running(
                    format!("{}:segment:{segment_ordinal}", self.id),
                    now_ms,
                )),
            });
        }
        match self.node_list.last_mut() {
            Some(InteractionNode::MainSegment { segment }) => segment,
            _ => unreachable!("running segment must be the final interaction node"),
        }
    }

    /// Return the current running main-agent segment when one exists.
    pub fn running_segment_mut(&mut self) -> Option<&mut MainSegment> {
        match self.node_list.last_mut() {
            Some(InteractionNode::MainSegment { segment })
                if segment.state == SegmentState::Running =>
            {
                Some(segment)
            }
            _ => None,
        }
    }

    /// Complete the running segment without creating a historical wait event.
    pub fn complete_running_segment(&mut self, now_ms: i64) {
        if let Some(segment) = self.running_segment_mut() {
            segment.complete(now_ms);
        }
    }

    /// Append one child reference only at its first observed spawn position.
    pub fn append_agent_reference(&mut self, run_id: &str, now_ms: i64) -> bool {
        if self.node_list.iter().any(|node| {
            matches!(
                node,
                InteractionNode::AgentReference { agent }
                    if agent.agent_run_id == run_id
            )
        }) {
            return false;
        }
        self.node_list.push(InteractionNode::AgentReference {
            agent: AgentReference {
                id: format!("{}:agent:{run_id}", self.id),
                agent_run_id: run_id.to_owned(),
                created_at_ms: now_ms,
            },
        });
        true
    }

    /// Append one acknowledged steering prompt at the current timeline tail.
    pub fn append_steering_prompt(&mut self, text: String, now_ms: i64) -> SteeringPrompt {
        let prompt_ordinal = self
            .node_list
            .iter()
            .filter(|node| matches!(node, InteractionNode::SteeringPrompt { .. }))
            .count()
            + 1;
        let prompt = SteeringPrompt {
            id: format!("{}:steering:{prompt_ordinal}", self.id),
            text,
            created_at_ms: now_ms,
        };
        self.node_list.push(InteractionNode::SteeringPrompt {
            prompt: prompt.clone(),
        });
        prompt
    }
}

/// Represents whether an interaction can still receive backend turns.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum InteractionState {
    Queued,
    Running,
    Complete,
    Failed,
    Cancelled,
    RolledBack,
    Superseded,
}

/// Represents one review comment anchored to an interaction diff.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InteractionComment {
    pub id: String,
    pub interaction_id: String,
    pub file_path: String,
    pub old_line: Option<u32>,
    pub new_line: Option<u32>,
    pub body: String,
    pub created_at_ms: i64,
}

#[cfg(test)]
mod test {
    use super::*;

    fn interaction() -> InteractionRecord {
        InteractionRecord {
            id: "interaction-one".into(),
            session_id: "session-one".into(),
            ordinal: 1,
            prompt: "inspect the repository".into(),
            kind: InteractionKind::Chat,
            plan_id: None,
            execution_id: None,
            state: InteractionState::Running,
            checkpoint_before: None,
            checkpoint_after: None,
            attributed_diff_text: None,
            checkpoint_diff_text: None,
            attributed_matches_checkpoint: false,
            created_at_ms: 1,
            completed_at_ms: None,
            node_list: Vec::new(),
            awaiting_input: false,
            elicitation: None,
            duration_ms: 0,
            token_count: None,
            comment: Vec::new(),
            task: None,
        }
    }

    #[test]
    fn preserves_segment_agent_and_steering_order_without_duplicate_agents() {
        let mut interaction = interaction();
        interaction.ensure_running_segment(10);
        interaction.complete_running_segment(20);
        assert!(interaction.append_agent_reference("agent-one", 21));
        assert!(!interaction.append_agent_reference("agent-one", 22));
        interaction.append_steering_prompt("also inspect tests".into(), 23);
        interaction.ensure_running_segment(24);

        let kind_list = interaction
            .node_list
            .iter()
            .map(|node| match node {
                InteractionNode::MainSegment { .. } => "segment",
                InteractionNode::AgentReference { .. } => "agent",
                InteractionNode::SteeringPrompt { .. } => "steering",
            })
            .collect::<Vec<_>>();
        assert_eq!(kind_list, ["segment", "agent", "steering", "segment"]);
    }
}
