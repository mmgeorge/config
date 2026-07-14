use super::{ActiveThoughtUpdate, CompletedThought};
use serde::{Deserialize, Serialize};

/// Represents one durable item at a fixed position in an interaction timeline.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum InteractionNode {
    MainSegment { segment: MainSegment },
    AgentReference { agent: AgentReference },
    SteeringPrompt { prompt: SteeringPrompt },
}

impl InteractionNode {
    pub fn id(&self) -> &str {
        match self {
            Self::MainSegment { segment } => &segment.id,
            Self::AgentReference { agent } => &agent.id,
            Self::SteeringPrompt { prompt } => &prompt.id,
        }
    }
}

/// Defines whether one main-agent segment still accepts streamed provider events.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SegmentState {
    Running,
    Complete,
}

/// Owns one uninterrupted stretch of main-agent work and its terminal response.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct MainSegment {
    pub id: String,
    pub state: SegmentState,
    pub started_at_ms: i64,
    pub completed_at_ms: Option<i64>,
    pub duration_ms: u64,
    pub token_count: Option<u64>,
    pub spawned_agent_count: u64,
    pub thought: Vec<CompletedThought>,
    pub active: Option<ActiveThoughtUpdate>,
    pub response: Option<String>,
}

impl MainSegment {
    pub fn running(id: String, started_at_ms: i64) -> Self {
        Self {
            id,
            state: SegmentState::Running,
            started_at_ms,
            completed_at_ms: None,
            duration_ms: 0,
            token_count: None,
            spawned_agent_count: 0,
            thought: Vec::new(),
            active: None,
            response: None,
        }
    }

    pub fn complete(&mut self, now_ms: i64) {
        self.state = SegmentState::Complete;
        self.completed_at_ms = Some(now_ms);
        self.duration_ms = now_ms.saturating_sub(self.started_at_ms) as u64;
        self.active = None;
    }
}

/// Retains one child-agent row at the position where the child first spawned.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AgentReference {
    pub id: String,
    pub agent_run_id: String,
    pub created_at_ms: i64,
}

/// Represents one acknowledged steering prompt inside the active interaction.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct SteeringPrompt {
    pub id: String,
    pub text: String,
    pub created_at_ms: i64,
}

/// Tracks transient waiting chrome without adding it to durable history.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ActiveWait {
    pub interaction_id: String,
    pub started_at_ms: i64,
    pub agent_count: usize,
}
