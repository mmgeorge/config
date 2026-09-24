use crate::agent::Delegation;
use crate::plan::PlanAnnotation;
use serde::{Deserialize, Serialize};

/// Represents one durable item at a fixed position in an interaction timeline.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ExchangeNode {
    QuestionPresented {
        id: String,
        question: crate::plan::PlanQuestionSet,
        answer: Option<String>,
    },
    PlanEvent { event: Box<crate::plan::ExchangePlanEvent> },
    TurnContent {
        id: String,
        turn_id: String,
        item: crate::turn::TurnItem,
    },
    AgentReference {
        agent: Delegation,
    },
    ExchangeInput {
        prompt: ExchangeInput,
    },
    PlanCommentResolution {
        resolution: PlanCommentResolution,
    },
    ArtifactChange {
        change: ArtifactChange,
    },
}

impl ExchangeNode {
    pub fn id(&self) -> &str {
        match self {
            Self::QuestionPresented { id, .. } => id,
            Self::PlanEvent { event } => &event.id,
            Self::TurnContent { id, .. } => id,
            Self::AgentReference { agent } => &agent.id,
            Self::ExchangeInput { prompt } => &prompt.id,
            Self::PlanCommentResolution { resolution } => &resolution.id,
            Self::ArtifactChange { change } => &change.id,
        }
    }
}

/// Represents inline review comments resolved by one submitted plan revision.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanCommentResolution {
    pub id: String,
    pub annotation: Vec<PlanAnnotation>,
    pub created_at_ms: i64,
}

/// Represents one canonical artifact delta produced by an interaction.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ArtifactChange {
    pub id: String,
    pub path: String,
    pub diff_text: String,
    pub created_at_ms: i64,
}

/// Retains one child-agent row at the position where the child first spawned.

/// Distinguishes user input that extends an existing exchange.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum InputIntent {
    Steering,
    Clarification,
    Answer,
}

/// Retains acknowledged user input in exchange order.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ExchangeInput {
    pub intent: InputIntent,
    pub id: String,
    pub text: String,
    pub created_at_ms: i64,
}

/// Tracks transient waiting chrome without adding it to durable history.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ActiveWait {
    pub exchange_id: String,
    pub started_at_ms: i64,
    pub agent_count: usize,
}
