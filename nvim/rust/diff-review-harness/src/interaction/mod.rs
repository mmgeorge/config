use crate::plan::PlanElicitation;
use serde::{Deserialize, Serialize};

mod task;
mod timeline;

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
    pub diff_text: Option<String>,
    pub created_at_ms: i64,
    pub completed_at_ms: Option<i64>,
    #[serde(default)]
    pub thought: Vec<CompletedThought>,
    #[serde(default)]
    pub response: Option<String>,
    #[serde(default)]
    pub awaiting_input: bool,
    #[serde(default)]
    pub elicitation: Option<PlanElicitation>,
    #[serde(default)]
    pub duration_ms: u64,
    #[serde(default)]
    pub token_count: Option<u64>,
    #[serde(default = "default_true")]
    pub attribution_complete: bool,
    #[serde(default)]
    pub comment: Vec<InteractionComment>,
    #[serde(default)]
    pub task: Option<TaskSnapshot>,
}

fn default_true() -> bool {
    true
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
