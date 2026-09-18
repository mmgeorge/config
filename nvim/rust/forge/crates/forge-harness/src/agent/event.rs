use super::AgentExecutionState;
use serde::{Deserialize, Serialize};

/// Represents one provider child-agent lifecycle update.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AgentLifecycleEvent {
    pub operation: String,
    #[serde(default)]
    pub starts_child: bool,
    pub parent_thread_id: Option<String>,
    pub provider_thread_id: Option<String>,
    pub turn_id: Option<String>,
    pub definition: Option<String>,
    pub nickname: Option<String>,
    pub task: Option<String>,
    pub status: AgentExecutionState,
}
