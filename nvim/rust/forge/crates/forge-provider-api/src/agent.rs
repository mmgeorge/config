use serde::{Deserialize, Serialize};

/// Describes the observed lifecycle of one native child-agent run.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AgentExecutionState {
    Starting,
    Running,
    Waiting,
    Completed,
    Failed,
    Interrupted,
    Closed,
}

impl AgentExecutionState {
    /// Report whether the run can still receive input or interruption.
    pub fn is_active(self) -> bool {
        matches!(self, Self::Starting | Self::Running | Self::Waiting)
    }
}

/// Carries a native child-agent observation independently of the host registry.
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

#[derive(Clone, Debug, Deserialize, Serialize, Eq, PartialEq)]
/// Reports Harness-owned execution independently of native provider delegation.
pub struct AgentCapability {
    pub observe: bool,
    pub spawn: bool,
    pub input: bool,
    pub interrupt: bool,
    pub parallel: bool,
}

impl Default for AgentCapability {
    fn default() -> Self {
        Self {
            observe: true,
            spawn: true,
            input: true,
            interrupt: true,
            parallel: true,
        }
    }
}
