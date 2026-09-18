use serde::{Deserialize, Serialize};

/// Defines how one backend exposes and controls child agents.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AgentControlMode {
    #[default]
    Unsupported,
    ParentMediated,
    Direct,
}

/// Represents the agent operations available through one backend.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct AgentCapability {
    pub observe: bool,
    pub catalog: bool,
    pub spawn: AgentControlMode,
    pub input: AgentControlMode,
    pub interrupt: AgentControlMode,
    pub parallel: bool,
}

impl AgentCapability {
    /// Build the native Codex child-agent contract exposed through app-server.
    pub fn codex() -> Self {
        Self {
            observe: true,
            catalog: true,
            spawn: AgentControlMode::ParentMediated,
            input: AgentControlMode::ParentMediated,
            interrupt: AgentControlMode::Direct,
            parallel: true,
        }
    }
}
