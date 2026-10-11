use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
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
