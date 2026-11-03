use serde::{Deserialize, Serialize};

/// Identify a child run without exposing the provider's native identifier format.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct AgentId(String);

impl AgentId {
    /// Create an opaque nonempty child identity.
    pub fn new(value: impl Into<String>) -> Result<Self, AgentIdError> {
        let value = value.into();
        if value.trim().is_empty() {
            return Err(AgentIdError);
        }
        Ok(Self(value))
    }

    /// Get the opaque child identity for equality or routing.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for AgentId {
    type Error = AgentIdError;

    /// Validate deserialized agent identities using the constructor invariant.
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl From<AgentId> for String {
    /// Encode the opaque identity for transport or persistence.
    fn from(value: AgentId) -> Self {
        value.0
    }
}

/// Reject an empty child identity.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AgentIdError;

impl std::fmt::Display for AgentIdError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("agent identity is empty")
    }
}

impl std::error::Error for AgentIdError {}

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

/// Carry one normalized child-agent lifecycle observation.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AgentLifecycleKind {
    /// Start a child execution.
    Started,
    /// Update a live child execution.
    Updated,
    /// Complete or interrupt a child execution.
    Finished,
}

/// Carry one normalized child-agent lifecycle observation.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AgentLifecycleEvent {
    /// Identify the child independently of native thread or task identifiers.
    pub agent_id: AgentId,
    /// Identify the parent child run when nested delegation is present.
    pub parent_agent_id: Option<AgentId>,
    /// Carry the opaque conversation identity used in this child's normalized turn addresses.
    /// Observed children do not acquire host-managed execution authority.
    pub conversation_id: Option<crate::ProviderSessionId>,
    /// Classify the normalized lifecycle transition.
    pub kind: AgentLifecycleKind,
    /// Identify the normalized child-agent definition when available.
    pub definition: Option<String>,
    /// Carry a user-visible child-agent label when available.
    pub nickname: Option<String>,
    /// Carry the normalized task summary when available.
    pub task: Option<String>,
    /// Report the normalized child-agent lifecycle state.
    pub state: AgentExecutionState,
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
