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

impl TryFrom<&str> for AgentId {
    type Error = AgentIdError;
    /// Validate a borrowed child identity at admission.
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl std::str::FromStr for AgentId {
    type Err = AgentIdError;
    /// Parse an opaque identity through the same checked admission constructor.
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Self::new(value)
    }
}

impl std::fmt::Display for AgentId {
    /// Render an opaque child identity at a presentation boundary.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(formatter)
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

/// Preserve valid active and terminal child lifecycle payloads.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum AgentLifecycleEvent {
    /// Register a new child with its initial active state.
    Started {
        agent: AgentDescription,
        state: AgentActiveState,
    },
    /// Refine metadata or active state for a registered child.
    Updated {
        agent: AgentDescription,
        state: AgentActiveState,
    },
    /// Preserve a terminal child outcome without inventing cleanup evidence.
    Finished {
        agent: AgentDescription,
        outcome: AgentOutcome,
    },
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

/// Identify the existing parent turn that initiated a child delegation.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct AgentDelegation {
    /// Identify the parent conversation.
    pub parent_thread_id: crate::ThreadId,
    /// Identify the original delegating turn.
    pub parent_turn_id: crate::TurnId,
    /// Identify an observed delegating item when available.
    pub parent_item_id: Option<crate::ItemId>,
}

/// Describe child metadata independently of the enclosing event's child identity.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct AgentDescription {
    /// Preserve an observed parent agent relationship.
    pub parent_agent_id: Option<AgentId>,
    /// Attribute delegation only to its original known parent turn.
    pub delegation: Option<AgentDelegation>,
    /// Identify the child's own normalized conversation when known.
    pub thread_id: Option<crate::ThreadId>,
    /// Preserve the normalized agent definition when available.
    pub definition: Option<String>,
    /// Preserve the user-visible child label when available.
    pub nickname: Option<String>,
    /// Preserve the observed task summary when available.
    pub task: Option<String>,
}

/// Associate an agent with its own conversation and native lifecycle.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum AgentEvent {
    /// Associate this agent with its own normalized conversation.
    ConversationResolved { thread_id: crate::ThreadId },
    /// Preserve a typed active or terminal lifecycle observation.
    Lifecycle { event: AgentLifecycleEvent },
}

/// Report an active child with live work or a resumable wait.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AgentActiveState {
    /// Initialize the child before native work begins.
    Starting,
    /// Perform native child work.
    Running,
    /// Wait for input or another dependency without finishing.
    Waiting,
}

/// Report an observed child end without implying resource collection.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AgentOutcome {
    /// Preserve normal native completion.
    Completed,
    /// Preserve the classified native failure.
    Failed(crate::ProviderFailure),
    /// Preserve a stop without normal completion.
    Interrupted,
    /// Preserve explicit child closure.
    Closed,
}
