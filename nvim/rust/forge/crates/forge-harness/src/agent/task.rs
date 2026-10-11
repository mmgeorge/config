use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Distinguishes root identity, model delegation, and explicit user admission.
pub enum AgentSource {
    /// Owns the visible session and its workflow controls.
    Root,
    /// Requires automatic delivery to the unfinished parent exchange.
    Model,
    /// Publishes results without independently scheduling the root.
    User,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Records whether an informational message still needs a provider input boundary.
pub enum MessageDelivery {
    /// Retains information for the next explicit task.
    Queued,
    /// Confirms native steering accepted the information.
    Steered,
    /// Includes the information in an admitted task's provider input.
    Attached,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Preserves one ordered informational message independently of child task cancellation.
pub struct AgentMessage {
    /// Identifies the message independently of its text.
    pub id: String,
    /// Preserves arrival order within one reusable agent inbox.
    pub ordinal: u64,
    /// Retains the caller's supplied information.
    pub message: String,
    /// Reports the confirmed delivery boundary.
    pub delivery: MessageDelivery,
    /// Records the task that accepted this message, when applicable.
    pub task_id: Option<String>,
    /// Records durable admission time.
    pub created_at_ms: i64,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Separates one child task's execution and parent delivery from the reusable identity.
pub struct AgentTask {
    /// Identifies the child exchange that owns this task's complete timeline.
    pub id: String,
    /// Restricts the task to one visible root session.
    pub session_id: String,
    /// Identifies the reusable child conversation.
    pub agent_id: String,
    /// Retains the authenticated admission identity for duplicate delivery recovery.
    pub operation_id: String,
    /// Preserves task ordering within one child.
    pub ordinal: u64,
    /// Distinguishes automatic parent delivery from user-only execution.
    pub source: AgentSource,
    /// Identifies the causal primary exchange, including manual admission history.
    pub parent_exchange_id: String,
    /// Retains the explicit task without copying parent conversation history.
    pub message: String,
    /// Uses the same durable lifecycle as the owning exchange.
    pub state: crate::exchange::ExchangeState,
    /// Retains the provider's final response separately from its complete transcript.
    pub final_response: Option<String>,
    /// Reports launch, provider, or cleanup failure without silently retrying work.
    pub error: Option<String>,
    /// Prevents replayed completion from scheduling duplicate parent synthesis.
    pub parent_reported: bool,
    /// Records admission before provider dispatch.
    pub created_at_ms: i64,
    #[serde(default)]
    /// Last accepted provider activity for this run, unchanged by inspection.
    pub last_activity_at_ms: Option<i64>,
    /// Records confirmed completion after required cleanup settles.
    pub completed_at_ms: Option<i64>,
}
