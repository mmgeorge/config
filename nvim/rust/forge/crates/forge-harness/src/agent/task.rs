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

impl AgentSource {
    /// Describe a child cancel this source requested, as the parent's report of the child states it.
    pub fn cancellation_reason(self) -> &'static str {
        match self {
            Self::Model => "Interrupted by its parent agent before it finished.",
            Self::User | Self::Root => "Cancelled by the user before it finished.",
        }
    }
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
    /// Names who interrupted a task that Harness cancelled, or `None` when nobody did.
    #[serde(default)]
    pub cancelled_by: Option<AgentSource>,
    /// Records admission before provider dispatch.
    pub created_at_ms: i64,
    #[serde(default)]
    /// Last accepted provider activity for this run, unchanged by inspection.
    pub last_activity_at_ms: Option<i64>,
    /// Records confirmed completion after required cleanup settles.
    pub completed_at_ms: Option<i64>,
}

impl AgentTask {
    /// Report whether this settled task's result wakes its waiting parent for a continuation.
    ///
    /// A child the parent's own model interrupted does not, because the model decided that
    /// during its turn and its report travels with the next input. A child the user cancelled
    /// does, so the model can report or respawn: a parent whose every child the user cancelled
    /// once settled on its last words, often a promise to report later, with nothing to wake it.
    pub fn starts_parent_continuation(&self) -> bool {
        self.state != crate::exchange::ExchangeState::Cancelled
            || self.cancelled_by != Some(AgentSource::Model)
    }

    /// Tell the model, with the next prompt after a request the user stopped, that this child
    /// was cancelled with that request, so it neither waits for the child nor reports it.
    pub const STOPPED_REQUEST_CANCELLATION: &str = "Cancelled when the user stopped the request \
        that started it. This agent will not report; do not wait for it or resume its task \
        unless the user asks.";
}

#[cfg(test)]
mod tests {
    use super::{AgentSource, AgentTask};
    use crate::exchange::ExchangeState;

    fn settled_task(state: ExchangeState, cancelled_by: Option<AgentSource>) -> AgentTask {
        AgentTask {
            id: "task".into(),
            session_id: "session".into(),
            agent_id: "agent".into(),
            operation_id: "operation".into(),
            ordinal: 1,
            source: AgentSource::Model,
            parent_exchange_id: "parent".into(),
            message: "inspect".into(),
            state,
            final_response: None,
            error: None,
            parent_reported: false,
            cancelled_by,
            created_at_ms: 1,
            last_activity_at_ms: None,
            completed_at_ms: Some(2),
        }
    }

    /// Only a cancel the parent's own model requested leaves its waiting parent asleep.
    #[test]
    fn only_a_model_interrupt_withholds_the_parent_continuation() {
        assert!(settled_task(ExchangeState::Complete, None).starts_parent_continuation());
        assert!(settled_task(ExchangeState::Failed, None).starts_parent_continuation());
        assert!(
            settled_task(ExchangeState::Cancelled, Some(AgentSource::User))
                .starts_parent_continuation()
        );
        assert!(
            !settled_task(ExchangeState::Cancelled, Some(AgentSource::Model))
                .starts_parent_continuation()
        );
    }
}
