use serde::{Deserialize, Serialize};

/// Records the task and causal exchange independently of the reusable child identity.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Delegation {
    pub id: String,
    pub parent_exchange_id: String,
    pub parent_turn_id: Option<String>,
    pub child_agent_id: String,
    pub child_exchange_id: String,
    pub task: String,
    pub created_at_ms: i64,
}
