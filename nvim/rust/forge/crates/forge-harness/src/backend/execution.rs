use serde::{Deserialize, Serialize};

use crate::turn::TurnOutcome;

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
/// Identifies exactly one execution in one provider conversation.
pub struct ProviderAddress {
    /// Provider conversation containing the turn.
    pub thread_id: String,
    /// Provider execution identifier, never inferred from a UI segment.
    pub turn_id: String,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
/// A provider lifecycle boundary normalized before broker routing.
pub enum TurnBoundary {
    /// The provider admitted a new execution.
    Started,
    /// The provider permanently settled an execution.
    Finished {
        /// Execution outcome reported by the adapter.
        outcome: TurnOutcome,
    },
}
