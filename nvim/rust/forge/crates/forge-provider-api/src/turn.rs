use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// The immutable outcome of one provider execution.
pub enum TurnOutcome {
    /// The provider finished its execution normally.
    Completed,
    /// The provider reported an unrecoverable execution error.
    Failed,
    /// The user cancelled execution.
    Cancelled,
    /// Execution stopped without a normal provider completion.
    Interrupted,
}
