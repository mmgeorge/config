use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// The immutable outcome of one provider execution.
pub enum TurnOutcome {
    /// The provider finished its execution normally.
    Completed {
        /// Preserve the native terminal stop condition.
        stop_reason: TurnCompletionReason,
    },
    /// The provider reported an unrecoverable execution error.
    Failed {
        /// Retain the normalized cause established by native terminal evidence.
        error: crate::ProviderFailure,
        /// Preserve the shared policy's explicit permission to retry.
        retry: RetryDecision,
    },
    /// Execution stopped without a normal provider completion.
    Interrupted,
}

/// Preserve native response-cycle stop conditions independently of Harness task success.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TurnCompletionReason {
    /// The provider ended without reporting a limiting condition.
    Normal,
    /// The response reached its token allowance.
    TokenLimit,
    /// The response reached its model or tool request allowance.
    RequestLimit,
    /// The provider refused the requested work.
    Refusal,
}

/// Authorize replay only after shared policy validates delivery and completed effects.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum RetryDecision {
    /// Do not automatically repeat the operation.
    NotAllowed,
    /// Permit retry while preserving any provider minimum delay.
    Allowed {
        /// Delay required before another attempt may start.
        retry_after: Option<std::time::Duration>,
    },
}

impl TurnOutcome {
    /// Permit automatic continuation only after a normal native completion.
    pub fn permits_automatic_continuation(self) -> bool {
        match self {
            Self::Completed {
                stop_reason: TurnCompletionReason::Normal,
            } => true,
            Self::Completed {
                stop_reason:
                    TurnCompletionReason::TokenLimit
                    | TurnCompletionReason::RequestLimit
                    | TurnCompletionReason::Refusal,
            }
            | Self::Failed { .. }
            | Self::Interrupted => false,
        }
    }
}

/// Record positive native start independently of input acceptance and model resolution.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TurnStarted {
    /// Timestamp at which native work was positively observed.
    pub started_at_ms: i64,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Native limits and refusal must never authorize another automatic response cycle.
    #[test]
    fn only_normal_completion_permits_automatic_continuation() {
        for reason in [
            TurnCompletionReason::Normal,
            TurnCompletionReason::TokenLimit,
            TurnCompletionReason::RequestLimit,
            TurnCompletionReason::Refusal,
        ] {
            assert_eq!(
                TurnOutcome::Completed {
                    stop_reason: reason
                }
                .permits_automatic_continuation(),
                reason == TurnCompletionReason::Normal
            );
        }
        for outcome in [
            TurnOutcome::Failed {
                error: crate::ProviderFailure::new(
                    crate::ProviderOperation::TurnStart,
                    crate::ProviderFailureKind::Unknown,
                )
                .at_phase(
                    crate::ProviderPhase::Observation,
                    crate::FailureDisposition::Uncertain,
                ),
                retry: crate::turn::RetryDecision::NotAllowed,
            },
            TurnOutcome::Interrupted,
            TurnOutcome::Interrupted,
        ] {
            assert!(!outcome.permits_automatic_continuation());
        }
    }
}
