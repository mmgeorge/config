use crate::TurnAddress;
use serde::{Deserialize, Serialize};
use std::{fmt, time::Duration};

/// Identify a retry-safe failure class without exposing native codes or payloads.
#[derive(Clone, Copy, Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub enum RetryCategory {
    RateLimited,
    TemporaryCapacity,
    TemporaryTransport,
}

/// Current retry attempt within one admitted provider request.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ProviderRetry {
    /// One-based retry number, excluding the initial attempt.
    pub attempt: usize,
    /// Maximum retries for this request.
    pub max_attempts: usize,
    /// Deadline for the delay, absent once the retry is running.
    pub retry_at_ms: Option<i64>,
}

/// Retain canonical native permission to retry after shared delivery and effect checks.
#[derive(Clone, Debug)]
pub struct RetryableFailure {
    address: TurnAddress,
    category: RetryCategory,
    result: crate::TurnResult,
}

impl RetryableFailure {
    /// Create retry authorization only from a retained failed turn that explicitly permits it.
    pub fn new(
        address: TurnAddress,
        result: &crate::TurnResult,
        category: RetryCategory,
    ) -> Result<Self, crate::ProviderError> {
        if address.thread_id != address.scope.owner.thread_id()
            || address.scope.turn_id().as_ref() != Ok(&address.turn_id)
        {
            return Err(crate::ProviderFailure::new(
                crate::ProviderOperation::TurnStart,
                crate::ProviderFailureKind::WrongInvocation,
            )
            .into());
        }
        match result.outcome() {
            crate::turn::TurnOutcome::Failed {
                retry: crate::turn::RetryDecision::Allowed { .. },
                ..
            } => Ok(Self {
                address,
                category,
                result: result.clone(),
            }),
            crate::turn::TurnOutcome::Completed { .. }
            | crate::turn::TurnOutcome::Interrupted
            | crate::turn::TurnOutcome::Failed {
                retry: crate::turn::RetryDecision::NotAllowed,
                ..
            } => Err(crate::ProviderFailure::new(
                crate::ProviderOperation::TurnStart,
                crate::ProviderFailureKind::Rejected,
            )
            .into()),
        }
    }
    /// Get the original completed turn that remains in native history.
    pub fn address(&self) -> &TurnAddress {
        &self.address
    }
    /// Get the normalized transient failure category.
    pub fn category(&self) -> RetryCategory {
        self.category
    }
    /// Get the provider-required delay from the retained canonical decision.
    pub fn retry_after(&self) -> Option<Duration> {
        match self.result.outcome() {
            crate::turn::TurnOutcome::Failed {
                retry: crate::turn::RetryDecision::Allowed { retry_after },
                ..
            } => retry_after,
            crate::turn::TurnOutcome::Completed { .. }
            | crate::turn::TurnOutcome::Interrupted
            | crate::turn::TurnOutcome::Failed {
                retry: crate::turn::RetryDecision::NotAllowed,
                ..
            } => unreachable!("retry authorization constructor retained an allowed failed turn"),
        }
    }
    /// Get the original native terminal evidence used to authorize this retry.
    pub fn result(&self) -> &crate::TurnResult {
        &self.result
    }
}

impl fmt::Display for RetryableFailure {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "provider failure is retryable ({:?})",
            self.category
        )
    }
}

impl std::error::Error for RetryableFailure {}
