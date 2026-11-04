use crate::TurnAddress;
use serde::{Deserialize, Serialize};
use std::fmt;

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

/// Adapter proof that a terminal transient failure produced no replay-sensitive output.
#[derive(Clone, Debug)]
pub struct RetryableFailure {
    /// Completed native turn that retains the original request in provider history.
    pub address: TurnAddress,
    /// Normalized retry class selected by the provider adapter.
    pub category: RetryCategory,
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
