use crate::TurnAddress;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::fmt;

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
#[derive(Debug)]
pub struct RetryableFailure {
    /// Completed native turn that retains the original request in provider history.
    pub address: TurnAddress,
    /// Structured provider error code used by the adapter's allowlist.
    pub code: String,
    /// Complete provider error payload retained in diagnostic logs.
    pub detail: Value,
    /// Original error displayed when retries stop.
    pub message: String,
}

impl fmt::Display for RetryableFailure {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.message)
    }
}

impl std::error::Error for RetryableFailure {}
