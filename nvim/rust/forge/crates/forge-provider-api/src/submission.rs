use crate::ProviderError;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// Identify the native history boundary committed before input is sent.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(tag = "provider", rename_all = "snake_case")]
pub enum SubmissionTarget {
    /// Correlate a Codex client message with its thread.
    Codex { thread_id: String },
    /// Retain the Copilot history cursor preceding this exact rendered prompt.
    Copilot {
        session_id: String,
        cursor: String,
        prompt: String,
    },
    /// Retain an ACP request whose protocol has no client-assigned message ID.
    Acp { session_id: String, method: String },
    /// Identify a provider without native history reconciliation.
    Untracked,
}

/// Commit host-owned delivery evidence around a native provider send.
#[async_trait]
pub trait InputReceipt: Send + Sync {
    /// Get the host-generated identity used for native input correlation.
    fn id(&self) -> &str;

    /// Commit an uncertain send before the provider receives input.
    async fn record_sending(self: Arc<Self>, target: SubmissionTarget)
    -> Result<(), ProviderError>;

    /// Commit native evidence that the provider accepted input.
    async fn record_accepted(self: Arc<Self>, receipt: String) -> Result<(), ProviderError>;

    /// Record an explicit native rejection that proves input was not accepted.
    async fn record_rejected(self: Arc<Self>, message: String) -> Result<(), ProviderError>;

    /// Record a failure without treating missing acknowledgment as rejection.
    async fn record_failure(self: Arc<Self>, message: String) -> Result<(), ProviderError>;
}
