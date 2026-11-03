use serde::{Deserialize, Serialize};

/// Identify a provider lifecycle trace event without exposing native event names.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProviderTraceEvent {
    SessionInitialized,
    SessionRecovered,
    SubmissionPrepared,
    SubmissionAccepted,
    ExecutionStarted,
    ExecutionFinished,
    ObservationLost,
    CallbackSettled,
    SessionRetired,
}

/// Carry sanitized provider activity using only normalized correlation identities.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderTraceRecord {
    /// Identify the live binding generation.
    pub binding_id: crate::BindingId,
    /// Identify the event category.
    pub event: ProviderTraceEvent,
    /// Identify the invocation when the event belongs to one.
    pub scope: Option<crate::InvocationScope>,
    /// Identify the accepted journal input when relevant.
    pub submission_id: Option<crate::SubmissionId>,
    /// Identify a normalized failure class without provider error text.
    pub failure: Option<crate::ProviderFailureKind>,
}

/// Record normalized provider lifecycle diagnostics without native payloads.
pub trait ProviderTrace: Send + Sync {
    /// Publish one sanitized record at the host tracing boundary.
    fn record(&self, record: ProviderTraceRecord);
}
