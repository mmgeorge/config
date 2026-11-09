//! Scoped lifecycle and history-operation evidence shared by adapters and the host owner.

use crate::{
    CheckpointId, InvocationScope, ProviderSessionId, RecoveryToken, SessionBinding,
    SessionConfiguration, SubmissionId,
};
use serde::{Deserialize, Serialize};

macro_rules! operation_identity {
    ($name:ident, $description:literal, $failure:ident) => {
        #[doc = $description]
        #[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
        #[serde(try_from = "String", into = "String")]
        pub struct $name(String);
        impl $name {
            /// Create a nonempty opaque identity before dispatch.
            pub fn new(value: impl Into<String>) -> Result<Self, crate::lifecycle::LifecycleError> {
                let value = value.into();
                if value.trim().is_empty() {
                    return Err(crate::lifecycle::LifecycleError::$failure);
                }
                Ok(Self(value))
            }
            /// Get the opaque token for persistence and safe correlation.
            pub fn as_str(&self) -> &str {
                &self.0
            }
        }
        impl TryFrom<String> for $name {
            type Error = crate::lifecycle::LifecycleError;
            /// Validate an identity decoded from persistent or wire storage.
            fn try_from(value: String) -> Result<Self, Self::Error> {
                Self::new(value)
            }
        }
        impl From<$name> for String {
            /// Encode the validated opaque identity without interpreting it.
            fn from(value: $name) -> Self {
                value.0
            }
        }
    };
}

operation_identity!(
    OperationId,
    "Identify one host-issued provider operation; reuse it only to observe its retained request and disposition.",
    EmptyOperation
);
operation_identity!(
    DeliveryAttemptId,
    "Identify one host-issued delivery attempt scoped to a logical submission; a safe retry allocates a new attempt.",
    EmptyDeliveryAttempt
);

/// Identify one published position in a binding's monotonically ordered event stream.
/// Zero is the initial boundary before any event. This is independent of manifest schema revisions.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(transparent)]
pub struct EventSequence(u64);
impl EventSequence {
    /// Create a published stream position from its transport representation.
    pub const fn new(value: u64) -> Self {
        Self(value)
    }
    /// Get the event position for comparison and transport.
    pub const fn get(self) -> u64 {
        self.0
    }
    /// Advance one event without wrapping an exhausted identity space.
    pub fn successor(self) -> Option<Self> {
        self.0.checked_add(1).map(Self)
    }
}

/// Own an atomically opened binding, initial effective support, and its sole event stream.
/// This runtime result is not a serialized session record and cannot be cloned or replayed
/// twice. Consume the stream before submitting; the adapter retains native resource ownership
/// across dropped open awaiters until close or shutdown collects it.
pub struct OpenedSession {
    /// Identify live generation and durable history separately.
    pub binding: SessionBinding,
    /// Retain negotiated binding-specific support before first admission.
    pub capabilities: crate::capability::CapabilityManifest,
    /// Consume this stream before submitting any input.
    pub events: crate::ProviderEventStream,
}

/// Own confirmed complete defaults and the matching effective support snapshot.
/// The host commits these values only after consuming the binding stream through watermark;
/// stream closure before that point is observation failure, not successful configuration.
#[derive(Clone, Debug)]
pub struct ConfiguredSession {
    /// Retain exactly the confirmed applied configuration.
    pub configuration: SessionConfiguration,
    /// Retain the matching effective capability snapshot.
    pub capabilities: crate::capability::CapabilityManifest,
    /// Consume through this stream boundary before committing dependent transitions.
    pub watermark: EventSequence,
}

/// Target cancellation at one exact invocation, including an already completed one.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CancelRequest {
    /// Identify installed cancellation and collection ownership; never select the successor.
    pub scope: InvocationScope,
}

/// Confirm interruption observed the original turn's collection and stream boundary.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct TurnInterruptResult {
    /// Identify the retained interruption operation.
    pub operation_id: OperationId,
    /// Identify the original host conversation.
    pub thread_id: crate::ThreadId,
    /// Identify the original host response cycle.
    pub turn_id: crate::TurnId,
    /// Consume through this published boundary before dependent transitions.
    pub watermark: EventSequence,
}

/// Own positively correlated native compaction evidence and its published stream boundary.
/// This result does not arise from an acknowledgement alone. Consume through watermark before
/// committing dependent history transitions; the request context identifies this operation.
#[derive(Clone, Debug)]
pub struct CompactionResult {
    /// Preserve positively correlated native compaction evidence.
    pub compaction: crate::compaction::Compaction,
    /// Consume through this boundary before treating the history operation as complete.
    pub watermark: EventSequence,
}

/// Own the exact durable delivery query, independent of any surviving live binding.
/// Submission and attempt IDs originate in Harness; tokens and receipts originate in the
/// adapter. These values authorize observation only and cannot authorize resending input.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InputReconcileRequest {
    /// Identify the original logical journal entry.
    pub submission_id: SubmissionId,
    /// Identify the original delivery attempt, never a new resend.
    pub attempt_id: DeliveryAttemptId,
    /// Scope recovery evidence to the originally retained durable history when known.
    pub provider_session_id: Option<ProviderSessionId>,
    /// Supply adapter-issued sending evidence without interpreting its bytes.
    pub token: Option<RecoveryToken>,
    /// Supply already accepted receipts to avoid ambiguous repeated-input matches.
    pub accepted_receipts: Vec<RecoveryToken>,
}

/// Authorize history-only retraction after the host checks absence of replay-sensitive output.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RetractionAuthorization {
    /// The owner checked output eligibility and authorizes restoring this exact draft.
    OutputFreeCancellation,
}

/// Own host authorization for one exact cancelled input and checked native boundary.
/// The enclosing OperationContext supplies the sole mutation ID. Reuse that complete context
/// and request when observing an uncertain result; a new operation ID is not permission to
/// repeat a possible rollback. Neither this request nor successful retraction undoes tool effects.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InputRetractRequest {
    /// Identify the settled invocation containing the cancelled submission.
    pub scope: InvocationScope,
    /// Identify the sole history input eligible for removal.
    pub submission_id: SubmissionId,
    /// Identify the delivery attempt whose evidence established the boundary.
    pub attempt_id: DeliveryAttemptId,
    /// Require this exact native history position, never the newest item by default.
    pub boundary: CheckpointId,
    /// Carry explicit host authorization; cancellation alone does not authorize removal.
    pub authorization: RetractionAuthorization,
}

/// Distinguish safe history evidence from uncertainty without claiming tool-effect rollback.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum InputRetractOutcome {
    /// Prove the exact cancelled submission was never consumed; no history mutation occurred.
    NeverConsumed,
    /// Prove the exact authorized history item was removed at the requested boundary.
    Retracted {
        /// Exact provider-issued boundary at which the authorized history item was removed.
        boundary: CheckpointId,
    },
    /// Explain why the exact requested history mutation is unavailable.
    Unavailable(InputRetractUnavailable),
    /// Retain a possible native mutation; forbid draft restoration and automatic blind rollback.
    Unknown,
}

/// Discard every settled native turn after one provider-issued checkpoint.
///
/// `after` names the checkpoint a completed turn reported, the same identity native fork uses.
/// `None` discards the whole conversation. The request is idempotent: when nothing follows the
/// checkpoint, the adapter succeeds without mutation, so the host may repeat an uncertain rewind
/// with the same request. Rewind never restores files or other tool effects.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ThreadRewindRequest {
    /// Retain history through this checkpoint and remove every later turn.
    pub after: Option<CheckpointId>,
}

/// Classify unavailable retraction without parsing adapter diagnostics.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InputRetractUnavailable {
    /// The adapter has no authoritative native history retraction mechanism.
    Unsupported,
    /// The requested invocation does not own the cancelled input.
    WrongInvocation,
    /// The requested immutable history position is no longer current.
    HistoryBoundaryChanged,
    /// Native terminal evidence does not establish cancellation eligibility.
    NotCancelled,
}
