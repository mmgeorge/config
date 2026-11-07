//! Execution completion independent of native request acknowledgment and turn boundaries.

use std::fmt;

use serde::{Deserialize, Serialize};

use crate::completion::{ProviderCompletion, TurnMetrics};

/// Identifies an existing input journal record without creating another identity space.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct SubmissionId(String);

impl SubmissionId {
    /// Get the journal's original identity.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for SubmissionId {
    type Error = LifecycleError;

    /// Validate a journal identity at the boundary.
    fn try_from(value: String) -> Result<Self, Self::Error> {
        if value.trim().is_empty() {
            return Err(LifecycleError::EmptySubmission);
        }
        Ok(Self(value))
    }
}

impl TryFrom<&str> for SubmissionId {
    type Error = LifecycleError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl std::str::FromStr for SubmissionId {
    type Err = LifecycleError;

    /// Validate a borrowed identity without interpreting its opaque contents.
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        value.to_owned().try_into()
    }
}

impl From<SubmissionId> for String {
    /// Encode the original journal identity without adding a prefix.
    fn from(value: SubmissionId) -> Self {
        value.0
    }
}

/// Reject malformed lifecycle ownership before publication or after decoding.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LifecycleError {
    /// A journal identity was empty.
    EmptySubmission,
    /// A retained mutation operation identity was empty.
    EmptyOperation,
    /// A journal delivery-attempt identity was empty.
    EmptyDeliveryAttempt,
    /// A binding-scoped terminal identity was empty.
    EmptyTerminal,
    /// The execution address belongs to a different invocation.
    OwnershipMismatch,
    /// Terminal coverage repeats an input journal identity.
    DuplicateSubmission(SubmissionId),
    /// A state transition references an input that was never admitted.
    UnknownSubmission(SubmissionId),
    /// An immutable delivery result was replaced by a conflicting result.
    DeliveryConflict(SubmissionId),
    /// A native execution was not started before its terminal evidence arrived.
    UnknownExecution,
    /// A completed execution was assigned a second terminal result.
    TerminalConflict,
}

impl fmt::Display for LifecycleError {
    /// Describe the invalid lifecycle contract at a diagnostic boundary.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EmptySubmission => f.write_str("submission identity is empty"),
            Self::EmptyOperation => f.write_str("provider operation identity is empty"),
            Self::EmptyDeliveryAttempt => f.write_str("delivery attempt identity is empty"),
            Self::EmptyTerminal => f.write_str("provider terminal identity is empty"),
            Self::OwnershipMismatch => {
                f.write_str("execution address belongs to another invocation")
            }
            Self::DuplicateSubmission(id) => {
                write!(f, "terminal coverage repeats submission {}", id.as_str())
            }
            Self::UnknownSubmission(id) => write!(f, "submission {} was not admitted", id.as_str()),
            Self::DeliveryConflict(id) => {
                write!(f, "conflicting delivery for submission {}", id.as_str())
            }
            Self::UnknownExecution => {
                f.write_str("terminal evidence has no matching execution start")
            }
            Self::TerminalConflict => f.write_str("execution already has terminal evidence"),
        }
    }
}

impl std::error::Error for LifecycleError {}

/// Carry all observed native metadata over the internal event transport.
/// The persisted completion representation continues to omit transient metrics.
#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
pub struct CompletionSnapshot {
    native: ProviderCompletion,
    metrics: TurnMetrics,
}

impl From<ProviderCompletion> for CompletionSnapshot {
    /// Capture transient metrics separately from the persisted representation.
    fn from(native: ProviderCompletion) -> Self {
        let ProviderCompletion {
            provider_session_id,
            provider_checkpoint_id,
            evidence,
            capability,
            runtime,
            metrics,
        } = native;
        Self {
            native: ProviderCompletion {
                provider_session_id,
                provider_checkpoint_id,
                evidence,
                capability,
                runtime,
                metrics: TurnMetrics::default(),
            },
            metrics,
        }
    }
}

impl From<CompletionSnapshot> for ProviderCompletion {
    /// Restore the complete native observations at host settlement.
    fn from(snapshot: CompletionSnapshot) -> Self {
        let CompletionSnapshot { native, metrics } = snapshot;
        let ProviderCompletion {
            provider_session_id,
            provider_checkpoint_id,
            evidence,
            capability,
            runtime,
            metrics: _,
        } = native;
        Self {
            provider_session_id,
            provider_checkpoint_id,
            evidence,
            capability,
            runtime,
            metrics,
        }
    }
}

/// Explain why reliable execution observation ended without proving native completion.
#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ObservationFailure {
    /// The native operation has no supported correlated terminal signal.
    LifecycleUnavailable,
    /// A bounded transport could not retain all required observations.
    Overflow,
    /// The native stream ended outside intentional session teardown.
    Disconnected,
    /// Native dispatch may have occurred without an acknowledgment.
    DeliveryUncertain,
    /// Required native data violated the adapter's protocol.
    Protocol,
    /// Foreground work conflicts with another admitted exchange on the same owner.
    OwnershipConflict,
    /// New execution arrived after the invocation's foreground receiver was released.
    UnexpectedExecution,
}

impl fmt::Display for ObservationFailure {
    /// Describe the loss without treating it as native terminal evidence.
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LifecycleUnavailable => {
                formatter.write_str("native lifecycle observation is unavailable")
            }
            Self::Overflow => formatter.write_str("provider observation queue overflowed"),
            Self::Disconnected => formatter.write_str("provider observation disconnected"),
            Self::DeliveryUncertain => formatter.write_str("provider delivery is uncertain"),
            Self::Protocol => formatter.write_str("provider observation violated its protocol"),
            Self::OwnershipConflict => {
                formatter.write_str("provider execution conflicts with the admitted owner")
            }
            Self::UnexpectedExecution => {
                formatter.write_str("provider started new execution after its input was settled")
            }
        }
    }
}

impl std::error::Error for ObservationFailure {}
