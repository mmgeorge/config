//! Execution completion independent of native request acknowledgment and turn boundaries.

use std::fmt;

use serde::{Deserialize, Serialize};

use crate::completion::{ProviderCompletion, TurnMetrics};
use crate::{InvocationScope, TurnAddress};

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

/// Retain proof of retry eligibility independently of the rendered error message.
#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ExecutionFailure {
    /// The native execution ended after the provider explicitly rejected the request.
    Rejected,
    /// The adapter reported a terminal failure that is not safe to retry.
    Terminal {
        #[serde(rename = "failure_kind")]
        kind: crate::error::ProviderFailureKind,
    },
    /// The adapter proved the terminal failure produced no replay-sensitive output.
    Retryable {
        address: TurnAddress,
        category: crate::retry::RetryCategory,
    },
}

/// Represent native terminal evidence for an execution containing one or more turns.
#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ExecutionResult {
    /// All covered work completed normally.
    Completed { completion: CompletionSnapshot },
    /// Native terminal evidence confirms cancellation.
    Cancelled { completion: CompletionSnapshot },
    /// Native terminal evidence confirms failure.
    Failed {
        completion: CompletionSnapshot,
        failure: ExecutionFailure,
    },
}

/// Publish live execution activity without coupling the reader to a submission future.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ExecutionUpdate {
    /// A positively correlated live execution began.
    Started { address: TurnAddress },
    /// Native terminal evidence accounts for the listed accepted journal inputs.
    Finished {
        address: TurnAddress,
        covered_submissions: Vec<SubmissionId>,
        completed_at_ms: i64,
        result: ExecutionResult,
    },
    /// Preserve a native terminal outcome whose required host effects could not be published.
    PublicationFailed {
        address: TurnAddress,
        covered_submissions: Vec<SubmissionId>,
        completed_at_ms: i64,
        result: ExecutionResult,
        failure: crate::session::HostEffectFailure,
    },
    /// A native control operation reached terminal evidence without starting a turn.
    ResolvedWithoutExecution {
        covered_submissions: Vec<SubmissionId>,
        completed_at_ms: i64,
        result: ExecutionResult,
    },
    /// Preserve a terminal native control result when its host effects failed without any turn.
    ResolutionPublicationFailed {
        covered_submissions: Vec<SubmissionId>,
        completed_at_ms: i64,
        result: ExecutionResult,
        failure: crate::session::HostEffectFailure,
    },
    /// Observation failed, possibly before a native turn identity existed.
    ObservationLost {
        failure: ObservationFailure,
        completion: Option<CompletionSnapshot>,
    },
}

/// Bind a lifecycle update to one invocation and validate redundant native ownership.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(try_from = "LifecycleRecord", into = "LifecycleRecord")]
pub struct ExecutionObservation {
    scope: InvocationScope,
    update: Box<ExecutionUpdate>,
}

impl ExecutionObservation {
    /// Create an observation only when its native address and coverage are valid.
    pub fn new(scope: InvocationScope, update: ExecutionUpdate) -> Result<Self, LifecycleError> {
        match &update {
            ExecutionUpdate::Started { address } => {
                if address.scope != scope {
                    return Err(LifecycleError::OwnershipMismatch);
                }
            }
            ExecutionUpdate::Finished {
                address,
                covered_submissions,
                result,
                ..
            }
            | ExecutionUpdate::PublicationFailed {
                address,
                covered_submissions,
                result,
                ..
            } => {
                if address.scope != scope {
                    return Err(LifecycleError::OwnershipMismatch);
                }
                if let ExecutionResult::Failed {
                    failure: ExecutionFailure::Retryable { address, .. },
                    ..
                } = result
                    && address.scope != scope
                {
                    return Err(LifecycleError::OwnershipMismatch);
                }
                let mut seen = std::collections::HashSet::new();
                for id in covered_submissions {
                    if !seen.insert(id) {
                        return Err(LifecycleError::DuplicateSubmission(id.clone()));
                    }
                }
            }
            ExecutionUpdate::ResolvedWithoutExecution {
                covered_submissions,
                result,
                ..
            }
            | ExecutionUpdate::ResolutionPublicationFailed {
                covered_submissions,
                result,
                ..
            } => {
                if let ExecutionResult::Failed {
                    failure: ExecutionFailure::Retryable { address, .. },
                    ..
                } = result
                    && address.scope != scope
                {
                    return Err(LifecycleError::OwnershipMismatch);
                }
                let mut seen = std::collections::HashSet::new();
                for id in covered_submissions {
                    if !seen.insert(id) {
                        return Err(LifecycleError::DuplicateSubmission(id.clone()));
                    }
                }
            }
            ExecutionUpdate::ObservationLost { .. } => {}
        }
        Ok(Self {
            scope,
            update: Box::new(update),
        })
    }

    /// Get the invocation that owns this observation.
    pub fn scope(&self) -> &InvocationScope {
        &self.scope
    }

    /// Get the validated lifecycle transition.
    pub fn update(&self) -> &ExecutionUpdate {
        &self.update
    }

    /// Consume the validated update without exposing mutable ownership fields.
    pub fn into_parts(self) -> (InvocationScope, ExecutionUpdate) {
        (self.scope, *self.update)
    }
}

/// Decode through the same checks as locally constructed observations.
#[derive(Clone, Debug, Serialize, Deserialize)]
struct LifecycleRecord {
    scope: InvocationScope,
    update: ExecutionUpdate,
}

impl TryFrom<LifecycleRecord> for ExecutionObservation {
    type Error = LifecycleError;

    /// Validate ownership and coverage after transport decoding.
    fn try_from(record: LifecycleRecord) -> Result<Self, Self::Error> {
        let LifecycleRecord { scope, update } = record;
        Self::new(scope, update)
    }
}

impl From<ExecutionObservation> for LifecycleRecord {
    /// Serialize the already validated observation.
    fn from(observation: ExecutionObservation) -> Self {
        let (scope, update) = observation.into_parts();
        Self { scope, update }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ExecutionOwner;

    /// Reject foreign ownership even when it entered through a serialized event.
    #[test]
    fn decoding_rejects_foreign_invocation() {
        let scope = InvocationScope::new(ExecutionOwner::root("one"), "exchange");
        let address = TurnAddress {
            scope: InvocationScope::new(ExecutionOwner::root("two"), "exchange"),
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        let encoded = serde_json::to_value(LifecycleRecord {
            scope,
            update: ExecutionUpdate::Started { address },
        })
        .unwrap();
        assert!(serde_json::from_value::<ExecutionObservation>(encoded).is_err());
    }

    /// Preserve transient native metrics without changing persisted completion encoding.
    #[test]
    fn completion_transport_preserves_metrics() {
        let mut native = ProviderCompletion::default();
        native.metrics.context_usage = crate::context::ContextUsage::reported(25, 100);
        native.metrics.native_compact_update = Some(true);
        assert!(
            serde_json::to_value(&native)
                .unwrap()
                .get("metrics")
                .is_none()
        );
        let snapshot = CompletionSnapshot::from(native);
        let encoded = serde_json::to_vec(&snapshot).unwrap();
        let restored: ProviderCompletion = serde_json::from_slice::<CompletionSnapshot>(&encoded)
            .unwrap()
            .into();
        assert_eq!(restored.metrics.context_usage.unwrap().used, 25);
        assert_eq!(restored.metrics.native_compact_update, Some(true));
    }

    /// Keep loss reportable before a provider assigns its first native turn.
    #[test]
    fn observation_loss_does_not_require_a_turn() {
        let scope = InvocationScope::new(ExecutionOwner::root("session"), "exchange");
        let observation = ExecutionObservation::new(
            scope.clone(),
            ExecutionUpdate::ObservationLost {
                failure: ObservationFailure::Disconnected,
                completion: None,
            },
        )
        .unwrap();
        let encoded = serde_json::to_vec(&observation).unwrap();
        let restored: ExecutionObservation = serde_json::from_slice(&encoded).unwrap();
        assert_eq!(restored.scope(), &scope);
        assert!(matches!(
            restored.update(),
            ExecutionUpdate::ObservationLost { .. }
        ));
        assert!(serde_json::from_str::<SubmissionId>("\"\"").is_err());
    }

    /// Preserve normalized failure categories independently of the tagged outcome shape.
    #[test]
    fn terminal_failure_round_trips_its_normalized_category() {
        let result = ExecutionResult::Failed {
            completion: ProviderCompletion::default().into(),
            failure: ExecutionFailure::Terminal {
                kind: crate::ProviderFailureKind::RateLimited,
            },
        };
        let encoded = serde_json::to_value(&result).unwrap();
        assert_eq!(encoded["kind"], "failed");
        assert_eq!(encoded["failure"]["kind"], "terminal");
        assert_eq!(encoded["failure"]["failure_kind"], "rate_limited");
        assert_eq!(
            serde_json::from_value::<ExecutionResult>(encoded).unwrap(),
            result
        );
    }
}
