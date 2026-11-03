use crate::{events, retry};

/// Identify the normalized operation whose provider boundary failed.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProviderOperation {
    InitializeSession,
    InspectRuntime,
    GenerateText,
    CompactSession,
    ResetSession,
    FenceSession,
    CancelSession,
    CleanupExecution,
    Shutdown,
    ReconcileSettings,
    CollectEventReader,
    CollectNativeRuntime,
    RetireSession,
    NativeTransport,
    AdmitInput,
    Authenticate,
    RecoverSession,
    HostEffectPublication,
    RecordInput,
}

/// Classify a provider failure without carrying its native code, payload, or message.
#[derive(Clone, Copy, Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProviderFailureKind {
    Authentication,
    Authorization,
    ContextLimit,
    Initialization,
    Policy,
    Protocol,
    Quota,
    RateLimited,
    Rejected,
    Runtime,
    SessionMissing,
    Transport,
    Timeout,
    Unknown,
    Persistence,
}

/// Preserve a normalized operation and failure class after adapter conversion.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProviderFailure {
    operation: ProviderOperation,
    kind: ProviderFailureKind,
}

impl ProviderFailure {
    /// Create a provider-neutral operation failure after native classification.
    pub const fn new(operation: ProviderOperation, kind: ProviderFailureKind) -> Self {
        Self { operation, kind }
    }

    /// Get the failed normalized provider operation.
    pub const fn operation(self) -> ProviderOperation {
        self.operation
    }

    /// Get the normalized failure category.
    pub const fn kind(self) -> ProviderFailureKind {
        self.kind
    }
}

impl std::fmt::Display for ProviderFailure {
    /// Render a normalized failure classification for diagnostics.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "provider {:?} failed: {:?}",
            self.operation, self.kind
        )
    }
}

impl std::error::Error for ProviderFailure {}

impl From<ProviderFailure> for ProviderError {
    fn from(failure: ProviderFailure) -> Self {
        Self::Failure(failure)
    }
}

/// Identify user cancellation across broker and task-coordinator error boundaries.
#[derive(Debug)]
pub struct TurnCancelled;

impl std::fmt::Display for TurnCancelled {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("Turn cancelled by user")
    }
}

impl std::error::Error for TurnCancelled {}

/// Preserve a failed cleanup phase without flattening its typed failure category.
#[derive(Clone, Debug)]
pub enum CleanupFailure {
    TimedOut,
    Failed(Box<ProviderError>),
}

impl std::fmt::Display for CleanupFailure {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TimedOut => formatter.write_str("provider cleanup timed out"),
            Self::Failed(error) => std::fmt::Display::fmt(error, formatter),
        }
    }
}

impl std::error::Error for CleanupFailure {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::TimedOut => None,
            Self::Failed(error) => Some(error.as_ref()),
        }
    }
}

/// Record observation loss together with the result of each required cleanup phase.
#[derive(Clone, Debug)]
pub struct ObservationCleanup {
    observation: crate::lifecycle::ObservationFailure,
    origin: Option<Box<ProviderError>>,
    fencing: Vec<CleanupFailure>,
    cancellation: Vec<CleanupFailure>,
    collection: Vec<CleanupFailure>,
}

impl ObservationCleanup {
    /// Create a report after fencing, cancellation, and collection complete.
    pub fn new(
        observation: crate::lifecycle::ObservationFailure,
        fencing: Option<CleanupFailure>,
        cancellation: Option<CleanupFailure>,
        collection: Option<CleanupFailure>,
    ) -> Self {
        Self {
            observation,
            origin: None,
            fencing: fencing.into_iter().collect(),
            cancellation: cancellation.into_iter().collect(),
            collection: collection.into_iter().collect(),
        }
    }

    /// Retain the first typed failure that triggered cleanup.
    pub fn with_origin(mut self, origin: ProviderError) -> Self {
        self.origin.get_or_insert_with(|| Box::new(origin));
        self
    }

    /// Preserve source observation loss discovered during intentional collection.
    pub fn with_observation(mut self, observation: crate::lifecycle::ObservationFailure) -> Self {
        self.observation = observation;
        self
    }

    /// Get the typed failure that triggered cleanup, when known.
    pub fn origin(&self) -> Option<&ProviderError> {
        self.origin.as_deref()
    }
    /// Get the normalized observation failure.
    pub fn observation(&self) -> &crate::lifecycle::ObservationFailure {
        &self.observation
    }
    /// Get every callback-fencing failure in observation order.
    pub fn fencing(&self) -> &[CleanupFailure] {
        &self.fencing
    }
    /// Get every native cancellation failure in observation order.
    pub fn cancellation(&self) -> &[CleanupFailure] {
        &self.cancellation
    }
    /// Get every native collection failure in observation order.
    pub fn collection(&self) -> &[CleanupFailure] {
        &self.collection
    }
    /// Append an independently observed collection failure without wrapping this report.
    pub fn record_collection_failure(&mut self, failure: CleanupFailure) {
        self.collection.push(failure);
    }
    /// Report whether every required cleanup phase succeeded.
    pub fn collected(&self) -> bool {
        self.fencing.is_empty() && self.cancellation.is_empty() && self.collection.is_empty()
    }
}

impl std::fmt::Display for ObservationCleanup {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "provider observation failed: {}",
            self.observation
        )?;
        for (phase, errors) in [
            ("fencing", &self.fencing),
            ("cancellation", &self.cancellation),
            ("collection", &self.collection),
        ] {
            for error in errors {
                write!(formatter, "; {phase}: {error}")?;
            }
        }
        Ok(())
    }
}

impl std::error::Error for ObservationCleanup {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.origin
            .as_deref()
            .map(|error| error as &(dyn std::error::Error + 'static))
            .or(Some(&self.observation))
    }
}

/// Preserve normalized provider failure semantics across return and stream boundaries.
#[derive(Clone, Debug)]
pub enum ProviderError {
    Contract(crate::lifecycle::LifecycleError),
    Failure(ProviderFailure),
    Combined {
        primary: Box<ProviderError>,
        secondary: Vec<ProviderError>,
    },
    Cleanup(std::sync::Arc<ObservationCleanup>),
    Unsupported(crate::capability::UnsupportedCapability),
    Observation(crate::lifecycle::ObservationFailure),
    RuntimeVersion(crate::version::RuntimeVersionError),
    PreparationFailed(Box<ProviderError>),
    PreparationCleanup {
        cause: Box<ProviderError>,
        cleanup: Box<ProviderError>,
    },
    /// The provider proved non-consumption and issued a single-use continuation capability.
    NotConsumed {
        continuation: crate::ContinuationToken,
    },
    /// Dispatch may have occurred, but acceptance was not established.
    DeliveryUncertain,
    Cancelled,
    Retryable(Box<retry::RetryableFailure>),
    Delivery(events::EventDeliveryError),
}

impl std::fmt::Display for ProviderError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Contract(error) => std::fmt::Display::fmt(error, formatter),
            Self::Failure(error) => std::fmt::Display::fmt(error, formatter),
            Self::Combined { primary, secondary } => {
                std::fmt::Display::fmt(primary, formatter)?;
                for error in secondary {
                    write!(formatter, "; cleanup: {error}")?;
                }
                Ok(())
            }
            Self::Cleanup(error) => std::fmt::Display::fmt(error, formatter),
            Self::Unsupported(error) => std::fmt::Display::fmt(error, formatter),
            Self::Observation(error) => write!(formatter, "provider observation failed: {error}"),
            Self::RuntimeVersion(error) => std::fmt::Display::fmt(error, formatter),
            Self::PreparationFailed(error) => {
                write!(formatter, "provider preparation failed: {error}")
            }
            Self::PreparationCleanup { cause, cleanup } => write!(
                formatter,
                "provider preparation failed: {cause}; cleanup failed: {cleanup}"
            ),
            Self::NotConsumed { .. } => {
                formatter.write_str("provider proved input was not consumed")
            }
            Self::DeliveryUncertain => formatter.write_str("provider input delivery is uncertain"),
            Self::Cancelled => formatter.write_str("provider execution cancelled"),
            Self::Retryable(error) => std::fmt::Display::fmt(error, formatter),
            Self::Delivery(error) => std::fmt::Display::fmt(error, formatter),
        }
    }
}

impl std::error::Error for ProviderError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Contract(error) => Some(error),
            Self::Failure(error) => Some(error),
            Self::Combined { primary, .. } => Some(primary.as_ref()),
            Self::Cleanup(error) => Some(error.as_ref()),
            Self::Unsupported(error) => Some(error),
            Self::Observation(error) => Some(error),
            Self::RuntimeVersion(error) => Some(error),
            Self::PreparationFailed(error) => Some(error.as_ref()),
            Self::PreparationCleanup { cause, .. } => Some(cause.as_ref()),
            Self::Retryable(error) => Some(error.as_ref()),
            Self::Delivery(error) => Some(error),
            Self::NotConsumed { .. } | Self::DeliveryUncertain | Self::Cancelled => None,
        }
    }
}

impl From<crate::capability::UnsupportedCapability> for ProviderError {
    fn from(error: crate::capability::UnsupportedCapability) -> Self {
        Self::Unsupported(error)
    }
}

impl From<crate::version::RuntimeVersionError> for ProviderError {
    fn from(error: crate::version::RuntimeVersionError) -> Self {
        Self::RuntimeVersion(error)
    }
}

impl From<events::EventDeliveryError> for ProviderError {
    fn from(error: events::EventDeliveryError) -> Self {
        match error {
            events::EventDeliveryError::Observation(failure) => Self::Observation(failure),
            error => Self::Delivery(error),
        }
    }
}

impl From<crate::lifecycle::LifecycleError> for ProviderError {
    fn from(error: crate::lifecycle::LifecycleError) -> Self {
        Self::Contract(error)
    }
}

impl From<crate::execution::ExecutionOwnerError> for ProviderError {
    fn from(_error: crate::execution::ExecutionOwnerError) -> Self {
        Self::Contract(crate::lifecycle::LifecycleError::OwnershipMismatch)
    }
}

impl From<crate::session::RecoveryTokenError> for ProviderError {
    fn from(_error: crate::session::RecoveryTokenError) -> Self {
        Self::Failure(ProviderFailure::new(
            ProviderOperation::RecoverSession,
            ProviderFailureKind::Rejected,
        ))
    }
}

impl From<TurnCancelled> for ProviderError {
    fn from(_error: TurnCancelled) -> Self {
        Self::Cancelled
    }
}

#[cfg(test)]
mod cleanup_tests {
    use super::{CleanupFailure, ObservationCleanup, ProviderError};
    use crate::lifecycle::ObservationFailure;

    #[test]
    fn cleanup_retains_each_failed_phase() {
        let report = ObservationCleanup::new(
            ObservationFailure::Disconnected,
            Some(CleanupFailure::TimedOut),
            Some(CleanupFailure::TimedOut),
            Some(CleanupFailure::TimedOut),
        );
        assert!(!report.fencing().is_empty());
        assert!(!report.cancellation().is_empty());
        assert!(!report.collection().is_empty());
        assert!(!report.collected());
        let description = report.to_string();
        assert!(description.contains("fencing:"));
        assert!(description.contains("cancellation:"));
        assert!(description.contains("collection:"));
    }

    /// Keep each phase and the first cause when further collection failures arrive.
    #[test]
    fn additional_collection_failures_preserve_phases_without_nested_reports() {
        use super::{ProviderFailure, ProviderFailureKind, ProviderOperation};
        let failure = |operation, kind| {
            CleanupFailure::Failed(Box::new(ProviderError::Failure(ProviderFailure::new(
                operation, kind,
            ))))
        };
        let first = std::sync::Arc::new(
            ObservationCleanup::new(
                ObservationFailure::Disconnected,
                Some(CleanupFailure::TimedOut),
                Some(failure(
                    ProviderOperation::CancelSession,
                    ProviderFailureKind::Transport,
                )),
                Some(failure(
                    ProviderOperation::CollectNativeRuntime,
                    ProviderFailureKind::Runtime,
                )),
            )
            .with_origin(ProviderError::DeliveryUncertain),
        );
        let mut report = first.clone();
        std::sync::Arc::make_mut(&mut report).record_collection_failure(failure(
            ProviderOperation::CollectEventReader,
            ProviderFailureKind::Protocol,
        ));
        let report = report
            .as_ref()
            .clone()
            .with_origin(ProviderError::Cancelled);
        assert!(matches!(
            report.origin(),
            Some(ProviderError::DeliveryUncertain)
        ));
        assert_eq!(report.observation(), &ObservationFailure::Disconnected);
        assert!(matches!(report.fencing(), [CleanupFailure::TimedOut]));
        assert_eq!(report.cancellation().len(), 1);
        assert_eq!(report.collection().len(), 2);
        assert_eq!(
            first.collection().len(),
            1,
            "published snapshots remain immutable"
        );
        assert!(report.collection().iter().all(|error| matches!(error,
            CleanupFailure::Failed(error) if matches!(error.as_ref(), ProviderError::Failure(_))
        )));
        let message = report.to_string();
        assert_eq!(message.matches("provider observation failed:").count(), 1);
        assert_eq!(message.matches("fencing:").count(), 1);
        assert_eq!(message.matches("cancellation:").count(), 1);
        assert_eq!(message.matches("collection:").count(), 2);
        assert!(!report.collected());
    }

    #[test]
    fn cleanup_preserves_typed_delivery_uncertainty_and_failed_phase() {
        let report = ObservationCleanup::new(
            ObservationFailure::Disconnected,
            Some(CleanupFailure::TimedOut),
            None,
            None,
        )
        .with_origin(ProviderError::DeliveryUncertain);
        assert!(matches!(
            report.origin(),
            Some(ProviderError::DeliveryUncertain)
        ));
        assert!(!report.fencing().is_empty());
        assert!(
            std::error::Error::source(&report)
                .unwrap()
                .is::<ProviderError>()
        );
    }
}
