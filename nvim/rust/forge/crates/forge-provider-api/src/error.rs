use crate::{events, retry};

/// Identify the normalized operation whose provider boundary failed.
#[derive(Clone, Copy, Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProviderOperation {
    /// Open a fresh live generation atomically with its event reader.
    SessionOpen,
    /// Apply complete settled session defaults.
    SessionConfigure,
    /// Create separate history at a stable boundary.
    SessionFork,
    /// Compact settled native history.
    SessionCompact,
    /// Revoke and collect an entire binding.
    SessionClose,
    /// Interrupt and settle one exact invocation.
    Cancel,
    /// Admit a journaled input into idle execution.
    InputSubmit,
    /// Deliver input into an exact active invocation.
    InputSteer,
    /// Observe stored native delivery evidence.
    InputReconcile,
    /// Retract an explicitly authorized exact cancelled input.
    InputRetract,
    /// Discover installation and runtime support without mutation.
    Inspect,
    /// Execute explicitly selected credential setup.
    Authenticate,
    /// Collect all provider-owned work and shared runtimes.
    Shutdown,
    /// Read normalized opaque model choices.
    ModelList,
    /// Read native skill definitions and state.
    SkillList,
    /// Read configured or observed MCP state.
    McpList,
    /// Read shells owned by one exact binding.
    TerminalList,
    /// Stop one shell without interrupting unrelated execution.
    TerminalTerminate,
}

/// Classify a provider failure without carrying its native code, payload, or message.
#[derive(Clone, Copy, Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProviderFailureKind {
    /// The host explicitly cancelled the addressed operation.
    Cancelled,
    /// A retained operation identity was reused with a different immutable context or request.
    OperationConflict,
    /// The sole owned open result was already transferred to its original consumer.
    OperationResultTaken,
    /// Required credentials are absent, invalid, or no longer authenticated.
    Authentication,
    /// The requested action is outside the admitted authority.
    Authorization,
    /// Native context capacity prevents the requested operation.
    ContextLimit,
    /// Runtime or session preparation could not be established.
    Initialization,
    /// The requested normalized policy cannot be enforced.
    Policy,
    /// Native evidence violated the adapter protocol or normalized contract.
    Protocol,
    /// The provider account exhausted a reported usage allowance.
    Quota,
    /// The provider temporarily limited the request rate.
    RateLimited,
    /// The provider positively rejected the operation; effect disposition remains explicit.
    Rejected,
    /// The native runtime failed outside a more specific normalized category.
    Runtime,
    /// The exact requested history or binding cannot be found.
    SessionMissing,
    /// The native communication channel failed.
    Transport,
    /// A bounded wait elapsed; this category alone does not prove non-consumption.
    Timeout,
    /// The adapter cannot classify the native cause more precisely.
    Unknown,
    /// Required journal or receipt persistence failed.
    Persistence,
    /// The addressed binding is permanently closed.
    BindingClosed,
    /// The binding lost authority after an unrecoverable partial mutation or observation failure.
    BindingInvalidated,
    /// Settled history is required but execution remains active.
    ExecutionActive,
    /// Native terminal evidence is required before collection or history mutation.
    ExecutionNotTerminal,
    /// The target invocation does not own the addressed native work.
    WrongInvocation,
    /// An exact history position no longer matches the requested mutation.
    HistoryConflict,
    /// Surviving resources prevent safe configuration replacement.
    ResourceConflict,
}

/// Identify a lifecycle subphase independently of the requested operation.
#[derive(Clone, Copy, Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProviderPhase {
    /// Validate identity and authority before native mutation.
    Admission,
    /// Prepare or replace private transport resources.
    Preparation,
    /// Hand work to the native transport.
    Dispatch,
    /// Observe native acknowledgement and lifecycle evidence.
    Observation,
    /// Request native interruption.
    Interruption,
    /// Join foreground workers and callbacks.
    Collection,
    /// Persist required delivery evidence.
    Persistence,
    /// Release an entire binding or shared runtime.
    Close,
}

/// Preserve what a failed operation proves about binding authority and native effects.
/// The failure category alone never establishes replay eligibility. Use this disposition
/// together with exact delivery/history evidence; Uncertain and Invalidated forbid blind replay.
#[derive(Clone, Copy, Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FailureDisposition {
    /// Native mutation did not occur and the binding remains usable.
    Unchanged,
    /// Native delivery or mutation may have occurred; automatic replay is forbidden.
    Uncertain,
    /// Callback authority is revoked and collection ownership remains retained.
    Invalidated,
    /// Required collection succeeded and the addressed binding is closed.
    Closed,
}

/// Own a safe normalized cause, actual requested operation, lifecycle phase, and effect disposition.
/// This value excludes native payloads and diagnostics. Reattributing an operation must preserve
/// the cause, phase, and effects; independent cleanup causes retain their own operation tags.
#[derive(Clone, Copy, Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
pub struct ProviderFailure {
    operation: ProviderOperation,
    kind: ProviderFailureKind,
    phase: ProviderPhase,
    disposition: FailureDisposition,
}

impl ProviderFailure {
    /// Create a rejection before native effects, with Admission/Unchanged evidence.
    /// Classify every later failure with at_phase; this constructor never proves that a
    /// request already handed to native transport was not consumed.
    pub const fn new(operation: ProviderOperation, kind: ProviderFailureKind) -> Self {
        Self {
            operation,
            kind,
            phase: ProviderPhase::Admission,
            disposition: FailureDisposition::Unchanged,
        }
    }

    /// Classify the failed phase and retained binding authority explicitly.
    pub const fn at_phase(mut self, phase: ProviderPhase, disposition: FailureDisposition) -> Self {
        self.phase = phase;
        self.disposition = disposition;
        self
    }
    /// Attribute a normalized cause to the public operation without changing its phase or effects.
    pub const fn for_operation(mut self, operation: ProviderOperation) -> Self {
        self.operation = operation;
        self
    }

    /// Get the lifecycle phase which failed.
    pub const fn phase(self) -> ProviderPhase {
        self.phase
    }
    /// Get the usable or invalidated binding disposition after failure.
    pub const fn disposition(self) -> FailureDisposition {
        self.disposition
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
    /// A bounded cleanup wait ended without proving collection completed.
    TimedOut,
    /// The attempted cleanup phase produced an independently typed cause.
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

/// Own normalized failure evidence across return and event boundaries.
/// Combined and preparation wrappers preserve independently failed cleanup phases. Neither
/// rendering nor operation reattribution may turn uncertain delivery into rejection or flatten
/// a scoped native cleanup failure into an unstructured string.
#[derive(Clone, Debug)]
pub enum ProviderError {
    /// A normalized identity, coverage, or lifecycle invariant was rejected.
    Contract(crate::lifecycle::LifecycleError),
    /// The actual requested operation failed with a typed cause, phase, and disposition.
    Failure(ProviderFailure),
    /// The requested operation failed together with independently scoped cleanup work.
    Combined {
        /// Original requested-operation cause.
        primary: Box<ProviderError>,
        /// Independent additional causes in observation order; operation tags remain unchanged.
        secondary: Vec<ProviderError>,
    },
    /// Observation failed and each attempted cleanup phase has a retained typed report.
    Cleanup(std::sync::Arc<ObservationCleanup>),
    /// An optional operation or mode was rejected before native mutation.
    Unsupported(crate::capability::UnsupportedCapability),
    /// Reliable native observation ended without proving terminal completion.
    Observation(crate::lifecycle::ObservationFailure),
    /// The selected runtime does not satisfy the adapter version contract.
    RuntimeVersion(crate::version::RuntimeVersionError),
    /// Native preparation failed before its requested execution could be admitted.
    PreparationFailed(Box<ProviderError>),
    /// Preparation failed and required preparation cleanup failed independently.
    PreparationCleanup {
        /// Preparation cause attributed to the requested operation.
        cause: Box<ProviderError>,
        /// Independent cleanup cause whose own operation, phase, and disposition remain intact.
        cleanup: Box<ProviderError>,
    },
    /// Positive final evidence proves this attempt was not consumed; only the host may retry.
    NotConsumed,
    /// Dispatch may have occurred, but acceptance was not established.
    DeliveryUncertain,
    /// Host cancellation was observed; independent cleanup failure must remain separate.
    Cancelled,
    /// Native terminal failure carries normalized evidence for host-owned retry policy.
    Retryable(Box<retry::RetryableFailure>),
    /// Normalized event delivery failed and its typed transport/observation cause is retained.
    Delivery(events::EventDeliveryError),
}

impl ProviderError {
    /// Attribute the primary normalized cause to the operation actually requested.
    /// Preserve independent cleanup causes and all non-normalized typed evidence unchanged.
    pub fn for_operation(self, operation: ProviderOperation) -> Self {
        match self {
            Self::Failure(failure) => Self::Failure(failure.for_operation(operation)),
            Self::Combined { primary, secondary } => Self::Combined {
                primary: Box::new(primary.for_operation(operation)),
                secondary,
            },
            Self::PreparationFailed(cause) => {
                Self::PreparationFailed(Box::new(cause.for_operation(operation)))
            }
            Self::PreparationCleanup { cause, cleanup } => Self::PreparationCleanup {
                cause: Box::new(cause.for_operation(operation)),
                cleanup,
            },
            unchanged @ (Self::Contract(_)
            | Self::Cleanup(_)
            | Self::Unsupported(_)
            | Self::Observation(_)
            | Self::RuntimeVersion(_)
            | Self::NotConsumed
            | Self::DeliveryUncertain
            | Self::Cancelled
            | Self::Retryable(_)
            | Self::Delivery(_)) => unchanged,
        }
    }

    /// Identify cancellation without hiding an independent cleanup or transport failure.
    pub fn is_clean_cancellation(&self) -> bool {
        match self {
            Self::Cancelled => true,
            Self::PreparationFailed(cause) => cause.is_clean_cancellation(),
            Self::Combined { primary, secondary } => {
                primary.is_clean_cancellation() && secondary.iter().all(Self::is_clean_cancellation)
            }
            Self::Contract(_)
            | Self::Failure(_)
            | Self::Cleanup(_)
            | Self::Unsupported(_)
            | Self::Observation(_)
            | Self::RuntimeVersion(_)
            | Self::PreparationCleanup { .. }
            | Self::NotConsumed
            | Self::DeliveryUncertain
            | Self::Retryable(_)
            | Self::Delivery(_) => false,
        }
    }
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
            Self::NotConsumed => formatter.write_str("provider proved input was not consumed"),
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
            Self::NotConsumed | Self::DeliveryUncertain | Self::Cancelled => None,
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
            ProviderOperation::SessionOpen,
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

    /// Reattribute only the requested cause while retaining independent cleanup operations and phases.
    #[test]
    fn operation_attribution_preserves_primary_phase_and_independent_cleanup_causes() {
        use super::{
            FailureDisposition, ProviderFailure, ProviderFailureKind, ProviderOperation,
            ProviderPhase,
        };
        let primary = ProviderFailure::new(
            ProviderOperation::InputSubmit,
            ProviderFailureKind::Persistence,
        )
        .at_phase(ProviderPhase::Persistence, FailureDisposition::Uncertain);
        let nested_cleanup = ProviderFailure::new(
            ProviderOperation::SessionClose,
            ProviderFailureKind::Transport,
        )
        .at_phase(ProviderPhase::Close, FailureDisposition::Invalidated);
        let independent =
            ProviderFailure::new(ProviderOperation::Cancel, ProviderFailureKind::Timeout)
                .at_phase(ProviderPhase::Collection, FailureDisposition::Invalidated);
        let error = ProviderError::Combined {
            primary: Box::new(ProviderError::PreparationFailed(Box::new(
                ProviderError::PreparationCleanup {
                    cause: Box::new(ProviderError::Failure(primary)),
                    cleanup: Box::new(ProviderError::Failure(nested_cleanup)),
                },
            ))),
            secondary: vec![
                ProviderError::Failure(independent),
                ProviderError::DeliveryUncertain,
            ],
        }
        .for_operation(ProviderOperation::InputSteer);
        let ProviderError::Combined {
            primary: attributed,
            secondary,
        } = error
        else {
            panic!("combined cause was flattened");
        };
        let ProviderError::PreparationFailed(preparation) = *attributed else {
            panic!("preparation cause was flattened");
        };
        let ProviderError::PreparationCleanup { cause, cleanup } = *preparation else {
            panic!("independent cleanup was flattened");
        };
        let ProviderError::Failure(attributed) = *cause else {
            panic!("normalized primary cause was lost");
        };
        assert_eq!(attributed.operation(), ProviderOperation::InputSteer);
        assert_eq!(attributed.kind(), primary.kind());
        assert_eq!(attributed.phase(), ProviderPhase::Persistence);
        assert_eq!(attributed.disposition(), FailureDisposition::Uncertain);
        assert!(matches!(*cleanup, ProviderError::Failure(failure) if failure == nested_cleanup));
        assert!(
            matches!(secondary.as_slice(), [ProviderError::Failure(failure), ProviderError::DeliveryUncertain] if *failure == independent)
        );
    }

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
                    ProviderOperation::Cancel,
                    ProviderFailureKind::Transport,
                )),
                Some(failure(
                    ProviderOperation::SessionClose,
                    ProviderFailureKind::Runtime,
                )),
            )
            .with_origin(ProviderError::DeliveryUncertain),
        );
        let mut report = first.clone();
        std::sync::Arc::make_mut(&mut report).record_collection_failure(failure(
            ProviderOperation::SessionClose,
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
