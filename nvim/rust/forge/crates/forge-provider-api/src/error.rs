use crate::{events, retry};

#[derive(Debug)]
/// Identifies user cancellation across broker and task-coordinator error boundaries.
pub struct TurnCancelled;

impl std::fmt::Display for TurnCancelled {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("Turn cancelled by user")
    }
}

impl std::error::Error for TurnCancelled {}

/// Preserve a failed cleanup phase without flattening its source error.
#[derive(Debug)]
pub enum CleanupFailure {
    TimedOut,
    Failed(Box<ProviderError>),
}

impl std::fmt::Display for CleanupFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TimedOut => f.write_str("exceeded its deadline"),
            Self::Failed(error) => std::fmt::Display::fmt(error, f),
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

/// Record observation loss together with the outcome of each cleanup phase.
#[derive(Debug)]
pub struct ObservationCleanup {
    observation: crate::lifecycle::ObservationFailure,
    origin: Option<Box<ProviderError>>,
    fencing: Option<CleanupFailure>,
    cancellation: Option<CleanupFailure>,
    collection: Option<CleanupFailure>,
}

impl ObservationCleanup {
    /// Create an immutable report after fencing, cancellation, and collection complete.
    pub fn new(
        observation: crate::lifecycle::ObservationFailure,
        fencing: Option<CleanupFailure>,
        cancellation: Option<CleanupFailure>,
        collection: Option<CleanupFailure>,
    ) -> Self {
        Self {
            observation,
            origin: None,
            fencing,
            cancellation,
            collection,
        }
    }
    /// Attach the original typed delivery failure when it triggered cleanup.
    pub fn with_origin(mut self, origin: ProviderError) -> Self {
        self.origin = Some(Box::new(origin));
        self
    }
    /// Get the typed error that triggered cleanup, when known.
    pub fn origin(&self) -> Option<&ProviderError> {
        self.origin.as_deref()
    }
    pub fn observation(&self) -> &crate::lifecycle::ObservationFailure {
        &self.observation
    }
    /// Get the failure to revoke the old generation's callback authority.
    pub fn fencing(&self) -> Option<&CleanupFailure> {
        self.fencing.as_ref()
    }
    pub fn cancellation(&self) -> Option<&CleanupFailure> {
        self.cancellation.as_ref()
    }
    pub fn collection(&self) -> Option<&CleanupFailure> {
        self.collection.as_ref()
    }
    pub fn collected(&self) -> bool {
        self.fencing.is_none() && self.cancellation.is_none() && self.collection.is_none()
    }
}

impl std::fmt::Display for ObservationCleanup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "provider observation failed: {}", self.observation)?;
        if let Some(error) = &self.fencing {
            write!(f, "; fencing: {error}")?;
        }
        if let Some(error) = &self.cancellation {
            write!(f, "; cancellation: {error}")?;
        }
        if let Some(error) = &self.collection {
            write!(f, "; collection: {error}")?;
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

#[derive(Debug)]
pub enum ProviderError {
    /// Observation failed and bounded cleanup did not complete successfully.
    Cleanup(std::sync::Arc<ObservationCleanup>),
    /// The selected provider does not implement this optional operation.
    Unsupported(crate::capability::UnsupportedCapability),
    /// The session cannot prove that all required lifecycle events were observed.
    Observation(crate::lifecycle::ObservationFailure),
    /// Native initialization failed its adapter-owned version requirement.
    RuntimeVersion(crate::version::RuntimeVersionError),
    /// Preparation failed before native input dispatch was attempted.
    PreparationFailed(std::sync::Arc<ProviderError>),
    /// Partial native initialization failed and could not be collected.
    PreparationCleanup {
        /// Failure observed before any prompt dispatch.
        cause: std::sync::Arc<ProviderError>,
        /// Failure to collect the partially initialized native resources.
        cleanup: std::sync::Arc<ProviderError>,
    },
    /// The adapter proved that this input was not consumed and may be continued safely.
    NotConsumed {
        message: String,
    },
    /// Dispatch may have occurred, but native acceptance was not established.
    DeliveryUncertain {
        message: String,
    },
    Cancelled,
    Transport {
        message: String,
    },
    Protocol {
        message: String,
    },
    Provider {
        code: Option<String>,
        message: String,
    },
    /// The provider cannot find the conversation requested for resumption.
    SessionMissing {
        message: String,
    },
    Rejected {
        message: String,
    },
    Retryable(Box<retry::RetryableFailure>),
    Delivery(events::EventDeliveryError),
}

impl std::fmt::Display for ProviderError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Cleanup(error) => std::fmt::Display::fmt(error, formatter),
            Self::Unsupported(error) => std::fmt::Display::fmt(error, formatter),
            Self::Observation(failure) => {
                write!(formatter, "provider observation failed: {failure}")
            }
            Self::RuntimeVersion(error) => std::fmt::Display::fmt(error, formatter),
            Self::PreparationCleanup { cause, cleanup } => write!(
                formatter,
                "provider preparation failed: {cause}; cleanup failed: {cleanup}"
            ),
            Self::PreparationFailed(error) => write!(
                formatter,
                "provider preparation failed before dispatch: {error}"
            ),
            Self::Cancelled => formatter.write_str("provider execution cancelled"),
            Self::NotConsumed { message }
            | Self::DeliveryUncertain { message }
            | Self::Transport { message }
            | Self::Protocol { message }
            | Self::Provider { message, .. }
            | Self::SessionMissing { message }
            | Self::Rejected { message } => formatter.write_str(message),
            Self::Retryable(error) => std::fmt::Display::fmt(error, formatter),
            Self::Delivery(error) => std::fmt::Display::fmt(error, formatter),
        }
    }
}

impl std::error::Error for ProviderError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Cleanup(error) => Some(error.as_ref()),
            Self::Unsupported(error) => Some(error),
            Self::RuntimeVersion(error) => Some(error),
            Self::PreparationFailed(error) => Some(error.as_ref()),
            Self::PreparationCleanup { cause, .. } => Some(cause.as_ref()),
            Self::Retryable(error) => Some(error.as_ref()),
            Self::Delivery(error) => Some(error),
            Self::Observation(error) => Some(error),
            Self::Cancelled
            | Self::NotConsumed { .. }
            | Self::DeliveryUncertain { .. }
            | Self::Transport { .. }
            | Self::Protocol { .. }
            | Self::Provider { .. }
            | Self::SessionMissing { .. }
            | Self::Rejected { .. } => None,
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
        Self::Delivery(error)
    }
}

impl From<anyhow::Error> for ProviderError {
    fn from(error: anyhow::Error) -> Self {
        let error = match error.downcast::<Self>() {
            Ok(error) => return error,
            Err(error) => error,
        };
        let error = match error.downcast::<crate::capability::UnsupportedCapability>() {
            Ok(error) => return Self::Unsupported(error),
            Err(error) => error,
        };
        let error = match error.downcast::<events::EventDeliveryError>() {
            Ok(error) => return Self::Delivery(error),
            Err(error) => error,
        };
        let error = match error.downcast::<retry::RetryableFailure>() {
            Ok(error) => return Self::Retryable(Box::new(error)),
            Err(error) => error,
        };
        if error.is::<TurnCancelled>() {
            return Self::Cancelled;
        }
        Self::Provider {
            code: None,
            message: format!("{error:#}"),
        }
    }
}

#[cfg(test)]
mod cleanup_tests {
    use super::{CleanupFailure, ObservationCleanup};
    use crate::lifecycle::ObservationFailure;

    #[test]
    fn cleanup_retains_each_failed_phase() {
        let report = ObservationCleanup::new(
            ObservationFailure::Disconnected,
            Some(CleanupFailure::TimedOut),
            Some(CleanupFailure::TimedOut),
            Some(CleanupFailure::TimedOut),
        );
        assert!(report.fencing().is_some());
        assert!(report.cancellation().is_some());
        assert!(report.collection().is_some());
        assert!(!report.collected());
        let description = report.to_string();
        assert!(description.contains("fencing:"));
        assert!(description.contains("cancellation:"));
        assert!(description.contains("collection:"));
    }
    #[test]
    fn cleanup_preserves_typed_delivery_uncertainty_and_failed_phase() {
        let report = ObservationCleanup::new(
            ObservationFailure::Disconnected,
            Some(CleanupFailure::TimedOut),
            None,
            None,
        )
        .with_origin(super::ProviderError::DeliveryUncertain {
            message: "start acknowledgment expired".into(),
        });
        assert!(matches!(
            report.origin(),
            Some(super::ProviderError::DeliveryUncertain { .. })
        ));
        assert!(report.fencing().is_some());
        assert!(
            std::error::Error::source(&report)
                .unwrap()
                .is::<super::ProviderError>()
        );
    }
}
