//! Classify the existing host adapter's legacy orchestration at lifecycle boundaries.

use super::ProviderError;
use forge_provider_api::error::{ProviderFailure, ProviderFailureKind, ProviderOperation};

/// Preserve known recovery identities while discarding unclassified external details.
///
/// This is the explicit compatibility boundary for existing host orchestration,
/// not an engine error channel. Provider and engine contracts never store anyhow.
pub(crate) fn classify(operation: ProviderOperation, error: anyhow::Error) -> ProviderError {
    let error = match error.downcast::<ProviderError>() {
        Ok(error) => return error,
        Err(error) => error,
    };
    let error = match error.downcast::<forge_provider_api::retry::RetryableFailure>() {
        Ok(error) => return ProviderError::Retryable(Box::new(error)),
        Err(error) => error,
    };
    let error = match error.downcast::<forge_provider_api::error::TurnCancelled>() {
        Ok(_) => return ProviderError::Cancelled,
        Err(error) => error,
    };
    let error = match error.downcast::<forge_provider_api::version::RuntimeVersionError>() {
        Ok(error) => return ProviderError::RuntimeVersion(error),
        Err(error) => error,
    };
    let error = match error.downcast::<forge_provider_api::capability::UnsupportedCapability>() {
        Ok(error) => return ProviderError::Unsupported(error),
        Err(error) => error,
    };
    let error = match error.downcast::<forge_provider_api::events::EventDeliveryError>() {
        Ok(error) => return ProviderError::Delivery(error),
        Err(error) => error,
    };
    classify_source(operation, error.as_ref())
}

/// Preserve the normalized primary failure without flattening its operation, phase, or disposition.
pub(crate) fn diagnostic(
    operation: ProviderOperation,
    source: &(dyn std::error::Error + 'static),
) -> ProviderFailure {
    fn primary(operation: ProviderOperation, error: ProviderError) -> ProviderFailure {
        let kind = match error {
            ProviderError::Failure(failure) => return failure,
            ProviderError::Combined { primary: cause, .. }
            | ProviderError::PreparationFailed(cause)
            | ProviderError::PreparationCleanup { cause, .. } => return primary(operation, *cause),
            ProviderError::Contract(_) => ProviderFailureKind::Protocol,
            ProviderError::Cleanup(_)
            | ProviderError::Observation(_)
            | ProviderError::Delivery(_)
            | ProviderError::DeliveryUncertain
            | ProviderError::Retryable(_) => ProviderFailureKind::Transport,
            ProviderError::Unsupported(_)
            | ProviderError::NotConsumed
            | ProviderError::Cancelled => ProviderFailureKind::Rejected,
            ProviderError::RuntimeVersion(_) => ProviderFailureKind::Initialization,
        };
        ProviderFailure::new(operation, kind)
    }
    primary(operation, classify_source(operation, source))
}

/// Convert only recognized typed causes; native text and source chains never cross this boundary.
pub(crate) fn classify_source(
    operation: ProviderOperation,
    source: &(dyn std::error::Error + 'static),
) -> ProviderError {
    let mut source = Some(source);
    while let Some(cause) = source {
        if let Some(error) = cause.downcast_ref::<ProviderError>() {
            return error.clone();
        }
        if cause.is::<forge_provider_api::error::TurnCancelled>() {
            return ProviderError::Cancelled;
        }
        let kind = if cause.is::<forge_provider_api::access::AccessPolicyError>()
            || cause.is::<crate::session::InputTimeoutError>()
        {
            Some(ProviderFailureKind::Rejected)
        } else if let Some(error) = cause.downcast_ref::<std::io::Error>() {
            Some(match error.kind() {
                std::io::ErrorKind::PermissionDenied => ProviderFailureKind::Authorization,
                std::io::ErrorKind::TimedOut => ProviderFailureKind::Timeout,
                std::io::ErrorKind::NotFound => ProviderFailureKind::SessionMissing,
                _ => ProviderFailureKind::Transport,
            })
        } else if cause.is::<tokio::time::error::Elapsed>() {
            Some(ProviderFailureKind::Timeout)
        } else if cause.is::<tokio::task::JoinError>()
            || cause.is::<super::session_execution::CleanupJoinFailure>()
        {
            // A stopped cleanup worker does not prove cancellation of provider execution.
            Some(ProviderFailureKind::Runtime)
        } else {
            None
        };
        if let Some(kind) = kind {
            return ProviderError::Failure(ProviderFailure::new(operation, kind));
        }
        source = cause.source();
    }
    ProviderError::Failure(ProviderFailure::new(
        operation,
        ProviderFailureKind::Unknown,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Preserve normalized identities while preventing native source text from escaping.
    #[test]
    fn lifecycle_boundary_preserves_identity_and_removes_native_sources() {
        assert!(matches!(
            classify(
                ProviderOperation::TurnInterrupt,
                forge_provider_api::error::TurnCancelled.into()
            ),
            ProviderError::Cancelled
        ));
        let uncertain = ProviderError::DeliveryUncertain;
        assert!(matches!(
            classify(ProviderOperation::ThreadOpen, uncertain.into()),
            ProviderError::DeliveryUncertain
        ));
        let error = classify(
            ProviderOperation::ThreadOpen,
            anyhow::Error::new(std::io::Error::new(
                std::io::ErrorKind::ConnectionReset,
                "native fixture",
            ))
            .context("legacy initialization boundary"),
        );
        let ProviderError::Failure(failure) = &error else {
            panic!("native failure lost its operation");
        };
        assert_eq!(failure.operation(), ProviderOperation::ThreadOpen);
        assert_eq!(failure.kind(), ProviderFailureKind::Transport);
        assert!(!format!("{error:?} {error}").contains("native fixture"));
        let mut source: Option<&(dyn std::error::Error + 'static)> = Some(&error);
        let mut original = false;
        while let Some(error) = source {
            original |= error.downcast_ref::<std::io::Error>().is_some();
            source = error.source();
        }
        assert!(
            !original,
            "native I/O source crossed the normalized boundary"
        );
    }
}
