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
    tracing::error!(target: "forge::provider", event_name = "provider.normalization",
        operation = ?operation, error = %format_args!("{error:#}"));
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
            ProviderError::RuntimeVersion(_) | ProviderError::RuntimeLaunch { .. } => {
                ProviderFailureKind::Initialization
            }
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
