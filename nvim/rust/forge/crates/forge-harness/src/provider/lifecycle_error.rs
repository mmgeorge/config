//! Classify the existing host adapter's legacy orchestration at lifecycle boundaries.

use super::ProviderError;
use forge_provider_api::error::{ProviderOperation, ProviderOperationFailure};

/// Preserve known recovery identities before retaining an external operation's source.
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
    ProviderError::Operation(ProviderOperationFailure::new(
        operation,
        std::sync::Arc::from(error.into_boxed_dyn_error()),
    ))
}

/// Preserve the first lifecycle failure and all distinct cleanup causes in execution order.
pub(crate) fn combine(
    failures: impl IntoIterator<Item = ProviderError>,
) -> Result<(), ProviderError> {
    let mut failures = failures.into_iter();
    let Some(primary) = failures.next() else {
        return Ok(());
    };
    let secondary: Vec<_> = failures.collect();
    if secondary.is_empty() {
        return Err(primary);
    }
    Err(ProviderError::Combined {
        primary: Box::new(primary),
        secondary,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Keep cancellation and retry permission independent of diagnostic source boxing.
    #[test]
    fn lifecycle_boundary_preserves_recovery_identities_and_native_sources() {
        assert!(matches!(
            classify(
                ProviderOperation::CancelSession,
                forge_provider_api::error::TurnCancelled.into()
            ),
            ProviderError::Cancelled
        ));
        let uncertain = ProviderError::DeliveryUncertain {
            message: "acknowledgment unavailable".into(),
        };
        assert!(matches!(
            classify(ProviderOperation::InitializeSession, uncertain.into()),
            ProviderError::DeliveryUncertain { .. }
        ));
        let error = classify(
            ProviderOperation::InitializeSession,
            anyhow::Error::new(std::io::Error::new(
                std::io::ErrorKind::ConnectionReset,
                "native fixture",
            ))
            .context("legacy initialization boundary"),
        );
        let ProviderError::Operation(failure) = &error else {
            panic!("native failure lost its operation");
        };
        assert_eq!(failure.operation(), ProviderOperation::InitializeSession);
        let mut source: Option<&(dyn std::error::Error + 'static)> = Some(&error);
        let mut original = false;
        while let Some(error) = source {
            original |= error.downcast_ref::<std::io::Error>().is_some();
            source = error.source();
        }
        assert!(original, "native I/O source was flattened");
    }

    /// Preserve every concrete cleanup failure while keeping the original cause first.
    #[test]
    fn lifecycle_cleanup_aggregation_preserves_primary_and_secondary_sources() {
        let error = combine([
            ProviderError::Cancelled,
            ProviderError::Observation(forge_provider_api::lifecycle::ObservationFailure::Overflow),
        ])
        .unwrap_err();
        let ProviderError::Combined { primary, secondary } = error else {
            panic!("cleanup was flattened");
        };
        assert!(matches!(*primary, ProviderError::Cancelled));
        assert!(matches!(
            &secondary[..],
            [ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Overflow
            )]
        ));
    }
}
