//! Retain host-issued provider operation identity and explicit interruption authority.

use std::time::Duration;
use std::{future::Future, sync::Arc};

use forge_provider_api::{
    OperationCancellation, OperationCancellationSource, OperationContext, OperationId,
    ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
};

/// Own an operation's immutable observation context and explicit cancellation source.
/// Dropping this control does not cancel the operation; its adapter retains admitted work.
pub(crate) struct OperationControl {
    operation: ProviderOperation,
    context: OperationContext,
    cancellation: OperationCancellationSource,
}

impl OperationControl {
    /// Create a fresh host operation before retaining its asynchronous worker.
    pub(crate) fn fresh(operation: ProviderOperation) -> Result<Self, ProviderError> {
        let id = OperationId::new(uuid::Uuid::new_v4().to_string())?;
        Self::new(operation, id)
    }

    /// Retain a stable host identity for retries of the same original operation.
    pub(crate) fn new(
        operation: ProviderOperation,
        id: OperationId,
    ) -> Result<Self, ProviderError> {
        let timeout = match operation {
            // Some native protocols acknowledge acceptance only when generation ends.
            ProviderOperation::InputSubmit | ProviderOperation::InputSteer => {
                Duration::from_secs(30 * 60)
            }
            ProviderOperation::Cancel
            | ProviderOperation::SessionClose
            | ProviderOperation::Shutdown => Duration::from_secs(60),
            ProviderOperation::Authenticate => Duration::from_secs(10 * 60),
            ProviderOperation::SessionOpen
            | ProviderOperation::SessionConfigure
            | ProviderOperation::SessionFork
            | ProviderOperation::SessionCompact
            | ProviderOperation::InputReconcile
            | ProviderOperation::InputRetract
            | ProviderOperation::Inspect
            | ProviderOperation::ModelList
            | ProviderOperation::SkillList
            | ProviderOperation::McpList
            | ProviderOperation::TerminalList
            | ProviderOperation::TerminalTerminate => Duration::from_secs(120),
        };
        let (cancellation, observer) = OperationCancellation::channel();
        let context = OperationContext::new(id, timeout, observer).map_err(|_| {
            ProviderError::Failure(ProviderFailure::new(
                operation,
                ProviderFailureKind::Rejected,
            ))
        })?;
        Ok(Self {
            operation,
            context,
            cancellation,
        })
    }

    /// Get the normalized operation associated with this retained identity.
    pub(crate) fn operation(&self) -> ProviderOperation {
        self.operation
    }

    /// Get the same context for the retained worker and every retry observer.
    pub(crate) fn context(&self) -> OperationContext {
        self.context.clone()
    }

    /// Request explicit interruption without dropping the worker or claiming non-consumption.
    pub(crate) fn cancel(&self) {
        self.cancellation.cancel();
    }
}

/// Record one host observation independently from the adapter's retained native worker.
struct OperationObservation {
    trace: Arc<crate::trace::TraceStore>,
    operation: ProviderOperation,
    id: OperationId,
    target: forge_provider_api::EventScope,
    binding: Option<forge_provider_api::BindingId>,
    responded: bool,
}

impl Drop for OperationObservation {
    /// Record a detached waiter without interpreting disposal as native cancellation.
    fn drop(&mut self) {
        if !self.responded {
            self.trace.record(
                self.target.owner().session_id.as_str(),
                "provider.operation.observation_detached",
                serde_json::json!({
                    "operation_id": self.id.as_str(), "operation": self.operation,
                    "scope": self.target, "binding_id": self.binding,
                }),
            );
        }
    }
}

/// Correlate a retained operation's host waiter without logging its input or native payloads.
/// A timeout response may leave native cleanup pending; its typed disposition remains visible.
pub(crate) async fn observe<T>(
    trace: Arc<crate::trace::TraceStore>,
    operation: ProviderOperation,
    context: &OperationContext,
    target: forge_provider_api::EventScope,
    binding: Option<forge_provider_api::BindingId>,
    work: impl Future<Output = Result<T, ProviderError>>,
) -> Result<T, ProviderError> {
    trace.record(
        target.owner().session_id.as_str(),
        "provider.operation.started",
        serde_json::json!({
            "operation_id": context.id().as_str(), "operation": operation,
            "scope": target, "binding_id": binding,
        }),
    );
    let mut observation = OperationObservation {
        trace,
        operation,
        id: context.id().clone(),
        target,
        binding,
        responded: false,
    };
    let result = work.await;
    let failure = result.as_ref().err().and_then(primary_failure);
    observation.trace.record(observation.target.owner().session_id.as_str(), "provider.operation.completed", serde_json::json!({
        "operation_id": observation.id.as_str(), "operation": operation,
        "scope": observation.target, "binding_id": observation.binding,
        "success": result.is_ok(),
        "reason": result.as_ref().err().map(|error| super::lifecycle_error::diagnostic(operation, error).kind),
        "phase": failure.map(ProviderFailure::phase),
        "disposition": failure.map(ProviderFailure::disposition),
    }));
    observation.responded = true;
    result
}

/// Read the primary typed phase while retaining independent cleanup causes separately.
fn primary_failure(error: &ProviderError) -> Option<ProviderFailure> {
    match error {
        ProviderError::Failure(failure) => Some(*failure),
        ProviderError::Combined { primary, .. } | ProviderError::PreparationFailed(primary) => {
            primary_failure(primary)
        }
        ProviderError::PreparationCleanup { cause, .. } => primary_failure(cause),
        ProviderError::Contract(_)
        | ProviderError::Cleanup(_)
        | ProviderError::Unsupported(_)
        | ProviderError::Observation(_)
        | ProviderError::RuntimeVersion(_)
        | ProviderError::NotConsumed
        | ProviderError::DeliveryUncertain
        | ProviderError::Cancelled
        | ProviderError::Retryable(_)
        | ProviderError::Delivery(_) => None,
    }
}
