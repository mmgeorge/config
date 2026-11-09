//! Retain host-issued provider operation identity and explicit interruption authority.
use crate::provider::ScopeOwner;

use std::time::Duration;
use std::{future::Future, sync::Arc};

use crate::exchange::core_log::{CoreEventLog, CoreOperation, CoreOutcome, CoreRecord};
use forge_provider_api::{
    OperationCancellation, OperationCancellationSource, OperationContext, OperationId,
    ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
};

/// Retain exact request identity and generation for operation-scoped native warnings.
#[derive(Default)]
pub(crate) struct OperationRegistry {
    records: std::sync::Mutex<std::collections::HashMap<OperationId, RegisteredOperation>>,
}
impl OperationRegistry {
    /// Register the original request before native work is polled; reject identity reassignment.
    pub(crate) fn register(
        &self,
        id: OperationId,
        scope: forge_provider_api::EventScope,
        binding: Option<forge_provider_api::BindingId>,
    ) -> Result<(), ProviderError> {
        let mut records = self
            .records
            .lock()
            .map_err(|_| operation_ownership_failure())?;
        if let Some(original) = records.get(&id) {
            if original.scope == scope && (original.binding == binding || binding.is_none()) {
                return Ok(());
            }
            return Err(operation_ownership_failure());
        }
        records.insert(id, RegisteredOperation { scope, binding });
        Ok(())
    }
    /// Attach the returned generation only to the exact request which opened it.
    pub(crate) fn bind_open(
        &self,
        id: &OperationId,
        binding: forge_provider_api::BindingId,
    ) -> Result<(), ProviderError> {
        let mut records = self
            .records
            .lock()
            .map_err(|_| operation_ownership_failure())?;
        let original = records
            .get_mut(id)
            .ok_or_else(operation_ownership_failure)?;
        if original
            .binding
            .as_ref()
            .is_some_and(|original| original != &binding)
        {
            return Err(operation_ownership_failure());
        }
        original.binding = Some(binding);
        Ok(())
    }
    /// Resolve only a registered operation under its original owner and binding generation.
    pub(crate) fn resolve(
        &self,
        id: &OperationId,
        binding: &forge_provider_api::BindingId,
        owner: &crate::provider::ThreadOwner,
    ) -> Result<forge_provider_api::EventScope, forge_provider_api::events::EventDeliveryError>
    {
        let records = self
            .records
            .lock()
            .map_err(|_| operation_warning_conflict())?;
        let original = records.get(id).ok_or_else(operation_warning_conflict)?;
        if original.binding.as_ref() != Some(binding) || original.scope.owner() != *owner {
            return Err(operation_warning_conflict());
        }
        Ok(forge_provider_api::EventScope::SessionOperation {
            thread_id: forge_provider_api::ThreadId::from(owner.clone()),
            operation_id: id.clone(),
        })
    }
}
struct RegisteredOperation {
    scope: forge_provider_api::EventScope,
    binding: Option<forge_provider_api::BindingId>,
}
/// Preserve the original host scope and native generation of one operation request.
pub(super) struct OperationTarget {
    pub(super) scope: forge_provider_api::EventScope,
    pub(super) binding: Option<forge_provider_api::BindingId>,
}

/// Preserve an owned successful result until its caller handles failed completion evidence.
pub(super) struct ObservedResult<T> {
    pub(super) result: Result<T, ProviderError>,
    pub(super) completion_failure: Option<ProviderError>,
}

/// Distinguish first effect admission from observation needed to finish already-owned cleanup.
pub(super) enum ObservationPurpose {
    /// Reject a new effect when required evidence cannot be retained.
    Admission,
    /// Reobserve the original native identity even when logging storage is unavailable.
    RetainedCleanup,
}

/// Get the deadline the host grants one provider operation before it observes uncertainty.
pub(crate) fn operation_timeout(operation: ProviderOperation) -> Option<Duration> {
    match operation {
        // Some native protocols acknowledge acceptance only when generation ends.
        ProviderOperation::TurnStart | ProviderOperation::TurnSteer => None,
        ProviderOperation::TurnInterrupt => Some(Duration::from_secs(30)),
        ProviderOperation::ThreadClose | ProviderOperation::Shutdown => {
            Some(Duration::from_secs(120))
        }
        ProviderOperation::ThreadOpen
        | ProviderOperation::ThreadConfigure
        | ProviderOperation::Inspect
        | ProviderOperation::ModelList
        | ProviderOperation::SkillList
        | ProviderOperation::McpList
        | ProviderOperation::TerminalList => Some(Duration::from_secs(30)),
        // These existing limits still require a separate policy decision.
        // They are neither generation nor discovery/whole-binding cleanup operations.
        ProviderOperation::ThreadFork
        | ProviderOperation::ThreadCompact
        | ProviderOperation::InputReconcile
        | ProviderOperation::InputRetract
        | ProviderOperation::ThreadRewind
        | ProviderOperation::TerminalTerminate => Some(Duration::from_secs(120)),
    }
}

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

    /// Create native collection owned beyond the enclosing operation's response.
    /// The exchange owner or tool watchdog owns reporting and retains this worker through
    /// settlement. Do not use this for a standalone finite cancellation observer.
    pub(super) fn cancellation_collection() -> Result<Self, ProviderError> {
        let id = OperationId::new(uuid::Uuid::new_v4().to_string())?;
        let (cancellation, observer) = OperationCancellation::channel();
        Ok(Self {
            operation: ProviderOperation::TurnInterrupt,
            context: OperationContext::unbounded(id, observer),
            cancellation,
        })
    }

    /// Retain a stable host identity for retries of the same original operation.
    pub(crate) fn new(
        operation: ProviderOperation,
        id: OperationId,
    ) -> Result<Self, ProviderError> {
        let timeout = operation_timeout(operation);
        let (cancellation, observer) = OperationCancellation::channel();
        let context = match timeout {
            Some(timeout) => OperationContext::new(id, timeout, observer).map_err(|_| {
                ProviderError::Failure(ProviderFailure::new(
                    operation,
                    ProviderFailureKind::Rejected,
                ))
            })?,
            None => OperationContext::unbounded(id, observer),
        };
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
                self.target.owner().session_id().as_str(),
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
    core: &CoreEventLog,
    operation: ProviderOperation,
    context: &OperationContext,
    target: forge_provider_api::EventScope,
    binding: Option<forge_provider_api::BindingId>,
    work: impl Future<Output = Result<T, ProviderError>>,
) -> Result<T, ProviderError> {
    let observed = observe_owned(
        trace,
        core,
        operation,
        context,
        OperationTarget {
            scope: target,
            binding,
        },
        ObservationPurpose::Admission,
        work,
    )
    .await;
    match (observed.result, observed.completion_failure) {
        (result, None) => result,
        (Ok(_), Some(error)) => Err(error),
        (Err(primary), Some(secondary)) => Err(ProviderError::Combined {
            primary: Box::new(primary),
            secondary: vec![secondary],
        }),
    }
}

/// Retain owned native output alongside metadata failure so open cannot lose its sole stream.
pub(super) async fn observe_owned<T>(
    trace: Arc<crate::trace::TraceStore>,
    core: &CoreEventLog,
    operation: ProviderOperation,
    context: &OperationContext,
    target: OperationTarget,
    purpose: ObservationPurpose,
    work: impl Future<Output = Result<T, ProviderError>>,
) -> ObservedResult<T> {
    let OperationTarget {
        scope: target,
        binding,
    } = target;
    if let Err(error) =
        core.operations()
            .register(context.id().clone(), target.clone(), binding.clone())
    {
        return ObservedResult {
            result: Err(error),
            completion_failure: None,
        };
    }
    let identity = CoreOperation::Provider {
        id: context.id().clone(),
        kind: operation,
        binding: binding.clone(),
        scope: target.clone(),
        invocation: target.invocation().cloned().map(Box::new),
    };
    let start_failure = core
        .append(
            target.owner().session_id().as_str(),
            &CoreRecord::OperationStarted {
                operation: identity.clone(),
            },
        )
        .err();
    if start_failure.is_some()
        && matches!(purpose, ObservationPurpose::Admission)
        && !matches!(
            operation,
            ProviderOperation::TurnInterrupt
                | ProviderOperation::ThreadClose
                | ProviderOperation::Shutdown
                | ProviderOperation::TerminalTerminate
        )
    {
        return ObservedResult {
            result: Err(persistence_failure(
                operation,
                forge_provider_api::FailureDisposition::Unchanged,
            )),
            completion_failure: None,
        };
    }
    trace.record(
        target.owner().session_id().as_str(),
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
    let outcome = match &result {
        Ok(_) => CoreOutcome::Succeeded,
        Err(error) => match primary_failure(error) {
            Some(failure) => CoreOutcome::ProviderFailed { failure },
            None => CoreOutcome::ProviderReason {
                kind: super::lifecycle_error::diagnostic(operation, error).kind(),
            },
        },
    };
    let completion_log_failure = core
        .append(
            observation.target.owner().session_id().as_str(),
            &CoreRecord::OperationCompleted {
                operation: identity,
                outcome,
            },
        )
        .err();
    let completion_failure = start_failure.or(completion_log_failure).map(|_| {
        persistence_failure(
            operation,
            match operation {
                ProviderOperation::TurnStart
                | ProviderOperation::TurnSteer
                | ProviderOperation::InputReconcile
                | ProviderOperation::InputRetract
                | ProviderOperation::ThreadRewind
                | ProviderOperation::ThreadFork
                | ProviderOperation::ThreadCompact
                | ProviderOperation::TurnInterrupt
                | ProviderOperation::TerminalTerminate => {
                    forge_provider_api::FailureDisposition::Uncertain
                }
                ProviderOperation::ThreadOpen
                | ProviderOperation::ThreadConfigure
                | ProviderOperation::ThreadClose
                | ProviderOperation::Shutdown => {
                    forge_provider_api::FailureDisposition::Invalidated
                }
                ProviderOperation::Inspect
                | ProviderOperation::ModelList
                | ProviderOperation::SkillList
                | ProviderOperation::McpList
                | ProviderOperation::TerminalList => {
                    forge_provider_api::FailureDisposition::Unchanged
                }
            },
        )
    });
    observation.trace.record(observation.target.owner().session_id().as_str(), "provider.operation.completed", serde_json::json!({
        "operation_id": observation.id.as_str(), "operation": operation,
        "scope": observation.target, "binding_id": observation.binding,
        "success": result.is_ok(),
        "kind": result.as_ref().err().map(|error| super::lifecycle_error::diagnostic(operation, error).kind()),
        "terminal_inventory_error": failure.and_then(ProviderFailure::terminal_inventory_error),
        "phase": failure.map(ProviderFailure::phase),
        "disposition": failure.map(ProviderFailure::disposition),
    }));
    observation.responded = true;
    ObservedResult {
        result,
        completion_failure,
    }
}

/// Classify metadata loss without copying SQLite text or claiming native non-consumption.
fn persistence_failure(
    operation: ProviderOperation,
    disposition: forge_provider_api::FailureDisposition,
) -> ProviderError {
    ProviderFailure::new(operation, ProviderFailureKind::Persistence)
        .at_phase(forge_provider_api::ProviderPhase::Persistence, disposition)
        .into()
}

/// Read the primary typed phase while retaining independent cleanup causes separately.
pub(super) fn primary_failure(error: &ProviderError) -> Option<ProviderFailure> {
    match error {
        ProviderError::Failure(failure) => Some(*failure),
        ProviderError::Combined { primary, .. } | ProviderError::PreparationFailed(primary) => {
            primary_failure(primary)
        }
        ProviderError::PreparationCleanup { cause, .. } => primary_failure(cause),
        ProviderError::Cleanup(report) => report.origin().and_then(primary_failure),
        ProviderError::Contract(_)
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

/// Reject operation identity reassignment before dispatch.
fn operation_ownership_failure() -> ProviderError {
    ProviderError::Observation(forge_provider_api::ObservationFailure::OwnershipConflict)
}
/// Reject an unregistered, foreign, or retired-generation operation warning.
fn operation_warning_conflict() -> forge_provider_api::events::EventDeliveryError {
    forge_provider_api::events::EventDeliveryError::Observation(
        forge_provider_api::ObservationFailure::OwnershipConflict,
    )
}
