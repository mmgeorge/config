//! Retain host-issued provider operation identity and explicit interruption authority.

use std::time::Duration;
use std::{future::Future, sync::Arc};

use crate::exchange::core_log::{CoreEventLog, CoreOperation, CoreOutcome, CoreRecord};
use forge_provider_api::{
    OperationCancellation, OperationCancellationSource, OperationContext, OperationId,
    ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
};

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
        ProviderOperation::InputSubmit | ProviderOperation::InputSteer => None,
        ProviderOperation::Cancel => Some(Duration::from_secs(30)),
        ProviderOperation::SessionClose | ProviderOperation::Shutdown => {
            Some(Duration::from_secs(120))
        }
        ProviderOperation::SessionOpen
        | ProviderOperation::SessionConfigure
        | ProviderOperation::Authenticate
        | ProviderOperation::Inspect
        | ProviderOperation::ModelList
        | ProviderOperation::SkillList
        | ProviderOperation::McpList
        | ProviderOperation::TerminalList => Some(Duration::from_secs(30)),
        // These existing limits still require a separate policy decision.
        // They are neither generation nor discovery/whole-binding cleanup operations.
        ProviderOperation::SessionFork
        | ProviderOperation::SessionCompact
        | ProviderOperation::InputReconcile
        | ProviderOperation::InputRetract
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
            operation: ProviderOperation::Cancel,
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
        target,
        binding,
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
    target: forge_provider_api::EventScope,
    binding: Option<forge_provider_api::BindingId>,
    purpose: ObservationPurpose,
    work: impl Future<Output = Result<T, ProviderError>>,
) -> ObservedResult<T> {
    let identity = CoreOperation::Provider {
        id: context.id().clone(),
        kind: operation,
        binding: binding.clone(),
        scope: target.clone(),
        invocation: target.invocation().cloned(),
    };
    let start_failure = core
        .append(
            target.owner().session_id.as_str(),
            &CoreRecord::OperationStarted {
                operation: identity.clone(),
            },
        )
        .err();
    if start_failure.is_some()
        && matches!(purpose, ObservationPurpose::Admission)
        && !matches!(
            operation,
            ProviderOperation::Cancel
                | ProviderOperation::SessionClose
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
    let outcome = match &result {
        Ok(_) => CoreOutcome::Succeeded,
        Err(error) => match primary_failure(error) {
            Some(failure) => CoreOutcome::ProviderFailed { failure },
            None => CoreOutcome::ProviderReason {
                kind: super::lifecycle_error::diagnostic(operation, error).kind,
            },
        },
    };
    let completion_log_failure = core
        .append(
            observation.target.owner().session_id.as_str(),
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
                ProviderOperation::InputSubmit
                | ProviderOperation::InputSteer
                | ProviderOperation::InputReconcile
                | ProviderOperation::InputRetract
                | ProviderOperation::SessionFork
                | ProviderOperation::SessionCompact
                | ProviderOperation::Cancel
                | ProviderOperation::TerminalTerminate
                | ProviderOperation::Authenticate => {
                    forge_provider_api::FailureDisposition::Uncertain
                }
                ProviderOperation::SessionOpen
                | ProviderOperation::SessionConfigure
                | ProviderOperation::SessionClose
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
    observation.trace.record(observation.target.owner().session_id.as_str(), "provider.operation.completed", serde_json::json!({
        "operation_id": observation.id.as_str(), "operation": operation,
        "scope": observation.target, "binding_id": observation.binding,
        "success": result.is_ok(),
        "reason": result.as_ref().err().map(|error| super::lifecycle_error::diagnostic(operation, error).kind),
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

#[cfg(test)]
mod tests {
    use super::*;

    /// Preserve possible credential mutation when completion evidence cannot be committed.
    #[tokio::test]
    async fn authentication_completion_log_failure_does_not_authorize_replay() -> anyhow::Result<()>
    {
        let root = tempfile::tempdir()?;
        crate::exchange::core_log::persist_test_session(root.path(), "session")?;
        let database = rusqlite::Connection::open(root.path().join("harness.sqlite3"))?;
        database.execute_batch("CREATE TRIGGER reject_auth_completion BEFORE INSERT ON owner_core_log WHEN json_extract(NEW.record,'$.record')='operation_completed' BEGIN SELECT RAISE(ABORT,'fixture completion storage failure'); END;")?;
        let trace = Arc::new(crate::trace::TraceStore::open(root.path())?);
        let core = CoreEventLog::new(root.path().to_owned());
        let control = OperationControl::fresh(ProviderOperation::Authenticate)?;
        let scope = forge_provider_api::EventScope::SessionOperation {
            owner: forge_provider_api::ExecutionOwner::root("session"),
            operation_id: control.context().id().clone(),
        };
        let mut credentials_changed = false;
        let result = observe(
            trace,
            &core,
            ProviderOperation::Authenticate,
            &control.context(),
            scope,
            None,
            async {
                credentials_changed = true;
                Ok(())
            },
        )
        .await;
        assert!(credentials_changed);
        assert!(matches!(result, Err(ProviderError::Failure(failure))
            if failure.kind() == ProviderFailureKind::Persistence
                && failure.disposition() == forge_provider_api::FailureDisposition::Uncertain));
        Ok(())
    }

    /// Disabled detail tracing still retains real provider IDs and exact normalized failure metadata.
    #[tokio::test]
    async fn disabled_trace_retains_provider_operation_identity_and_failure() -> anyhow::Result<()>
    {
        let root = tempfile::tempdir()?;
        drop(crate::storage::SqliteStore::open(root.path())?);
        let database = rusqlite::Connection::open(root.path().join("harness.sqlite3"))?;
        database.execute("INSERT INTO session_record(id,workspace,updated_at_ms,payload) VALUES('session','workspace',1,'{}')", [])?;
        let trace = Arc::new(crate::trace::TraceStore::open(root.path())?);
        trace.configure(false)?;
        let core = CoreEventLog::new(root.path().to_owned());
        let control = OperationControl::fresh(ProviderOperation::InputSubmit)?;
        let context = control.context();
        let scope = forge_provider_api::InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("session"),
            "exchange",
        );
        let expected = ProviderFailure::new(
            ProviderOperation::InputSubmit,
            ProviderFailureKind::Transport,
        )
        .at_phase(
            forge_provider_api::ProviderPhase::Observation,
            forge_provider_api::FailureDisposition::Uncertain,
        );
        let error = observe(
            trace,
            &core,
            ProviderOperation::InputSubmit,
            &context,
            scope.clone().into(),
            Some(forge_provider_api::BindingId::new("binding")?),
            async { Err::<(), _>(ProviderError::Failure(expected)) },
        )
        .await
        .unwrap_err();
        assert_eq!(primary_failure(&error), Some(expected));
        let encoded: String = database.query_row("SELECT record FROM owner_core_log WHERE json_extract(record,'$.record')='operation_completed'", [], |row| row.get(0))?;
        let record: CoreRecord = serde_json::from_str(&encoded)?;
        assert!(matches!(record, CoreRecord::OperationCompleted {
            operation: CoreOperation::Provider { id, kind: ProviderOperation::InputSubmit, invocation: Some(invocation), .. },
            outcome: CoreOutcome::ProviderFailed { failure },
        } if &id == context.id() && invocation == scope && failure == expected));
        Ok(())
    }

    /// Broken evidence storage cannot prevent safety cleanup or replace its original typed failure.
    #[tokio::test]
    async fn core_start_failure_preserves_cleanup_dispatch_and_native_cause() -> anyhow::Result<()>
    {
        let root = tempfile::tempdir()?;
        drop(crate::storage::SqliteStore::open(root.path())?);
        let database = rusqlite::Connection::open(root.path().join("harness.sqlite3"))?;
        database.execute("INSERT INTO session_record(id,workspace,updated_at_ms,payload) VALUES('session','workspace',1,'{}')", [])?;
        database.execute_batch("CREATE TRIGGER reject_operation_evidence BEFORE INSERT ON owner_core_log BEGIN SELECT RAISE(ABORT,'fixture core storage failure'); END;")?;
        let trace = Arc::new(crate::trace::TraceStore::open(root.path())?);
        trace.configure(false)?;
        let core = CoreEventLog::new(root.path().to_owned());
        let context = OperationControl::fresh(ProviderOperation::Cancel)?.context();
        let scope = forge_provider_api::InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("session"),
            "exchange",
        );
        let called = std::sync::atomic::AtomicBool::new(false);
        let expected =
            ProviderFailure::new(ProviderOperation::Cancel, ProviderFailureKind::Transport)
                .at_phase(
                    forge_provider_api::ProviderPhase::Interruption,
                    forge_provider_api::FailureDisposition::Uncertain,
                );
        let error = observe(
            trace,
            &core,
            ProviderOperation::Cancel,
            &context,
            scope.into(),
            None,
            async {
                called.store(true, std::sync::atomic::Ordering::Release);
                Err::<(), _>(ProviderError::Failure(expected))
            },
        )
        .await
        .unwrap_err();
        assert!(called.load(std::sync::atomic::Ordering::Acquire));
        assert!(
            matches!(error, ProviderError::Combined { primary, secondary }
            if matches!(*primary, ProviderError::Failure(failure) if failure == expected)
                && matches!(secondary.as_slice(), [ProviderError::Failure(failure)] if failure.kind() == ProviderFailureKind::Persistence))
        );
        Ok(())
    }
}
