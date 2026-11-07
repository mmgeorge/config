//! Keep host operation identity and native dispatch evidence inside the Copilot adapter.

use forge_provider_api::{OperationContext, OperationId, ProviderError, ProviderOperation};
use std::{
    future::Future,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};

tokio::task_local! { static CURRENT: Admission; }

type RevocationAction = Arc<dyn Fn() + Send + Sync>;

/// Retain the actual caller context and its independently observed native handoff boundary.
#[derive(Clone)]
pub(super) struct Admission {
    pub context: OperationContext,
    pub operation: ProviderOperation,
    dispatched: Arc<AtomicBool>,
    fence: Arc<std::sync::Mutex<Option<RevocationAction>>>,
}

impl Admission {
    /// Create a preparation boundary before any native mutation is possible.
    pub(super) fn new(context: OperationContext, operation: ProviderOperation) -> Self {
        Self {
            context,
            operation,
            dispatched: Arc::new(AtomicBool::new(false)),
            fence: Arc::new(std::sync::Mutex::new(None)),
        }
    }
    /// Get whether this exact operation crossed its adapter-owned mutation boundary.
    pub(super) fn dispatched(&self) -> bool {
        self.dispatched.load(Ordering::Acquire)
    }
    /// Revoke the exact native authority synchronously before asynchronous cleanup can race admission.
    pub(super) fn revoke(&self) {
        let fence = self
            .fence
            .lock()
            .expect("native operation fence is not poisoned")
            .clone();
        if let Some(fence) = fence {
            fence();
        }
    }
    /// Poll owned work under its immutable operation context without inheriting an observer lifetime.
    pub(super) async fn scope<T>(self, work: impl Future<Output = T>) -> T {
        CURRENT.scope(self, work).await
    }
}

/// Copy the current boundary only into native preparation or initial input dispatch workers.
pub(super) fn current() -> Option<Admission> {
    CURRENT.try_with(Clone::clone).ok()
}

/// Check the original deadline again after preparation locks and discovery waits.
pub(super) fn check() -> Result<(), ProviderError> {
    CURRENT
        .try_with(|admission| admission.context.check_admission(admission.operation))
        .unwrap_or(Ok(()))
}

/// Admit a native mutation immediately before handing its request to the SDK.
pub(super) fn dispatch() -> Result<(), ProviderError> {
    CURRENT
        .try_with(|admission| {
            admission.context.check_admission(admission.operation)?;
            admission.dispatched.store(true, Ordering::Release);
            Ok(())
        })
        .unwrap_or(Ok(()))
}

/// Check a native mutation only when it belongs to this public operation, preserving admitted cleanup.
pub(super) fn dispatch_for(operation: ProviderOperation) -> Result<(), ProviderError> {
    if CURRENT
        .try_with(|admission| admission.operation == operation)
        .unwrap_or(false)
    {
        dispatch()
    } else {
        Ok(())
    }
}

/// Install a weak, exact-owner revocation barrier before the first native mutation.
pub(super) fn fence(callback: impl Fn() + Send + Sync + 'static) {
    let _ = CURRENT.try_with(|admission| {
        *admission
            .fence
            .lock()
            .expect("native operation fence is not poisoned") = Some(Arc::new(callback));
    });
}

/// Get the host-issued identity without allocating another adapter operation identity.
pub(super) fn id() -> Result<OperationId, ProviderError> {
    CURRENT
        .try_with(|admission| admission.context.id().clone())
        .map_err(|_| {
            forge_provider_api::ProviderFailure::new(
                ProviderOperation::Inspect,
                forge_provider_api::ProviderFailureKind::OperationConflict,
            )
            .into()
        })
}

/// Preserve preparation context across the adapter's independently retained startup worker.
pub(super) async fn scope<T>(admission: Option<Admission>, work: impl Future<Output = T>) -> T {
    match admission {
        Some(admission) => admission.scope(work).await,
        None => work.await,
    }
}

/// Bound an individual native wait by its caller's original deadline while retaining its owner.
/// Cleanup without a caller deadline keeps waiting until its retained native work completes.
pub(super) async fn wait<T>(work: impl Future<Output = T>) -> Result<T, ProviderError> {
    let Some(admission) = current() else {
        return Ok(work.await);
    };
    let Some(deadline) = admission.context.deadline() else {
        return Ok(work.await);
    };
    tokio::time::timeout_at(deadline, work).await.map_err(|_| {
        forge_provider_api::ProviderFailure::new(
            admission.operation,
            forge_provider_api::ProviderFailureKind::Timeout,
        )
        .at_phase(
            forge_provider_api::ProviderPhase::Observation,
            forge_provider_api::FailureDisposition::Uncertain,
        )
        .into()
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    /// Consecutive native phases share the caller's original budget instead of resetting it.
    #[tokio::test(start_paused = true)]
    async fn native_phases_preserve_original_operation_deadline() {
        let (_source, cancellation) = forge_provider_api::OperationCancellation::channel();
        let context = OperationContext::new(
            OperationId::new("phases").unwrap(),
            Duration::from_secs(30),
            cancellation,
        )
        .unwrap();
        Admission::new(context, ProviderOperation::ThreadConfigure)
            .scope(async {
                wait(tokio::time::sleep(Duration::from_secs(20)))
                    .await
                    .unwrap();
                let began = tokio::time::Instant::now();
                let result = wait(std::future::pending::<()>()).await;
                assert_eq!(began.elapsed(), Duration::from_secs(10));
                assert!(matches!(result, Err(ProviderError::Failure(failure))
                if failure.kind() == forge_provider_api::ProviderFailureKind::Timeout));
            })
            .await;
    }

    /// A caller without a generation deadline keeps its native acknowledgement wait alive.
    #[tokio::test(start_paused = true)]
    async fn unbounded_input_wait_survives_previous_acceptance_limit() {
        let (_source, cancellation) = forge_provider_api::OperationCancellation::channel();
        let context = OperationContext::unbounded(OperationId::new("input").unwrap(), cancellation);
        Admission::new(context, ProviderOperation::TurnStart)
            .scope(async {
                let (send, receive) = tokio::sync::oneshot::channel();
                let waiting = wait(receive);
                tokio::pin!(waiting);
                assert!(futures_util::poll!(waiting.as_mut()).is_pending());
                tokio::time::advance(Duration::from_secs(3600)).await;
                assert!(futures_util::poll!(waiting.as_mut()).is_pending());
                send.send(()).unwrap();
                waiting.await.unwrap().unwrap();
            })
            .await;
    }
}
