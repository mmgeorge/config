//! Host-issued identities, bounded deadlines, and explicit cancellation for retained operations.

use crate::{
    FailureDisposition, OperationId, ProviderError, ProviderFailure, ProviderFailureKind,
    ProviderOperation, ProviderPhase,
};
use std::{sync::Arc, time::Duration};
use tokio::sync::watch;
use tokio::time::Instant;

/// Retain one immutable host-issued operation identity and its explicit interruption policy.
/// Clones denote the same operation. Adapters replay an existing disposition before checking
/// admission expiry, and reject reuse with a changed context or typed request. Dropping this
/// value or a future awaiting the operation never cancels native work. This context is runtime-only:
/// its monotonic deadline and cancellation-channel identity are not a cross-process recovery token.
/// Keep one instance per retained job; rebuilding an equal-looking ID with a new deadline or
/// cancellation channel denotes a conflicting request, not an extension of the original timeout.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct OperationContext {
    id: OperationId,
    deadline: Instant,
    cancellation: OperationCancellation,
}

impl OperationContext {
    /// Create a bounded operation from a validated host identity and explicit cancellation observer.
    /// Reject zero timeout or monotonic timer overflow without starting work. The deadline is
    /// captured once from the current monotonic clock; clones preserve that exact deadline.
    pub fn new(
        id: OperationId,
        timeout: Duration,
        cancellation: OperationCancellation,
    ) -> Result<Self, OperationContextError> {
        if timeout.is_zero() {
            return Err(OperationContextError::ExpiryRequired);
        }
        let deadline = Instant::now()
            .checked_add(timeout)
            .ok_or(OperationContextError::TimerRange)?;
        Ok(Self {
            id,
            deadline,
            cancellation,
        })
    }

    /// Get the host-issued identity retained for the entire operation retry lifetime.
    pub fn id(&self) -> &OperationId {
        &self.id
    }

    /// Get the bounded monotonic deadline; retries keep this original instant.
    pub const fn deadline(&self) -> Instant {
        self.deadline
    }

    /// Get the explicit interruption observer without acquiring cancellation authority.
    pub fn cancellation(&self) -> &OperationCancellation {
        &self.cancellation
    }

    /// Reject a new operation before any native mutation when its deadline or cancellation is reached.
    /// Resolve an existing retained record before calling this method. This proof applies only
    /// before dispatch; later interruption must preserve uncertain effects and cleanup ownership.
    pub fn check_admission(&self, operation: ProviderOperation) -> Result<(), ProviderError> {
        if self.cancellation.is_cancelled() {
            return Err(OperationInterruption::Cancelled.failure(
                operation,
                ProviderPhase::Admission,
                FailureDisposition::Unchanged,
            ));
        }
        if Instant::now() >= self.deadline {
            return Err(OperationInterruption::DeadlineElapsed.failure(
                operation,
                ProviderPhase::Admission,
                FailureDisposition::Unchanged,
            ));
        }
        Ok(())
    }

    /// Observe explicit cancellation or deadline expiry while the adapter retains operation ownership.
    /// When both are already ready, explicit cancellation takes precedence. This future only
    /// observes; dropping it does not request interruption or drop any native worker.
    pub async fn interrupted(&self) -> OperationInterruption {
        tokio::select! {
            biased;
            _ = self.cancellation.cancelled() => OperationInterruption::Cancelled,
            _ = tokio::time::sleep_until(self.deadline) => OperationInterruption::DeadlineElapsed,
        }
    }
}

/// Reject an absent deadline or a duration outside the runtime timer range.
#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
pub enum OperationContextError {
    /// Every retained operation requires a positive bounded timeout.
    #[error("provider operation timeout must be greater than zero")]
    ExpiryRequired,
    /// The configured timeout cannot be represented by the monotonic runtime clock.
    #[error("provider operation timeout exceeds the timer range")]
    TimerRange,
}

/// Classify explicit interruption without implying whether native mutation occurred.
#[derive(Clone, Copy, Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub enum OperationInterruption {
    /// The host explicitly requested interruption through its retained source.
    Cancelled,
    /// The immutable operation deadline elapsed.
    DeadlineElapsed,
}

impl OperationInterruption {
    /// Preserve the actual requested operation, phase, and known effect disposition.
    /// The caller supplies evidence for disposition; this conversion cannot determine whether
    /// native dispatch or mutation occurred and must not infer Unchanged from a timeout.
    pub fn failure(
        self,
        operation: ProviderOperation,
        phase: ProviderPhase,
        disposition: FailureDisposition,
    ) -> ProviderError {
        let kind = match self {
            Self::Cancelled => ProviderFailureKind::Cancelled,
            Self::DeadlineElapsed => ProviderFailureKind::Timeout,
        };
        ProviderFailure::new(operation, kind)
            .at_phase(phase, disposition)
            .into()
    }
}

/// Observe explicit host cancellation; source or observer disposal never requests it.
/// Equality compares the identity of the cancellation channel, not its mutable requested state.
#[derive(Clone, Debug)]
pub struct OperationCancellation {
    state: Arc<watch::Sender<bool>>,
}

impl OperationCancellation {
    /// Create explicit host authority and an independently clonable observation handle.
    /// All clones share one sticky request; dropping any or every source does not request it.
    pub fn channel() -> (OperationCancellationSource, Self) {
        let state = Arc::new(watch::channel(false).0);
        (
            OperationCancellationSource {
                state: state.clone(),
            },
            Self { state },
        )
    }

    /// Get whether the host explicitly requested this operation's interruption.
    pub fn is_cancelled(&self) -> bool {
        *self.state.borrow()
    }

    /// Await the sticky explicit request while retaining the channel independently of its source.
    pub async fn cancelled(&self) {
        let mut receiver = self.state.subscribe();
        loop {
            if *receiver.borrow_and_update() {
                return;
            }
            receiver
                .changed()
                .await
                .expect("operation observer retains its cancellation channel sender");
        }
    }
}

impl PartialEq for OperationCancellation {
    /// Compare cancellation authority identity without reading its mutable requested state.
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.state, &other.state)
    }
}
impl Eq for OperationCancellation {}

/// Own explicit host authority to request one operation's interruption.
/// Dropping every source leaves its observers uncancelled; call cancel to request interruption.
#[derive(Clone, Debug)]
pub struct OperationCancellationSource {
    state: Arc<watch::Sender<bool>>,
}

impl OperationCancellationSource {
    /// Request interruption once; all current and future observers retain the same sticky request.
    pub fn cancel(&self) {
        self.state.send_replace(true);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Keep operation cancellation explicit when every source is dropped before its observer.
    #[tokio::test]
    async fn dropping_cancellation_source_never_requests_interruption() {
        let (source, cancellation) = OperationCancellation::channel();
        let context = OperationContext::new(
            OperationId::new("explicit-cancellation").unwrap(),
            Duration::from_secs(60),
            cancellation.clone(),
        )
        .unwrap();
        drop(source);
        assert!(!cancellation.is_cancelled());
        assert!(
            context
                .check_admission(ProviderOperation::SessionOpen)
                .is_ok()
        );
        let (source, cancellation) = OperationCancellation::channel();
        let clone = cancellation.clone();
        source.cancel();
        clone.cancelled().await;
        drop(source);
        cancellation.cancelled().await;
        assert!(cancellation.is_cancelled());
    }

    /// Preserve cancellation as a typed unchanged admission rejection instead of a delivery claim.
    #[test]
    fn explicit_cancellation_rejects_new_admission_with_unchanged_disposition() {
        let (source, cancellation) = OperationCancellation::channel();
        let context = OperationContext::new(
            OperationId::new("cancelled-operation").unwrap(),
            Duration::from_secs(60),
            cancellation,
        )
        .unwrap();
        source.cancel();
        let ProviderError::Failure(failure) = context
            .check_admission(ProviderOperation::InputSubmit)
            .unwrap_err()
        else {
            panic!("interruption lost its typed failure");
        };
        assert_eq!(failure.kind(), ProviderFailureKind::Cancelled);
        assert_eq!(failure.disposition(), FailureDisposition::Unchanged);
        assert_eq!(failure.operation(), ProviderOperation::InputSubmit);
    }

    /// Observe the immutable deadline even when no host cancellation source remains alive.
    #[tokio::test]
    async fn operation_deadline_remains_bounded_without_a_cancellation_source() {
        let (source, cancellation) = OperationCancellation::channel();
        let context = OperationContext::new(
            OperationId::new("bounded-operation").unwrap(),
            Duration::from_millis(1),
            cancellation,
        )
        .unwrap();
        drop(source);
        assert_eq!(
            context.interrupted().await,
            OperationInterruption::DeadlineElapsed
        );
        let ProviderError::Failure(failure) = context
            .check_admission(ProviderOperation::McpList)
            .unwrap_err()
        else {
            panic!("deadline lost its typed failure");
        };
        assert_eq!(failure.kind(), ProviderFailureKind::Timeout);
        assert_eq!(failure.disposition(), FailureDisposition::Unchanged);
    }
}
