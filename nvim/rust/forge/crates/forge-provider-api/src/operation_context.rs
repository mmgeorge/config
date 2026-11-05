//! Host-issued identities, optional deadlines, and explicit cancellation for retained operations.

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
    deadline: Option<Instant>,
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
            deadline: Some(deadline),
            cancellation,
        })
    }

    /// Create an operation without an implicit duration limit, retaining explicit cancellation.
    /// Use this for generation and input acceptance unless the caller selected a finite limit.
    pub fn unbounded(id: OperationId, cancellation: OperationCancellation) -> Self {
        Self {
            id,
            deadline: None,
            cancellation,
        }
    }

    /// Get the host-issued identity retained for the entire operation retry lifetime.
    pub fn id(&self) -> &OperationId {
        &self.id
    }

    /// Get the original monotonic deadline, or None when duration is explicitly unbounded.
    pub const fn deadline(&self) -> Option<Instant> {
        self.deadline
    }

    /// Get the remaining original budget without restarting a timer at an internal phase.
    /// None means no implicit expiry; Some(Duration::ZERO) means the finite budget elapsed.
    pub fn remaining(&self) -> Option<Duration> {
        self.deadline
            .map(|deadline| deadline.saturating_duration_since(Instant::now()))
    }

    /// Restrict a fresh context to an enclosing command's original deadline before dispatch.
    /// Call this before exposing any clone to retained admission. Re-observations must reuse
    /// the resulting context unchanged; changing an already admitted context is a conflict.
    pub fn bounded_by(mut self, deadline: Instant) -> Self {
        self.deadline = Some(
            self.deadline
                .map_or(deadline, |existing| existing.min(deadline)),
        );
        self
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
        if self
            .deadline
            .is_some_and(|deadline| Instant::now() >= deadline)
        {
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
            _ = async {
                match self.deadline {
                    Some(deadline) => tokio::time::sleep_until(deadline).await,
                    None => std::future::pending().await,
                }
            } => OperationInterruption::DeadlineElapsed,
        }
    }
}

/// Reject an invalid finite deadline or a duration outside the runtime timer range.
#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
pub enum OperationContextError {
    /// A finite operation timeout must be positive; use unbounded for no expiry.
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

/// Select the reporting policy while cleanup ownership outlives its command response.
/// Start elapsed time at the original command admission. Closing a binding must not restart
/// or replace the response deadline of an already pending cancellation command.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CleanupPolicy {
    /// Interrupt one invocation and retain its collection after the cancellation response.
    Cancellation,
    /// Collect an entire binding or runtime outside an existing cancellation command.
    Close,
}

impl CleanupPolicy {
    /// Get the elapsed boundary for publishing one pending indication.
    pub const fn pending_after(self) -> Duration {
        match self {
            Self::Cancellation => Duration::from_secs(10),
            Self::Close => Duration::from_secs(30),
        }
    }

    /// Get the elapsed boundary for the sole unresolved command response.
    /// This ends observation, never native cleanup ownership or the reuse fence.
    pub const fn response_after(self) -> Duration {
        match self {
            Self::Cancellation => Duration::from_secs(30),
            Self::Close => Duration::from_secs(120),
        }
    }

    /// Classify elapsed cleanup reporting without claiming settlement or input non-consumption.
    /// The command owner publishes each transition once and suppresses a second response
    /// when a late authoritative result arrives. Check successful completion before this policy.
    pub fn progress(self, elapsed: Duration) -> Option<CleanupProgress> {
        if elapsed >= self.response_after() {
            return Some(CleanupProgress::TimedOut);
        }
        if elapsed >= self.pending_after() {
            return Some(CleanupProgress::Pending);
        }
        None
    }

    /// Get the safe user-facing message for a cleanup observation.
    pub const fn message(self, progress: CleanupProgress) -> &'static str {
        match (self, progress) {
            (Self::Cancellation, CleanupProgress::Pending) => "Cancellation still pending",
            (Self::Cancellation, CleanupProgress::TimedOut) => "Exceeded cancellation timeout",
            (Self::Close, CleanupProgress::Pending) => "Cleanup still pending",
            (Self::Close, CleanupProgress::TimedOut) => "Cleanup remains unresolved",
        }
    }
}

/// Observe unfinished cleanup independently of acceptance, execution outcome, and settlement.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CleanupProgress {
    /// Publish progress while retaining the command's original response deadline.
    Pending,
    /// Respond once while retaining cleanup resources and blocking unsafe reuse.
    TimedOut,
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

    /// Keep unbounded acceptance pending until explicit cancellation, preserving cloned identity.
    #[tokio::test]
    async fn unbounded_acceptance_waits_for_explicit_interruption() {
        let (source, cancellation) = OperationCancellation::channel();
        let context = OperationContext::unbounded(
            OperationId::new("unbounded-acceptance").unwrap(),
            cancellation,
        );
        assert_eq!(context.deadline(), None);
        assert_eq!(context.remaining(), None);
        let clone = context.clone();
        assert_eq!(context, clone);
        context
            .check_admission(ProviderOperation::InputSubmit)
            .unwrap();
        let interrupted = context.interrupted();
        tokio::pin!(interrupted);
        tokio::select! {
            biased;
            reason = &mut interrupted => panic!("unbounded acceptance expired: {reason:?}"),
            _ = std::future::ready(()) => {}
        }
        source.cancel();
        assert_eq!(interrupted.await, OperationInterruption::Cancelled);
    }

    /// Preserve the same finite budget through a cloned context after its timer elapses.
    #[tokio::test]
    async fn finite_remaining_budget_does_not_restart_after_expiry() {
        let (_, cancellation) = OperationCancellation::channel();
        let context = OperationContext::new(
            OperationId::new("propagated-budget").unwrap(),
            Duration::from_millis(1),
            cancellation,
        )
        .unwrap();
        let clone = context.clone();
        assert_eq!(context.deadline(), clone.deadline());
        assert_eq!(
            context.interrupted().await,
            OperationInterruption::DeadlineElapsed
        );
        assert_eq!(clone.remaining(), Some(Duration::ZERO));
    }

    /// Propagate an enclosing deadline without extending an already shorter operation budget.
    #[test]
    fn enclosing_deadline_only_shortens_fresh_operation_context() {
        let (_, cancellation) = OperationCancellation::channel();
        let context = OperationContext::new(
            OperationId::new("enclosing-budget").unwrap(),
            Duration::from_secs(2),
            cancellation.clone(),
        )
        .unwrap();
        let deadline = context.deadline().unwrap();
        assert_eq!(
            context
                .clone()
                .bounded_by(deadline + Duration::from_secs(1))
                .deadline(),
            Some(deadline)
        );
        let enclosing = deadline - Duration::from_secs(1);
        assert_eq!(context.bounded_by(enclosing).deadline(), Some(enclosing));
        let unbounded =
            OperationContext::unbounded(OperationId::new("explicit-budget").unwrap(), cancellation);
        assert_eq!(unbounded.bounded_by(enclosing).deadline(), Some(enclosing));
    }

    /// Report cancellation overdue without substituting the longer close observation budget.
    #[test]
    fn cleanup_reporting_preserves_cancellation_response_boundary() {
        let cancellation = CleanupPolicy::Cancellation;
        assert_eq!(cancellation.progress(Duration::from_secs(9)), None);
        assert_eq!(
            cancellation.progress(Duration::from_secs(10)),
            Some(CleanupProgress::Pending)
        );
        assert_eq!(
            cancellation.progress(Duration::from_secs(29)),
            Some(CleanupProgress::Pending)
        );
        assert_eq!(
            cancellation.progress(Duration::from_secs(30)),
            Some(CleanupProgress::TimedOut)
        );
        assert_eq!(
            cancellation.message(CleanupProgress::TimedOut),
            "Exceeded cancellation timeout"
        );
        assert_eq!(
            CleanupPolicy::Close.progress(Duration::from_secs(30)),
            Some(CleanupProgress::Pending)
        );
        assert_eq!(
            CleanupPolicy::Close.progress(Duration::from_secs(119)),
            Some(CleanupProgress::Pending)
        );
        assert_eq!(
            CleanupPolicy::Close.progress(Duration::from_secs(120)),
            Some(CleanupProgress::TimedOut)
        );
    }

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
