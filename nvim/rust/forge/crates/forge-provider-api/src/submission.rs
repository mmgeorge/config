use crate::ProviderError;
use async_trait::async_trait;
use std::sync::Arc;

/// Identify the journaled input handed to native transport.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DispatchTarget {
    /// Input accountability remains with the identified durable journal entry.
    Input(crate::SubmissionId),
}

/// Identify a confirmed transport handoff, independently of native acceptance.
/// A confirmation is valid only for this binding, invocation, and dispatch target.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DispatchConfirmation {
    binding: crate::BindingId,
    scope: crate::InvocationScope,
    target: DispatchTarget,
    attempt: crate::DeliveryAttemptId,
}

impl DispatchConfirmation {
    /// Get the live binding whose adapter owns cancellation and collection.
    pub fn binding(&self) -> &crate::BindingId {
        &self.binding
    }

    /// Get the exact invocation installed before transport handoff.
    pub fn scope(&self) -> &crate::InvocationScope {
        &self.scope
    }

    /// Get the immutable host-issued delivery attempt installed for this handoff.
    pub fn attempt_id(&self) -> &crate::DeliveryAttemptId {
        &self.attempt
    }

    /// Get the typed handoff target without implying acceptance or inspecting opaque authorization.
    pub fn target(&self) -> &DispatchTarget {
        &self.target
    }
}

/// Observe one attempt before its live binding is known, without taking native ownership.
/// Clones share the attempt; a retry must create a fresh tracker.
#[derive(Clone, Debug)]
pub struct DispatchTracker {
    identity: Arc<()>,
    progress: tokio::sync::watch::Sender<DispatchProgress>,
}

impl DispatchTracker {
    /// Get an observer that can attach before binding preparation or after confirmation.
    pub fn observer(&self) -> DispatchObserver {
        DispatchObserver {
            progress: self.progress.subscribe(),
        }
    }

    /// Bind a journaled send exactly once after immutable configuration and callbacks are installed.
    pub fn track_input(
        &self,
        binding: crate::BindingId,
        scope: crate::InvocationScope,
        submission: crate::SubmissionId,
        attempt: crate::DeliveryAttemptId,
    ) -> Result<DispatchReporter, DispatchBindingError> {
        self.track(binding, scope, DispatchTarget::Input(submission), attempt)
    }

    /// Mark an ended send observation as uncertain while preserving an already-confirmed handoff.
    /// This neither cancels work nor proves non-delivery and never authorizes replay.
    pub fn finish_observation(&self) {
        self.progress.send_if_modified(|state| match state {
            DispatchProgress::Preparing | DispatchProgress::Tracked(_) => {
                *state = DispatchProgress::Lost;
                true
            }
            DispatchProgress::Confirmed(_) | DispatchProgress::Lost => false,
        });
    }

    /// Retain observation cleanup in the actual native worker, independently of result waiters.
    pub fn observe_worker(&self) -> DispatchWorkerGuard {
        DispatchWorkerGuard {
            tracker: self.clone(),
        }
    }

    /// Install exact target identity atomically; a second binding cannot replace this attempt.
    fn track(
        &self,
        binding: crate::BindingId,
        scope: crate::InvocationScope,
        target: DispatchTarget,
        attempt: crate::DeliveryAttemptId,
    ) -> Result<DispatchReporter, DispatchBindingError> {
        let confirmation = DispatchConfirmation {
            binding,
            scope,
            target,
            attempt,
        };
        let installed = self.progress.send_if_modified(|state| match state {
            DispatchProgress::Preparing => {
                *state = DispatchProgress::Tracked(confirmation.clone());
                true
            }
            DispatchProgress::Tracked(_)
            | DispatchProgress::Confirmed(_)
            | DispatchProgress::Lost => false,
        });
        if !installed {
            return Err(DispatchBindingError);
        }
        Ok(DispatchReporter {
            identity: self.identity.clone(),
            confirmation,
            progress: self.progress.clone(),
        })
    }
}

impl Default for DispatchTracker {
    /// Create one unbound handoff attempt; it contains no native or persisted acceptance evidence.
    fn default() -> Self {
        Self {
            identity: Arc::new(()),
            progress: tokio::sync::watch::channel(DispatchProgress::Preparing).0,
        }
    }
}

/// End observation when its retained native worker exits, without owning cancellation.
/// Dropping an ordinary observer never drops this guard or changes the attempt.
#[must_use = "retain this guard for the native worker's lifetime"]
pub struct DispatchWorkerGuard {
    tracker: DispatchTracker,
}

impl Drop for DispatchWorkerGuard {
    /// Preserve confirmed dispatch and classify an unfinished worker observation as lost.
    fn drop(&mut self) {
        self.tracker.finish_observation();
    }
}

/// Publish tracked handoff only after cancellation/collection ownership is installed.
/// Clones retain one immutable target; they cannot confirm another attempt.
#[derive(Clone, Debug)]
pub struct DispatchReporter {
    identity: Arc<()>,
    confirmation: DispatchConfirmation,
    progress: tokio::sync::watch::Sender<DispatchProgress>,
}

impl PartialEq for DispatchReporter {
    /// Compare immutable handoff identity and the exact tracker channel, never its mutable progress.
    fn eq(&self, other: &Self) -> bool {
        self.confirmation == other.confirmation && Arc::ptr_eq(&self.identity, &other.identity)
    }
}
impl Eq for DispatchReporter {}

/// Preserve handoff identity for replay metadata without keeping a progress sender alive.
#[derive(Clone)]
pub(crate) struct DispatchIdentity {
    confirmation: DispatchConfirmation,
    identity: std::sync::Weak<()>,
}
impl PartialEq for DispatchIdentity {
    /// Compare immutable target and tracker allocation identity after observers have been released.
    fn eq(&self, other: &Self) -> bool {
        self.confirmation == other.confirmation
            && std::sync::Weak::ptr_eq(&self.identity, &other.identity)
    }
}
impl Eq for DispatchIdentity {}

impl DispatchReporter {
    /// Snapshot exact immutable correlation without prolonging handoff observation lifetime.
    pub(crate) fn identity(&self) -> DispatchIdentity {
        DispatchIdentity {
            confirmation: self.confirmation.clone(),
            identity: Arc::downgrade(&self.identity),
        }
    }

    /// Create a bound journal-input handoff channel for an adapter request.
    pub fn channel(
        binding: crate::BindingId,
        scope: crate::InvocationScope,
        submission: crate::SubmissionId,
        attempt: crate::DeliveryAttemptId,
    ) -> (Self, DispatchObserver) {
        let tracker = DispatchTracker::default();
        let observer = tracker.observer();
        let reporter = tracker
            .track_input(binding, scope, submission, attempt)
            .expect("a fresh dispatch tracker has no prior binding");
        (reporter, observer)
    }

    /// Confirm installed tracked handoff; repeated confirmation is idempotent.
    /// A late confirmation cannot revive a lost observation or manufacture native acceptance.
    pub fn confirm(&self) {
        self.progress.send_if_modified(|state| match state {
            DispatchProgress::Tracked(confirmation) if *confirmation == self.confirmation => {
                *state = DispatchProgress::Confirmed(self.confirmation.clone());
                true
            }
            DispatchProgress::Preparing
            | DispatchProgress::Tracked(_)
            | DispatchProgress::Confirmed(_)
            | DispatchProgress::Lost => false,
        });
    }

    /// Validate the journal input and immutable request scope before adapter admission.
    pub fn matches(
        &self,
        binding: &crate::BindingId,
        scope: &crate::InvocationScope,
        submission: &crate::SubmissionId,
        attempt: &crate::DeliveryAttemptId,
    ) -> bool {
        self.confirmation.binding == *binding
            && self.confirmation.scope == *scope
            && self.confirmation.target == DispatchTarget::Input(submission.clone())
            && self.confirmation.attempt == *attempt
    }
}

/// Observe handoff without owning or cancelling the underlying native work.
#[derive(Clone, Debug)]
pub struct DispatchObserver {
    progress: tokio::sync::watch::Receiver<DispatchProgress>,
}

impl DispatchObserver {
    /// Read currently available evidence without inferring dispatch from output or acceptance.
    pub fn current(&self) -> Result<Option<DispatchConfirmation>, DispatchObservationLost> {
        match &*self.progress.borrow() {
            DispatchProgress::Confirmed(confirmation) => Ok(Some(confirmation.clone())),
            DispatchProgress::Lost => Err(DispatchObservationLost),
            DispatchProgress::Preparing | DispatchProgress::Tracked(_) => {
                if self.progress.has_changed().is_err() {
                    Err(DispatchObservationLost)
                } else {
                    Ok(None)
                }
            }
        }
    }

    /// Await tracked handoff before or after binding, without waiting for the remote answer.
    /// Lost observation proves uncertainty, never non-delivery or permission to replay input.
    pub async fn wait(&mut self) -> Result<DispatchConfirmation, DispatchObservationLost> {
        loop {
            let progress = self.progress.borrow_and_update().clone();
            match progress {
                DispatchProgress::Confirmed(confirmation) => return Ok(confirmation),
                DispatchProgress::Lost => return Err(DispatchObservationLost),
                DispatchProgress::Preparing | DispatchProgress::Tracked(_) => {}
            }
            if self.progress.changed().await.is_err() {
                if let DispatchProgress::Confirmed(confirmation) =
                    self.progress.borrow_and_update().clone()
                {
                    return Ok(confirmation);
                }
                return Err(DispatchObservationLost);
            }
        }
    }
}

/// Reject replacement of an attempt's immutable binding or dispatch target.
#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
#[error("provider dispatch attempt was already bound or observation ended")]
pub struct DispatchBindingError;

/// Report an ended handoff observation without making a delivery assertion.
#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
#[error("provider dispatch confirmation was not observed")]
pub struct DispatchObservationLost;

/// Retain only facts about a single handoff attempt, independently of exchange lifecycle.
#[derive(Clone, Debug)]
enum DispatchProgress {
    Preparing,
    Tracked(DispatchConfirmation),
    Confirmed(DispatchConfirmation),
    Lost,
}

/// Commit host-owned delivery evidence around a native provider send.
/// Each instance binds one immutable logical input and delivery attempt. A retry supplies
/// a fresh instance; stale callbacks must validate that attempt atomically before journal writes.
#[async_trait]
pub trait InputReceipt: Send + Sync {
    /// Get the host-generated identity used for native input correlation.
    fn id(&self) -> &crate::SubmissionId;

    /// Get this receipt's current immutable delivery attempt for native correlation.
    fn attempt_id(&self) -> Result<crate::DeliveryAttemptId, ProviderError>;

    /// Commit opaque adapter recovery evidence before the provider receives input.
    async fn record_sending(
        self: Arc<Self>,
        token: Option<crate::RecoveryToken>,
    ) -> Result<(), ProviderError>;

    /// Commit native evidence that the provider accepted input.
    async fn record_accepted(
        self: Arc<Self>,
        receipt: Option<crate::RecoveryToken>,
    ) -> Result<(), ProviderError>;

    /// Record an explicit native rejection that proves input was not accepted.
    async fn record_rejected(self: Arc<Self>, reason: ProviderError) -> Result<(), ProviderError>;

    /// Record a failure without treating missing acknowledgment as rejection.
    async fn record_failure(self: Arc<Self>, reason: ProviderError) -> Result<(), ProviderError>;
}

#[cfg(test)]
mod tests {
    use crate::RecoveryToken;

    /// Release handoff observation while native acceptance is still outstanding.
    #[tokio::test]
    async fn dispatch_is_scoped_and_survives_dropped_waiters() {
        let binding = crate::BindingId::new("binding").unwrap();
        let scope = crate::InvocationScope::new(crate::ExecutionOwner::root("session"), "exchange");
        let submission = crate::SubmissionId::try_from("input").unwrap();
        let (reporter, mut observer) = super::DispatchReporter::channel(
            binding.clone(),
            scope.clone(),
            submission.clone(),
            crate::DeliveryAttemptId::new("attempt").unwrap(),
        );
        assert!(reporter.matches(
            &binding,
            &scope,
            &submission,
            &crate::DeliveryAttemptId::new("attempt").unwrap()
        ));
        assert!(!reporter.matches(
            &crate::BindingId::new("retired").unwrap(),
            &scope,
            &submission,
            &crate::DeliveryAttemptId::new("attempt").unwrap()
        ));
        assert!(!reporter.matches(
            &binding,
            &scope,
            &submission,
            &crate::DeliveryAttemptId::new("different-attempt").unwrap()
        ));
        drop(observer.clone());
        reporter.confirm();
        reporter.confirm();
        drop(reporter);
        let confirmation = observer.wait().await.unwrap();
        assert_eq!(confirmation.binding(), &binding);
        assert_eq!(confirmation.scope(), &scope);
        assert_eq!(
            confirmation.target(),
            &super::DispatchTarget::Input(submission)
        );
    }

    /// Treat a missing confirmation as uncertainty rather than permission to replay.
    #[tokio::test]
    async fn dispatch_reporter_loss_is_not_confirmation() {
        let (reporter, mut observer) = super::DispatchReporter::channel(
            crate::BindingId::new("binding").unwrap(),
            crate::InvocationScope::new(crate::ExecutionOwner::root("session"), "exchange"),
            crate::SubmissionId::try_from("input").unwrap(),
            crate::DeliveryAttemptId::new("attempt").unwrap(),
        );
        drop(reporter);
        assert_eq!(observer.wait().await, Err(super::DispatchObservationLost));
    }

    /// End worker observation without reviving it through a stale reporter or cancelling another attempt.
    #[tokio::test]
    async fn lost_dispatch_worker_cannot_confirm_a_later_attempt() {
        let tracker = super::DispatchTracker::default();
        let mut observer = tracker.observer();
        let binding = crate::BindingId::new("binding").unwrap();
        let scope = crate::InvocationScope::new(crate::ExecutionOwner::root("session"), "exchange");
        let submission = crate::SubmissionId::try_from("input").unwrap();
        let stale = tracker
            .track_input(
                binding.clone(),
                scope.clone(),
                submission.clone(),
                crate::DeliveryAttemptId::new("attempt").unwrap(),
            )
            .unwrap();
        let worker = tracker.observe_worker();
        drop(worker);
        stale.confirm();
        assert_eq!(observer.wait().await, Err(super::DispatchObservationLost));
        let next = super::DispatchTracker::default();
        let mut next_observer = next.observer();
        let current = next
            .track_input(
                binding,
                scope,
                submission,
                crate::DeliveryAttemptId::new("next-attempt").unwrap(),
            )
            .unwrap();
        drop(observer);
        current.confirm();
        assert!(next_observer.wait().await.is_ok());
    }

    /// Keep cached handoff identity from masking observation loss after its real reporter disappears.
    #[tokio::test]
    async fn cached_dispatch_identity_does_not_retain_progress_sender() {
        let (reporter, mut observer) = super::DispatchReporter::channel(
            crate::BindingId::new("weak-dispatch").unwrap(),
            crate::InvocationScope::new(crate::ExecutionOwner::root("session"), "exchange"),
            crate::SubmissionId::try_from("input").unwrap(),
            crate::DeliveryAttemptId::new("attempt").unwrap(),
        );
        let retained_identity = reporter.identity();
        drop(reporter);
        assert_eq!(observer.wait().await, Err(super::DispatchObservationLost));
        drop(retained_identity);
    }

    /// Persist opaque recovery evidence without exposing an adapter format to the host.
    #[test]
    fn recovery_token_round_trips_as_opaque_bytes() {
        let token = RecoveryToken::new(vec![4, 2, 1]).unwrap();
        let encoded = serde_json::to_vec(&token).unwrap();
        let restored: RecoveryToken = serde_json::from_slice(&encoded).unwrap();
        assert_eq!(restored.as_bytes(), &[4, 2, 1]);
        assert!(serde_json::from_str::<RecoveryToken>("[]").is_err());
    }
}
