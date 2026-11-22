//! Retain native result, provider collection, and sole lifecycle publication together.

use async_trait::async_trait;
use forge_provider_api::{
    CompletedTurn, FailureDisposition, NativeTurnStart, ProviderError, ProviderEventSink,
    ProviderFailure, ProviderFailureKind, ProviderOperation, ProviderPhase, TurnAddress,
    TurnResult,
};
use futures_util::FutureExt;
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::{Instrument, instrument::WithSubscriber};

use crate::retained_task::RetainedTask;

/// Keep protocol-specific work and collection behind one retained native owner.
#[async_trait]
pub trait NativeTurnDriver: Send + Sync + 'static {
    /// Drain admitted native activity and end foreground items before returning the authoritative result.
    async fn run(&self, start: NativeTurnStart) -> Result<TurnResult, ProviderError>;
    /// Request native interruption without assigning a replacement terminal outcome.
    async fn interrupt(&self) -> Result<(), ProviderError>;
    /// Join original foreground resources; preserve unfinished ownership when collection fails.
    async fn collect(&self) -> Result<(), ProviderError>;
}

/// Own the same native worker and collection across dropped or repeated result waits.
pub struct RetainedTurn {
    driver: Arc<dyn NativeTurnDriver>,
    worker: RetainedTask<CompletedTurn>,
    start: NativeTurnStart,
    native_result: Arc<Mutex<Option<Result<TurnResult, ProviderError>>>>,
}

impl RetainedTurn {
    /// Create the sole retained run, collection, and ordered publication worker.
    pub fn spawn(
        driver: Arc<dyn NativeTurnDriver>,
        sink: ProviderEventSink,
        address: TurnAddress,
    ) -> Result<Self, ProviderError> {
        let publisher = sink.turn_publisher(address)?;
        Ok(Self::spawn_owned(driver, publisher))
    }

    /// Retain an explicitly observed child's independent foreground work on its original binding.
    pub fn spawn_observed(
        driver: Arc<dyn NativeTurnDriver>,
        sink: ProviderEventSink,
        address: TurnAddress,
    ) -> Result<Self, ProviderError> {
        let publisher = sink.observed_turn_publisher(address)?;
        Ok(Self::spawn_owned(driver, publisher))
    }

    /// Spawn one lifecycle worker after its original publication authority has been validated.
    fn spawn_owned(
        driver: Arc<dyn NativeTurnDriver>,
        publisher: forge_provider_api::TurnPublisher,
    ) -> Self {
        let start = publisher.start_handle();
        let owned_start = start.clone();
        let native_result = Arc::new(Mutex::new(None));
        let retained = native_result.clone();
        let owned_driver = driver.clone();
        let work = async move {
            // Retain native evidence before entering provider-owned collection.
            let result = std::panic::AssertUnwindSafe(owned_driver.run(owned_start))
                .catch_unwind()
                .await
                .unwrap_or_else(|_| Err(runtime_failure(ProviderPhase::Observation)));
            *retained.lock().await = Some(result.clone());
            publisher.native_ended().await;

            // Collect the same resources even when observation failed or panicked.
            let collected = std::panic::AssertUnwindSafe(owned_driver.collect())
                .catch_unwind()
                .await
                .unwrap_or_else(|_| Err(runtime_failure(ProviderPhase::Collection)));
            if let Err(error) = collected {
                let original = match result {
                    Ok(_) => error,
                    Err(primary) => ProviderError::Combined {
                        primary: Box::new(primary),
                        secondary: vec![error],
                    },
                };
                publisher
                    .unavailable(normalized_failure(&original, ProviderPhase::Collection))
                    .await;
                return Err(original);
            }
            let result = match result {
                Ok(result) => result,
                Err(error) => {
                    let started = publisher.has_started().await;
                    if started || invalidates_binding(&error) {
                        publisher
                            .unavailable(normalized_failure(&error, ProviderPhase::Observation))
                            .await;
                    }
                    if !started {
                        publisher.not_started(error.clone());
                    }
                    return Err(error);
                }
            };

            // Publish only the retained native result after foreground ownership is collected.
            let collected_at_ms = timestamp_ms();
            publisher.completed(result, collected_at_ms).await
        };
        let worker = RetainedTask::new(
            ProviderOperation::TurnStart,
            tokio::spawn(work.in_current_span().with_current_subscriber()),
        );
        Self {
            driver,
            worker,
            start,
            native_result,
        }
    }

    /// Observe positive native start or preserve the original definitive pre-start operation error.
    pub async fn started(&self) -> Result<(), ProviderError> {
        tokio::select! {
            biased;
            () = self.start.wait_observed() => Ok(()),
            result = self.worker.join() => {
                if self.start.is_observed() { Ok(()) } else { result.map(|_| ()) }
            },
        }
    }

    /// Observe the same completed turn without cancelling its worker when the waiter drops.
    pub async fn join(&self) -> Result<CompletedTurn, ProviderError> {
        self.worker.join().await
    }

    /// Request interruption of only this retained native driver.
    pub async fn interrupt(&self) -> Result<(), ProviderError> {
        self.driver.interrupt().await
    }

    /// Check whether the retained publication worker has completed.
    pub fn is_finished(&self) -> bool {
        self.worker.is_finished()
    }

    /// Get native evidence retained before any subsequent cleanup or publication failure.
    pub async fn native_result(&self) -> Option<Result<TurnResult, ProviderError>> {
        self.native_result.lock().await.clone()
    }
}

/// Classify a turn's settlement for the interrupt that waited on it.
///
/// An interrupt succeeds when the turn left no native work running. A turn that completed, ended
/// as cancelled, or was cancelled before dispatch meets that: a pre-dispatch cancellation wrote
/// nothing to the provider, so no native turn exists to stop. The host retires a prepared
/// submission before it interrupts the turn, so the journal refuses the adapter's own rejection
/// record and the turn settles as `PreparationCleanup`; that refusal leaves the binding unchanged
/// and is not an interrupt failure. Every other settlement stays the interrupt's error.
pub(crate) fn interrupt_outcome<Settled>(
    settlement: Result<Settled, ProviderError>,
) -> Result<(), ProviderError> {
    match settlement {
        Ok(_) => Ok(()),
        Err(error) if left_no_native_work(&error) => Ok(()),
        Err(error) => Err(error),
    }
}

/// Identify a settlement whose turn stopped cleanly or never reached the provider.
fn left_no_native_work(error: &ProviderError) -> bool {
    match error {
        ProviderError::PreparationCleanup { cause, cleanup } => {
            cause.is_clean_cancellation() && !invalidates_binding(cleanup)
        }
        error => error.is_clean_cancellation(),
    }
}

/// Preserve explicit invalidation while leaving ordinary pre-start operation errors local.
fn invalidates_binding(error: &ProviderError) -> bool {
    match error {
        ProviderError::Failure(failure) => failure.disposition() == FailureDisposition::Invalidated,
        ProviderError::Delivery(_)
        | ProviderError::Contract(_)
        | ProviderError::Observation(_)
        | ProviderError::Cleanup(_) => true,
        ProviderError::Combined { primary, secondary } => {
            invalidates_binding(primary) || secondary.iter().any(invalidates_binding)
        }
        ProviderError::PreparationFailed(cause) | ProviderError::Explained { cause, .. } => {
            invalidates_binding(cause)
        }
        ProviderError::PreparationCleanup { cause, cleanup } => {
            invalidates_binding(cause) || invalidates_binding(cleanup)
        }
        ProviderError::RuntimeVersion(_)
        | ProviderError::RuntimeLaunch { .. }
        | ProviderError::Unsupported(_)
        | ProviderError::NotConsumed
        | ProviderError::DeliveryUncertain
        | ProviderError::Cancelled
        | ProviderError::Interrupted
        | ProviderError::Retryable(_) => false,
    }
}

/// Preserve an existing typed failure and normalize nonfailure operation errors at this boundary.
fn normalized_failure(error: &ProviderError, phase: ProviderPhase) -> ProviderFailure {
    match error {
        ProviderError::Failure(failure) => *failure,
        ProviderError::Combined { primary, .. } => normalized_failure(primary, phase),
        ProviderError::PreparationFailed(cause) => normalized_failure(cause, phase),
        ProviderError::PreparationCleanup { cause, .. }
        | ProviderError::Explained { cause, .. } => normalized_failure(cause, phase),
        ProviderError::Delivery(_)
        | ProviderError::Contract(_)
        | ProviderError::Observation(_)
        | ProviderError::Cleanup(_) => {
            ProviderFailure::new(ProviderOperation::TurnStart, ProviderFailureKind::Protocol)
                .at_phase(phase, FailureDisposition::Invalidated)
        }
        ProviderError::RuntimeVersion(_)
        | ProviderError::RuntimeLaunch { .. }
        | ProviderError::Unsupported(_)
        | ProviderError::NotConsumed
        | ProviderError::DeliveryUncertain
        | ProviderError::Cancelled
        | ProviderError::Interrupted
        | ProviderError::Retryable(_) => {
            ProviderFailure::new(ProviderOperation::TurnStart, ProviderFailureKind::Runtime)
                .at_phase(phase, FailureDisposition::Invalidated)
        }
    }
}

/// Normalize panics without exposing native panic text through the provider boundary.
fn runtime_failure(phase: ProviderPhase) -> ProviderError {
    ProviderFailure::new(ProviderOperation::TurnStart, ProviderFailureKind::Runtime)
        .at_phase(phase, FailureDisposition::Invalidated)
        .into()
}

/// Capture host collection time independently of the immutable native completion timestamp.
fn timestamp_ms() -> i64 {
    let elapsed = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    i64::try_from(elapsed.as_millis()).unwrap_or(i64::MAX)
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::turn::TurnOutcome;
    use forge_provider_api::{BindingId, InvocationScope, ProviderEvent, TurnEvent, events};
    use std::sync::atomic::{AtomicUsize, Ordering};

    struct Driver {
        start: bool,
        run_error: Option<ProviderError>,
        cleanup_error: Option<ProviderError>,
        activity: Option<(
            forge_provider_api::AdapterActivityWriter,
            Vec<forge_provider_api::AdapterActivity>,
        )>,
        cleanup_entered: tokio::sync::Notify,
        cleanup_release: tokio::sync::Notify,
        runs: AtomicUsize,
        collections: AtomicUsize,
    }

    impl Driver {
        /// Create a driver whose collection must be explicitly released by the test.
        fn new(
            start: bool,
            run_error: Option<ProviderError>,
            cleanup_error: Option<ProviderError>,
        ) -> Self {
            Self {
                start,
                run_error,
                cleanup_error,
                activity: None,
                cleanup_entered: Default::default(),
                cleanup_release: Default::default(),
                runs: AtomicUsize::new(0),
                collections: AtomicUsize::new(0),
            }
        }
    }

    #[async_trait]
    impl NativeTurnDriver for Driver {
        /// Supply one positively observed native outcome independently of cleanup.
        async fn run(&self, start: NativeTurnStart) -> Result<TurnResult, ProviderError> {
            self.runs.fetch_add(1, Ordering::Relaxed);
            if self.start {
                start.observed(1).await?;
            }
            if let Some((writer, activity)) = &self.activity {
                for event in activity {
                    writer.send_wait(event.clone()).await?;
                }
            }
            if let Some(error) = &self.run_error {
                return Err(error.clone());
            }
            Ok(
                TurnResult::new(TurnOutcome::Interrupted, 2, Some(1), vec![], vec![], None)
                    .unwrap(),
            )
        }
        /// Leave terminal interpretation to the native run result.
        async fn interrupt(&self) -> Result<(), ProviderError> {
            Ok(())
        }
        /// Hold cleanup open to expose dropped-waiter and publication races.
        async fn collect(&self) -> Result<(), ProviderError> {
            self.collections.fetch_add(1, Ordering::Relaxed);
            self.cleanup_entered.notify_one();
            self.cleanup_release.notified().await;
            self.cleanup_error.clone().map_or(Ok(()), Err)
        }
    }

    /// Allocate an admitted host turn before any driver work starts.
    fn fixture(
        driver: Arc<Driver>,
    ) -> (
        RetainedTurn,
        forge_provider_api::ProviderEventStream,
        forge_provider_api::TurnId,
    ) {
        let scope = InvocationScope::new(
            forge_provider_api::testing::root_thread("session"),
            "exchange",
        );
        let address = TurnAddress {
            thread_id: scope.thread_id.clone(),
            turn_id: scope.turn_id().unwrap(),
            scope: scope.clone(),
        };
        let (sink, stream) = events::channel(BindingId::new("binding").unwrap(), scope);
        (
            RetainedTurn::spawn(driver, sink, address.clone()).unwrap(),
            stream,
            address.turn_id,
        )
    }

    /// Failed provider collection preserves the established result and never emits completion.
    #[tokio::test]
    async fn cleanup_failure_retains_native_result_and_fences_without_completion() {
        let driver = Arc::new(Driver::new(
            true,
            None,
            Some(runtime_failure(ProviderPhase::Collection)),
        ));
        let (turn, mut stream, _turn_id) = fixture(driver.clone());
        driver.cleanup_entered.notified().await;
        driver.cleanup_release.notify_one();
        assert!(turn.join().await.is_err());
        assert_eq!(
            turn.native_result().await.unwrap().unwrap().outcome(),
            TurnOutcome::Interrupted
        );
        assert!(matches!(
            stream.recv().await.unwrap().unwrap().event,
            ProviderEvent::Turn {
                event: TurnEvent::Started(_),
                ..
            }
        ));
        assert!(matches!(
            stream.recv().await.unwrap().unwrap().event,
            ProviderEvent::Provider {
                event: forge_provider_api::ProviderConnectionEvent::Unavailable(_),
                ..
            }
        ));
        assert!(stream.recv().await.is_err());
    }

    /// An interrupt reports a turn collected when it stopped cleanly or never reached the provider.
    ///
    /// A cancellation before dispatch settles as a preparation failure, and as a preparation
    /// cleanup failure once the host has retired the journal entry that the adapter then tries
    /// to reject. Neither left native work behind. A cleanup failure that invalidates the
    /// binding, a preparation that failed for another reason, and a native failure all remain
    /// interrupt failures.
    #[test]
    fn interrupt_outcome_accepts_only_turns_that_left_no_native_work() {
        let journal_refusal: ProviderError = ProviderFailure::new(
            ProviderOperation::TurnStart,
            ProviderFailureKind::Persistence,
        )
        .at_phase(ProviderPhase::Persistence, FailureDisposition::Unchanged)
        .into();
        let invalidation: ProviderError =
            ProviderFailure::new(ProviderOperation::TurnStart, ProviderFailureKind::Transport)
                .at_phase(ProviderPhase::Collection, FailureDisposition::Invalidated)
                .into();
        let cancelled_before_dispatch =
            || ProviderError::PreparationFailed(Box::new(ProviderError::Cancelled));
        let collected = [
            ProviderError::Cancelled,
            cancelled_before_dispatch(),
            ProviderError::PreparationCleanup {
                cause: Box::new(ProviderError::Cancelled),
                cleanup: Box::new(journal_refusal.clone()),
            },
        ];
        for settlement in collected {
            assert!(
                interrupt_outcome::<()>(Err(settlement.clone())).is_ok(),
                "{settlement:?} left native work to stop"
            );
        }
        assert!(interrupt_outcome(Ok(())).is_ok());

        let uncollected = [
            ProviderError::PreparationCleanup {
                cause: Box::new(ProviderError::Cancelled),
                cleanup: Box::new(invalidation.clone()),
            },
            ProviderError::PreparationFailed(Box::new(journal_refusal.clone())),
            ProviderError::PreparationCleanup {
                cause: Box::new(journal_refusal),
                cleanup: Box::new(ProviderError::Cancelled),
            },
            ProviderError::NotConsumed,
            invalidation,
        ];
        for settlement in uncollected {
            assert!(
                interrupt_outcome::<()>(Err(settlement.clone())).is_err(),
                "{settlement:?} was reported collected"
            );
        }
    }
}
