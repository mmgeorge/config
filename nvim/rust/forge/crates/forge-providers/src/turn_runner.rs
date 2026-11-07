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
        let worker = RetainedTask::new(
            ProviderOperation::TurnStart,
            tokio::spawn(async move {
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
            }),
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
        ProviderError::PreparationFailed(cause) => invalidates_binding(cause),
        ProviderError::PreparationCleanup { cause, cleanup } => {
            invalidates_binding(cause) || invalidates_binding(cleanup)
        }
        ProviderError::RuntimeVersion(_)
        | ProviderError::Unsupported(_)
        | ProviderError::NotConsumed
        | ProviderError::DeliveryUncertain
        | ProviderError::Cancelled
        | ProviderError::Retryable(_) => false,
    }
}

/// Preserve an existing typed failure and normalize nonfailure operation errors at this boundary.
fn normalized_failure(error: &ProviderError, phase: ProviderPhase) -> ProviderFailure {
    match error {
        ProviderError::Failure(failure) => *failure,
        ProviderError::Combined { primary, .. } => normalized_failure(primary, phase),
        ProviderError::PreparationFailed(cause) => normalized_failure(cause, phase),
        ProviderError::PreparationCleanup { cause, .. } => normalized_failure(cause, phase),
        ProviderError::Delivery(_)
        | ProviderError::Contract(_)
        | ProviderError::Observation(_)
        | ProviderError::Cleanup(_) => {
            ProviderFailure::new(ProviderOperation::TurnStart, ProviderFailureKind::Protocol)
                .at_phase(phase, FailureDisposition::Invalidated)
        }
        ProviderError::RuntimeVersion(_)
        | ProviderError::Unsupported(_)
        | ProviderError::NotConsumed
        | ProviderError::DeliveryUncertain
        | ProviderError::Cancelled
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

    /// A cancelled result waiter must not release native completion before collection succeeds.
    #[tokio::test]
    async fn dropped_waiter_retains_native_outcome_and_publishes_once_after_collection() {
        let driver = Arc::new(Driver::new(true, None, None));
        let (turn, mut stream, _turn_id) = fixture(driver.clone());
        driver.cleanup_entered.notified().await;
        assert_eq!(
            turn.native_result().await.unwrap().unwrap().outcome(),
            TurnOutcome::Interrupted
        );
        assert!(!turn.is_finished());
        turn.started().await.unwrap();
        let mut waiter = Box::pin(turn.join());
        assert!(futures_util::poll!(waiter.as_mut()).is_pending());
        drop(waiter);
        turn.interrupt().await.unwrap();
        assert!(matches!(
            stream.try_recv().unwrap().event,
            ProviderEvent::Turn {
                event: TurnEvent::Started(_),
                ..
            }
        ));
        assert!(stream.try_next().unwrap().is_none());
        driver.cleanup_release.notify_one();
        let completed = tokio::time::timeout(std::time::Duration::from_secs(2), turn.join())
            .await
            .unwrap()
            .unwrap();
        assert_eq!(completed.result().outcome(), TurnOutcome::Interrupted);
        assert_eq!(turn.join().await.unwrap(), completed);
        assert!(matches!(
            stream.recv().await.unwrap().unwrap().event,
            ProviderEvent::Turn {
                event: TurnEvent::Completed(_),
                ..
            }
        ));
        assert!(stream.try_next().unwrap().is_none());
        assert_eq!(driver.runs.load(Ordering::Relaxed), 1);
        assert_eq!(driver.collections.load(Ordering::Relaxed), 1);
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

    /// Collection cannot reopen foreground work, while original background work remains independent.
    #[tokio::test]
    async fn native_end_seals_foreground_before_collection_and_preserves_background() {
        let scope = InvocationScope::new(
            forge_provider_api::testing::root_thread("session"),
            "exchange",
        );
        let address = TurnAddress {
            thread_id: scope.thread_id.clone(),
            turn_id: scope.turn_id().unwrap(),
            scope: scope.clone(),
        };
        let (sink, mut stream) = events::channel(BindingId::new("binding").unwrap(), scope);
        let writer = sink.activity_writer();
        let item = |id: &str, event| forge_provider_api::AdapterActivity::Item {
            thread_id: address.thread_id.clone(),
            turn_id: address.turn_id.clone(),
            item_id: forge_provider_api::ItemId::new(id).unwrap(),
            event,
        };
        let descriptor = forge_provider_api::ToolDescriptor {
            name: "command".into(),
            title: "Command".into(),
            semantics: Default::default(),
            arguments: forge_provider_api::InspectableArguments::new(serde_json::json!({})),
        };
        let mut driver = Driver::new(true, None, None);
        driver.activity = Some((
            writer.clone(),
            vec![
                item(
                    "background",
                    forge_provider_api::ItemEvent::Started(
                        forge_provider_api::ItemStart::CommandExecution(descriptor),
                    ),
                ),
                item(
                    "background",
                    forge_provider_api::ItemEvent::Tool {
                        event: forge_provider_api::ToolItemEvent::ExecutionStarted,
                    },
                ),
                item(
                    "background",
                    forge_provider_api::ItemEvent::Tool {
                        event: forge_provider_api::ToolItemEvent::Backgrounded,
                    },
                ),
            ],
        ));
        let driver = Arc::new(driver);
        let turn = RetainedTurn::spawn(driver.clone(), sink, address.clone()).unwrap();
        driver.cleanup_entered.notified().await;
        assert!(!turn.is_finished());
        while let Some(event) = stream.try_next().unwrap() {
            assert!(!matches!(
                event.event,
                ProviderEvent::Turn {
                    event: TurnEvent::Completed(_),
                    ..
                }
            ));
        }
        writer
            .send_wait(item(
                "background",
                forge_provider_api::ItemEvent::Tool {
                    event: forge_provider_api::ToolItemEvent::Output(
                        forge_provider_api::TextUpdate::Append("still running".into()),
                    ),
                },
            ))
            .await
            .unwrap();
        assert!(
            writer
                .send_wait(item(
                    "new-foreground",
                    forge_provider_api::ItemEvent::Started(
                        forge_provider_api::ItemStart::AgentMessage(
                            forge_provider_api::AgentMessageStart {
                                text: "unexpected".into(),
                                classification:
                                    forge_provider_api::MessageClassification::Unclassified,
                            }
                        ),
                    )
                ))
                .await
                .is_err()
        );
        driver.cleanup_release.notify_one();
        assert!(turn.join().await.is_err());
        assert_eq!(driver.collections.load(Ordering::Relaxed), 1);
    }

    /// A failed native collection must wake the original operation waiter without certifying no start.
    #[tokio::test]
    async fn prestart_collection_failure_wakes_waiter_without_no_start_certificate() {
        let driver = Arc::new(Driver::new(
            false,
            Some(ProviderError::Cancelled),
            Some(runtime_failure(ProviderPhase::Collection)),
        ));
        let (turn, _stream, turn_id) = fixture(driver.clone());
        driver.cleanup_entered.notified().await;
        let registrations = _stream.registrations();
        let unknown = forge_provider_api::TurnId::new("unknown").unwrap();
        assert!(matches!(
            registrations.start_result(&unknown).await,
            Err(ProviderError::Failure(error)) if error.kind() == ProviderFailureKind::WrongInvocation
        ));
        let mut waiting = Box::pin(registrations.start_result(&turn_id));
        assert!(futures_util::poll!(waiting.as_mut()).is_pending());
        driver.cleanup_release.notify_one();
        assert!(turn.join().await.is_err());
        let observed = tokio::time::timeout(std::time::Duration::from_secs(2), waiting)
            .await
            .expect("original retained operation must wake after binding failure");
        assert!(observed.is_err());
        assert!(registrations.not_started_error(&turn_id).is_none());
    }

    /// Cancellation before positive native start remains an operation error without invented turn events.
    #[tokio::test]
    async fn prestart_cancellation_collects_without_manufacturing_a_turn() {
        let driver = Arc::new(Driver::new(false, Some(ProviderError::Cancelled), None));
        let (turn, mut stream, turn_id) = fixture(driver.clone());
        driver.cleanup_entered.notified().await;
        let registrations = stream.registrations();
        assert!(registrations.not_started_error(&turn_id).is_none());
        let mut retained_observation = Box::pin(registrations.start_result(&turn_id));
        assert!(futures_util::poll!(retained_observation.as_mut()).is_pending());
        let mut starting = Box::pin(turn.started());
        assert!(futures_util::poll!(starting.as_mut()).is_pending());
        drop(starting);
        driver.cleanup_release.notify_one();
        assert!(matches!(turn.join().await, Err(ProviderError::Cancelled)));
        assert!(matches!(
            turn.started().await,
            Err(ProviderError::Cancelled)
        ));
        assert!(matches!(
            retained_observation.await,
            Err(ProviderError::Cancelled)
        ));
        assert!(matches!(
            registrations.not_started_error(&turn_id),
            Some(ProviderError::Cancelled)
        ));
        assert!(stream.try_next().unwrap().is_none());
        assert_eq!(driver.collections.load(Ordering::Relaxed), 1);
    }
}
