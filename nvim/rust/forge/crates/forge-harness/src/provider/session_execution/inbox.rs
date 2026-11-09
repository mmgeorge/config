use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, Weak};

use crate::provider::ProviderError;
type Result<T> = std::result::Result<T, ProviderError>;
use tokio::sync::Notify;

use super::OwnerObservation;
use crate::provider::HarnessEvent;

/// Distinguish explicitly collected owners from a native observation failure.
#[derive(Clone, Debug)]
pub(crate) enum ObservationEnd {
    Collected {
        reason: super::OwnerCloseReason,
        retired_binding: Option<forge_provider_api::BindingId>,
    },
    Lost {
        failure: forge_provider_api::lifecycle::ObservationFailure,
        retired_binding: Option<forge_provider_api::BindingId>,
    },
}

/// Capture an actual source boundary that may release a deferred configuration intent.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum NativeAvailabilityReceipt {
    TurnCollected {
        binding_id: forge_provider_api::BindingId,
        sequence: forge_provider_api::EventSequence,
    },
    AgentEnded {
        binding_id: forge_provider_api::BindingId,
        sequence: forge_provider_api::EventSequence,
    },
    BindingRetired(forge_provider_api::BindingId),
}

/// Retain provider session events while the foreground request owns the broker.
#[derive(Default)]
pub(crate) struct SessionObservationInbox {
    pending: Mutex<VecDeque<OwnerObservation>>,
    ready: Notify,
    closed: Arc<AtomicBool>,
    monitor_closed: Arc<Notify>,
    failure_reported: AtomicBool,
    failure: Mutex<Option<forge_provider_api::lifecycle::ObservationFailure>>,
    collected: Mutex<Option<super::OwnerCloseReason>>,
    retired_binding: Mutex<Option<forge_provider_api::BindingId>>,
    availability: Mutex<Option<NativeAvailabilityReceipt>>,
    watchdogs: Mutex<
        HashMap<
            crate::provider::InvocationId,
            Vec<Weak<crate::provider::tool_timeout::ToolWatchdog>>,
        >,
    >,
    retained_watchdogs: Mutex<
        HashMap<crate::provider::InvocationId, Arc<crate::provider::tool_timeout::ToolWatchdog>>,
    >,
    background_tasks: Mutex<tokio::task::JoinSet<()>>,
    background_collection: tokio::sync::Mutex<()>,
    watchdog_failure: Mutex<Option<Arc<forge_provider_api::error::ObservationCleanup>>>,
}

impl SessionObservationInbox {
    /// Get terminal ownership evidence after the accepted event prefix has drained.
    pub(crate) fn end(&self) -> Option<ObservationEnd> {
        if !self.is_closed() {
            return None;
        }
        let retired_binding = self
            .retired_binding
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone();
        if let Some(failure) = self
            .failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
        {
            return Some(ObservationEnd::Lost {
                failure,
                retired_binding,
            });
        }
        self.collected
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .map(|reason| ObservationEnd::Collected {
                reason,
                retired_binding,
            })
    }

    /// Get the last accepted native availability boundary, without treating it as collection proof.
    pub(crate) fn configuration_availability(&self) -> Option<NativeAvailabilityReceipt> {
        self.availability
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
    }

    /// Retain only an actual closed binding generation without changing the original observation cause.
    pub(crate) fn record_retired_binding(
        &self,
        binding: forge_provider_api::BindingId,
    ) -> Result<()> {
        let mut retired = self
            .retired_binding
            .lock()
            .map_err(|_| inbox_poisoned("retired binding receipt lock poisoned"))?;
        if retired
            .as_ref()
            .is_some_and(|previous| previous != &binding)
        {
            return Err(ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
            ));
        }
        *self
            .availability
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) =
            Some(NativeAvailabilityReceipt::BindingRetired(binding.clone()));
        *retired = Some(binding);
        Ok(())
    }

    /// Publish collected resource ownership without inventing native execution completion.
    pub(crate) fn close_collected(&self, reason: super::OwnerCloseReason) {
        *self
            .collected
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(reason);
        self.close();
    }
    /// Close observation only after the native owner has stopped publishing.
    pub(crate) fn close(&self) {
        let _tasks = self
            .background_tasks
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let _pending = self
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        self.closed.store(true, Ordering::Release);
        self.monitor_closed.notify_waiters();
        self.ready.notify_waiters();
    }

    /// Report whether the native owner has ended observation.
    pub(crate) fn is_closed(&self) -> bool {
        self.closed.load(Ordering::Acquire)
    }

    /// Retain the first terminal failure until preceding events drain.
    pub(crate) fn fail(&self, failure: forge_provider_api::lifecycle::ObservationFailure) {
        let mut current = self
            .failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        current.get_or_insert(failure);
        drop(current);
        self.ready.notify_waiters();
    }

    /// Queue a complete event before notifying either idle or active broker delivery.
    pub(crate) fn push(&self, event: HarnessEvent) -> Result<()> {
        self.observe_tools(&event)?;
        let mut pending = self
            .pending
            .lock()
            .map_err(|_| inbox_poisoned("session event inbox poisoned"))?;
        if self.is_closed() {
            return Err(inbox_poisoned("session observation generation is closed"));
        }
        if pending.len() == forge_protocol::MAX_PENDING_FRAMES {
            return Err(crate::provider::ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Overflow,
            ));
        }
        let availability = match &event {
            HarnessEvent::Native { source, event, .. } => match event.as_ref() {
                forge_provider_api::ProviderEvent::Turn {
                    event: forge_provider_api::TurnEvent::Completed(_),
                    ..
                } => Some(NativeAvailabilityReceipt::TurnCollected {
                    binding_id: source.binding_id.clone(),
                    sequence: source.sequence,
                }),
                forge_provider_api::ProviderEvent::Agent {
                    event:
                        forge_provider_api::agent::AgentEvent::Lifecycle {
                            event: forge_provider_api::agent::AgentLifecycleEvent::Finished { .. },
                        },
                    ..
                } => Some(NativeAvailabilityReceipt::AgentEnded {
                    binding_id: source.binding_id.clone(),
                    sequence: source.sequence,
                }),
                _ => None,
            },
            HarnessEvent::Host { .. } => None,
        };
        if let Some(availability) = availability {
            *self
                .availability
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(availability);
        }
        pending.push_back(OwnerObservation::Event(event));
        drop(pending);
        self.ready.notify_one();
        Ok(())
    }

    /// Preserve events and actual input receipts in one ordered owner queue.
    pub(crate) fn push_observation(&self, observation: OwnerObservation) -> Result<()> {
        match observation {
            OwnerObservation::Event(event) => self.push(event),
            OwnerObservation::HostFileChange {
                binding_id,
                scope,
                change,
            } => {
                if scope != change.scope {
                    return Err(ProviderError::Observation(
                        forge_provider_api::ObservationFailure::OwnershipConflict,
                    ));
                }
                let mut pending = self
                    .pending
                    .lock()
                    .map_err(|_| inbox_poisoned("session event inbox poisoned"))?;
                if self.is_closed() {
                    return Err(inbox_poisoned("session observation generation is closed"));
                }
                if pending.len() == forge_protocol::MAX_PENDING_FRAMES {
                    return Err(ProviderError::Observation(
                        forge_provider_api::ObservationFailure::Overflow,
                    ));
                }
                pending.push_back(OwnerObservation::HostFileChange {
                    binding_id,
                    scope,
                    change,
                });
                drop(pending);
                self.ready.notify_one();
                Ok(())
            }
            OwnerObservation::InputAcceptance {
                scope,
                receipt,
                presentation,
            } => self.push_acceptance(scope, receipt, presentation),
        }
    }

    /// Retain a committed input receipt without publishing a native or host event.
    pub(crate) fn push_acceptance(
        &self,
        scope: crate::provider::InvocationScope,
        receipt: Arc<crate::provider::submission::Submission>,
        presentation: super::InputPresentation,
    ) -> Result<()> {
        let mut pending = self
            .pending
            .lock()
            .map_err(|_| inbox_poisoned("session event inbox poisoned"))?;
        if self.is_closed() {
            return Err(inbox_poisoned("session observation generation is closed"));
        }
        if pending.len() == forge_protocol::MAX_PENDING_FRAMES {
            return Err(ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Overflow,
            ));
        }
        pending.push_back(OwnerObservation::InputAcceptance {
            scope,
            receipt,
            presentation,
        });
        drop(pending);
        self.ready.notify_one();
        Ok(())
    }

    /// Notify every retained attempt watchdog before foreground or idle delivery.
    fn observe_tools(&self, event: &HarnessEvent) -> Result<()> {
        if let Some(scope) = event.scope().invocation() {
            let watchdogs: Vec<_> = self
                .watchdogs
                .lock()
                .map_err(|_| inbox_poisoned("tool watchdog registry poisoned"))?
                .get(&scope.invocation_id)
                .into_iter()
                .flatten()
                .filter_map(Weak::upgrade)
                .collect();
            for watchdog in watchdogs {
                watchdog.observe(event).map_err(|error| {
                    crate::provider::lifecycle_error::classify(
                        forge_provider_api::error::ProviderOperation::TurnStart,
                        error,
                    )
                })?;
            }
        }
        Ok(())
    }

    /// Route late command observations to the watchdog that owns their original invocation.
    pub(crate) fn watch_tools(
        &self,
        scope: &crate::provider::InvocationScope,
        watchdog: &Arc<crate::provider::tool_timeout::ToolWatchdog>,
    ) -> Result<()> {
        let mut current = self
            .retained_watchdogs
            .lock()
            .map_err(|_| inbox_poisoned("retained watchdog lock poisoned"))?;
        let mut watchdogs = self
            .watchdogs
            .lock()
            .map_err(|_| inbox_poisoned("tool watchdog registry poisoned"))?;
        for retained in watchdogs.values_mut() {
            retained.retain(|watchdog| watchdog.strong_count() > 0);
        }
        watchdogs.retain(|_, retained| !retained.is_empty());
        let retained = watchdogs.entry(scope.invocation_id.clone()).or_default();
        if !retained
            .iter()
            .any(|previous| previous.ptr_eq(&Arc::downgrade(watchdog)))
        {
            retained.push(Arc::downgrade(watchdog));
        }
        if let Some(previous) = current.get(&scope.invocation_id)
            && !Arc::ptr_eq(previous, watchdog)
        {
            previous.retire_idle().map_err(|error| {
                crate::provider::lifecycle_error::classify(
                    forge_provider_api::error::ProviderOperation::TurnStart,
                    error,
                )
            })?;
        }
        current.insert(scope.invocation_id.clone(), watchdog.clone());
        Ok(())
    }

    /// Retain admitted tool deadlines after the foreground result waiter leaves.
    pub(crate) fn monitor_background(
        self: &Arc<Self>,
        scope: crate::provider::InvocationScope,
        watchdog: Arc<crate::provider::tool_timeout::ToolWatchdog>,
        provider: Arc<crate::provider::Provider>,
    ) -> Result<()> {
        watchdog.finish_foreground().map_err(|error| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::TurnStart,
                error,
            )
        })?;
        self.start_tool_monitor(scope, watchdog, provider)
    }

    /// Observe admitted tools independently of foreground request and result-waiter lifetimes.
    pub(crate) fn start_tool_monitor(
        self: &Arc<Self>,
        scope: crate::provider::InvocationScope,
        watchdog: Arc<crate::provider::tool_timeout::ToolWatchdog>,
        provider: Arc<crate::provider::Provider>,
    ) -> Result<()> {
        let mut tasks = self
            .background_tasks
            .lock()
            .map_err(|_| inbox_poisoned("background watchdog task registry poisoned"))?;
        if self.is_closed() {
            return Err(inbox_poisoned("session observation generation is closed"));
        }
        while let Some(result) = tasks.try_join_next() {
            if let Err(error) = result
                && !error.is_cancelled()
            {
                let failure = self.retain_watchdog_failure(
                    crate::provider::lifecycle_error::classify_source(
                        forge_provider_api::error::ProviderOperation::TurnStart,
                        &error,
                    ),
                );
                return Err(crate::provider::ProviderError::Cleanup(failure));
            }
        }
        if watchdog.is_monitored() {
            return Ok(());
        }
        if !watchdog.start_monitor() {
            return Ok(());
        }
        let inbox = Arc::downgrade(self);
        let closed = self.closed.clone();
        let monitor_closed = self.monitor_closed.clone();
        tasks.spawn(async move {
            let result: Result<()> = async {
                loop {
                    let timeout = tokio::select! {
                        biased;
                        () = wait_for_close(&closed, &monitor_closed) => return Ok(()),
                        timeout = watchdog.expired() => timeout.map_err(|error| crate::provider::lifecycle_error::classify(forge_provider_api::error::ProviderOperation::TurnStart, error))?,
                    };
                    let Some(timeout) = timeout else {
                        if !tokio::select! {
                            biased;
                            () = wait_for_close(&closed, &monitor_closed) => Ok(false),
                            resumed = watchdog.resumed() => resumed.map_err(|error| crate::provider::lifecycle_error::classify(forge_provider_api::error::ProviderOperation::TurnStart, error)),
                        }? {
                            return Ok(());
                        }
                        continue;
                    };
                    // The attached foreground fails its exchange and collects the native turn
                    // itself. Collecting here as well closed the observation stream the
                    // foreground was still reading, so its failure settlement could stop in
                    // Finalizing. Only a deadline left unreported by the released foreground is
                    // the monitor's to collect.
                    tokio::select! {
                        biased;
                        () = wait_for_close(&closed, &monitor_closed) => return Ok(()),
                        released = watchdog.foreground_released() => released.map_err(|error| crate::provider::lifecycle_error::classify(forge_provider_api::error::ProviderOperation::TurnStart, error))?,
                    }
                    if !timeout.claim_report() {
                        return Ok(());
                    }
                    let publication = (|| -> Result<()> {
                        let Some(inbox) = inbox.upgrade() else {
                            return Ok(());
                        };
                        inbox.push(HarnessEvent::Host { scope: scope.clone().into(), event: crate::provider::HostEvent::Failure {
                            failure: forge_provider_api::ProviderFailure::new(forge_provider_api::ProviderOperation::TurnStart,
                                forge_provider_api::ProviderFailureKind::Timeout),
                            message: forge_provider_api::SanitizedMessage::new("Tool execution timed out").expect("static text is nonempty"),
                        } })

                    })();
                    if let Err(ProviderError::Observation(failure)) = &publication
                        && let Some(inbox) = inbox.upgrade()
                    {
                        inbox.fail(failure.clone());
                    }
                    // Stop native work even when timeout publication fails.
                    let cancellation = provider.collect_invocation(scope.clone()).await.map(|_| ());
                    match (publication, cancellation) {
                        (Err(publication), Err(cancellation)) => {
                            return Err(ProviderError::Combined { primary: Box::new(publication), secondary: vec![cancellation] });
                        }
                        (Err(error), Ok(())) | (Ok(()), Err(error)) => return Err(error),
                        (Ok(()), Ok(())) => {}
                    }
                    return Ok(());
                }
            }
            .await;
            if let Err(error) = result
                && let Some(inbox) = inbox.upgrade()
            {
                let _ = inbox.push(HarnessEvent::Host { scope: scope.into(), event: crate::provider::HostEvent::Failure {
                    failure: forge_provider_api::ProviderFailure::new(forge_provider_api::ProviderOperation::ThreadClose,
                        forge_provider_api::ProviderFailureKind::Runtime),
                    message: forge_provider_api::SanitizedMessage::new("Tool timeout cleanup failed").expect("static text is nonempty"),
                } });
                inbox.retain_watchdog_failure(error);
                inbox.fail(forge_provider_api::lifecycle::ObservationFailure::LifecycleUnavailable);
                inbox.close();
            }
        });
        Ok(())
    }

    /// Close idle watchdogs and join already-started collection before replacing their native owner.
    pub(crate) async fn retire_background(&self) -> Result<()> {
        let _collection = self.background_collection.lock().await;
        self.close();
        loop {
            let result = std::future::poll_fn(|context| match self.background_tasks.lock() {
                Ok(mut tasks) => tasks.poll_join_next(context).map(Ok),
                Err(_) => std::task::Poll::Ready(Err(inbox_poisoned(
                    "background watchdog task registry poisoned",
                ))),
            })
            .await?;
            let Some(result) = result else {
                break;
            };
            if let Err(error) = result
                && !error.is_cancelled()
            {
                self.retain_watchdog_failure(crate::provider::lifecycle_error::classify_source(
                    forge_provider_api::error::ProviderOperation::TurnStart,
                    &error,
                ));
            }
        }
        let failure = self
            .watchdog_failure
            .lock()
            .map_err(|_| inbox_poisoned("background watchdog failure lock poisoned"))?
            .clone();
        if let Some(failure) = failure {
            return Err(crate::provider::ProviderError::Cleanup(failure));
        }
        Ok(())
    }

    /// Retain every monitor failure while preserving the first observation across collection.
    fn retain_watchdog_failure(
        &self,
        error: crate::provider::ProviderError,
    ) -> Arc<forge_provider_api::error::ObservationCleanup> {
        let observation = self
            .failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
            .unwrap_or(forge_provider_api::lifecycle::ObservationFailure::LifecycleUnavailable);
        let mut failure = self
            .watchdog_failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let report = failure.get_or_insert_with(|| {
            Arc::new(forge_provider_api::error::ObservationCleanup::new(
                observation,
                None,
                None,
                None,
            ))
        });
        Arc::make_mut(report).record_collection_failure(
            forge_provider_api::error::CleanupFailure::Failed(Box::new(error)),
        );
        report.clone()
    }

    /// Take queued events in provider order without holding the lock during delivery.
    pub(crate) fn drain(&self) -> Result<Vec<OwnerObservation>> {
        self.drain_up_to(usize::MAX)
    }

    /// Take only the prefix that fits its next owner, preserving sticky failure until a nonzero drain.
    pub(crate) fn drain_up_to(&self, limit: usize) -> Result<Vec<OwnerObservation>> {
        if limit == 0 {
            return Ok(Vec::new());
        }
        let events: Vec<_> = {
            let mut pending = self
                .pending
                .lock()
                .map_err(|_| inbox_poisoned("session event inbox poisoned"))?;
            let count = limit.min(pending.len());
            pending.drain(..count).collect()
        };
        if events.is_empty()
            && self.is_closed()
            && let Some(failure) = self
                .failure
                .lock()
                .map_err(|_| inbox_poisoned("session observation failure poisoned"))?
                .clone()
        {
            self.failure_reported.store(true, Ordering::Release);
            return Err(crate::provider::ProviderError::Observation(failure));
        }
        Ok(events)
    }

    /// Require old observations and their failure to be consumed before replacing the owner.
    pub(crate) fn ready_for_replacement(&self) -> Result<bool> {
        let empty = self
            .pending
            .lock()
            .map_err(|_| inbox_poisoned("session event inbox poisoned"))?
            .is_empty();
        let failed = self
            .failure
            .lock()
            .map_err(|_| inbox_poisoned("session observation failure poisoned"))?
            .is_some();
        Ok(self.is_closed() && empty && (!failed || self.failure_reported.load(Ordering::Acquire)))
    }

    /// Check for work before the idle collector waits for broker ownership.
    pub(crate) fn has_pending(&self) -> Result<bool> {
        let pending = !self
            .pending
            .lock()
            .map_err(|_| inbox_poisoned("session event inbox poisoned"))?
            .is_empty();
        Ok(pending
            || (self.is_closed()
                && self
                    .failure
                    .lock()
                    .map_err(|_| inbox_poisoned("session observation failure poisoned"))?
                    .is_some()))
    }

    /// Wait for pending events without losing a notification between inspection and sleep.
    pub(crate) async fn notified(&self) -> Result<()> {
        let notified = self.ready.notified();
        tokio::pin!(notified);
        notified.as_mut().enable();
        if self.is_closed() {
            return Ok(());
        }
        if !self
            .pending
            .lock()
            .map_err(|_| inbox_poisoned("session event inbox poisoned"))?
            .is_empty()
        {
            return Ok(());
        }
        notified.await;
        Ok(())
    }
}

impl Drop for SessionObservationInbox {
    fn drop(&mut self) {
        self.background_tasks
            .get_mut()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .abort_all();
    }
}

/// Wait for generation closure independently of ordinary event-queue readiness.
async fn wait_for_close(closed: &AtomicBool, changed: &Notify) {
    loop {
        let notified = changed.notified();
        tokio::pin!(notified);
        notified.as_mut().enable();
        if closed.load(Ordering::Acquire) {
            return;
        }
        notified.await;
    }
}

impl crate::provider::events::EventObserver for SessionObservationInbox {
    /// Route active invocation events to retained background watchdogs as well as the current attempt.
    fn before_delivery(
        &self,
        event: &HarnessEvent,
    ) -> std::result::Result<(), forge_provider_api::events::EventDeliveryError> {
        self.observe_tools(event)
            .map_err(forge_provider_api::events::EventDeliveryError::transport)
    }
}

/// Preserve typed inbox synchronization failures at the engine boundary.
fn inbox_poisoned(_component: &'static str) -> ProviderError {
    ProviderError::Failure(forge_provider_api::error::ProviderFailure::new(
        forge_provider_api::error::ProviderOperation::TurnStart,
        forge_provider_api::ProviderFailureKind::Runtime,
    ))
}

#[cfg(test)]
mod tests {
    use crate::provider::ScopeOwner;

    use super::*;
    use anyhow::Result;

    /// Create a scoped host notification for queue ordering fixtures.
    fn notice(scope: crate::provider::EventScope, message: impl Into<String>) -> HarnessEvent {
        HarnessEvent::Host {
            scope,
            event: crate::provider::HostEvent::Failure {
                failure: forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Unknown,
                ),
                message: forge_provider_api::SanitizedMessage::new(message)
                    .expect("fixture text is nonempty"),
            },
        }
    }
    /// Create an original-owner tool observation for deadline fixtures.
    fn tool_observation(
        address: crate::provider::ToolAddress,
        event: forge_provider_api::message::ItemEvent,
    ) -> HarnessEvent {
        crate::provider::test_support::native_event(
            address.turn.scope.clone(),
            forge_provider_api::ProviderEvent::Item {
                thread_id: address.turn.thread_id,
                turn_id: address.turn.turn_id,
                item_id: address.item_id,
                event,
            },
        )
    }

    /// Retain real admitted native work independently of its foreground result waiter.
    struct AdmittedWork {
        _session: crate::provider::test_support::ProviderTestSession,
        registry: super::super::SessionExecutionRegistry,
        _engine: Arc<super::super::SessionExecution>,
        provider: Arc<crate::provider::Provider>,
        scope: crate::provider::InvocationScope,
        turn: crate::provider::TurnAddress,
        result: tokio::task::JoinHandle<
            std::result::Result<crate::provider::ProviderCompletion, ProviderError>,
        >,
        // Declared last: fields drop in order, and Windows cannot delete files still open.
        _directory: tempfile::TempDir,
    }

    impl AdmittedWork {
        /// Admit a cancellable invocation and await its real native Started boundary.
        async fn start() -> Result<Self> {
            let directory = tempfile::tempdir()?;
            let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
            let store = crate::provider::submission::SubmissionStore::open(
                directory.path(),
                trace.clone(),
            )?;
            let started = Arc::new(tokio::sync::Semaphore::new(0));
            let entered = started.clone();
            let native = crate::provider::test_support::EventFixtureProvider::new(
                forge_provider_api::ProviderDescriptor {
                    kind: forge_providers::registry::nonselectable_id()
                        .expect("registry supplies fixture identity"),
                    label: "Watchdog collection fixture".into(),
                    capability: Default::default(),
                },
                move |request, _sink, start, mut cancelled| {
                    let entered = entered.clone();
                    async move {
                        start.observed(1).await?;
                        entered.add_permits(1);
                        loop {
                            if *cancelled.borrow_and_update() {
                                break;
                            }
                            if cancelled.changed().await.is_err() {
                                return Err(ProviderError::DeliveryUncertain);
                            }
                        }
                        Ok(crate::provider::test_support::native_result(
                            &request,
                            forge_provider_api::turn::TurnOutcome::Interrupted,
                        ))
                    }
                },
            );
            let session = crate::provider::test_support::ProviderTestSession::from_native(native)?;
            let provider = session.provider_handle();
            let registry = super::super::SessionExecutionRegistry::new(provider.clone(), trace);
            let scope = crate::provider::InvocationScope::new(
                crate::provider::ThreadOwner::root("session"),
                "exchange",
            );
            let engine = registry.owner(scope.owner().clone()).await?;
            let input = crate::provider::ProviderRequest {
                dispatch: forge_provider_api::submission::DispatchTracker::default(),
                execution_owner: Some(scope.owner().clone()),
                harness_session_id: scope.owner().session_id().to_string(),
                workspace: directory.path().to_string_lossy().into_owned(),
                input: crate::provider::ProviderInput::from_text("timeout fixture"),
                mode: crate::provider::PromptMode::Chat,
                model: "default".into(),
                effort: "default".into(),
                context_window: None,
                service_tier: Default::default(),
                access: Default::default(),
                approval_mode: crate::session::ApprovalMode::Ask,
                provider_session_id: None,
                submission: None,
                acknowledged_child_reports: Vec::new(),
                recovered_input: Vec::new(),
                control_context: None,
            };
            let turn = crate::provider::TurnAddress {
                scope: scope.clone(),
                thread_id: scope.owner().thread_id(),
                turn_id: scope.turn_id()?,
            };
            let (sink, _events) = crate::provider::events::channel(scope.clone());
            let running = engine.clone();
            let result = tokio::spawn(async move { running.run_once(input, sink, &store).await });
            crate::provider::test_support::wait_gate(&started).await?;
            Ok(Self {
                _directory: directory,
                _session: session,
                registry,
                _engine: engine,
                provider,
                scope,
                turn,
                result,
            })
        }
    }

    /// Preserve typed loss and still cancel native work when timeout publication meets a full inbox.
    ///
    /// The paused clock auto-advances to the watchdog deadline once the stalled fixture idles.
    #[tokio::test(start_paused = true)]
    async fn timeout_cancels_native_work_even_when_publication_is_full() -> Result<()> {
        let work = AdmittedWork::start().await?;
        let scope = work.scope.clone();
        let inbox = Arc::new(SessionObservationInbox::default());
        let watchdog = Arc::new(crate::provider::tool_timeout::ToolWatchdog::new(
            crate::session::InputTimeout {
                amount: 1,
                unit: crate::session::TimeoutUnit::S,
            },
        )?);
        inbox.watch_tools(&scope, &watchdog)?;
        inbox.push(tool_observation(
            crate::provider::ToolAddress {
                turn: work.turn.clone(),
                item_id: forge_provider_api::ItemId::new("stalled").unwrap(),
            },
            forge_provider_api::message::ItemEvent::Tool {
                event: forge_provider_api::tool::ToolItemEvent::ExecutionStarted,
            },
        ))?;
        let event_count = forge_protocol::MAX_PENDING_FRAMES;
        for _ in 1..event_count {
            inbox.push(notice(
                scope.clone().into(),
                forge_provider_api::SanitizedMessage::new("accepted output")
                    .expect("fixture diagnostic is nonempty")
                    .to_string(),
            ))?;
        }
        inbox.start_tool_monitor(scope, watchdog, work.provider.clone())?;
        let cancellation = work.result.await?;
        assert!(
            matches!(cancellation, Err(ProviderError::Cancelled)),
            "{cancellation:?}"
        );
        wait_for_close(&inbox.closed, &inbox.monitor_closed).await;
        assert_eq!(inbox.drain()?.len(), forge_protocol::MAX_PENDING_FRAMES);
        let failure = inbox.drain().unwrap_err();
        assert!(
            matches!(
                failure,
                crate::provider::ProviderError::Observation(
                    forge_provider_api::lifecycle::ObservationFailure::Overflow
                )
            ),
            "{failure:#}"
        );
        let error = inbox.retire_background().await.unwrap_err();
        let crate::provider::ProviderError::Cleanup(cleanup) = error else {
            panic!("expected typed watchdog cleanup failure: {error:#}");
        };
        assert!(matches!(
            cleanup.observation(),
            forge_provider_api::lifecycle::ObservationFailure::Overflow
        ));
        assert!(
            matches!(cleanup.collection(), [forge_provider_api::error::CleanupFailure::Failed(error)]
            if matches!(error.as_ref(), crate::provider::ProviderError::Observation(forge_provider_api::lifecycle::ObservationFailure::Overflow)))
        );
        assert!(inbox.retire_background().await.is_err());
        work.registry.close().await?;
        work.provider.shutdown().await?;
        Ok(())
    }
}
