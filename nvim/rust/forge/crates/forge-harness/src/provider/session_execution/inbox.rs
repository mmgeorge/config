use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, Weak};

use crate::provider::ProviderError;
type Result<T> = std::result::Result<T, ProviderError>;
use tokio::sync::Notify;

use crate::provider::ProviderEvent;

/// Distinguish explicitly collected owners from a native observation failure.
#[derive(Clone, Debug)]
pub(crate) enum ObservationEnd {
    Collected(super::OwnerCloseReason),
    Lost(forge_provider_api::lifecycle::ObservationFailure),
}

/// Retain provider session events while the foreground request owns the broker.
#[derive(Default)]
pub(crate) struct SessionObservationInbox {
    pending: Mutex<VecDeque<ProviderEvent>>,
    ready: Notify,
    closed: Arc<AtomicBool>,
    monitor_closed: Arc<Notify>,
    failure_reported: AtomicBool,
    idle_settlement_requested: AtomicBool,
    failure: Mutex<Option<forge_provider_api::lifecycle::ObservationFailure>>,
    collected: Mutex<Option<super::OwnerCloseReason>>,
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
        if let Some(failure) = self
            .failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
        {
            return Some(ObservationEnd::Lost(failure));
        }
        self.collected
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .map(ObservationEnd::Collected)
    }

    /// Publish collected resource ownership without inventing native execution completion.
    pub(crate) fn close_collected(&self, reason: super::OwnerCloseReason) {
        *self
            .collected
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(reason);
        self.close();
    }
    /// Wake the session owner when a foreground admission finds idle host work.
    pub(crate) fn request_idle_settlement(&self) {
        self.idle_settlement_requested
            .store(true, Ordering::Release);
        self.ready.notify_one();
    }

    pub(crate) fn take_idle_settlement_request(&self) -> bool {
        self.idle_settlement_requested.swap(false, Ordering::AcqRel)
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
    pub(crate) fn push(&self, event: ProviderEvent) -> Result<()> {
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
        pending.push_back(event);
        drop(pending);
        self.ready.notify_one();
        Ok(())
    }

    /// Notify every retained attempt watchdog before foreground or idle delivery.
    fn observe_tools(&self, event: &ProviderEvent) -> Result<()> {
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
                        forge_provider_api::error::ProviderOperation::CollectEventReader,
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
                    forge_provider_api::error::ProviderOperation::CollectEventReader,
                    error,
                )
            })?;
        }
        current.insert(scope.invocation_id.clone(), watchdog.clone());
        Ok(())
    }

    /// Retain deadlines through certified idle reopening and background execution.
    pub(crate) fn monitor_background(
        self: &Arc<Self>,
        scope: crate::provider::InvocationScope,
        watchdog: Arc<crate::provider::tool_timeout::ToolWatchdog>,
        provider: Arc<crate::provider::Provider>,
    ) -> Result<()> {
        watchdog.finish_foreground().map_err(|error| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::CollectEventReader,
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
                        forge_provider_api::error::ProviderOperation::CollectEventReader,
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
                        timeout = watchdog.expired() => timeout.map_err(|error| crate::provider::lifecycle_error::classify(forge_provider_api::error::ProviderOperation::CollectEventReader, error))?,
                    };
                    let Some(timeout) = timeout else {
                        if !tokio::select! {
                            biased;
                            () = wait_for_close(&closed, &monitor_closed) => Ok(false),
                            resumed = watchdog.resumed() => resumed.map_err(|error| crate::provider::lifecycle_error::classify(forge_provider_api::error::ProviderOperation::CollectEventReader, error)),
                        }? {
                            return Ok(());
                        }
                        continue;
                    };
                    let publication = (|| -> Result<()> {
                        if !timeout.claim_report() {
                            return Ok(());
                        }
                        let Some(inbox) = inbox.upgrade() else {
                            return Ok(());
                        };
                        inbox.push(ProviderEvent::Tool {
                            address: timeout.address.clone(),
                            update: crate::provider::ToolUpdate::Finished {
                                outcome: crate::provider::ToolOutcome::Failed {
                                    diagnostic: crate::provider::ToolDiagnostic {
                                        kind: forge_provider_api::ProviderFailureKind::Timeout,
                                        message: forge_provider_api::SanitizedMessage::new("Tool execution timed out")
                                            .expect("static diagnostic is nonempty"),
                                    },
                                },
                            },
                        })?;
                        inbox.push(ProviderEvent::Diagnostic {
                            scope: scope.clone().into(),
                            diagnostic: crate::provider::ProviderDiagnostic {
                                kind: forge_provider_api::ProviderFailureKind::Timeout,
                                message: forge_provider_api::SanitizedMessage::new("Tool execution timed out")
                                    .expect("static diagnostic is nonempty"),
                                fatal: true,
                            },
                        })
                    })();
                    if let Err(ProviderError::Observation(failure)) = &publication
                        && let Some(inbox) = inbox.upgrade()
                    {
                        inbox.fail(failure.clone());
                    }
                    // Stop native work even when timeout publication fails.
                    let cancellation = provider.cancel_session(&scope.owner.runtime_key()).await;
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
                let _ = inbox.push(ProviderEvent::Diagnostic {
                    scope: scope.into(),
                    diagnostic: crate::provider::ProviderDiagnostic {
                        kind: forge_provider_api::ProviderFailureKind::Runtime,
                        message: forge_provider_api::SanitizedMessage::new("Tool timeout cleanup failed")
                            .expect("static diagnostic is nonempty"),
                        fatal: true,
                    },
                });
                inbox.retain_watchdog_failure(error);
                inbox.fail(forge_provider_api::lifecycle::ObservationFailure::LifecycleUnavailable);
                inbox.close();
            }
        });
        Ok(())
    }

    /// Abort and collect old-generation watchdogs before replacing their native owner.
    pub(crate) async fn retire_background(&self) -> Result<()> {
        let _collection = self.background_collection.lock().await;
        self.close();
        self.background_tasks
            .lock()
            .map_err(|_| inbox_poisoned("background watchdog task registry poisoned"))?
            .abort_all();
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
                    forge_provider_api::error::ProviderOperation::CollectEventReader,
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
    pub(crate) fn drain(&self) -> Result<Vec<ProviderEvent>> {
        self.drain_up_to(usize::MAX)
    }

    /// Take only the prefix that fits its next owner, preserving sticky failure until a nonzero drain.
    pub(crate) fn drain_up_to(&self, limit: usize) -> Result<Vec<ProviderEvent>> {
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
            || self.idle_settlement_requested.load(Ordering::Acquire)
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
        if self.is_closed() || self.idle_settlement_requested.load(Ordering::Acquire) {
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

impl forge_provider_api::events::EventObserver for SessionObservationInbox {
    /// Route active invocation events to retained background watchdogs as well as the current attempt.
    fn before_delivery(
        &self,
        event: &ProviderEvent,
    ) -> std::result::Result<(), forge_provider_api::events::EventDeliveryError> {
        self.observe_tools(event)
            .map_err(forge_provider_api::events::EventDeliveryError::transport)
    }
}

/// Preserve typed inbox synchronization failures at the engine boundary.
fn inbox_poisoned(_component: &'static str) -> ProviderError {
    ProviderError::Failure(forge_provider_api::error::ProviderFailure::new(
        forge_provider_api::error::ProviderOperation::CollectEventReader,
        forge_provider_api::ProviderFailureKind::Runtime,
    ))
}

#[cfg(test)]
mod tests {
    /// Keep aborted-but-running workers owned when a background collection waiter is cancelled.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn cancelled_background_collection_retains_unjoined_workers() {
        let inbox = std::sync::Arc::new(super::SessionObservationInbox::default());
        let (started, ready) = tokio::sync::oneshot::channel();
        let (release, wait) = std::sync::mpsc::channel::<()>();
        inbox
            .background_tasks
            .lock()
            .unwrap()
            .spawn_blocking(move || {
                let _ = started.send(());
                let _ = wait.recv();
            });
        ready.await.unwrap();
        let collecting = {
            let inbox = inbox.clone();
            tokio::spawn(async move { inbox.retire_background().await })
        };
        while !inbox.is_closed() {
            tokio::task::yield_now().await;
        }
        collecting.abort();
        assert!(collecting.await.unwrap_err().is_cancelled());
        assert_eq!(inbox.background_tasks.lock().unwrap().len(), 1);
        release.send(()).unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(2), inbox.retire_background())
            .await
            .unwrap()
            .unwrap();
        assert!(inbox.background_tasks.lock().unwrap().is_empty());
    }

    /// Retain independent watchdog failures once without overwriting published snapshots.
    #[tokio::test]
    async fn watchdog_collection_preserves_all_failures_across_repeated_retirement() {
        use forge_provider_api::{ProviderFailure, ProviderFailureKind, ProviderOperation};
        let inbox = super::SessionObservationInbox::default();
        let first = inbox.retain_watchdog_failure(
            ProviderFailure::new(
                ProviderOperation::CancelSession,
                ProviderFailureKind::Transport,
            )
            .into(),
        );
        let second = inbox.retain_watchdog_failure(
            ProviderFailure::new(
                ProviderOperation::CollectEventReader,
                ProviderFailureKind::Protocol,
            )
            .into(),
        );
        assert_eq!(first.collection().len(), 1);
        assert_eq!(second.collection().len(), 2);
        for _ in 0..2 {
            let Err(crate::provider::ProviderError::Cleanup(report)) =
                inbox.retire_background().await
            else {
                panic!("watchdog failures remain observable");
            };
            assert_eq!(report.collection().len(), 2);
            assert_eq!(
                report
                    .to_string()
                    .matches("provider observation failed:")
                    .count(),
                1
            );
        }
    }

    use super::*;
    use anyhow::Result;

    fn fixture_diagnostic(message: impl Into<String>) -> crate::provider::ProviderDiagnostic {
        crate::provider::ProviderDiagnostic {
            kind: forge_provider_api::ProviderFailureKind::Unknown,
            message: forge_provider_api::SanitizedMessage::new(message)
                .expect("fixture diagnostic is nonempty"),
            fatal: false,
        }
    }

    /// Accept sustained traffic above one inbox capacity when the owner drains each bounded window.
    #[test]
    fn bounded_draining_preserves_large_observation_stream() -> Result<()> {
        let inbox = SessionObservationInbox::default();
        let scope = crate::provider::EventScope::SessionOperation {
            owner: crate::provider::ExecutionOwner::root("session"),
            operation_id: "large-observation-stream".into(),
        };
        let mut observed = Vec::new();
        let count = forge_protocol::MAX_PENDING_FRAMES * 4;
        for index in 0..count {
            inbox.push(ProviderEvent::Diagnostic {
                scope: scope.clone(),
                diagnostic: fixture_diagnostic(format!("frame-{index}: α🦀")),
            })?;
            if (index + 1) % forge_protocol::MAX_PENDING_FRAMES == 0 {
                loop {
                    let prefix = inbox.drain_up_to(17)?;
                    if prefix.is_empty() {
                        break;
                    }
                    assert!(prefix.len() <= 17);
                    for frame in prefix {
                        let ProviderEvent::Diagnostic { diagnostic, .. } = frame else {
                            anyhow::bail!("bounded transfer changed an observation");
                        };
                        observed.push(diagnostic.message.to_string());
                    }
                }
            }
        }
        assert_eq!(
            observed,
            (0..count)
                .map(|index| format!("frame-{index}: α🦀"))
                .collect::<Vec<_>>()
        );
        assert!(!inbox.is_closed());
        assert!(inbox.end().is_none());
        assert!(inbox.drain()?.is_empty());
        Ok(())
    }

    /// Keep a full accepted prefix ordered across partial drains and report real loss only afterward.
    #[test]
    fn bounded_drain_preserves_full_prefix_and_does_not_acknowledge_loss_without_capacity()
    -> Result<()> {
        let inbox = SessionObservationInbox::default();
        let event = |index| ProviderEvent::Diagnostic {
            scope: crate::provider::EventScope::SessionOperation {
                owner: crate::provider::ExecutionOwner::root("session"),
                operation_id: format!("operation-{index}"),
            },
            diagnostic: fixture_diagnostic(format!("frame-{index:03}: α🦀")),
        };
        for index in 0..forge_protocol::MAX_PENDING_FRAMES {
            inbox.push(event(index))?;
        }
        assert!(matches!(
            inbox.push(event(forge_protocol::MAX_PENDING_FRAMES)),
            Err(crate::provider::ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Overflow
            ))
        ));
        inbox.fail(forge_provider_api::lifecycle::ObservationFailure::Overflow);
        inbox.close();
        assert!(inbox.drain_up_to(0)?.is_empty());
        assert!(!inbox.ready_for_replacement()?);
        let mut observed = Vec::new();
        while observed.len() < forge_protocol::MAX_PENDING_FRAMES {
            let prefix = inbox.drain_up_to(17)?;
            assert!(!prefix.is_empty() && prefix.len() <= 17);
            for frame in prefix {
                let ProviderEvent::Diagnostic { diagnostic, .. } = frame else {
                    anyhow::bail!("partial drain changed a frame");
                };
                observed.push(diagnostic.message.to_string());
            }
            assert!(
                !inbox.ready_for_replacement()?,
                "accepted prefix acknowledged real loss"
            );
        }
        assert_eq!(
            observed,
            (0..forge_protocol::MAX_PENDING_FRAMES)
                .map(|index| format!("frame-{index:03}: α🦀"))
                .collect::<Vec<_>>()
        );
        assert!(inbox.drain_up_to(0)?.is_empty());
        assert!(
            !inbox.ready_for_replacement()?,
            "zero capacity acknowledged sticky loss"
        );
        assert!(matches!(
            inbox.drain_up_to(1),
            Err(crate::provider::ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Overflow
            ))
        ));
        assert!(inbox.ready_for_replacement()?);
        Ok(())
    }

    /// Preserve typed loss and still cancel native work when timeout publication meets a full inbox.
    #[tokio::test(start_paused = true)]
    async fn timeout_cancels_native_work_even_when_publication_is_full() -> Result<()> {
        let data = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(data.path())?);
        let provider: Arc<crate::provider::Provider> = crate::provider::build(
            crate::provider::ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(data.path())?,
            trace,
            data.path().into(),
        )?;
        let scope = crate::provider::InvocationScope::new(
            crate::provider::ExecutionOwner::root("session"),
            "exchange",
        );
        provider
            .initialize_session(crate::provider::ProviderRequest {
                execution_owner: Some(scope.owner.clone()),
                harness_session_id: scope.owner.session_id.to_string(),
                workspace: data.path().to_string_lossy().into_owned(),
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
                recovered_input: Vec::new(),
                control_context: None,
            })
            .await?;
        let runtime_key = scope.owner.runtime_key();
        let control = provider.mock_response_control().unwrap();
        let held = control
            .dispatch(
                &runtime_key,
                crate::provider::mock_response::MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let waiting = control.dispatch(
            &runtime_key,
            crate::provider::mock_response::MockResponseCommand::Wait {
                id: held["id"].as_str().unwrap().to_owned(),
            },
        );
        tokio::pin!(waiting);
        assert!(futures_util::poll!(waiting.as_mut()).is_pending());

        let inbox = Arc::new(SessionObservationInbox::default());
        let watchdog = Arc::new(crate::provider::tool_timeout::ToolWatchdog::new(
            crate::session::InputTimeout {
                amount: 1,
                unit: crate::session::TimeoutUnit::S,
            },
        )?);
        inbox.watch_tools(&scope, &watchdog)?;
        inbox.push(ProviderEvent::Tool {
            address: crate::provider::ToolAddress {
                turn: crate::provider::TurnAddress {
                    scope: scope.clone(),
                    thread_id: "thread".into(),
                    turn_id: "turn".into(),
                },
                tool_id: "stalled".into(),
            },
            update: crate::provider::ToolUpdate::Started,
        })?;
        let event_count = forge_protocol::MAX_PENDING_FRAMES;
        for _ in 1..event_count {
            inbox.push(ProviderEvent::Diagnostic {
                scope: scope.clone().into(),
                diagnostic: crate::provider::ProviderDiagnostic {
                    kind: forge_provider_api::ProviderFailureKind::Unknown,
                    message: forge_provider_api::SanitizedMessage::new("accepted output")
                        .expect("fixture diagnostic is nonempty"),
                    fatal: false,
                },
            })?;
        }
        inbox.start_tool_monitor(scope, watchdog, provider.clone())?;
        let cancellation = tokio::time::timeout(std::time::Duration::from_secs(2), waiting)
            .await?
            .unwrap_err();
        assert!(cancellation.to_string().contains("cancelled"));
        assert!(inbox.is_closed());
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
        provider.shutdown().await?;
        Ok(())
    }

    /// Keep timeout observation alive when native work starts after its result waiter has gone.
    #[tokio::test(start_paused = true)]
    async fn idle_reopening_rearms_tool_deadlines_without_a_result_waiter() -> Result<()> {
        let data = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(data.path())?);
        let provider: Arc<crate::provider::Provider> = crate::provider::build(
            crate::provider::ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(data.path())?,
            trace,
            data.path().into(),
        )?;
        let inbox = Arc::new(SessionObservationInbox::default());
        let scope = crate::provider::InvocationScope::new(
            crate::provider::ExecutionOwner::root("session"),
            "exchange",
        );
        let watchdog = Arc::new(crate::provider::tool_timeout::ToolWatchdog::new(
            crate::session::InputTimeout {
                amount: 1,
                unit: crate::session::TimeoutUnit::S,
            },
        )?);
        inbox.watch_tools(&scope, &watchdog)?;
        inbox.monitor_background(scope.clone(), watchdog, provider)?;
        tokio::task::yield_now().await;
        tokio::time::advance(std::time::Duration::from_secs(2)).await;
        assert!(inbox.drain()?.is_empty());

        let turn = crate::provider::TurnAddress {
            scope: scope.clone(),
            thread_id: "thread".into(),
            turn_id: "idle-continuation".into(),
        };
        inbox.push(ProviderEvent::Execution {
            observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                scope,
                forge_provider_api::lifecycle::ExecutionUpdate::Started {
                    address: turn.clone(),
                },
            )?,
        })?;
        inbox.push(ProviderEvent::Tool {
            address: crate::provider::ToolAddress {
                turn,
                tool_id: "resumed-tool".into(),
            },
            update: crate::provider::ToolUpdate::Started,
        })?;
        assert_eq!(inbox.drain()?.len(), 2);
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            loop {
                if inbox.drain()?.iter().any(|event| matches!(event,
                    ProviderEvent::Diagnostic { diagnostic, .. } if diagnostic.kind == forge_provider_api::ProviderFailureKind::Timeout)) {
                    return Ok::<(), anyhow::Error>(());
                }
                inbox.notified().await?;
            }
        }).await??;
        inbox.close();
        inbox.retire_background().await?;
        assert!(inbox.background_tasks.lock().unwrap().is_empty());
        Ok(())
    }

    /// Route a late background completion to its retired watchdog after a new attempt registers.
    #[tokio::test(start_paused = true)]
    async fn retained_watchdogs_keep_their_original_background_tools() -> Result<()> {
        let inbox = Arc::new(SessionObservationInbox::default());
        let scope = crate::provider::InvocationScope::new(
            crate::provider::ExecutionOwner::root("session"),
            "exchange",
        );
        let address = crate::provider::ToolAddress {
            turn: crate::provider::TurnAddress {
                scope: scope.clone(),
                thread_id: "thread".into(),
                turn_id: "first".into(),
            },
            tool_id: "background".into(),
        };
        let timeout = crate::session::InputTimeout {
            amount: 1,
            unit: crate::session::TimeoutUnit::S,
        };
        let first = Arc::new(crate::provider::tool_timeout::ToolWatchdog::new(timeout)?);
        first.observe(&ProviderEvent::Tool {
            address: address.clone(),
            update: crate::provider::ToolUpdate::Started,
        })?;
        first.observe(&ProviderEvent::Tool {
            address: address.clone(),
            update: crate::provider::ToolUpdate::Backgrounded,
        })?;
        assert!(first.finish_foreground()?);
        inbox.watch_tools(&scope, &first)?;
        let second = Arc::new(crate::provider::tool_timeout::ToolWatchdog::new(timeout)?);
        inbox.watch_tools(&scope, &second)?;
        let (sink, _stream) = crate::provider::events::channel(scope);
        let sink = sink.with_observer(inbox.clone());
        sink.send_wait(ProviderEvent::Tool {
            address: address.clone(),
            update: crate::provider::ToolUpdate::Finished {
                outcome: crate::provider::ToolOutcome::Succeeded,
            },
        })
        .await?;
        let mut later = address;
        later.turn.turn_id = "second".into();
        sink.send_wait(ProviderEvent::Tool {
            address: later.clone(),
            update: crate::provider::ToolUpdate::Started,
        })
        .await?;

        assert!(first.expired().await?.is_none());
        assert_eq!(second.expired().await?.unwrap().address, later);
        Ok(())
    }

    /// Keep an older completed scope observable after a later exchange registers its own tracker.
    #[tokio::test(start_paused = true)]
    async fn distinct_retained_scopes_keep_idle_watchdogs() -> Result<()> {
        let inbox = Arc::new(SessionObservationInbox::default());
        let owner = crate::provider::ExecutionOwner::root("session");
        let first_scope = crate::provider::InvocationScope::new(owner.clone(), "first");
        let second_scope = crate::provider::InvocationScope::new(owner, "second");
        let timeout = crate::session::InputTimeout {
            amount: 1,
            unit: crate::session::TimeoutUnit::S,
        };
        let first = Arc::new(crate::provider::tool_timeout::ToolWatchdog::new(timeout)?);
        first.finish_foreground()?;
        inbox.watch_tools(&first_scope, &first)?;
        let second = Arc::new(crate::provider::tool_timeout::ToolWatchdog::new(timeout)?);
        second.finish_foreground()?;
        inbox.watch_tools(&second_scope, &second)?;

        let turn = crate::provider::TurnAddress {
            scope: first_scope,
            thread_id: "thread".into(),
            turn_id: "reopened-first".into(),
        };
        inbox.push(ProviderEvent::Execution {
            observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                turn.scope.clone(),
                forge_provider_api::lifecycle::ExecutionUpdate::Started {
                    address: turn.clone(),
                },
            )?,
        })?;
        let tool = crate::provider::ToolAddress {
            turn,
            tool_id: "old-scope-tool".into(),
        };
        inbox.push(ProviderEvent::Tool {
            address: tool.clone(),
            update: crate::provider::ToolUpdate::Started,
        })?;
        assert_eq!(first.expired().await?.unwrap().address, tool);
        assert!(second.expired().await?.is_none());
        Ok(())
    }

    /// Keep watchdog collection failure sticky and reject publication after close.
    #[tokio::test]
    async fn retired_generation_collects_watchdogs_and_rejects_late_events() -> Result<()> {
        let inbox = SessionObservationInbox::default();
        let failed = inbox
            .background_tasks
            .lock()
            .unwrap()
            .spawn(async { panic!("fixture watchdog failure") });
        while !failed.is_finished() {
            tokio::task::yield_now().await;
        }
        inbox
            .background_tasks
            .lock()
            .unwrap()
            .spawn(std::future::pending());
        inbox.close();
        assert!(inbox.retire_background().await.is_err());
        assert!(inbox.retire_background().await.is_err());
        assert!(
            inbox
                .push(ProviderEvent::Diagnostic {
                    scope: crate::provider::EventScope::SessionOperation {
                        owner: crate::provider::ExecutionOwner::root("session"),
                        operation_id: "old".into(),
                    },
                    diagnostic: crate::provider::ProviderDiagnostic {
                        kind: forge_provider_api::ProviderFailureKind::Unknown,
                        message: forge_provider_api::SanitizedMessage::new("stale")
                            .expect("fixture diagnostic is nonempty"),
                        fatal: false,
                    },
                })
                .is_err()
        );
        Ok(())
    }

    /// Drain committed observations before exposing a terminal failure outside queue capacity.
    #[tokio::test]
    async fn failure_survives_until_preceding_events_drain() -> Result<()> {
        let inbox = SessionObservationInbox::default();
        let event = ProviderEvent::Diagnostic {
            scope: crate::provider::EventScope::SessionOperation {
                owner: crate::provider::ExecutionOwner::root("session"),
                operation_id: "initialization".into(),
            },
            diagnostic: crate::provider::ProviderDiagnostic {
                kind: forge_provider_api::ProviderFailureKind::Unknown,
                message: forge_provider_api::SanitizedMessage::new("before failure")
                    .expect("fixture diagnostic is nonempty"),
                fatal: false,
            },
        };
        inbox.push(event)?;
        inbox.fail(forge_provider_api::lifecycle::ObservationFailure::Overflow);
        inbox.close();
        assert!(!inbox.ready_for_replacement()?);
        assert_eq!(inbox.drain()?.len(), 1);
        assert!(inbox.has_pending()?);
        assert!(!inbox.ready_for_replacement()?);
        assert!(matches!(
            inbox.drain().unwrap_err(),
            crate::provider::ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Overflow
            )
        ));
        assert!(inbox.ready_for_replacement()?);
        inbox.notified().await?;
        Ok(())
    }

    /// Keep queued events observable even if publication precedes the waiter.
    #[tokio::test]
    async fn notification_retains_events_before_waiter_starts() -> Result<()> {
        let inbox = SessionObservationInbox::default();
        let event = ProviderEvent::Tool {
            address: crate::provider::ToolAddress {
                turn: crate::provider::TurnAddress {
                    scope: crate::provider::InvocationScope::new(
                        crate::provider::ExecutionOwner::root("session"),
                        "exchange",
                    ),
                    thread_id: "thread".into(),
                    turn_id: "turn".into(),
                },
                tool_id: "background".into(),
            },
            update: crate::provider::ToolUpdate::Backgrounded,
        };
        inbox.push(event)?;
        inbox.notified().await?;
        assert_eq!(inbox.drain()?.len(), 1);
        assert!(inbox.drain()?.is_empty());
        Ok(())
    }
}
