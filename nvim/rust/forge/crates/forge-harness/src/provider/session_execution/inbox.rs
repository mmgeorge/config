use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, Weak};

use anyhow::{Result, anyhow};
use tokio::sync::Notify;

use crate::provider::ProviderEvent;

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
    watchdog_failure: Mutex<Option<Arc<forge_provider_api::error::ObservationCleanup>>>,
}

impl SessionObservationInbox {
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
            .map_err(|_| anyhow!("session event inbox poisoned"))?;
        anyhow::ensure!(
            !self.is_closed(),
            "session observation generation is closed"
        );
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
                .map_err(|_| anyhow!("tool watchdog registry poisoned"))?
                .get(&scope.invocation_id)
                .into_iter()
                .flatten()
                .filter_map(Weak::upgrade)
                .collect();
            for watchdog in watchdogs {
                watchdog.observe(event)?;
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
            .map_err(|_| anyhow!("retained watchdog lock poisoned"))?;
        let mut watchdogs = self
            .watchdogs
            .lock()
            .map_err(|_| anyhow!("tool watchdog registry poisoned"))?;
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
            previous.retire_idle()?;
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
        watchdog.finish_foreground()?;
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
            .map_err(|_| anyhow!("background watchdog task registry poisoned"))?;
        anyhow::ensure!(
            !self.is_closed(),
            "session observation generation is closed"
        );
        while let Some(result) = tasks.try_join_next() {
            if let Err(error) = result
                && !error.is_cancelled()
            {
                let failure =
                    self.retain_watchdog_failure(crate::provider::ProviderError::Protocol {
                        message: format!("background watchdog task failed: {error}"),
                    });
                return Err(crate::provider::ProviderError::Cleanup(failure).into());
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
                        timeout = watchdog.expired() => timeout?,
                    };
                    let Some(timeout) = timeout else {
                        if !tokio::select! {
                            biased;
                            () = wait_for_close(&closed, &monitor_closed) => Ok(false),
                            resumed = watchdog.resumed() => resumed,
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
                                        code: Some("tool_timeout".into()),
                                        message: timeout.to_string(),
                                    },
                                },
                            },
                        })?;
                        inbox.push(ProviderEvent::Diagnostic {
                            scope: scope.clone().into(),
                            diagnostic: crate::provider::ProviderDiagnostic {
                                code: Some("tool_timeout".into()),
                                message: timeout.to_string(),
                                fatal: true,
                            },
                        })
                    })();
                    // Stop native work even when timeout publication fails.
                    let cancellation = provider.cancel_session(&scope.owner.runtime_key()).await;
                    match (publication, cancellation) {
                        (Err(publication), Err(cancellation)) => {
                            return Err(publication.context(cancellation));
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
                        code: Some("tool_timeout_cleanup".into()),
                        message: format!("Tool timeout cleanup failed: {error:#}"),
                        fatal: true,
                    },
                });
                inbox.retain_watchdog_failure(crate::provider::ProviderError::from(error));
                inbox.fail(forge_provider_api::lifecycle::ObservationFailure::LifecycleUnavailable);
                inbox.close();
            }
        });
        Ok(())
    }

    /// Abort and collect old-generation watchdogs before replacing their native owner.
    pub(crate) async fn retire_background(&self) -> Result<()> {
        let mut failure = self
            .watchdog_failure
            .lock()
            .map_err(|_| anyhow!("background watchdog failure lock poisoned"))?
            .clone();
        let mut tasks = std::mem::take(
            &mut *self
                .background_tasks
                .lock()
                .map_err(|_| anyhow!("background watchdog task registry poisoned"))?,
        );
        tasks.abort_all();
        while let Some(result) = tasks.join_next().await {
            if let Err(error) = result
                && !error.is_cancelled()
            {
                failure.get_or_insert_with(|| {
                    self.retain_watchdog_failure(crate::provider::ProviderError::Protocol {
                        message: format!("background watchdog task failed: {error}"),
                    })
                });
            }
        }
        if failure.is_none() {
            failure = self
                .watchdog_failure
                .lock()
                .map_err(|_| anyhow!("background watchdog failure lock poisoned"))?
                .clone();
        }
        if let Some(failure) = failure {
            *self
                .watchdog_failure
                .lock()
                .map_err(|_| anyhow!("background watchdog failure lock poisoned"))? =
                Some(failure.clone());
            return Err(crate::provider::ProviderError::Cleanup(failure).into());
        }
        Ok(())
    }

    /// Preserve the first monitor failure as a typed cleanup result across repeated collection.
    fn retain_watchdog_failure(
        &self,
        error: crate::provider::ProviderError,
    ) -> Arc<forge_provider_api::error::ObservationCleanup> {
        let mut failure = self
            .watchdog_failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        failure
            .get_or_insert_with(|| {
                Arc::new(forge_provider_api::error::ObservationCleanup::new(
                    forge_provider_api::lifecycle::ObservationFailure::LifecycleUnavailable,
                    None,
                    None,
                    Some(forge_provider_api::error::CleanupFailure::Failed(Box::new(
                        error,
                    ))),
                ))
            })
            .clone()
    }

    /// Take queued events in provider order without holding the lock during delivery.
    pub(crate) fn drain(&self) -> Result<Vec<ProviderEvent>> {
        let events = self.take_pending()?;
        if events.is_empty()
            && self.is_closed()
            && let Some(failure) = self
                .failure
                .lock()
                .map_err(|_| anyhow!("session observation failure poisoned"))?
                .clone()
        {
            self.failure_reported.store(true, Ordering::Release);
            return Err(crate::provider::ProviderError::Observation(failure).into());
        }
        Ok(events)
    }

    /// Take accepted session observations without acknowledging a terminal failure.
    fn take_pending(&self) -> Result<Vec<ProviderEvent>> {
        Ok(self
            .pending
            .lock()
            .map_err(|_| anyhow!("session event inbox poisoned"))?
            .drain(..)
            .collect())
    }

    /// Require old observations and their failure to be consumed before replacing the owner.
    pub(crate) fn ready_for_replacement(&self) -> Result<bool> {
        let empty = self
            .pending
            .lock()
            .map_err(|_| anyhow!("session event inbox poisoned"))?
            .is_empty();
        let failed = self
            .failure
            .lock()
            .map_err(|_| anyhow!("session observation failure poisoned"))?
            .is_some();
        Ok(self.is_closed() && empty && (!failed || self.failure_reported.load(Ordering::Acquire)))
    }

    /// Check for work before the idle collector waits for broker ownership.
    pub(crate) fn has_pending(&self) -> Result<bool> {
        let pending = !self
            .pending
            .lock()
            .map_err(|_| anyhow!("session event inbox poisoned"))?
            .is_empty();
        Ok(pending
            || self.idle_settlement_requested.load(Ordering::Acquire)
            || (self.is_closed()
                && self
                    .failure
                    .lock()
                    .map_err(|_| anyhow!("session observation failure poisoned"))?
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
            .map_err(|_| anyhow!("session event inbox poisoned"))?
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

#[cfg(test)]
mod tests {
    use super::*;

    /// Cancel native work without losing timeout evidence behind a publication backlog.
    #[tokio::test(start_paused = true)]
    async fn timeout_cancels_native_work_behind_large_publication_backlog() -> Result<()> {
        let data = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(data.path())?);
        let provider: Arc<crate::provider::Provider> = Arc::from(crate::provider::build(
            crate::provider::ProviderLaunch {
                kind: "mock".into(),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(data.path())?,
            trace,
            data.path().into(),
        )?);
        let scope = crate::provider::InvocationScope::new(
            crate::provider::ExecutionOwner::root("session"),
            "exchange",
        );
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
        let event_count = forge_protocol::MAX_PENDING_FRAMES * 4;
        for _ in 1..event_count {
            inbox.push(ProviderEvent::Diagnostic {
                scope: scope.clone().into(),
                diagnostic: crate::provider::ProviderDiagnostic {
                    code: None,
                    message: "accepted output".into(),
                    fatal: false,
                },
            })?;
        }
        inbox.start_tool_monitor(scope, watchdog, provider.clone())?;
        let cancellation = tokio::time::timeout(std::time::Duration::from_secs(2), waiting)
            .await?
            .unwrap_err();
        assert!(cancellation.to_string().contains("cancelled"));
        assert!(!inbox.is_closed());
        let events = inbox.drain()?;
        assert_eq!(events.len(), event_count + 2);
        assert!(matches!(&events[event_count], ProviderEvent::Tool {
            update: crate::provider::ToolUpdate::Finished {
                outcome: crate::provider::ToolOutcome::Failed { diagnostic },
            }, ..
        } if diagnostic.code.as_deref() == Some("tool_timeout")));
        assert!(
            matches!(&events[event_count + 1], ProviderEvent::Diagnostic {
            diagnostic, ..
        } if diagnostic.code.as_deref() == Some("tool_timeout"))
        );
        assert!(inbox.drain()?.is_empty());
        inbox.retire_background().await?;
        Ok(())
    }

    /// Keep timeout observation alive when native work starts after its result waiter has gone.
    #[tokio::test(start_paused = true)]
    async fn idle_reopening_rearms_tool_deadlines_without_a_result_waiter() -> Result<()> {
        let data = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(data.path())?);
        let provider: Arc<crate::provider::Provider> = Arc::from(crate::provider::build(
            crate::provider::ProviderLaunch {
                kind: "mock".into(),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(data.path())?,
            trace,
            data.path().into(),
        )?);
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
                    ProviderEvent::Diagnostic { diagnostic, .. } if diagnostic.code.as_deref() == Some("tool_timeout"))) {
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
                        code: None,
                        message: "stale".into(),
                        fatal: false
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
                code: None,
                message: "before failure".into(),
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
        assert!(
            inbox
                .drain()
                .unwrap_err()
                .is::<crate::provider::ProviderError>()
        );
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
