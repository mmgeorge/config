use std::collections::{HashMap, VecDeque};
use std::sync::{Arc, Mutex, Weak};

use anyhow::{Result, anyhow};
use tokio::sync::Notify;

use crate::provider::ProviderEvent;

/// Retain provider session events while the foreground request owns the broker.
#[derive(Default)]
pub(crate) struct SessionEventInbox {
    pending: Mutex<VecDeque<ProviderEvent>>,
    ready: Notify,
    watchdogs: Mutex<
        HashMap<crate::provider::InvocationId, Weak<crate::provider::tool_timeout::ToolWatchdog>>,
    >,
    background_tasks: Mutex<Vec<tokio::task::JoinHandle<()>>>,
}

impl SessionEventInbox {
    /// Queue a complete event before notifying either idle or active broker delivery.
    pub(crate) fn push(&self, event: ProviderEvent) -> Result<()> {
        if let Some(scope) = event.scope().invocation()
            && let Some(watchdog) = self
                .watchdogs
                .lock()
                .map_err(|_| anyhow!("tool watchdog registry poisoned"))?
                .get(&scope.invocation_id)
                .and_then(Weak::upgrade)
        {
            watchdog.observe(&event)?;
        }
        self.pending
            .lock()
            .map_err(|_| anyhow!("session event inbox poisoned"))?
            .push_back(event);
        self.ready.notify_one();
        Ok(())
    }

    /// Route late command observations to the watchdog that owns their original invocation.
    pub(crate) fn watch_tools(
        &self,
        scope: &crate::provider::InvocationScope,
        watchdog: &Arc<crate::provider::tool_timeout::ToolWatchdog>,
    ) -> Result<()> {
        let mut watchdogs = self
            .watchdogs
            .lock()
            .map_err(|_| anyhow!("tool watchdog registry poisoned"))?;
        watchdogs.retain(|_, watchdog| watchdog.strong_count() > 0);
        watchdogs.insert(scope.invocation_id.clone(), Arc::downgrade(watchdog));
        Ok(())
    }

    /// Keep a background shell's deadline active after its foreground response completes.
    pub(crate) fn monitor_background(
        self: &Arc<Self>,
        scope: crate::provider::InvocationScope,
        watchdog: Arc<crate::provider::tool_timeout::ToolWatchdog>,
        provider: Arc<crate::provider::Provider>,
    ) -> Result<()> {
        if !watchdog.finish_foreground()? {
            return Ok(());
        }
        let inbox = Arc::downgrade(self);
        let task = tokio::spawn(async move {
            let result: Result<()> = async {
                if let Some(timeout) = watchdog.expired().await? {
                    if let Some(inbox) = inbox.upgrade() {
                        inbox.push(ProviderEvent::Diagnostic {
                            scope: scope.clone().into(),
                            diagnostic: crate::provider::ProviderDiagnostic {
                                code: Some("tool_timeout".into()),
                                message: timeout.to_string(),
                                fatal: true,
                            },
                        })?;
                    }
                    provider.cancel_session(&scope.owner.runtime_key()).await?;
                }
                Ok(())
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
            }
        });
        let mut tasks = self
            .background_tasks
            .lock()
            .map_err(|_| anyhow!("background watchdog task registry poisoned"))?;
        tasks.retain(|task| !task.is_finished());
        tasks.push(task);
        Ok(())
    }

    /// Take queued events in provider order without holding the lock during delivery.
    pub(crate) fn drain(&self) -> Result<Vec<ProviderEvent>> {
        Ok(self
            .pending
            .lock()
            .map_err(|_| anyhow!("session event inbox poisoned"))?
            .drain(..)
            .collect())
    }

    /// Deliver queued invocation markers before already-arrived session completions.
    pub(crate) fn drain_after(
        &self,
        invocation: &mut crate::provider::events::ProviderEventStream,
    ) -> Result<Vec<ProviderEvent>> {
        let mut event = Vec::new();
        while let Some(observation) = invocation.try_next()? {
            event.push(observation);
        }
        event.extend(self.drain()?);
        Ok(event)
    }

    /// Check for work before the idle collector waits for broker ownership.
    pub(crate) fn has_pending(&self) -> Result<bool> {
        Ok(!self
            .pending
            .lock()
            .map_err(|_| anyhow!("session event inbox poisoned"))?
            .is_empty())
    }

    /// Wait for pending events without losing a notification between inspection and sleep.
    pub(crate) async fn notified(&self) -> Result<()> {
        let notified = self.ready.notified();
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

impl Drop for SessionEventInbox {
    fn drop(&mut self) {
        for task in self
            .background_tasks
            .get_mut()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
        {
            task.abort();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Keep queued events observable even if publication precedes the waiter.
    #[tokio::test]
    async fn notification_retains_events_before_waiter_starts() -> Result<()> {
        let inbox = SessionEventInbox::default();
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

    /// Keep invocation lifetime admission ahead of completion on the independent session stream.
    #[tokio::test]
    async fn immediate_background_completion_follows_queued_lifetime_marker() -> Result<()> {
        let inbox = SessionEventInbox::default();
        let scope = crate::provider::InvocationScope::new(
            crate::provider::ExecutionOwner::root("session"),
            "exchange",
        );
        let address = crate::provider::ToolAddress {
            turn: crate::provider::TurnAddress {
                scope: scope.clone(),
                thread_id: "thread".into(),
                turn_id: "turn".into(),
            },
            tool_id: "background".into(),
        };
        let (sink, mut stream) = crate::provider::events::channel(scope);
        inbox.push(ProviderEvent::Tool {
            address: address.clone(),
            update: crate::provider::ToolUpdate::Finished {
                outcome: crate::provider::ToolOutcome::Succeeded,
            },
        })?;
        sink.send_wait(ProviderEvent::Tool {
            address,
            update: crate::provider::ToolUpdate::Backgrounded,
        })
        .await?;
        let event = inbox.drain_after(&mut stream)?;
        assert_eq!(event.len(), 2);
        assert!(matches!(
            event[0],
            ProviderEvent::Tool {
                update: crate::provider::ToolUpdate::Backgrounded,
                ..
            }
        ));
        assert!(matches!(
            event[1],
            ProviderEvent::Tool {
                update: crate::provider::ToolUpdate::Finished { .. },
                ..
            }
        ));
        Ok(())
    }
}
