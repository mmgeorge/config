use std::collections::VecDeque;
use std::sync::Mutex;

use anyhow::{Result, anyhow};
use tokio::sync::Notify;

use crate::backend::BackendEvent;

/// Retain provider session events while the foreground request owns the broker.
#[derive(Default)]
pub(crate) struct SessionEventInbox {
    pending: Mutex<VecDeque<BackendEvent>>,
    ready: Notify,
}

impl SessionEventInbox {
    /// Queue a complete event before notifying either idle or active broker delivery.
    pub(crate) fn push(&self, event: BackendEvent) -> Result<()> {
        self.pending
            .lock()
            .map_err(|_| anyhow!("session event inbox poisoned"))?
            .push_back(event);
        self.ready.notify_one();
        Ok(())
    }

    /// Take queued events in provider order without holding the lock during delivery.
    pub(crate) fn drain(&self) -> Result<Vec<BackendEvent>> {
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
        invocation: &mut crate::backend::events::BackendEventStream,
    ) -> Result<Vec<BackendEvent>> {
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

#[cfg(test)]
mod tests {
    use super::*;

    /// Keep queued events observable even if publication precedes the waiter.
    #[tokio::test]
    async fn notification_retains_events_before_waiter_starts() -> Result<()> {
        let inbox = SessionEventInbox::default();
        let event = BackendEvent::Tool {
            address: crate::backend::ToolAddress {
                turn: crate::backend::TurnAddress {
                    scope: crate::backend::InvocationScope::new(
                        crate::backend::ExecutionOwner::root("session"),
                        "exchange",
                    ),
                    thread_id: "thread".into(),
                    turn_id: "turn".into(),
                },
                tool_id: "background".into(),
            },
            update: crate::backend::ToolUpdate::Backgrounded,
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
        let scope = crate::backend::InvocationScope::new(
            crate::backend::ExecutionOwner::root("session"),
            "exchange",
        );
        let address = crate::backend::ToolAddress {
            turn: crate::backend::TurnAddress {
                scope: scope.clone(),
                thread_id: "thread".into(),
                turn_id: "turn".into(),
            },
            tool_id: "background".into(),
        };
        let (sink, mut stream) = crate::backend::events::channel(scope);
        inbox.push(BackendEvent::Tool {
            address: address.clone(),
            update: crate::backend::ToolUpdate::Finished {
                outcome: crate::backend::ToolOutcome::Succeeded,
            },
        })?;
        sink.send_wait(BackendEvent::Tool {
            address,
            update: crate::backend::ToolUpdate::Backgrounded,
        })
        .await?;
        let event = inbox.drain_after(&mut stream)?;
        assert_eq!(event.len(), 2);
        assert!(matches!(
            event[0],
            BackendEvent::Tool {
                update: crate::backend::ToolUpdate::Backgrounded,
                ..
            }
        ));
        assert!(matches!(
            event[1],
            BackendEvent::Tool {
                update: crate::backend::ToolUpdate::Finished { .. },
                ..
            }
        ));
        Ok(())
    }
}
