use crate::backend::{BackendEvent, BackendEventSink};
use anyhow::{Context, Result};
use github_copilot_sdk::SessionEvent;
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicBool, Ordering},
};
use tokio::sync::Notify;

pub(super) enum CopilotRetraction {
    Unsent,
    Event(String),
    History(String),
}

/// Coordinates prompt admission, interruption, and rewind identity for one Copilot runtime.
#[derive(Default)]
pub(super) struct CopilotTurnEvent {
    state: Mutex<CopilotTurnState>,
    dispatch: tokio::sync::Mutex<()>,
    finished: Notify,
}

impl CopilotTurnEvent {
    pub(super) fn reset(&self) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        anyhow::ensure!(!state.active, "Copilot turn is already active");
        *state = CopilotTurnState::default();
        Ok(())
    }

    /// Attach the active consumer before any provider startup or prompt submission.
    pub(super) fn activate(
        &self,
        event_sink: Option<BackendEventSink>,
        submission_cancelled: Option<Arc<AtomicBool>>,
    ) -> Result<CopilotTurnEventGuard<'_>> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        anyhow::ensure!(!state.active, "Copilot turn is already active");
        *state = CopilotTurnState {
            active: true,
            event_sink,
            submission_cancelled,
            ..Default::default()
        };
        Ok(CopilotTurnEventGuard { owner: self })
    }

    pub(super) fn is_active(&self) -> bool {
        self.state
            .lock()
            .expect("Copilot turn lock poisoned")
            .active
    }

    pub(super) fn event_sink(&self) -> Option<BackendEventSink> {
        self.state
            .lock()
            .expect("Copilot turn lock poisoned")
            .event_sink
            .clone()
    }

    /// Serialize native sends against stop requests without canceling a committed SDK send.
    pub(super) async fn send(
        &self,
        send: impl std::future::Future<Output = Result<String>>,
    ) -> Result<()> {
        let _dispatch = self.dispatch.lock().await;
        {
            let mut state = self
                .state
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
            if state.cancelled
                || state
                    .submission_cancelled
                    .as_ref()
                    .is_some_and(|flag| flag.load(Ordering::Acquire))
            {
                return Err(anyhow::Error::new(crate::broker::TurnCancelled));
            }
            anyhow::ensure!(state.active, "Copilot turn is not active");
            state.submitted = true;
        }
        send.await?;
        Ok(())
    }

    /// Fence future sends and wait only for a send that already owns dispatch.
    pub(super) async fn stop(&self) -> Result<bool> {
        self.request_stop()?;
        let _dispatch = self.dispatch.lock().await;
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        Ok(state.submitted)
    }

    /// Fence new sends immediately and identify a submitted send that still needs collection.
    pub(super) fn request_stop(&self) -> Result<StopRequest> {
        let submitted = {
            let mut state = self
                .state
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
            state.cancelled = true;
            state.submitted
        };
        Ok(StopRequest {
            submitted,
            sending: self.dispatch.try_lock().is_err(),
        })
    }

    pub(super) async fn wait_finished(&self) {
        loop {
            let finished = self.finished.notified();
            if !self.is_active() {
                return;
            }
            finished.await;
        }
    }

    /// Retain the actual first root user event rather than an SDK request or reminder identity.
    pub(super) fn observe(&self, event: &SessionEvent) -> Result<()> {
        if event.agent_id.is_none() && event.event_type == "user.message" {
            let mut state = self
                .state
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
            if state.submitted && state.user_event_id.is_none() {
                state.user_event_id = Some(event.id.clone());
            }
        }
        Ok(())
    }

    pub(super) fn set_history_cursor(&self, cursor: String) -> Result<()> {
        self.state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?
            .history_cursor = Some(cursor);
        Ok(())
    }

    pub(super) fn retraction_target(&self) -> Result<CopilotRetraction> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        anyhow::ensure!(
            !state.active,
            "Copilot turn must stop before history rewind"
        );
        if !state.submitted {
            return Ok(CopilotRetraction::Unsent);
        }
        if let Some(event_id) = &state.user_event_id {
            return Ok(CopilotRetraction::Event(event_id.clone()));
        }
        Ok(CopilotRetraction::History(
            state
                .history_cursor
                .clone()
                .context("Copilot submission has no native history boundary for rewind")?,
        ))
    }

    pub(super) fn finish_retraction(&self) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        state.submitted = false;
        state.user_event_id = None;
        state.history_cursor = None;
        Ok(())
    }

    /// Publish steering only after the Copilot SDK accepts the immediate message.
    pub(super) async fn publish_steering(&self, text: String) -> Result<()> {
        let event_sink = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?
            .event_sink
            .clone();
        if let Some(event_sink) = event_sink {
            event_sink
                .send_wait(BackendEvent::steering_input(text))
                .await
                .map_err(|_| anyhow::anyhow!("Copilot turn event receiver closed"))?;
        }
        Ok(())
    }
}

/// Separates the native abort signal from collection of a committed SDK send.
pub(super) struct StopRequest {
    /// Reports that a native send was admitted before the stop fence.
    pub(super) submitted: bool,
    /// Reports that its SDK response has not yet released dispatch ownership.
    pub(super) sending: bool,
}

/// Retains send ownership and the exact native history boundary across prompt completion.
#[derive(Default)]
struct CopilotTurnState {
    active: bool,
    submitted: bool,
    cancelled: bool,
    submission_cancelled: Option<Arc<AtomicBool>>,
    user_event_id: Option<String>,
    history_cursor: Option<String>,
    event_sink: Option<BackendEventSink>,
}

/// Clears active delivery while preserving the canceled submission's rewind target.
pub(super) struct CopilotTurnEventGuard<'a> {
    owner: &'a CopilotTurnEvent,
}

impl Drop for CopilotTurnEventGuard<'_> {
    fn drop(&mut self) {
        if let Ok(mut state) = self.owner.state.lock() {
            state.active = false;
            state.event_sink = None;
        }
        self.owner.finished.notify_waiters();
    }
}
