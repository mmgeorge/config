use crate::backend::{BackendEvent, BackendEventSink};
use anyhow::Result;
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
        anyhow::ensure!(
            !matches!(state.delivery, TurnDelivery::Active(_)),
            "Copilot turn is already active"
        );
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
        anyhow::ensure!(
            !matches!(state.delivery, TurnDelivery::Active(_)),
            "Copilot turn is already active"
        );
        *state = CopilotTurnState {
            delivery: TurnDelivery::Active(ActiveDelivery {
                event_sink,
                submission_cancelled,
            }),
            ..Default::default()
        };
        Ok(CopilotTurnEventGuard { owner: self })
    }

    pub(super) fn is_active(&self) -> bool {
        matches!(
            self.state
                .lock()
                .expect("Copilot turn lock poisoned")
                .delivery,
            TurnDelivery::Active(_)
        )
    }

    pub(super) fn event_sink(&self) -> Option<BackendEventSink> {
        let state = self.state.lock().expect("Copilot turn lock poisoned");
        match &state.delivery {
            TurnDelivery::Active(active) => active.event_sink.clone(),
            TurnDelivery::Idle => None,
        }
    }

    pub(super) async fn steer(
        &self,
        send: impl std::future::Future<Output = Result<String>>,
    ) -> Result<()> {
        let _dispatch = self.dispatch.lock().await;
        {
            let state = self
                .state
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
            let TurnDelivery::Active(active) = &state.delivery else {
                anyhow::bail!("Copilot turn is not active");
            };
            if state.cancelled
                || active
                    .submission_cancelled
                    .as_ref()
                    .is_some_and(|flag| flag.load(Ordering::Acquire))
            {
                return Err(anyhow::Error::new(crate::broker::TurnCancelled));
            }
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
        Ok(matches!(state.submission, NativeSubmission::Sent { .. }))
    }

    /// Fence new sends immediately and identify a submitted send that still needs collection.
    pub(super) fn request_stop(&self) -> Result<StopRequest> {
        let submitted = {
            let mut state = self
                .state
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
            state.cancelled = true;
            matches!(state.submission, NativeSubmission::Sent { .. })
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
            if let NativeSubmission::Sent { user_event_id, .. } = &mut state.submission
                && user_event_id.is_none()
            {
                *user_event_id = Some(event.id.clone());
            }
        }
        Ok(())
    }

    pub(super) fn retraction_target(&self) -> Result<CopilotRetraction> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        anyhow::ensure!(
            !matches!(state.delivery, TurnDelivery::Active(_)),
            "Copilot turn must stop before history rewind"
        );
        Ok(match &state.submission {
            NativeSubmission::Unsent => CopilotRetraction::Unsent,
            NativeSubmission::Sent {
                user_event_id: Some(event_id),
                ..
            } => CopilotRetraction::Event(event_id.clone()),
            NativeSubmission::Sent {
                cursor,
                user_event_id: None,
            } => CopilotRetraction::History(cursor.clone()),
        })
    }

    pub(super) fn finish_retraction(&self) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        state.submission = NativeSubmission::Unsent;
        Ok(())
    }

    /// Publish steering only after the Copilot SDK accepts the immediate message.
    pub(super) async fn publish_steering(
        &self,
        text: String,
        input_message_id: Option<String>,
        task_start: Option<crate::exchange::TaskStart>,
    ) -> Result<()> {
        let event_sink = {
            let state = self
                .state
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
            match &state.delivery {
                TurnDelivery::Active(active) => active.event_sink.clone(),
                TurnDelivery::Idle => None,
            }
        };
        if let Some(event_sink) = event_sink {
            let mut event = match task_start {
                Some(start) => BackendEvent::task_started(start, text),
                None => BackendEvent::steering_input(text),
            };
            if !event.data.is_object() {
                event.data = serde_json::json!({});
            }
            event.data["input_message_id"] = serde_json::json!(input_message_id);
            event_sink
                .send_wait(event)
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
    delivery: TurnDelivery,
    submission: NativeSubmission,
    cancelled: bool,
}

#[derive(Default)]
enum TurnDelivery {
    #[default]
    Idle,
    Active(ActiveDelivery),
}

struct ActiveDelivery {
    submission_cancelled: Option<Arc<AtomicBool>>,
    event_sink: Option<BackendEventSink>,
}

#[derive(Default)]
enum NativeSubmission {
    #[default]
    Unsent,
    Sent {
        cursor: String,
        user_event_id: Option<String>,
    },
}

/// Clears active delivery while preserving the canceled submission's rewind target.
pub(super) struct CopilotTurnEventGuard<'a> {
    owner: &'a CopilotTurnEvent,
}

impl CopilotTurnEventGuard<'_> {
    /// Keeps active delivery borrowed until the native send releases dispatch ownership.
    pub(super) async fn send(
        &self,
        cursor: String,
        send: impl std::future::Future<Output = Result<String>>,
    ) -> Result<()> {
        let _dispatch = self.owner.dispatch.lock().await;
        {
            let mut state = self
                .owner
                .state
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
            let cancelled = match &state.delivery {
                TurnDelivery::Active(active) => active
                    .submission_cancelled
                    .as_ref()
                    .is_some_and(|flag| flag.load(Ordering::Acquire)),
                TurnDelivery::Idle => true,
            };
            if state.cancelled || cancelled {
                return Err(anyhow::Error::new(crate::broker::TurnCancelled));
            }
            state.submission = NativeSubmission::Sent {
                cursor,
                user_event_id: None,
            };
        }
        send.await?;
        Ok(())
    }
}

impl Drop for CopilotTurnEventGuard<'_> {
    fn drop(&mut self) {
        if let Ok(mut state) = self.owner.state.lock() {
            state.delivery = TurnDelivery::Idle;
        }
        self.owner.finished.notify_waiters();
    }
}
