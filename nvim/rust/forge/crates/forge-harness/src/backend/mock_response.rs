//! Explicit response barriers for tests that run the real Harness with a mock provider.

use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

use anyhow::{Context, Result, bail, ensure};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use tokio::sync::watch;

use super::{BackendRequest, PromptMode};

/// Test-only operations accepted by `backend.mock_response` for the selected mock session.
#[derive(Debug, Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
pub(crate) enum MockResponseCommand {
    /// Hold the next matching prompt. An omitted mode matches any prompt mode.
    Hold { mode: Option<PromptMode> },
    /// Wait for the provider to reach a previously armed hold without polling.
    Wait { id: String },
    /// Release an arrived response. An unclaimed, cancelled, or unknown hold is an error.
    Release { id: String },
    /// Discard an armed hold or cancel a prompt already waiting on it.
    Cancel { id: String },
}

/// Identifies the actual provider request stopped at a response barrier.
#[derive(Clone, Debug, Serialize)]
struct PendingResponse {
    id: String,
    session_id: String,
    mode: PromptMode,
    prompt: String,
}

#[derive(Clone, Debug)]
enum ResponseState {
    Armed,
    Pending(PendingResponse),
    Released,
    Cancelled,
}

struct ResponseHold {
    id: String,
    session_id: String,
    mode: Option<PromptMode>,
    state: watch::Sender<ResponseState>,
}

/// Owns session-scoped, one-response barriers independently of the broker execution lock.
/// Unarmed prompts run normally. Released and cancelled barriers leave no retained history.
#[derive(Default)]
pub struct MockResponseControl {
    hold_queue: Mutex<VecDeque<Arc<ResponseHold>>>,
}

impl MockResponseControl {
    /// Apply an explicit test operation. IDs belong to one session and one response only.
    pub(crate) async fn dispatch(
        &self,
        session_id: &str,
        command: MockResponseCommand,
    ) -> Result<Value> {
        let cancel = matches!(&command, MockResponseCommand::Cancel { .. });
        match command {
            MockResponseCommand::Hold { mode } => {
                let id = uuid::Uuid::new_v4().to_string();
                let (state, _) = watch::channel(ResponseState::Armed);
                self.hold_queue
                    .lock()
                    .unwrap()
                    .push_back(Arc::new(ResponseHold {
                        id: id.clone(),
                        session_id: session_id.to_owned(),
                        mode,
                        state,
                    }));
                Ok(json!({"id":id}))
            }
            MockResponseCommand::Wait { id } => {
                let mut state = {
                    let hold_queue = self.hold_queue.lock().unwrap();
                    let hold = hold_queue
                        .iter()
                        .find(|hold| hold.id == id && hold.session_id == session_id)
                        .context("unknown mock response hold for this session")?;
                    hold.state.subscribe()
                };
                loop {
                    let observed = state.borrow_and_update().clone();
                    match observed {
                        ResponseState::Pending(request) => {
                            return Ok(serde_json::to_value(request)?);
                        }
                        ResponseState::Cancelled => bail!("mock response hold was cancelled"),
                        ResponseState::Released => bail!("mock response hold was already released"),
                        ResponseState::Armed => {}
                    }
                    state.changed().await.context("mock response hold closed")?;
                }
            }
            MockResponseCommand::Release { id } | MockResponseCommand::Cancel { id } => {
                let mut hold_queue = self.hold_queue.lock().unwrap();
                let index = hold_queue
                    .iter()
                    .position(|hold| hold.id == id && hold.session_id == session_id)
                    .context("unknown mock response hold for this session")?;
                let hold = &hold_queue[index];
                ensure!(
                    cancel || matches!(*hold.state.borrow(), ResponseState::Pending(_)),
                    "mock response has not reached its hold"
                );
                hold.state.send_replace(if cancel {
                    ResponseState::Cancelled
                } else {
                    ResponseState::Released
                });
                hold_queue.remove(index);
                Ok(json!({"id":id, "released":!cancel, "cancelled":cancel}))
            }
        }
    }

    /// Claim the earliest matching armed barrier and publish the request before any response.
    pub(crate) fn arrive(&self, request: &BackendRequest) -> Option<MockResponseLease<'_>> {
        let hold_queue = self.hold_queue.lock().unwrap();
        let hold = hold_queue.iter().find(|hold| {
            hold.session_id == request.harness_session_id
                && hold.mode.is_none_or(|mode| mode == request.mode)
                && matches!(*hold.state.borrow(), ResponseState::Armed)
        })?;
        hold.state
            .send_replace(ResponseState::Pending(PendingResponse {
                id: hold.id.clone(),
                session_id: request.harness_session_id.clone(),
                mode: request.mode,
                prompt: request.input.text(),
            }));
        Some(MockResponseLease {
            control: self,
            hold: Arc::clone(hold),
        })
    }

    /// Cancel outstanding barriers for a session, or every barrier during provider shutdown.
    pub(crate) fn cancel_session(&self, session_id: Option<&str>) {
        self.hold_queue.lock().unwrap().retain(|hold| {
            if session_id.is_none_or(|session_id| hold.session_id == session_id) {
                hold.state.send_replace(ResponseState::Cancelled);
                false
            } else {
                true
            }
        });
    }
}

/// Keeps cancellation observable when the provider future is dropped before release.
pub(crate) struct MockResponseLease<'control> {
    control: &'control MockResponseControl,
    hold: Arc<ResponseHold>,
}

impl MockResponseLease<'_> {
    /// Suspend this response until explicit release or cancellation, without a timer.
    pub(crate) async fn released(&self) -> Result<()> {
        let mut state = self.hold.state.subscribe();
        loop {
            let observed = state.borrow_and_update().clone();
            match observed {
                ResponseState::Released => return Ok(()),
                ResponseState::Cancelled => bail!("mock response hold was cancelled"),
                ResponseState::Armed | ResponseState::Pending(_) => {}
            }
            state.changed().await.context("mock response hold closed")?;
        }
    }
}

impl Drop for MockResponseLease<'_> {
    fn drop(&mut self) {
        let mut hold_queue = self.control.hold_queue.lock().unwrap();
        if let Some(index) = hold_queue.iter().position(|hold| hold.id == self.hold.id) {
            self.hold.state.send_replace(ResponseState::Cancelled);
            hold_queue.remove(index);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn request(session_id: &str, mode: PromptMode) -> BackendRequest {
        BackendRequest {
            execution_owner: None,
            harness_session_id: session_id.into(),
            workspace: ".".into(),
            input: super::super::BackendInput::from_text("Why this interface?"),
            mode,
            model: "mock-model".into(),
            effort: "low".into(),
            context_window: None,
            service_tier: Default::default(),
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            backend_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        }
    }

    #[tokio::test(start_paused = true)]
    async fn response_waits_for_explicit_release_and_not_elapsed_time() -> Result<()> {
        let control = MockResponseControl::default();
        let held = control
            .dispatch(
                "session",
                MockResponseCommand::Hold {
                    mode: Some(PromptMode::PlanQuestion),
                },
            )
            .await?;
        let id = held["id"].as_str().unwrap().to_owned();
        let mut waiting =
            Box::pin(control.dispatch("session", MockResponseCommand::Wait { id: id.clone() }));
        assert!(futures_util::poll!(&mut waiting).is_pending());
        assert!(
            control
                .arrive(&request("other-session", PromptMode::PlanQuestion))
                .is_none()
        );
        assert!(
            control
                .arrive(&request("session", PromptMode::Chat))
                .is_none()
        );
        let hold = control
            .arrive(&request("session", PromptMode::PlanQuestion))
            .unwrap();
        let observed = waiting.await?;
        assert_eq!(observed["id"], id);
        assert_eq!(observed["mode"], "plan_question");
        assert_eq!(observed["prompt"], "Why this interface?");
        let mut response = Box::pin(hold.released());
        assert!(futures_util::poll!(&mut response).is_pending());
        tokio::time::advance(std::time::Duration::from_secs(3600)).await;
        assert!(futures_util::poll!(&mut response).is_pending());
        control
            .dispatch("session", MockResponseCommand::Release { id: id.clone() })
            .await?;
        response.await?;
        assert!(control.hold_queue.lock().unwrap().is_empty());
        assert!(
            control
                .dispatch("session", MockResponseCommand::Release { id })
                .await
                .is_err()
        );
        Ok(())
    }

    #[tokio::test]
    async fn early_release_and_wrong_session_leave_the_hold_armed() -> Result<()> {
        let control = MockResponseControl::default();
        let held = control
            .dispatch("session", MockResponseCommand::Hold { mode: None })
            .await?;
        let id = held["id"].as_str().unwrap().to_owned();
        let early = control
            .dispatch("session", MockResponseCommand::Release { id: id.clone() })
            .await
            .unwrap_err();
        assert!(early.to_string().contains("has not reached"));
        assert!(
            control
                .dispatch(
                    "other-session",
                    MockResponseCommand::Cancel { id: id.clone() }
                )
                .await
                .is_err()
        );
        let hold = control
            .arrive(&request("session", PromptMode::Chat))
            .unwrap();
        control
            .dispatch("session", MockResponseCommand::Release { id })
            .await?;
        hold.released().await?;
        Ok(())
    }

    #[tokio::test]
    async fn cancellation_and_dropped_requests_wake_waiters_and_remove_holds() -> Result<()> {
        let control = MockResponseControl::default();
        let held = control
            .dispatch("session", MockResponseCommand::Hold { mode: None })
            .await?;
        let id = held["id"].as_str().unwrap().to_owned();
        let mut waiting =
            Box::pin(control.dispatch("session", MockResponseCommand::Wait { id: id.clone() }));
        assert!(futures_util::poll!(&mut waiting).is_pending());
        let hold = control
            .arrive(&request("session", PromptMode::Chat))
            .unwrap();
        drop(hold);
        assert!(waiting.await.unwrap_err().to_string().contains("cancelled"));
        assert!(control.hold_queue.lock().unwrap().is_empty());

        let held = control
            .dispatch("session", MockResponseCommand::Hold { mode: None })
            .await?;
        let id = held["id"].as_str().unwrap().to_owned();
        let hold = control
            .arrive(&request("session", PromptMode::Chat))
            .unwrap();
        control.cancel_session(Some("other-session"));
        assert_eq!(control.hold_queue.lock().unwrap().len(), 1);
        control.cancel_session(Some("session"));
        assert!(
            hold.released()
                .await
                .unwrap_err()
                .to_string()
                .contains("cancelled")
        );
        assert!(
            control
                .dispatch("session", MockResponseCommand::Release { id })
                .await
                .is_err()
        );

        let held = control
            .dispatch("session", MockResponseCommand::Hold { mode: None })
            .await?;
        let id = held["id"].as_str().unwrap().to_owned();
        let mut waiting =
            Box::pin(control.dispatch("session", MockResponseCommand::Wait { id: id.clone() }));
        assert!(futures_util::poll!(&mut waiting).is_pending());
        control
            .dispatch("session", MockResponseCommand::Cancel { id })
            .await?;
        assert!(waiting.await.unwrap_err().to_string().contains("cancelled"));
        assert!(control.hold_queue.lock().unwrap().is_empty());
        Ok(())
    }

    #[tokio::test]
    async fn repeated_holds_claim_one_matching_response_in_order() -> Result<()> {
        let control = MockResponseControl::default();
        let first = control
            .dispatch("session", MockResponseCommand::Hold { mode: None })
            .await?;
        let second = control
            .dispatch("session", MockResponseCommand::Hold { mode: None })
            .await?;
        let first_hold = control
            .arrive(&request("session", PromptMode::Chat))
            .unwrap();
        let second_hold = control
            .arrive(&request("session", PromptMode::Chat))
            .unwrap();
        assert_eq!(first_hold.hold.id, first["id"].as_str().unwrap());
        assert_eq!(second_hold.hold.id, second["id"].as_str().unwrap());
        assert!(
            control
                .arrive(&request("session", PromptMode::Chat))
                .is_none()
        );
        control.cancel_session(None);
        assert!(first_hold.released().await.is_err());
        assert!(second_hold.released().await.is_err());
        assert!(control.hold_queue.lock().unwrap().is_empty());
        Ok(())
    }
}
