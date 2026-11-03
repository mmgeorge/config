use anyhow::Result;
use forge_provider_api::{ProviderEvent, ProviderEventSink};
use github_copilot_sdk::SessionEvent;
use std::collections::{HashMap, HashSet};
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicBool, Ordering},
};
use tokio::sync::Notify;

/// Describe the terminal state established by a native idle observation.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum IdleOutcome {
    Completed,
    Aborted,
}

pub enum CopilotRetraction {
    Unsent,
    Event(String),
    History(String),
}

/// Coordinates prompt admission, interruption, and rewind identity for one Copilot runtime.
#[derive(Default)]
pub struct CopilotTurnEvent {
    state: Mutex<CopilotTurnState>,
    sends_changed: Notify,
    finished: Notify,
}

impl CopilotTurnEvent {
    pub fn reset(&self) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        anyhow::ensure!(
            !matches!(state.delivery, TurnDelivery::Active(_)) && state.pending_sends == 0,
            "Copilot turn or send is already active"
        );
        *state = CopilotTurnState::default();
        Ok(())
    }

    /// Attach the active consumer before any provider startup or prompt submission.
    pub fn activate(
        &self,
        event_sink: Option<ProviderEventSink>,
        submission_cancelled: Option<Arc<AtomicBool>>,
    ) -> Result<CopilotTurnEventGuard<'_>> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        anyhow::ensure!(
            !matches!(state.delivery, TurnDelivery::Active(_)) && state.pending_sends == 0,
            "Copilot turn or send is already active"
        );
        *state = CopilotTurnState {
            delivery: TurnDelivery::Active(Box::new(ActiveDelivery {
                event_sink,
                submission_cancelled,
                admission: SendAdmission::Open,
            })),
            ..Default::default()
        };
        Ok(CopilotTurnEventGuard { owner: self })
    }

    pub fn is_active(&self) -> bool {
        matches!(
            self.state
                .lock()
                .expect("Copilot turn lock poisoned")
                .delivery,
            TurnDelivery::Active(_)
        )
    }

    pub fn event_sink(&self) -> Option<ProviderEventSink> {
        let state = self.state.lock().expect("Copilot turn lock poisoned");
        match &state.delivery {
            TurnDelivery::Active(active) | TurnDelivery::Retained(active) => {
                active.event_sink.clone()
            }
            TurnDelivery::Idle => None,
        }
    }

    /// Admit a native send before polling it and retain its exact accepted message identity.
    pub async fn steer(
        &self,
        submission: Option<forge_provider_api::lifecycle::SubmissionId>,
        send: impl std::future::Future<Output = Result<String>>,
    ) -> Result<()> {
        let pending = self.admit_send(submission)?;
        match send.await {
            Ok(message_id) => pending.complete(Some(message_id)),
            Err(error) => {
                pending.failed();
                Err(error)
            }
        }
    }

    /// Freeze admission at native idle and retain the inputs observed before that boundary.
    pub fn observe_idle(&self) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        match &mut state.delivery {
            TurnDelivery::Active(delivery) | TurnDelivery::Retained(delivery) => {
                delivery.admission = SendAdmission::Closed
            }
            TurnDelivery::Idle => {}
        }
        state.terminal_inputs = Some(state.observed_inputs.clone());
        drop(state);
        self.sends_changed.notify_waiters();
        Ok(())
    }

    /// Record the durable idle event used to certify a terminal event chain.
    pub(crate) fn observe_idle_at(&self, event: &SessionEvent, outcome: IdleOutcome) -> Result<()> {
        self.observe_idle()?;
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        state.terminal_event = Some((event.id.clone(), outcome));
        Ok(())
    }

    /// Certify host submissions only when each consumed turn ended before the retained idle.
    pub fn certified_submissions(
        &self,
    ) -> Result<Vec<forge_provider_api::lifecycle::SubmissionId>> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        let (idle, outcome) = state
            .terminal_event
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("Copilot idle lacks a terminal event"))?;
        let covered = state
            .terminal_inputs
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("Copilot idle lacks input coverage"))?;
        anyhow::ensure!(
            state.pending_sends == 0
                && !state.uncertain_send
                && state.accepted_inputs.is_subset(covered),
            "Copilot terminal boundary has unresolved delivery"
        );
        let mut result = Vec::new();
        for (message, submission) in &state.accepted_submissions {
            let (turn, input_event) = state
                .input_turns
                .get(message)
                .ok_or_else(|| anyhow::anyhow!("Copilot input lacks consumed turn identity"))?;
            let proven = covered.contains(message)
                && match outcome {
                    IdleOutcome::Completed => {
                        state.root_finishes.iter().any(|(finished_turn, finish)| {
                            finished_turn == turn
                                && chain_contains(&state.event_parents, finish, input_event)
                                && chain_contains(&state.event_parents, idle, finish)
                        })
                    }
                    IdleOutcome::Aborted => chain_contains(&state.event_parents, idle, input_event),
                };
            anyhow::ensure!(
                proven,
                "Copilot idle does not cover the accepted input's completed turn"
            );
            result.push(submission.clone());
        }
        Ok(result)
    }

    /// Group terminal-covered inputs by their exact native start and terminal outcome.
    pub fn certified_executions(
        &self,
    ) -> Result<
        Vec<(
            String,
            Vec<forge_provider_api::lifecycle::SubmissionId>,
            forge_provider_api::lifecycle::ExecutionResult,
        )>,
    > {
        self.certified_submissions()?;
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        let (idle, outcome) = state
            .terminal_event
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("Copilot idle lacks a terminal event"))?;
        let mut groups: Vec<(
            String,
            Vec<forge_provider_api::lifecycle::SubmissionId>,
            forge_provider_api::lifecycle::ExecutionResult,
        )> = Vec::new();
        for (message, submission) in &state.accepted_submissions {
            let (turn, input_event) = state
                .input_turns
                .get(message)
                .ok_or_else(|| anyhow::anyhow!("Copilot input lacks consumed turn identity"))?;
            let end = state
                .root_finishes
                .iter()
                .find(|(finished_turn, end)| {
                    finished_turn == turn
                        && chain_contains(&state.event_parents, end, input_event)
                        && chain_contains(&state.event_parents, idle, end)
                })
                .map(|(_, end)| end.as_str());
            let (boundary, terminal) = match (outcome, end) {
                (_, Some(end)) => (
                    end,
                    forge_provider_api::lifecycle::ExecutionResult::Completed {
                        completion: forge_provider_api::completion::ProviderCompletion::default()
                            .into(),
                    },
                ),
                (IdleOutcome::Aborted, None) => (
                    idle.as_str(),
                    forge_provider_api::lifecycle::ExecutionResult::Cancelled {
                        completion: forge_provider_api::completion::ProviderCompletion::default()
                            .into(),
                    },
                ),
                (IdleOutcome::Completed, None) => {
                    anyhow::bail!("Copilot input has no completed native turn")
                }
            };
            let (_, anchor) = state
                .root_starts
                .iter()
                .rev()
                .find(|(started_turn, start)| {
                    started_turn == turn && chain_contains(&state.event_parents, boundary, start)
                })
                .ok_or_else(|| {
                    anyhow::anyhow!("Copilot input has no correlated foreground start")
                })?;
            if let Some((_, covered, previous)) = groups
                .iter_mut()
                .find(|(existing, _, _)| existing == anchor)
            {
                anyhow::ensure!(
                    *previous == terminal,
                    "Copilot execution has conflicting terminal evidence"
                );
                covered.push(submission.clone());
            } else {
                groups.push((anchor.clone(), vec![submission.clone()], terminal));
            }
        }
        groups.sort_by_key(|(anchor, _, _)| {
            state
                .root_starts
                .iter()
                .position(|(_, id)| id == anchor)
                .unwrap_or(usize::MAX)
        });
        Ok(groups)
    }

    /// Wait until native idle covers all sends admitted before its write fence.
    pub async fn idle_settled(&self) -> Result<()> {
        loop {
            let changed = self.sends_changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            {
                let state = self
                    .state
                    .lock()
                    .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
                if state.uncertain_send {
                    return Err(forge_provider_api::ProviderError::DeliveryUncertain {
                        message: "Copilot send ended without acceptance evidence".into(),
                    }
                    .into());
                }
                if state.pending_sends == 0
                    && state
                        .terminal_inputs
                        .as_ref()
                        .is_some_and(|covered| state.accepted_inputs.is_subset(covered))
                {
                    return Ok(());
                }
            }
            changed.await;
        }
    }

    /// Register dispatch under a short state lock; native responses never hold admission serialization.
    fn admit_send(
        &self,
        submission: Option<forge_provider_api::lifecycle::SubmissionId>,
    ) -> Result<PendingSend<'_>> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        let delivery = match &state.delivery {
            TurnDelivery::Active(delivery) | TurnDelivery::Retained(delivery) => delivery,
            TurnDelivery::Idle => {
                return Err(forge_provider_api::ProviderError::NotConsumed {
                    message: "Copilot session has no admitted input context".into(),
                }
                .into());
            }
        };
        if state.cancelled
            || delivery
                .submission_cancelled
                .as_ref()
                .is_some_and(|flag| flag.load(Ordering::Acquire))
        {
            return Err(forge_provider_api::error::TurnCancelled.into());
        }
        if delivery.admission == SendAdmission::Closed {
            return Err(forge_provider_api::ProviderError::NotConsumed {
                message: "Copilot native idle closed this execution's input admission".into(),
            }
            .into());
        }
        state.pending_sends += 1;
        Ok(PendingSend {
            owner: self,
            submission,
            completed: false,
        })
    }

    /// Fence future sends and wait only for a send that already owns dispatch.
    pub async fn stop(&self) -> Result<bool> {
        self.request_stop()?;
        loop {
            let changed = self.sends_changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            if self
                .state
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?
                .pending_sends
                == 0
            {
                break;
            }
            changed.await;
        }
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        Ok(matches!(state.submission, NativeSubmission::Sent { .. }))
    }

    /// Fence new sends immediately and identify a submitted send that still needs collection.
    pub fn request_stop(&self) -> Result<StopRequest> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        state.cancelled = true;
        Ok(StopRequest {
            submitted: matches!(state.submission, NativeSubmission::Sent { .. }),
            sending: state.pending_sends != 0,
        })
    }

    pub async fn wait_finished(&self) {
        loop {
            let finished = self.finished.notified();
            tokio::pin!(finished);
            finished.as_mut().enable();
            if !self.is_active() {
                return;
            }
            finished.await;
        }
    }

    /// Match the admitted input to a later root start in the native event chain.
    pub fn foreground_anchor(&self) -> Result<Option<(String, String)>> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        let NativeSubmission::Sent {
            message_id: Some(message_id),
            ..
        } = &state.submission
        else {
            return Ok(None);
        };
        let Some((turn_id, input_event)) = state.input_turns.get(message_id) else {
            return Ok(None);
        };
        for (started_turn, event_id) in &state.root_starts {
            if started_turn != turn_id {
                continue;
            }
            let mut current = Some(event_id.as_str());
            let mut seen = HashSet::new();
            while let Some(id) = current {
                if !seen.insert(id) {
                    break;
                }
                if id == input_event {
                    return Ok(Some((turn_id.clone(), event_id.clone())));
                }
                current = state.event_parents.get(id).and_then(Option::as_deref);
            }
        }
        Ok(None)
    }

    /// Retain the actual first root user event rather than an SDK request or reminder identity.
    pub fn observe(&self, event: &SessionEvent) -> Result<()> {
        {
            let mut state = self
                .state
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
            state
                .event_parents
                .insert(event.id.clone(), event.parent_id.clone());
        }
        // Root inputs also carry parentAgentTaskId; envelope ownership and receipt identity govern coverage.
        if event.agent_id.is_none() && event.event_type == "user.message" {
            let mut state = self
                .state
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
            if let Some(message_id) = event
                .data
                .get("messageId")
                .and_then(serde_json::Value::as_str)
            {
                state.observed_inputs.insert(message_id.to_owned());
                if let Some(turn_id) = event.data.get("turnId").and_then(serde_json::Value::as_str)
                {
                    state.input_turns.insert(
                        message_id.to_owned(),
                        (turn_id.to_owned(), event.id.clone()),
                    );
                }
            }
            if let NativeSubmission::Sent { user_event_id, .. } = &mut state.submission
                && user_event_id.is_none()
            {
                *user_event_id = Some(event.id.clone());
            }
        }
        if event.agent_id.is_none()
            && event
                .data
                .get("parentToolCallId")
                .is_none_or(serde_json::Value::is_null)
            && event.event_type == "assistant.turn_start"
            && let Some(turn_id) = event.data.get("turnId").and_then(serde_json::Value::as_str)
        {
            self.state
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?
                .root_starts
                .push((turn_id.to_owned(), event.id.clone()));
        }
        if event.agent_id.is_none()
            && event
                .data
                .get("parentToolCallId")
                .is_none_or(serde_json::Value::is_null)
            && event.event_type == "assistant.turn_end"
            && let Some(turn_id) = event.data.get("turnId").and_then(serde_json::Value::as_str)
        {
            self.state
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?
                .root_finishes
                .push((turn_id.to_owned(), event.id.clone()));
        }
        Ok(())
    }

    pub fn retraction_target(&self) -> Result<CopilotRetraction> {
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
                ..
            } => CopilotRetraction::History(cursor.clone()),
        })
    }

    pub fn finish_retraction(&self) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        state.submission = NativeSubmission::Unsent;
        Ok(())
    }

    /// Publish steering only after the Copilot SDK accepts the immediate message.
    pub async fn publish_steering(
        &self,
        text: String,
        input_message_id: Option<String>,
        task_start: Option<forge_provider_api::input::TaskStart>,
    ) -> Result<()> {
        let event_sink = {
            let state = self
                .state
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
            match &state.delivery {
                TurnDelivery::Active(active) | TurnDelivery::Retained(active) => {
                    active.event_sink.clone()
                }
                TurnDelivery::Idle => None,
            }
        };
        if let Some(event_sink) = event_sink {
            let acknowledgement = match task_start {
                Some(start) => forge_provider_api::InputAcknowledgement::TaskStarted {
                    input_id: input_message_id,
                    start,
                    prompt: text,
                },
                None => forge_provider_api::InputAcknowledgement::Steering {
                    input_id: input_message_id,
                    text,
                },
            };
            event_sink
                .send_wait(ProviderEvent::InputAccepted {
                    scope: event_sink.invocation_scope()?.clone(),
                    acknowledgement,
                })
                .await?;
        }
        Ok(())
    }
}

/// Separates the native abort signal from collection of a committed SDK send.
pub struct StopRequest {
    /// Reports that a native send was admitted before the stop fence.
    pub submitted: bool,
    /// Reports that its SDK response has not yet released dispatch ownership.
    pub sending: bool,
}

/// Retains send ownership and the exact native history boundary across prompt completion.
#[derive(Default)]
struct CopilotTurnState {
    delivery: TurnDelivery,
    submission: NativeSubmission,
    cancelled: bool,
    pending_sends: usize,
    accepted_inputs: HashSet<String>,
    accepted_submissions: HashMap<String, forge_provider_api::lifecycle::SubmissionId>,
    input_turns: HashMap<String, (String, String)>,
    root_starts: Vec<(String, String)>,
    root_finishes: Vec<(String, String)>,
    terminal_event: Option<(String, IdleOutcome)>,
    event_parents: HashMap<String, Option<String>>,
    observed_inputs: HashSet<String>,
    terminal_inputs: Option<HashSet<String>>,
    uncertain_send: bool,
}

#[derive(Default)]
enum TurnDelivery {
    #[default]
    Idle,
    Active(Box<ActiveDelivery>),
    Retained(Box<ActiveDelivery>),
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum SendAdmission {
    Open,
    Closed,
}

struct ActiveDelivery {
    admission: SendAdmission,
    submission_cancelled: Option<Arc<AtomicBool>>,
    event_sink: Option<ProviderEventSink>,
}

#[derive(Default)]
enum NativeSubmission {
    #[default]
    Unsent,
    Sent {
        cursor: String,
        user_event_id: Option<String>,
        message_id: Option<String>,
    },
}

/// Retain idle delivery and rewind identity after the foreground observer leaves.
pub struct CopilotTurnEventGuard<'a> {
    owner: &'a CopilotTurnEvent,
}

impl CopilotTurnEventGuard<'_> {
    /// Preserve initial send ownership until native acceptance or failure settles.
    pub async fn send(
        &self,
        cursor: String,
        submission: Option<forge_provider_api::lifecycle::SubmissionId>,
        send: impl std::future::Future<Output = Result<String>>,
    ) -> Result<()> {
        let pending = self.owner.admit_send(submission)?;
        {
            let mut state = self
                .owner
                .state
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
            state.submission = NativeSubmission::Sent {
                cursor,
                user_event_id: None,
                message_id: None,
            };
        }
        match send.await {
            Ok(message_id) => {
                let mut state = self
                    .owner
                    .state
                    .lock()
                    .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
                if let NativeSubmission::Sent {
                    message_id: stored, ..
                } = &mut state.submission
                {
                    *stored = Some(message_id.clone());
                }
                drop(state);
                pending.complete(Some(message_id))
            }
            Err(error) => {
                pending.failed();
                Err(error)
            }
        }
    }
}

/// Release in-flight admission even when the response future is cancelled or panics.
struct PendingSend<'a> {
    owner: &'a CopilotTurnEvent,
    submission: Option<forge_provider_api::lifecycle::SubmissionId>,
    completed: bool,
}

impl PendingSend<'_> {
    fn complete(mut self, message_id: Option<String>) -> Result<()> {
        if message_id.as_ref().is_some_and(String::is_empty) {
            return Err(forge_provider_api::ProviderError::DeliveryUncertain {
                message: "Copilot acknowledged a send without a message identity".into(),
            }
            .into());
        }
        let mut state = self
            .owner
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        if let Some(message_id) = message_id {
            if let Some(submission) = self.submission.take() {
                state
                    .accepted_submissions
                    .insert(message_id.clone(), submission);
            }
            state.accepted_inputs.insert(message_id);
        }
        state.pending_sends -= 1;
        self.completed = true;
        drop(state);
        self.owner.sends_changed.notify_waiters();
        Ok(())
    }

    fn failed(mut self) {
        self.release();
    }

    fn release(&mut self) {
        if self.completed {
            return;
        }
        let mut state = self
            .owner
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        state.pending_sends -= 1;
        state.uncertain_send = true;
        self.completed = true;
        drop(state);
        self.owner.sends_changed.notify_waiters();
    }
}

impl Drop for PendingSend<'_> {
    fn drop(&mut self) {
        self.release();
    }
}

/// Follow only known durable ancestry; missing links never prove coverage.
fn chain_contains(parents: &HashMap<String, Option<String>>, event: &str, ancestor: &str) -> bool {
    let mut current = Some(event);
    let mut seen = HashSet::new();
    while let Some(id) = current {
        if !seen.insert(id) {
            return false;
        }
        if id == ancestor {
            return true;
        }
        current = parents.get(id).and_then(Option::as_deref);
    }
    false
}

impl Drop for CopilotTurnEventGuard<'_> {
    fn drop(&mut self) {
        if let Ok(mut state) = self.owner.state.lock()
            && let TurnDelivery::Active(active) = std::mem::take(&mut state.delivery)
        {
            state.delivery = TurnDelivery::Retained(active);
        }
        self.owner.finished.notify_waiters();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::{ExecutionOwner, InvocationScope};

    /// Correlate the SDK receipt to its user message and root turn, not child or replay events.
    #[tokio::test]
    async fn foreground_anchor_requires_matching_input_and_root_turn() -> Result<()> {
        let turn = CopilotTurnEvent::default();
        let guard = turn.activate(None, None)?;
        guard
            .send("cursor".into(), None, async { Ok("receipt-id".into()) })
            .await?;
        for (id, agent, turn_id) in [
            ("child-event", Some("child"), "child-turn"),
            ("old-event", None, "old-turn"),
        ] {
            turn.observe(&serde_json::from_value(serde_json::json!({
                "id": id, "agentId": agent, "type":"assistant.turn_start",
                "timestamp":"2026-11-01T00:00:00Z", "data":{"turnId":turn_id}
            }))?)?;
        }
        assert!(turn.foreground_anchor()?.is_none());
        turn.observe(&serde_json::from_value(serde_json::json!({
            "id":"child-input", "agentId":"child", "type":"user.message",
            "timestamp":"2026-11-01T00:00:00Z",
            "data":{"messageId":"receipt-id", "turnId":"child-turn", "parentAgentTaskId":"child-task"}
        }))?)?;
        turn.observe(&serde_json::from_value(serde_json::json!({
            "id":"foreign-start", "parentId":"child-input", "type":"assistant.turn_start",
            "timestamp":"2026-11-01T00:00:00Z", "data":{"turnId":"child-turn"}
        }))?)?;
        assert!(
            turn.foreground_anchor()?.is_none(),
            "child input established root ownership"
        );
        turn.observe(&serde_json::from_value(serde_json::json!({
            "id":"different-event-uuid", "type":"user.message", "timestamp":"2026-11-01T00:00:00Z",
            "data":{"messageId":"receipt-id", "turnId":"current-turn", "content":"hello",
                "parentAgentTaskId":"root-task"}
        }))?)?;
        assert!(turn.foreground_anchor()?.is_none());
        turn.observe(&serde_json::from_value(serde_json::json!({
            "id":"child-without-agent-id", "parentId":"different-event-uuid",
            "type":"assistant.turn_start", "timestamp":"2026-11-01T00:00:00Z",
            "data":{"turnId":"current-turn", "parentToolCallId":"child-tool"}
        }))?)?;
        assert!(turn.foreground_anchor()?.is_none());
        turn.observe(&serde_json::from_value(serde_json::json!({
            "id":"current-event", "parentId":"different-event-uuid", "type":"assistant.turn_start", "timestamp":"2026-11-01T00:00:00Z",
            "data":{"turnId":"current-turn"}
        }))?)?;
        assert_eq!(
            turn.foreground_anchor()?,
            Some(("current-turn".into(), "current-event".into()))
        );
        Ok(())
    }

    /// A successful RPC lacking a message identity leaves delivery uncertain.
    #[tokio::test]
    async fn missing_native_message_identity_fails_uncertain() -> Result<()> {
        let turn = CopilotTurnEvent::default();
        let guard = turn.activate(None, None)?;
        let error = guard
            .send("cursor".into(), None, async { Ok(String::new()) })
            .await
            .unwrap_err();
        assert!(matches!(
            error.downcast_ref::<forge_provider_api::ProviderError>(),
            Some(forge_provider_api::ProviderError::DeliveryUncertain { .. })
        ));
        assert!(turn.idle_settled().await.is_err());
        Ok(())
    }

    /// Certify a long execution without rejecting its complete native ancestry.
    #[tokio::test]
    async fn terminal_certificate_preserves_long_event_history() -> Result<()> {
        let turn = CopilotTurnEvent::default();
        let guard = turn.activate(None, None)?;
        let submission = forge_provider_api::lifecycle::SubmissionId::try_from("long".to_owned())?;
        guard
            .send("cursor".into(), Some(submission.clone()), async {
                Ok("message".into())
            })
            .await?;
        let mut parent = None;
        for sequence in 0..16_388 {
            let (kind, data) = match sequence {
                0 => (
                    "user.message",
                    serde_json::json!({"messageId":"message","turnId":"turn"}),
                ),
                1 => ("assistant.turn_start", serde_json::json!({"turnId":"turn"})),
                16_386 => ("assistant.turn_end", serde_json::json!({"turnId":"turn"})),
                16_387 => ("session.idle", serde_json::json!({})),
                _ => (
                    "assistant.message_delta",
                    serde_json::json!({"messageId":"answer","deltaContent":"text"}),
                ),
            };
            let event: SessionEvent = serde_json::from_value(serde_json::json!({
                "id":format!("event-{sequence}"),"parentId":parent,"type":kind,
                "timestamp":"2026-11-02T00:00:00Z","data":data
            }))?;
            turn.observe(&event)?;
            if kind == "session.idle" {
                turn.observe_idle_at(&event, IdleOutcome::Completed)?;
            }
            parent = Some(event.id);
        }
        turn.idle_settled().await?;
        assert_eq!(turn.certified_submissions()?, vec![submission]);
        Ok(())
    }

    /// Preserve host-to-native identity and require each completed turn in the idle chain.
    #[tokio::test]
    async fn terminal_certificate_covers_only_inputs_with_completed_turns() -> Result<()> {
        let turn = CopilotTurnEvent::default();
        let guard = turn.activate(None, None)?;
        let first = forge_provider_api::lifecycle::SubmissionId::try_from("first".to_owned())?;
        let second = forge_provider_api::lifecycle::SubmissionId::try_from("second".to_owned())?;
        guard
            .send("cursor".into(), Some(first.clone()), async {
                Ok("native-first".into())
            })
            .await?;
        turn.steer(Some(second.clone()), async { Ok("native-second".into()) })
            .await?;
        let events = [
            (
                "user-a",
                None,
                "user.message",
                serde_json::json!({"messageId":"native-first","turnId":"a","content":"a","parentAgentTaskId":"first-task"}),
            ),
            (
                "start-a",
                Some("user-a"),
                "assistant.turn_start",
                serde_json::json!({"turnId":"a"}),
            ),
            (
                "end-a",
                Some("start-a"),
                "assistant.turn_end",
                serde_json::json!({"turnId":"a"}),
            ),
            (
                "user-b",
                Some("end-a"),
                "user.message",
                serde_json::json!({"messageId":"native-second","turnId":"b","content":"b","parentAgentTaskId":"second-task"}),
            ),
            (
                "start-b",
                Some("user-b"),
                "assistant.turn_start",
                serde_json::json!({"turnId":"b"}),
            ),
            (
                "end-b",
                Some("start-b"),
                "assistant.turn_end",
                serde_json::json!({"turnId":"b"}),
            ),
            (
                "idle",
                Some("end-b"),
                "assistant.idle",
                serde_json::json!({}),
            ),
        ];
        for (id, parent, kind, data) in events {
            let event: SessionEvent = serde_json::from_value(serde_json::json!({
                "id":id, "parentId":parent, "type":kind, "timestamp":"2026-11-01T00:00:00Z", "data":data
            }))?;
            turn.observe(&event)?;
            if id == "idle" {
                turn.observe_idle_at(&event, IdleOutcome::Completed)?;
            }
        }
        turn.idle_settled().await?;
        let covered = turn.certified_submissions()?;
        assert_eq!(covered.len(), 2);
        assert!(covered.contains(&first) && covered.contains(&second));
        let executions = turn.certified_executions()?;
        assert_eq!(executions.len(), 2);
        assert_eq!(executions[0].0, "start-a");
        assert_eq!(executions[0].1, vec![first]);
        assert_eq!(executions[1].0, "start-b");
        assert_eq!(executions[1].1, vec![second]);
        Ok(())
    }

    /// Aborted idle proves cancellation only after every admitted send has settled.
    #[tokio::test]
    async fn aborted_idle_requires_resolved_delivery_and_input_ancestry() -> Result<()> {
        let turn = CopilotTurnEvent::default();
        let guard = turn.activate(None, None)?;
        let first = forge_provider_api::lifecycle::SubmissionId::try_from("first".to_owned())?;
        guard
            .send("cursor".into(), Some(first.clone()), async {
                Ok("native-first".into())
            })
            .await?;
        let (ack, response) = tokio::sync::oneshot::channel();
        let send = turn.steer(None, async {
            response.await?;
            Ok("later".into())
        });
        tokio::pin!(send);
        assert!(futures_util::poll!(send.as_mut()).is_pending());
        let user: SessionEvent = serde_json::from_value(serde_json::json!({
            "id":"user", "type":"user.message", "timestamp":"2026-11-01T00:00:00Z",
            "data":{"messageId":"native-first", "turnId":"turn", "content":"first"}
        }))?;
        let idle: SessionEvent = serde_json::from_value(serde_json::json!({
            "id":"idle", "parentId":"user", "type":"session.idle",
            "timestamp":"2026-11-01T00:00:00Z", "data":{"aborted":true}
        }))?;
        turn.observe(&user)?;
        turn.observe(&idle)?;
        turn.observe_idle_at(&idle, IdleOutcome::Aborted)?;
        assert!(turn.certified_submissions().is_err());
        ack.send(()).unwrap();
        send.await?;
        assert!(turn.certified_submissions().is_err());
        Ok(())
    }

    /// An idle boundary cannot cover an accepted input whose native message appeared later.
    #[tokio::test]
    async fn idle_before_steering_acceptance_waits_for_matching_later_idle() -> Result<()> {
        let turn = CopilotTurnEvent::default();
        let _guard = turn.activate(None, None)?;
        let (acknowledge, acknowledged) = tokio::sync::oneshot::channel();
        let send = turn.steer(None, async {
            acknowledged.await?;
            Ok("input".into())
        });
        tokio::pin!(send);
        assert!(futures_util::poll!(send.as_mut()).is_pending());
        turn.observe_idle()?;
        acknowledge.send(()).unwrap();
        send.await?;
        let message: SessionEvent = serde_json::from_value(serde_json::json!({
            "id":"event", "type":"user.message", "timestamp":"2026-11-01T00:00:00Z",
            "data":{"messageId":"input","content":"follow up"}
        }))?;
        turn.observe(&message)?;
        let settled = turn.idle_settled();
        tokio::pin!(settled);
        assert!(futures_util::poll!(settled.as_mut()).is_pending());
        turn.observe_idle()?;
        settled.await?;
        Ok(())
    }

    /// A delayed RPC response can use input evidence already observed before native idle.
    #[tokio::test]
    async fn idle_before_acknowledgment_uses_prior_native_input_evidence() -> Result<()> {
        let turn = CopilotTurnEvent::default();
        let _guard = turn.activate(None, None)?;
        let (acknowledge, acknowledged) = tokio::sync::oneshot::channel();
        let send = turn.steer(None, async {
            acknowledged.await?;
            Ok("input".into())
        });
        tokio::pin!(send);
        assert!(futures_util::poll!(send.as_mut()).is_pending());
        turn.observe(&serde_json::from_value(serde_json::json!({
            "id":"event", "type":"user.message", "timestamp":"2026-11-01T00:00:00Z",
            "data":{"messageId":"input","content":"follow up"}
        }))?)?;
        turn.observe_idle()?;
        acknowledge.send(()).unwrap();
        send.await?;
        turn.idle_settled().await?;
        let called = AtomicBool::new(false);
        let rejected = turn
            .steer(None, async {
                called.store(true, Ordering::Release);
                Ok("late".into())
            })
            .await;
        assert!(
            rejected
                .unwrap_err()
                .downcast_ref::<forge_provider_api::ProviderError>()
                .is_some_and(|error| matches!(
                    error,
                    forge_provider_api::ProviderError::NotConsumed { .. }
                ))
        );
        assert!(!called.load(Ordering::Acquire));
        Ok(())
    }

    /// An idle native session retains its original publication and accepts another send.
    #[tokio::test]
    async fn idle_context_keeps_steering_until_cancelled() -> Result<()> {
        let turn = CopilotTurnEvent::default();
        let scope = InvocationScope::new(ExecutionOwner::root("session"), "exchange");
        let (sink, _events) = forge_provider_api::events::channel(scope.clone());
        drop(turn.activate(Some(sink), None)?);
        assert!(!turn.is_active());
        assert_eq!(turn.event_sink().unwrap().invocation_scope()?, &scope);
        turn.steer(None, async { Ok("accepted".into()) }).await?;
        turn.request_stop()?;
        let called = AtomicBool::new(false);
        let result = turn
            .steer(None, async {
                called.store(true, Ordering::Release);
                Ok("unexpected".into())
            })
            .await;
        assert!(result.is_err());
        assert!(!called.load(Ordering::Acquire));
        Ok(())
    }

    /// A new admission replaces the old idle context and its cancellation fence.
    #[tokio::test]
    async fn new_admission_replaces_retained_scope() -> Result<()> {
        let turn = CopilotTurnEvent::default();
        let owner = ExecutionOwner::root("session");
        let old = InvocationScope::new(owner.clone(), "old");
        let (sink, _events) = forge_provider_api::events::channel(old);
        drop(turn.activate(Some(sink), None)?);
        turn.request_stop()?;
        let new = InvocationScope::new(owner, "new");
        let (sink, _events) = forge_provider_api::events::channel(new.clone());
        let guard = turn.activate(Some(sink), None)?;
        assert_eq!(turn.event_sink().unwrap().invocation_scope()?, &new);
        turn.steer(None, async { Ok("accepted".into()) }).await?;
        drop(guard);
        Ok(())
    }
}
