use anyhow::Result;
use forge_provider_api::ProviderEventSink;
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
    HostHandoff,
}

impl IdleOutcome {
    /// Permit an unfinished native root only after interruption established its terminal idle.
    fn interrupted(self) -> bool {
        matches!(self, Self::Aborted | Self::HostHandoff)
    }
}

pub enum CopilotRetraction {
    Unsent,
    Event(String),
    History(String),
}

#[derive(Clone, Eq, PartialEq)]
struct NativeRootTurnId(String);

#[derive(Clone)]
enum RootTurnPhase {
    Running,
    Finished { event_id: String },
}

#[derive(Clone)]
struct RootTurnEvidence {
    native_id: NativeRootTurnId,
    start_event: String,
    phase: RootTurnPhase,
}

struct ConsumedInput {
    native_turn: NativeRootTurnId,
    placement: InputPlacement,
}

enum InputPlacement {
    AwaitingStart,
    RunningTurn { start_event: String },
    FollowingStart { start_event: String },
}

struct IdleEvidence {
    event_id: String,
    outcome: IdleOutcome,
    root_turns: Vec<RootTurnEvidence>,
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

    /// Snapshot concrete live root instances at a native idle without rebasing duplicate idles.
    pub(crate) fn observe_idle_at(&self, event: &SessionEvent, outcome: IdleOutcome) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        anyhow::ensure!(
            event.agent_id.is_none()
                && matches!(event.event_type.as_str(), "assistant.idle" | "session.idle")
                && state.live_events.contains(&event.id),
            "Copilot terminal idle was not observed on the live root stream"
        );
        if let Some(previous) = state.observed_idles.get(&event.id) {
            anyhow::ensure!(
                *previous == outcome,
                "Copilot idle replay conflicts with its native outcome"
            );
            return Ok(());
        }
        match &mut state.delivery {
            TurnDelivery::Active(delivery) | TurnDelivery::Retained(delivery) => {
                delivery.admission = SendAdmission::Closed
            }
            TurnDelivery::Idle => {}
        }
        state.terminal_inputs = Some(state.observed_inputs.clone());
        state.observed_idles.insert(event.id.clone(), outcome);
        state.terminal_event = Some(IdleEvidence {
            event_id: event.id.clone(),
            outcome,
            root_turns: state.root_turns.clone(),
        });
        drop(state);
        self.sends_changed.notify_waiters();
        Ok(())
    }

    /// Validate accepted submissions against one correlated native idle boundary.
    pub fn covered_submissions(&self) -> Result<Vec<forge_provider_api::lifecycle::SubmissionId>> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        let idle = state
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
        anyhow::ensure!(
            idle.outcome.interrupted()
                || idle
                    .root_turns
                    .iter()
                    .all(|root| matches!(root.phase, RootTurnPhase::Finished { .. })),
            "Copilot completed idle precedes a live root turn finish"
        );
        let mut result = Vec::new();
        for (message, submission) in &state.accepted_submissions {
            let input = state
                .input_turns
                .get(message)
                .ok_or_else(|| anyhow::anyhow!("Copilot input lacks consumed turn identity"))?;
            let root = input_root(&idle.root_turns, input);
            let proven = covered.contains(message)
                && root.is_some_and(|root| match idle.outcome {
                    IdleOutcome::Completed => matches!(root.phase, RootTurnPhase::Finished { .. }),
                    IdleOutcome::Aborted | IdleOutcome::HostHandoff => true,
                });
            anyhow::ensure!(
                proven,
                "Copilot idle {} does not cover the accepted input's completed turn",
                idle.event_id
            );
            result.push(submission.clone());
        }
        Ok(result)
    }

    /// Group terminal-covered inputs by their exact native start and terminal outcome.
    pub fn terminal_executions(
        &self,
    ) -> Result<
        Vec<(
            String,
            Vec<forge_provider_api::lifecycle::SubmissionId>,
            forge_provider_api::lifecycle::ExecutionResult,
        )>,
    > {
        self.covered_submissions()?;
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        let idle = state
            .terminal_event
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("Copilot idle lacks a terminal event"))?;
        let mut groups: Vec<(
            String,
            Vec<forge_provider_api::lifecycle::SubmissionId>,
            forge_provider_api::lifecycle::ExecutionResult,
        )> = Vec::new();
        for (message, submission) in &state.accepted_submissions {
            let input = state
                .input_turns
                .get(message)
                .ok_or_else(|| anyhow::anyhow!("Copilot input lacks consumed turn identity"))?;
            let root = input_root(&idle.root_turns, input).ok_or_else(|| {
                anyhow::anyhow!("Copilot input has no correlated foreground start")
            })?;
            let terminal = match (&root.phase, idle.outcome) {
                (RootTurnPhase::Finished { .. }, _)
                | (RootTurnPhase::Running, IdleOutcome::HostHandoff) => {
                    forge_provider_api::lifecycle::ExecutionResult::Completed {
                        completion: forge_provider_api::completion::ProviderCompletion::default()
                            .into(),
                    }
                }
                (RootTurnPhase::Running, IdleOutcome::Aborted) => {
                    forge_provider_api::lifecycle::ExecutionResult::Cancelled {
                        completion: forge_provider_api::completion::ProviderCompletion::default()
                            .into(),
                    }
                }
                (RootTurnPhase::Running, IdleOutcome::Completed) => {
                    anyhow::bail!("Copilot input has no completed native turn")
                }
            };
            let anchor = &root.start_event;
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
                .root_turns
                .iter()
                .position(|root| &root.start_event == anchor)
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
                    return Err(forge_provider_api::ProviderError::DeliveryUncertain.into());
                }
                if state.pending_sends == 0
                    && state
                        .terminal_inputs
                        .as_ref()
                        .is_some_and(|covered| state.accepted_inputs.is_subset(covered))
                    && state.terminal_event.as_ref().is_some_and(|idle| {
                        (idle.outcome.interrupted()
                            || idle
                                .root_turns
                                .iter()
                                .all(|root| matches!(root.phase, RootTurnPhase::Finished { .. })))
                            && state.accepted_inputs.iter().all(|message| {
                                state
                                    .input_turns
                                    .get(message)
                                    .and_then(|input| consumed_root(&idle.root_turns, input))
                                    .is_some_and(|root| {
                                        idle.outcome.interrupted()
                                            || matches!(root.phase, RootTurnPhase::Finished { .. })
                                    })
                            })
                    })
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
                    continuation: forge_provider_api::ContinuationToken::new(
                        uuid::Uuid::new_v4().as_bytes().to_vec(),
                    )
                    .expect("UUID continuation tokens are nonempty"),
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
                continuation: forge_provider_api::ContinuationToken::new(
                    uuid::Uuid::new_v4().as_bytes().to_vec(),
                )
                .expect("UUID continuation tokens are nonempty"),
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

    /// Fence new sends immediately; retained execution owns collection.
    pub fn request_stop(&self) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        state.cancelled = true;
        Ok(())
    }

    /// Fence abort behind every native send admitted before stop closed new input.
    pub(crate) async fn finish_pending_sends(
        &self,
    ) -> std::result::Result<(), forge_provider_api::ProviderError> {
        loop {
            let changed = self.sends_changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            {
                let state = self.state.lock().map_err(|_| {
                    forge_provider_api::ProviderError::Failure(
                        forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::CancelSession,
                            forge_provider_api::ProviderFailureKind::Runtime,
                        ),
                    )
                })?;
                if state.pending_sends == 0 {
                    return if state.uncertain_send {
                        Err(forge_provider_api::ProviderError::DeliveryUncertain)
                    } else {
                        Ok(())
                    };
                }
            }
            changed.await;
        }
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
        let Some(input) = state.input_turns.get(message_id) else {
            return Ok(None);
        };
        Ok(input_root(&state.root_turns, input)
            .map(|root| (root.native_id.0.clone(), root.start_event.clone())))
    }

    /// Retain the actual first root user event rather than an SDK request or reminder identity.
    pub fn observe(&self, event: &SessionEvent) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        let envelope_root = event.agent_id.is_none()
            && event
                .data
                .get("parentToolCallId")
                .is_none_or(serde_json::Value::is_null);
        let lifecycle = matches!(
            event.event_type.as_str(),
            "assistant.turn_start" | "assistant.turn_end"
        );
        let child_lifecycle = lifecycle
            && event
                .parent_id
                .as_ref()
                .is_some_and(|parent| state.child_events.contains(parent));
        let root = envelope_root && !child_lifecycle;
        let native_turn = event
            .data
            .get("turnId")
            .and_then(serde_json::Value::as_str)
            .map(|id| NativeRootTurnId(id.to_owned()));

        // Validate immutable identities and ordered root lifecycle before mutation.
        if let Some(parent) = state.event_parents.get(&event.id) {
            anyhow::ensure!(
                parent == &event.parent_id,
                "Copilot live event conflicts with retained ancestry"
            );
        }
        if state.live_events.contains(&event.id) {
            if root && event.event_type == "assistant.turn_start" {
                anyhow::ensure!(
                    state
                        .root_turns
                        .iter()
                        .any(|turn| turn.start_event == event.id
                            && Some(&turn.native_id) == native_turn.as_ref()),
                    "Copilot replayed root start conflicts with its native identity"
                );
            }
            if root && event.event_type == "assistant.turn_end" {
                let identical = state.root_turns.iter().any(|turn| {
                    Some(&turn.native_id) == native_turn.as_ref()
                        && matches!(&turn.phase, RootTurnPhase::Finished { event_id } if event_id == &event.id)
                });
                anyhow::ensure!(
                    identical,
                    "Copilot replayed root finish conflicts with its native identity"
                );
            }
            return Ok(());
        }
        if root && event.event_type == "assistant.turn_start" {
            anyhow::ensure!(
                native_turn.is_some(),
                "Copilot root start lacks its native turn identity"
            );
            anyhow::ensure!(
                state
                    .root_turns
                    .last()
                    .is_none_or(|turn| matches!(turn.phase, RootTurnPhase::Finished { .. })),
                "Copilot root starts overlap before native finish"
            );
        }
        if root && event.event_type == "assistant.turn_end" {
            anyhow::ensure!(
                state
                    .root_turns
                    .last()
                    .is_some_and(|turn| Some(&turn.native_id) == native_turn.as_ref()
                        && matches!(turn.phase, RootTurnPhase::Running)),
                "Copilot root finish does not match its live start"
            );
        }
        state
            .event_parents
            .insert(event.id.clone(), event.parent_id.clone());
        state.live_events.insert(event.id.clone());
        if !envelope_root || child_lifecycle {
            state.child_events.insert(event.id.clone());
        }

        // Bind consumption to an already-running instance, or retain the input for its later start.
        // Root inputs may carry parentAgentTaskId; only envelope/causal child ownership excludes them.
        if root && event.event_type == "user.message" {
            if let Some(message_id) = event
                .data
                .get("messageId")
                .and_then(serde_json::Value::as_str)
            {
                state.observed_inputs.insert(message_id.to_owned());
                if let Some(native_turn) = native_turn.clone() {
                    let placement = state
                        .root_turns
                        .last()
                        .filter(|turn| {
                            turn.native_id == native_turn
                                && matches!(turn.phase, RootTurnPhase::Running)
                        })
                        .map_or(InputPlacement::AwaitingStart, |turn| {
                            InputPlacement::RunningTurn {
                                start_event: turn.start_event.clone(),
                            }
                        });
                    state.input_turns.insert(
                        message_id.to_owned(),
                        ConsumedInput {
                            native_turn,
                            placement,
                        },
                    );
                }
            }
            if let NativeSubmission::Sent { user_event_id, .. } = &mut state.submission
                && user_event_id.is_none()
            {
                *user_event_id = Some(event.id.clone());
            }
        }
        // Pair live starts and ends; hidden durable accounting records are not terminal authority.
        if root && event.event_type == "assistant.turn_start" {
            let native_id = native_turn.expect("root start identity validated before mutation");
            for input in state.input_turns.values_mut() {
                if input.native_turn == native_id
                    && matches!(input.placement, InputPlacement::AwaitingStart)
                {
                    input.placement = InputPlacement::FollowingStart {
                        start_event: event.id.clone(),
                    };
                }
            }
            state.root_turns.push(RootTurnEvidence {
                native_id,
                start_event: event.id.clone(),
                phase: RootTurnPhase::Running,
            });
        }
        if root && event.event_type == "assistant.turn_end" {
            state
                .root_turns
                .last_mut()
                .expect("root finish has a validated live start")
                .phase = RootTurnPhase::Finished {
                event_id: event.id.clone(),
            };
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
    input_turns: HashMap<String, ConsumedInput>,
    root_turns: Vec<RootTurnEvidence>,
    terminal_event: Option<IdleEvidence>,
    observed_idles: HashMap<String, IdleOutcome>,
    live_events: HashSet<String>,
    child_events: HashSet<String>,
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
            return Err(forge_provider_api::ProviderError::DeliveryUncertain.into());
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

/// Resolve one concrete root execution from native turn identity and event ordering.
fn input_root<'a>(
    turns: &'a [RootTurnEvidence],
    input: &ConsumedInput,
) -> Option<&'a RootTurnEvidence> {
    let root = consumed_root(turns, input)?;
    match &input.placement {
        InputPlacement::RunningTurn { .. } => Some(root),
        InputPlacement::FollowingStart { .. } => Some(root),
        InputPlacement::AwaitingStart => None,
    }
}

/// Get the exact consumed instance without treating missing ancestry as unfinished native work.
fn consumed_root<'a>(
    turns: &'a [RootTurnEvidence],
    input: &ConsumedInput,
) -> Option<&'a RootTurnEvidence> {
    let start_event = match &input.placement {
        InputPlacement::RunningTurn { start_event }
        | InputPlacement::FollowingStart { start_event } => start_event,
        InputPlacement::AwaitingStart => return None,
    };
    turns
        .iter()
        .find(|root| root.native_id == input.native_turn && &root.start_event == start_event)
}

/// Follow only known durable ancestry; missing links never prove input-to-start correlation.
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

    /// Create metadata-only native evidence without copying session prompts or tool payloads.
    fn live_event(
        id: &str,
        parent: Option<&str>,
        kind: &str,
        data: serde_json::Value,
    ) -> Result<SessionEvent> {
        Ok(serde_json::from_value(serde_json::json!({
            "id":id, "parentId":parent, "type":kind, "timestamp":"2026-11-02T17:33:08Z", "data":data
        }))?)
    }

    /// Observe an actual root idle in admission timing fixtures.
    fn native_idle(turn: &CopilotTurnEvent, id: &str) -> Result<()> {
        let event = live_event(id, None, "session.idle", serde_json::json!({}))?;
        turn.observe(&event)?;
        turn.observe_idle_at(&event, IdleOutcome::Completed)
    }

    /// Eight ordered native loops remain provable when public APIs omit internal parent nodes.
    #[tokio::test]
    async fn completed_idle_certifies_eight_live_turns_without_internal_parent_links() -> Result<()>
    {
        for acknowledge_first in [false, true] {
            let turn = CopilotTurnEvent::default();
            let guard = turn.activate(None, None)?;
            let submission =
                forge_provider_api::lifecycle::SubmissionId::try_from("input".to_owned())?;
            let (ack, response) = tokio::sync::oneshot::channel();
            let send = guard.send("before-input".into(), Some(submission.clone()), async {
                response.await?;
                Ok("native-receipt".into())
            });
            tokio::pin!(send);
            assert!(futures_util::poll!(send.as_mut()).is_pending());
            let mut ack = Some(ack);
            if acknowledge_first {
                ack.take().unwrap().send(()).unwrap();
                send.as_mut().await?;
            }
            turn.observe(&live_event(
                "user",
                None,
                "user.message",
                serde_json::json!({"messageId":"native-receipt","turnId":"0"}),
            )?)?;
            turn.observe(&live_event(
                "system",
                Some("user"),
                "system.message",
                serde_json::json!({}),
            )?)?;
            let mut previous = "system".to_owned();
            for index in 0..8 {
                let start = format!("start-{index}");
                let end = format!("end-{index}");
                let native = index.to_string();
                turn.observe(&live_event(
                    &start,
                    Some(&previous),
                    "assistant.turn_start",
                    serde_json::json!({"turnId":native,"interactionId":"native-interaction"}),
                )?)?;
                turn.observe(&live_event(
                    &end,
                    Some(&format!("hidden-usage-or-skill-{index}")),
                    "assistant.turn_end",
                    serde_json::json!({"turnId":native}),
                )?)?;
                previous = end;
            }
            let idle = live_event(
                "idle",
                Some("hidden-usage-checkpoint"),
                "session.idle",
                serde_json::json!({"mode":"interactive"}),
            )?;
            turn.observe(&idle)?;
            turn.observe_idle_at(&idle, IdleOutcome::Completed)?;
            if let Some(ack) = ack.take() {
                assert!(turn.covered_submissions().is_err());
                ack.send(()).unwrap();
                send.await?;
            }
            turn.idle_settled().await?;
            let executions = turn.terminal_executions()?;
            assert_eq!(executions.len(), 1);
            assert_eq!(executions[0].0, "start-0");
            assert_eq!(executions[0].1, vec![submission]);
        }
        Ok(())
    }

    /// Exact consumed-turn metadata can place two accepted steering messages in one running loop.
    #[tokio::test]
    async fn steering_consumed_by_running_turn_shares_its_exact_start() -> Result<()> {
        for acknowledge_first in [false, true] {
            let turn = CopilotTurnEvent::default();
            let guard = turn.activate(None, None)?;
            let initial =
                forge_provider_api::lifecycle::SubmissionId::try_from("initial".to_owned())?;
            guard
                .send("cursor".into(), Some(initial.clone()), async {
                    Ok("initial-receipt".into())
                })
                .await?;
            turn.observe(&live_event(
                "user",
                None,
                "user.message",
                serde_json::json!({"messageId":"initial-receipt","turnId":"0"}),
            )?)?;
            turn.observe(&live_event(
                "start",
                Some("user"),
                "assistant.turn_start",
                serde_json::json!({"turnId":"0"}),
            )?)?;
            let first =
                forge_provider_api::lifecycle::SubmissionId::try_from("steer-first".to_owned())?;
            let second =
                forge_provider_api::lifecycle::SubmissionId::try_from("steer-second".to_owned())?;
            turn.steer(Some(first.clone()), async {
                Ok("steer-first-receipt".into())
            })
            .await?;
            let (ack, response) = tokio::sync::oneshot::channel();
            let send = turn.steer(Some(second.clone()), async {
                response.await?;
                Ok("steer-second-receipt".into())
            });
            tokio::pin!(send);
            assert!(futures_util::poll!(send.as_mut()).is_pending());
            let mut ack = Some(ack);
            if acknowledge_first {
                ack.take().unwrap().send(()).unwrap();
                send.as_mut().await?;
            }
            for (event_id, receipt) in [
                ("steer-user-first", "steer-first-receipt"),
                ("steer-user-second", "steer-second-receipt"),
            ] {
                turn.observe(&live_event(
                    event_id,
                    Some("hidden-current-loop"),
                    "user.message",
                    serde_json::json!({"messageId":receipt,"turnId":"0","delivery":"immediate"}),
                )?)?;
            }
            turn.observe(&live_event(
                "end",
                Some("hidden-current-loop"),
                "assistant.turn_end",
                serde_json::json!({"turnId":"0"}),
            )?)?;
            let idle = live_event(
                "idle",
                Some("hidden-usage"),
                "session.idle",
                serde_json::json!({}),
            )?;
            turn.observe(&idle)?;
            turn.observe_idle_at(&idle, IdleOutcome::Completed)?;
            if let Some(ack) = ack.take() {
                assert!(turn.covered_submissions().is_err());
                ack.send(()).unwrap();
                send.await?;
            }
            let executions = turn.terminal_executions()?;
            assert_eq!(executions.len(), 1);
            assert_eq!(executions[0].0, "start");
            let covered = &executions[0].1;
            assert!(
                covered.len() == 3
                    && [initial, first, second]
                        .iter()
                        .all(|id| covered.contains(id))
            );
        }
        Ok(())
    }

    /// An idle cannot lend an old root instance to a later input reusing the loop ID.
    #[tokio::test]
    async fn idle_snapshot_rejects_reused_turn_identity() -> Result<()> {
        let turn = CopilotTurnEvent::default();
        let guard = turn.activate(None, None)?;
        let first = forge_provider_api::lifecycle::SubmissionId::try_from("first".to_owned())?;
        let second = forge_provider_api::lifecycle::SubmissionId::try_from("second".to_owned())?;
        guard
            .send("cursor".into(), Some(first), async {
                Ok("first-receipt".into())
            })
            .await?;
        turn.steer(Some(second), async { Ok("second-receipt".into()) })
            .await?;
        turn.observe(&live_event(
            "user-first",
            None,
            "user.message",
            serde_json::json!({"messageId":"first-receipt","turnId":"0"}),
        )?)?;
        turn.observe(&live_event(
            "start-first",
            Some("user-first"),
            "assistant.turn_start",
            serde_json::json!({"turnId":"0"}),
        )?)?;
        let end = live_event(
            "end-first",
            Some("hidden"),
            "assistant.turn_end",
            serde_json::json!({"turnId":"0"}),
        )?;
        turn.observe(&end)?;
        turn.observe(&live_event(
            "user-second",
            Some("end-first"),
            "user.message",
            serde_json::json!({"messageId":"second-receipt","turnId":"0"}),
        )?)?;
        let old_idle = live_event(
            "old-idle",
            Some("user-second"),
            "session.idle",
            serde_json::json!({}),
        )?;
        turn.observe(&old_idle)?;
        turn.observe_idle_at(&old_idle, IdleOutcome::Completed)?;
        let settled = turn.idle_settled();
        tokio::pin!(settled);
        assert!(futures_util::poll!(settled.as_mut()).is_pending());
        assert!(
            turn.covered_submissions().is_err(),
            "reused loop ID borrowed a completed instance"
        );
        turn.observe(&live_event(
            "start-second",
            Some("user-second"),
            "assistant.turn_start",
            serde_json::json!({"turnId":"0"}),
        )?)?;
        turn.observe(&live_event(
            "end-second",
            Some("hidden-second"),
            "assistant.turn_end",
            serde_json::json!({"turnId":"0"}),
        )?)?;
        turn.observe_idle_at(&old_idle, IdleOutcome::Completed)?;
        assert!(futures_util::poll!(settled.as_mut()).is_pending());
        assert!(
            turn.covered_submissions().is_err(),
            "old idle gained a new instance"
        );
        let final_idle = live_event(
            "final-idle",
            Some("hidden-final"),
            "session.idle",
            serde_json::json!({}),
        )?;
        turn.observe(&final_idle)?;
        turn.observe_idle_at(&final_idle, IdleOutcome::Completed)?;
        settled.await?;
        let executions = turn.terminal_executions()?;
        assert_eq!(executions.len(), 2);
        assert_eq!(executions[0].0, "start-first");
        assert_eq!(executions[1].0, "start-second");
        Ok(())
    }

    /// Late or replayed finishes cannot revise an already-observed idle snapshot.
    #[tokio::test]
    async fn idle_snapshot_does_not_gain_late_live_or_historical_finishes() -> Result<()> {
        let turn = CopilotTurnEvent::default();
        let guard = turn.activate(None, None)?;
        let submission = forge_provider_api::lifecycle::SubmissionId::try_from("input".to_owned())?;
        let (ack, response) = tokio::sync::oneshot::channel();
        let send = guard.send("cursor".into(), Some(submission), async {
            response.await?;
            Ok("receipt".into())
        });
        tokio::pin!(send);
        assert!(futures_util::poll!(send.as_mut()).is_pending());
        turn.observe(&live_event(
            "user",
            None,
            "user.message",
            serde_json::json!({"messageId":"receipt","turnId":"0"}),
        )?)?;
        turn.observe(&live_event(
            "start",
            Some("user"),
            "assistant.turn_start",
            serde_json::json!({"turnId":"0"}),
        )?)?;
        let idle = live_event(
            "idle",
            Some("hidden"),
            "session.idle",
            serde_json::json!({}),
        )?;
        turn.observe(&idle)?;
        turn.observe_idle_at(&idle, IdleOutcome::Completed)?;
        let settled = turn.idle_settled();
        tokio::pin!(settled);
        assert!(futures_util::poll!(settled.as_mut()).is_pending());
        ack.send(()).unwrap();
        send.await?;
        assert!(
            futures_util::poll!(settled.as_mut()).is_pending(),
            "acceptance upgraded an unfinished idle"
        );
        assert!(turn.covered_submissions().is_err());
        let end = live_event(
            "end",
            Some("start"),
            "assistant.turn_end",
            serde_json::json!({"turnId":"0"}),
        )?;
        turn.observe(&end)?;
        assert!(
            futures_util::poll!(settled.as_mut()).is_pending(),
            "late end upgraded the old idle"
        );
        turn.observe_idle_at(&idle, IdleOutcome::Completed)?;
        assert!(futures_util::poll!(settled.as_mut()).is_pending());
        assert!(
            turn.covered_submissions().is_err(),
            "duplicate idle gained a later live finish"
        );
        let next = live_event(
            "next-idle",
            Some("end"),
            "session.idle",
            serde_json::json!({}),
        )?;
        turn.observe(&next)?;
        turn.observe_idle_at(&next, IdleOutcome::Completed)?;
        settled.await?;
        assert_eq!(turn.covered_submissions()?.len(), 1);
        Ok(())
    }

    /// Root completion must match its one live start, while identical replays stay idempotent.
    #[test]
    fn root_lifecycle_rejects_unmatched_overlap_and_conflicting_replay() -> Result<()> {
        let turn = CopilotTurnEvent::default();
        let _guard = turn.activate(None, None)?;
        let start = live_event(
            "start",
            None,
            "assistant.turn_start",
            serde_json::json!({"turnId":"0"}),
        )?;
        let end = live_event(
            "end",
            Some("hidden"),
            "assistant.turn_end",
            serde_json::json!({"turnId":"0"}),
        )?;
        assert!(turn.observe(&end).is_err());
        turn.observe(&start)?;
        turn.observe(&start)?;
        assert!(
            turn.observe(&live_event(
                "overlap",
                None,
                "assistant.turn_start",
                serde_json::json!({"turnId":"1"})
            )?)
            .is_err()
        );
        assert!(
            turn.observe(&live_event(
                "wrong-end",
                None,
                "assistant.turn_end",
                serde_json::json!({"turnId":"1"})
            )?)
            .is_err()
        );
        assert!(
            turn.observe(&live_event(
                "start",
                None,
                "assistant.turn_start",
                serde_json::json!({"turnId":"1"})
            )?)
            .is_err()
        );
        turn.observe(&end)?;
        turn.observe(&end)?;
        assert!(
            turn.observe(&live_event(
                "later-end",
                None,
                "assistant.turn_end",
                serde_json::json!({"turnId":"0"})
            )?)
            .is_err()
        );
        Ok(())
    }

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
        turn.observe(&serde_json::from_value(serde_json::json!({
            "id":"old-end", "parentId":"old-event", "type":"assistant.turn_end",
            "timestamp":"2026-11-01T00:00:00Z", "data":{"turnId":"old-turn"}
        }))?)?;
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

    /// Certify a correlated root lifecycle without filtered durable ancestry records.
    #[tokio::test]
    async fn root_receipt_completes_without_filtered_ancestry_in_either_ack_order() -> Result<()> {
        for acknowledge_first in [false, true] {
            let turn = CopilotTurnEvent::default();
            let guard = turn.activate(None, None)?;
            let submission =
                forge_provider_api::lifecycle::SubmissionId::try_from("root-input".to_owned())?;
            let (ack, response) = tokio::sync::oneshot::channel();
            let send = guard.send("before-input".into(), Some(submission.clone()), async {
                response.await?;
                Ok("receipt".into())
            });
            tokio::pin!(send);
            assert!(futures_util::poll!(send.as_mut()).is_pending());
            let mut ack = Some(ack);
            if acknowledge_first {
                ack.take().unwrap().send(()).unwrap();
                send.as_mut().await?;
            }
            let event = |id: &str,
                         parent: Option<&str>,
                         kind: &str,
                         data|
             -> Result<SessionEvent> {
                Ok(serde_json::from_value(serde_json::json!({
                    "id":id,"parentId":parent,"type":kind,"timestamp":"2026-11-02T17:01:44Z","data":data
                }))?)
            };
            // An unrelated child or root message cannot account for the acknowledged receipt.
            let mut child = event(
                "child",
                None,
                "user.message",
                serde_json::json!({"messageId":"receipt","turnId":"0"}),
            )?;
            child.agent_id = Some("foreign-child".into());
            turn.observe(&child)?;
            let foreign = event(
                "foreign",
                None,
                "user.message",
                serde_json::json!({
                    "messageId":"foreign-receipt","turnId":"0","parentAgentTaskId":"parent-task"
                }),
            )?;
            turn.observe(&foreign)?;
            native_idle(&turn, "prior-idle")?;
            {
                let pending = std::pin::pin!(turn.idle_settled());
                assert!(futures_util::poll!(pending).is_pending());
            }
            let user = event(
                "user",
                None,
                "user.message",
                serde_json::json!({
                    "messageId":"receipt","turnId":"0","parentAgentTaskId":"parent-task"
                }),
            )?;
            let start = event(
                "start",
                Some("filtered-system"),
                "assistant.turn_start",
                serde_json::json!({"turnId":"0"}),
            )?;
            let finish = event(
                "finish",
                Some("filtered-skill"),
                "assistant.turn_end",
                serde_json::json!({"turnId":"0"}),
            )?;
            let idle = event(
                "idle",
                Some("filtered-usage"),
                "session.idle",
                serde_json::json!({"mode":"interactive"}),
            )?;
            for observed in [&user, &start, &finish, &idle] {
                turn.observe(observed)?;
            }
            turn.observe_idle_at(&idle, IdleOutcome::Completed)?;
            if let Some(ack) = ack.take() {
                {
                    let pending = std::pin::pin!(turn.idle_settled());
                    assert!(futures_util::poll!(pending).is_pending());
                }
                ack.send(()).unwrap();
                send.await?;
            }
            turn.idle_settled().await?;
            let executions = turn.terminal_executions()?;
            assert_eq!(executions.len(), 1);
            assert_eq!(executions[0].0, "start");
            assert_eq!(executions[0].1, vec![submission]);
            assert_eq!(
                executions[0].2,
                forge_provider_api::lifecycle::ExecutionResult::Completed {
                    completion: forge_provider_api::completion::ProviderCompletion::default()
                        .into(),
                }
            );
        }
        Ok(())
    }

    /// Keep repeated native turn numbers attached to their accepted input occurrence.
    #[tokio::test]
    async fn reused_native_turn_number_correlates_by_live_execution_occurrence() -> Result<()> {
        let turn = CopilotTurnEvent::default();
        let guard = turn.activate(None, None)?;
        let first = forge_provider_api::lifecycle::SubmissionId::try_from("first".to_owned())?;
        let second = forge_provider_api::lifecycle::SubmissionId::try_from("second".to_owned())?;
        guard
            .send("before-first".into(), Some(first.clone()), async {
                Ok("receipt-first".into())
            })
            .await?;
        let event = |id: &str, kind: &str, data| -> Result<SessionEvent> {
            Ok(serde_json::from_value(serde_json::json!({
                "id":id,"type":kind,"timestamp":"2026-11-02T17:01:44Z","data":data
            }))?)
        };
        for observed in [
            event(
                "user-first",
                "user.message",
                serde_json::json!({"messageId":"receipt-first","turnId":"0","parentAgentTaskId":"root-task"}),
            )?,
            event(
                "start-first",
                "assistant.turn_start",
                serde_json::json!({"turnId":"0"}),
            )?,
            event(
                "end-first",
                "assistant.turn_end",
                serde_json::json!({"turnId":"0"}),
            )?,
        ] {
            turn.observe(&observed)?;
        }
        turn.steer(Some(second.clone()), async { Ok("receipt-second".into()) })
            .await?;
        for observed in [
            event(
                "user-second",
                "user.message",
                serde_json::json!({"messageId":"receipt-second","turnId":"0","parentAgentTaskId":"root-task"}),
            )?,
            event(
                "start-second",
                "assistant.turn_start",
                serde_json::json!({"turnId":"0"}),
            )?,
            event(
                "end-second",
                "assistant.turn_end",
                serde_json::json!({"turnId":"0"}),
            )?,
            event("idle", "session.idle", serde_json::json!({}))?,
        ] {
            turn.observe(&observed)?;
            if observed.event_type == "session.idle" {
                turn.observe_idle_at(&observed, IdleOutcome::Completed)?;
            }
        }
        turn.idle_settled().await?;
        let executions = turn.terminal_executions()?;
        assert_eq!(executions.len(), 2);
        assert_eq!(executions[0].0, "start-first");
        assert_eq!(executions[0].1, vec![first]);
        assert_eq!(executions[1].0, "start-second");
        assert_eq!(executions[1].1, vec![second]);
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
            Some(forge_provider_api::ProviderError::DeliveryUncertain)
        ));
        assert!(turn.idle_settled().await.is_err());
        Ok(())
    }

    /// Certify a long execution without rejecting its complete native ancestry.
    #[tokio::test]
    async fn terminal_outcome_preserves_long_event_history() -> Result<()> {
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
        assert_eq!(turn.covered_submissions()?, vec![submission]);
        Ok(())
    }

    /// Preserve host-to-native identity and require each completed turn in the idle chain.
    #[tokio::test]
    async fn terminal_outcome_covers_only_inputs_with_completed_turns() -> Result<()> {
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
        let covered = turn.covered_submissions()?;
        assert_eq!(covered.len(), 2);
        assert!(covered.contains(&first) && covered.contains(&second));
        let executions = turn.terminal_executions()?;
        assert_eq!(executions.len(), 2);
        assert_eq!(executions[0].0, "start-a");
        assert_eq!(executions[0].1, vec![first]);
        assert_eq!(executions[1].0, "start-b");
        assert_eq!(executions[1].1, vec![second]);
        Ok(())
    }

    /// Native abort cannot overtake an already-admitted steering write.
    #[tokio::test]
    async fn cancellation_fences_new_sends_and_waits_for_admitted_steering() -> Result<()> {
        let turn = CopilotTurnEvent::default();
        let _guard = turn.activate(None, None)?;
        let (acknowledge, acknowledged) = tokio::sync::oneshot::channel();
        let steering = turn.steer(None, async {
            acknowledged.await?;
            Ok("receipt".into())
        });
        tokio::pin!(steering);
        assert!(futures_util::poll!(&mut steering).is_pending());
        turn.request_stop()?;
        let fenced = turn.finish_pending_sends();
        tokio::pin!(fenced);
        assert!(futures_util::poll!(&mut fenced).is_pending());
        assert!(
            turn.steer(None, async {
                panic!("closed input reached native dispatch")
            })
            .await
            .is_err()
        );
        acknowledge.send(()).unwrap();
        steering.await?;
        fenced.await?;
        Ok(())
    }

    /// A dropped write must retain uncertain delivery rather than certify a clean abort fence.
    #[tokio::test]
    async fn cancellation_fence_preserves_uncertain_send_outcome() -> Result<()> {
        let turn = CopilotTurnEvent::default();
        let _guard = turn.activate(None, None)?;
        let mut steering = Box::pin(turn.steer(None, std::future::pending()));
        assert!(futures_util::poll!(&mut steering).is_pending());
        turn.request_stop()?;
        drop(steering);
        assert!(matches!(
            turn.finish_pending_sends().await,
            Err(forge_provider_api::ProviderError::DeliveryUncertain)
        ));
        Ok(())
    }

    /// Host-requested interruption can finish a structured handoff without a native turn-end.
    #[tokio::test]
    async fn host_handoff_idle_completes_only_its_correlated_execution() -> Result<()> {
        let turn = CopilotTurnEvent::default();
        let guard = turn.activate(None, None)?;
        let submission = forge_provider_api::SubmissionId::try_from("handoff".to_owned())?;
        guard
            .send("cursor".into(), Some(submission.clone()), async {
                Ok("receipt".into())
            })
            .await?;
        turn.observe(&live_event(
            "user",
            None,
            "user.message",
            serde_json::json!({"messageId":"receipt","turnId":"0"}),
        )?)?;
        turn.observe(&live_event(
            "start",
            Some("user"),
            "assistant.turn_start",
            serde_json::json!({"turnId":"0"}),
        )?)?;
        let idle = live_event(
            "idle",
            Some("start"),
            "session.idle",
            serde_json::json!({"aborted":true}),
        )?;
        turn.observe(&idle)?;
        turn.observe_idle_at(&idle, IdleOutcome::HostHandoff)?;
        turn.idle_settled().await?;
        let executions = turn.terminal_executions()?;
        assert_eq!(executions.len(), 1);
        assert_eq!(executions[0].0, "start");
        assert_eq!(executions[0].1, vec![submission]);
        assert!(matches!(
            executions[0].2,
            forge_provider_api::lifecycle::ExecutionResult::Completed { .. }
        ));
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
        assert!(turn.covered_submissions().is_err());
        ack.send(()).unwrap();
        send.await?;
        assert!(turn.covered_submissions().is_err());
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
        native_idle(&turn, "prior-idle")?;
        acknowledge.send(()).unwrap();
        send.await?;
        let message: SessionEvent = serde_json::from_value(serde_json::json!({
            "id":"event", "type":"user.message", "timestamp":"2026-11-01T00:00:00Z",
            "data":{"messageId":"input","turnId":"0","content":"follow up"}
        }))?;
        turn.observe(&message)?;
        let settled = turn.idle_settled();
        tokio::pin!(settled);
        assert!(futures_util::poll!(settled.as_mut()).is_pending());
        turn.observe(&live_event(
            "start",
            Some("event"),
            "assistant.turn_start",
            serde_json::json!({"turnId":"0"}),
        )?)?;
        turn.observe(&live_event(
            "end",
            Some("start"),
            "assistant.turn_end",
            serde_json::json!({"turnId":"0"}),
        )?)?;
        native_idle(&turn, "next-idle")?;
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
            "data":{"messageId":"input","turnId":"0","content":"follow up"}
        }))?)?;
        turn.observe(&live_event(
            "start",
            Some("event"),
            "assistant.turn_start",
            serde_json::json!({"turnId":"0"}),
        )?)?;
        turn.observe(&live_event(
            "end",
            Some("start"),
            "assistant.turn_end",
            serde_json::json!({"turnId":"0"}),
        )?)?;
        native_idle(&turn, "before-ack-idle")?;
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
        let (sink, _events) = forge_provider_api::events::channel(
            forge_provider_api::BindingId::new("turn-idle").unwrap(),
            scope.clone(),
        );
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
        let (sink, _events) = forge_provider_api::events::channel(
            forge_provider_api::BindingId::new("turn-old").unwrap(),
            old,
        );
        drop(turn.activate(Some(sink), None)?);
        turn.request_stop()?;
        let new = InvocationScope::new(owner, "new");
        let (sink, _events) = forge_provider_api::events::channel(
            forge_provider_api::BindingId::new("turn-new").unwrap(),
            new.clone(),
        );
        let guard = turn.activate(Some(sink), None)?;
        assert_eq!(turn.event_sink().unwrap().invocation_scope()?, &new);
        turn.steer(None, async { Ok("accepted".into()) }).await?;
        drop(guard);
        Ok(())
    }
}
