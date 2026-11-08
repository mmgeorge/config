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

/// Retain the positively matched input and pre-dispatch history cursor for one rollback.
pub struct CopilotRetraction {
    pub cursor: String,
    pub event_id: String,
    pub message_id: String,
    pub boundary: String,
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
    completed_at_ms: i64,
    outcome: IdleOutcome,
    root_turns: Vec<RootTurnEvidence>,
}

/// Coordinates prompt admission, interruption, and rewind identity for one Copilot runtime.
#[derive(Default)]
pub struct CopilotTurnEvent {
    state: Mutex<CopilotTurnState>,
    sends_changed: Notify,
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

    /// Exercise send accounting without a transport in deterministic native lifecycle fixtures.
    #[cfg(test)]
    pub async fn steer(
        &self,
        submission: Option<forge_provider_api::SubmissionId>,
        send: impl std::future::Future<Output = Result<String>>,
    ) -> Result<()> {
        self.steer_native(None, submission, |_| send).await
    }

    /// Retain steering and its exact synchronous transport-handoff authority through acknowledgement.
    pub async fn steer_native<'a, F, Fut>(
        &'a self,
        expected_scope: Option<&forge_provider_api::InvocationScope>,
        submission: Option<forge_provider_api::SubmissionId>,
        send: F,
    ) -> Result<()>
    where
        F: FnOnce(NativeSendAuthority<'a>) -> Fut,
        Fut: std::future::Future<Output = Result<String>>,
    {
        let pending = self.admit_send(submission, expected_scope)?;
        match send(NativeSendAuthority(self)).await {
            Ok(message_id) => pending.complete(Some(message_id)),
            Err(error) => {
                if matches!(
                    error.downcast_ref::<forge_provider_api::ProviderError>(),
                    Some(forge_provider_api::ProviderError::NotConsumed)
                ) || super::operation::current().is_some_and(|operation| !operation.dispatched())
                {
                    pending.reject();
                } else {
                    pending.failed();
                }
                Err(error)
            }
        }
    }

    /// Snapshot a fixture's native idle at its immediate observation boundary.
    #[cfg(test)]
    pub(crate) fn observe_idle_at(&self, event: &SessionEvent, outcome: IdleOutcome) -> Result<()> {
        self.observe_idle_timestamp(event, outcome, super::decode::timestamp_ms())
    }

    /// Freeze terminal idle and its first SDK receipt time before any acknowledgement wait.
    pub(crate) fn observe_idle_timestamp(
        &self,
        event: &SessionEvent,
        outcome: IdleOutcome,
        completed_at_ms: i64,
    ) -> Result<()> {
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
            completed_at_ms,
            outcome,
            root_turns: state.root_turns.clone(),
        });
        drop(state);
        self.sends_changed.notify_waiters();
        Ok(())
    }

    /// Retain a host-checked input attempt before its native send can be admitted.
    pub(crate) fn register_input(
        &self,
        scope: &forge_provider_api::InvocationScope,
        input: forge_provider_api::CoveredInput,
    ) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        let TurnDelivery::Active(active) = &state.delivery else {
            anyhow::bail!("Copilot input has no active turn");
        };
        anyhow::ensure!(
            active.admission == SendAdmission::Open
                && active
                    .event_sink
                    .as_ref()
                    .is_some_and(|sink| sink.scope().invocation() == Some(scope)),
            "Copilot input has a different or closed invocation"
        );
        if let Some(attempt) = state.input_attempts.get(input.submission_id()) {
            anyhow::ensure!(
                attempt == input.attempt_id(),
                "Copilot input attempt conflicts with retained authority"
            );
        } else {
            state
                .input_attempts
                .insert(input.submission_id().clone(), input.attempt_id().clone());
        }
        Ok(())
    }

    /// Bind recovered journal entries to the primary receipt covering their combined native message.
    pub(crate) fn register_combined_inputs(
        &self,
        scope: &forge_provider_api::InvocationScope,
        primary: &forge_provider_api::SubmissionId,
        inputs: Vec<forge_provider_api::CoveredInput>,
    ) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        let TurnDelivery::Active(active) = &state.delivery else {
            anyhow::bail!("combined input has no active owner");
        };
        anyhow::ensure!(
            active.admission == SendAdmission::Open
                && active
                    .event_sink
                    .as_ref()
                    .is_some_and(|sink| sink.scope().invocation() == Some(scope)),
            "combined input belongs to a different or closed invocation"
        );
        let mut observed = HashSet::new();
        for input in &inputs {
            anyhow::ensure!(
                observed.insert(input.submission_id()),
                "combined input repeats a journal entry"
            );
            if let Some(attempt) = state.input_attempts.get(input.submission_id()) {
                anyhow::ensure!(
                    attempt == input.attempt_id(),
                    "combined input changed a retained attempt"
                );
            }
        }
        anyhow::ensure!(
            observed.contains(primary),
            "combined input omitted its primary receipt"
        );
        if let Some(previous) = state.input_groups.get(primary) {
            anyhow::ensure!(
                previous == &inputs,
                "combined input changed its original coverage"
            );
            return Ok(());
        }
        for input in &inputs {
            state
                .input_attempts
                .insert(input.submission_id().clone(), input.attempt_id().clone());
        }
        state.input_groups.insert(primary.clone(), inputs);
        Ok(())
    }

    /// Validate accepted submissions against one correlated native idle boundary.
    #[cfg(test)]
    pub fn covered_submissions(&self) -> Result<Vec<forge_provider_api::SubmissionId>> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        terminal_coverage(&state)
    }

    /// Resolve exact accepted attempts from the original native terminal coverage.
    pub(crate) fn covered_inputs(&self) -> Result<Vec<forge_provider_api::CoveredInput>> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        let mut inputs = Vec::new();
        for submission in terminal_coverage(&state)? {
            if let Some(group) = state.input_groups.get(&submission) {
                inputs.extend(group.iter().cloned());
            } else {
                let attempt = state.input_attempts.get(&submission).ok_or_else(|| {
                    anyhow::anyhow!("Copilot terminal input lacks its registered attempt")
                })?;
                inputs.push(forge_provider_api::CoveredInput::new(
                    submission,
                    attempt.clone(),
                ));
            }
        }
        Ok(inputs)
    }

    /// Group terminal-covered inputs by their exact native start and terminal outcome.
    #[cfg(test)]
    pub fn terminal_native_roots(
        &self,
    ) -> Result<
        Vec<(
            String,
            Vec<forge_provider_api::lifecycle::SubmissionId>,
            forge_provider_api::turn::TurnOutcome,
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
            forge_provider_api::turn::TurnOutcome,
        )> = Vec::new();
        for (message, submission) in &state.accepted_submissions {
            if cancelled_before_start(&state, idle, message) {
                continue;
            }
            let input = state
                .input_turns
                .get(message)
                .ok_or_else(|| anyhow::anyhow!("Copilot input lacks consumed turn identity"))?;
            let root = input_root(&idle.root_turns, input).ok_or_else(|| {
                anyhow::anyhow!("Copilot input has no correlated foreground start")
            })?;
            let terminal = match (&root.phase, idle.outcome) {
                (RootTurnPhase::Finished { .. }, IdleOutcome::Completed) => {
                    forge_provider_api::turn::TurnOutcome::Completed {
                        stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                    }
                }
                (
                    RootTurnPhase::Running | RootTurnPhase::Finished { .. },
                    IdleOutcome::Aborted | IdleOutcome::HostHandoff,
                ) => forge_provider_api::turn::TurnOutcome::Interrupted,
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
                                    || cancelled_before_start(&state, idle, message)
                            })
                    })
                {
                    return Ok(());
                }
            }
            changed.await;
        }
    }

    /// Report whether every accepted input is still queued natively, with no root turn started.
    ///
    /// Copilot queues a sent message until the session can process it, for example while a
    /// foreground MCP load is in flight. An abort in that window discards the queue and emits only
    /// `abort`: no `user.message` and no idle follow, so the turn must settle on the abort itself.
    pub fn accepted_inputs_unconsumed(&self) -> Result<bool> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        Ok(state.pending_sends == 0
            && !state.accepted_inputs.is_empty()
            && state.root_turns.is_empty()
            && state
                .accepted_inputs
                .iter()
                .all(|message| !state.observed_inputs.contains(message)))
    }

    /// Resolve the host submission whose accepted native message this user event consumes.
    pub fn accepted_submission(
        &self,
        message_id: &str,
    ) -> Result<Option<forge_provider_api::lifecycle::SubmissionId>> {
        Ok(self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?
            .accepted_submissions
            .get(message_id)
            .cloned())
    }

    /// Get the positively correlated terminal idle after its delivery coverage settles.
    pub fn terminal_outcome(&self) -> Result<IdleOutcome> {
        self.state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?
            .terminal_event
            .as_ref()
            .map(|idle| idle.outcome)
            .ok_or_else(|| anyhow::anyhow!("Copilot terminal outcome is absent"))
    }

    /// Get the first native idle timestamp without charging acknowledgement or cleanup waits.
    pub(crate) fn completed_at_ms(&self) -> Result<i64> {
        self.state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?
            .terminal_event
            .as_ref()
            .map(|idle| idle.completed_at_ms)
            .ok_or_else(|| anyhow::anyhow!("Copilot terminal timestamp is absent"))
    }

    /// Register dispatch under a short state lock; native responses never hold admission serialization.
    fn admit_send(
        &self,
        submission: Option<forge_provider_api::lifecycle::SubmissionId>,
        expected_scope: Option<&forge_provider_api::InvocationScope>,
    ) -> Result<PendingSend<'_>> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        let delivery = match &state.delivery {
            TurnDelivery::Active(delivery) | TurnDelivery::Retained(delivery) => delivery,
            TurnDelivery::Idle => {
                return Err(forge_provider_api::ProviderError::NotConsumed.into());
            }
        };
        if expected_scope.is_some_and(|expected| {
            delivery
                .event_sink
                .as_ref()
                .and_then(|sink| sink.invocation_scope().ok())
                != Some(expected)
        }) {
            return Err(forge_provider_api::ProviderError::from(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnSteer,
                    forge_provider_api::ProviderFailureKind::WrongInvocation,
                ),
            )
            .into());
        }
        if state.cancelled
            || delivery
                .submission_cancelled
                .as_ref()
                .is_some_and(|flag| flag.load(Ordering::Acquire))
        {
            return Err(forge_provider_api::error::TurnCancelled.into());
        }
        if delivery.admission == SendAdmission::Closed {
            return Err(forge_provider_api::ProviderError::NotConsumed.into());
        }
        state.pending_sends += 1;
        Ok(PendingSend {
            owner: self,
            submission,
            completed: false,
        })
    }

    /// Revoke handoff only if this operation still owns the exact retained native invocation.
    pub fn request_stop_for(&self, scope: &forge_provider_api::InvocationScope) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        let matches = match &state.delivery {
            TurnDelivery::Active(delivery) | TurnDelivery::Retained(delivery) => delivery
                .event_sink
                .as_ref()
                .and_then(|sink| sink.invocation_scope().ok())
                .is_some_and(|active| active == scope),
            TurnDelivery::Idle => false,
        };
        if matches {
            state.cancelled = true;
        }
        Ok(())
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
                            forge_provider_api::ProviderOperation::TurnInterrupt,
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
                state
                    .input_events
                    .insert(message_id.to_owned(), event.id.clone());
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

    /// Match retraction only to the cancelled invocation's sole accepted input and exact boundary.
    pub fn checked_retraction(
        &self,
        submission: &forge_provider_api::SubmissionId,
        boundary: &str,
    ) -> Result<Option<CopilotRetraction>> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn lock poisoned"))?;
        if matches!(state.delivery, TurnDelivery::Active(_))
            || state.pending_sends != 0
            || state.uncertain_send
            || !state.cancelled
        {
            return Ok(None);
        }
        let NativeSubmission::Sent {
            cursor,
            user_event_id: Some(event_id),
            message_id: Some(message_id),
        } = &state.submission
        else {
            return Ok(None);
        };
        let boundary_matches = state.terminal_event.as_ref().is_some_and(|idle| {
            idle.event_id == boundary
                || state
                    .event_parents
                    .get(&idle.event_id)
                    .and_then(Option::as_deref)
                    == Some(boundary)
        });
        if !boundary_matches
            || state.accepted_inputs.len() != 1
            || state.accepted_submissions.len() != 1
            || state.accepted_submissions.get(message_id) != Some(submission)
        {
            return Ok(None);
        }
        Ok(Some(CopilotRetraction {
            cursor: cursor.clone(),
            event_id: event_id.clone(),
            message_id: message_id.clone(),
            boundary: boundary.to_owned(),
        }))
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
    input_attempts:
        HashMap<forge_provider_api::SubmissionId, forge_provider_api::DeliveryAttemptId>,
    input_groups: HashMap<forge_provider_api::SubmissionId, Vec<forge_provider_api::CoveredInput>>,
    input_turns: HashMap<String, ConsumedInput>,
    input_events: HashMap<String, String>,
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
    /// Get the original event ledger whose active ownership this guard retains.
    pub(super) fn events(&self) -> &CopilotTurnEvent {
        self.owner
    }

    /// Exercise initial send accounting with a deterministic transport fixture.
    #[cfg(test)]
    pub async fn send(
        &self,
        cursor: String,
        submission: Option<forge_provider_api::SubmissionId>,
        send: impl std::future::Future<Output = Result<String>>,
    ) -> Result<()> {
        self.send_native(cursor, submission, |_| send).await
    }

    /// Preserve initial send ownership until native acceptance or failure settles.
    pub async fn send_native<'a, F, Fut>(
        &'a self,
        cursor: String,
        submission: Option<forge_provider_api::lifecycle::SubmissionId>,
        send: F,
    ) -> Result<()>
    where
        F: FnOnce(NativeSendAuthority<'a>) -> Fut,
        Fut: std::future::Future<Output = Result<String>>,
    {
        let pending = self.owner.admit_send(submission, None)?;
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
        match send(NativeSendAuthority(self.owner)).await {
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
                if matches!(
                    error.downcast_ref::<forge_provider_api::ProviderError>(),
                    Some(forge_provider_api::ProviderError::NotConsumed)
                ) || super::operation::current().is_some_and(|operation| !operation.dispatched())
                {
                    pending.reject();
                } else {
                    pending.failed();
                }
                Err(error)
            }
        }
    }
}

/// Serialize the writer handoff against exact invocation interruption.
pub struct NativeSendAuthority<'a>(&'a CopilotTurnEvent);

impl NativeSendAuthority<'_> {
    /// Poll native writer admission while stop holds the same state boundary.
    pub fn commit<T>(
        &self,
        handoff: impl FnOnce() -> T,
    ) -> std::result::Result<T, forge_provider_api::ProviderError> {
        let state = self.0.state.lock().map_err(|_| {
            forge_provider_api::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::Runtime,
            ))
        })?;
        let open = match &state.delivery {
            TurnDelivery::Active(delivery) | TurnDelivery::Retained(delivery) => {
                delivery.admission == SendAdmission::Open
            }
            TurnDelivery::Idle => false,
        };
        if state.cancelled || !open {
            return Err(forge_provider_api::ProviderError::NotConsumed);
        }
        Ok(handoff())
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

    /// Release positively rejected dispatch without manufacturing uncertain native delivery.
    fn reject(mut self) {
        let mut state = self
            .owner
            .state
            .lock()
            .expect("Copilot send accounting lock is not poisoned");
        state.pending_sends -= 1;
        self.completed = true;
        drop(state);
        self.owner.sends_changed.notify_waiters();
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

/// Prove cancelled consumption without a start only along the exact input-to-idle ancestry.
fn cancelled_before_start(state: &CopilotTurnState, idle: &IdleEvidence, message: &str) -> bool {
    if idle.outcome != IdleOutcome::Aborted
        || state
            .input_turns
            .get(message)
            .and_then(|input| input_root(&idle.root_turns, input))
            .is_some()
    {
        return false;
    }
    let Some(input_event) = state.input_events.get(message) else {
        return false;
    };
    let mut cursor = Some(idle.event_id.as_str());
    let mut seen = HashSet::new();
    while let Some(event) = cursor {
        if event == input_event {
            return true;
        }
        if !seen.insert(event) {
            return false;
        }
        cursor = state.event_parents.get(event).and_then(Option::as_deref);
    }
    false
}

/// Preserve invocation accounting after its foreground reader has been collected.
impl Drop for CopilotTurnEventGuard<'_> {
    fn drop(&mut self) {
        if let Ok(mut state) = self.owner.state.lock()
            && let TurnDelivery::Active(active) = std::mem::take(&mut state.delivery)
        {
            state.delivery = TurnDelivery::Retained(active);
        }
    }
}

/// Validate native terminal coverage while the caller holds the original turn state.
fn terminal_coverage(state: &CopilotTurnState) -> Result<Vec<forge_provider_api::SubmissionId>> {
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
        let root = state
            .input_turns
            .get(message)
            .and_then(|input| input_root(&idle.root_turns, input));
        let proven = covered.contains(message)
            && (root.is_some_and(|root| match idle.outcome {
                IdleOutcome::Completed => matches!(root.phase, RootTurnPhase::Finished { .. }),
                IdleOutcome::Aborted | IdleOutcome::HostHandoff => true,
            }) || cancelled_before_start(state, idle, message));
        anyhow::ensure!(
            proven,
            "Copilot idle {} does not cover the accepted input's completed turn",
            idle.event_id
        );
        result.push(submission.clone());
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

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
        let executions = turn.terminal_native_roots()?;
        assert_eq!(executions.len(), 2);
        assert_eq!(executions[0].0, "start-a");
        assert_eq!(executions[0].1, vec![first]);
        assert_eq!(executions[1].0, "start-b");
        assert_eq!(executions[1].1, vec![second]);
        Ok(())
    }

    /// Exact checked retraction cannot substitute a different input or stale terminal boundary.
    #[tokio::test]
    async fn retraction_requires_sole_cancelled_submission_and_current_boundary() -> Result<()> {
        let turn = CopilotTurnEvent::default();
        let guard = turn.activate(None, None)?;
        let submission = forge_provider_api::SubmissionId::try_from("cancelled")?;
        guard
            .send("cursor".into(), Some(submission.clone()), async {
                Ok("message".into())
            })
            .await?;
        turn.observe(&live_event(
            "user",
            None,
            "user.message",
            serde_json::json!({"messageId":"message", "turnId":"0"}),
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
        turn.observe_idle_at(&idle, IdleOutcome::Aborted)?;
        turn.request_stop()?;
        drop(guard);
        assert!(turn.checked_retraction(&submission, "idle")?.is_some());
        assert!(turn.checked_retraction(&submission, "stale")?.is_none());
        assert!(
            turn.checked_retraction(
                &forge_provider_api::SubmissionId::try_from("another")?,
                "idle"
            )?
            .is_none()
        );
        let successor = turn.activate(None, None)?;
        successor
            .send(
                "later-cursor".into(),
                Some(forge_provider_api::SubmissionId::try_from("later")?),
                async { Ok("later-message".into()) },
            )
            .await?;
        drop(successor);
        assert!(turn.checked_retraction(&submission, "idle")?.is_none());
        Ok(())
    }
}
