use super::CodexBackend;
use super::json_rpc::{CodexJsonRpc, agent_lifecycle_list};
use crate::backend::steering::{ActiveSteering, ActiveTurnOperation, SteerCommand};
use crate::backend::{BackendEvent, BackendEventSink, BackendOutput, BackendRequest};
use anyhow::{Context, Result};
use serde_json::{Value, json};
use std::collections::{HashMap, HashSet};

const MAX_FINALIZATION_ATTEMPT: usize = 3;

#[derive(Default)]
struct DescendantTracker {
    active_thread: HashSet<String>,
}

impl DescendantTracker {
    fn observe(&mut self, message: &Value, parent_thread_id: &str) {
        for lifecycle in agent_lifecycle_list(message) {
            let Some(thread_id) = lifecycle.provider_thread_id else {
                continue;
            };
            if thread_id == parent_thread_id {
                continue;
            }
            if lifecycle.status.is_active() {
                self.active_thread.insert(thread_id);
            } else {
                self.active_thread.remove(&thread_id);
            }
        }
        if message.get("method").and_then(Value::as_str) == Some("turn/completed") {
            let thread_id = message
                .pointer("/params/threadId")
                .or_else(|| message.pointer("/params/thread_id"))
                .or_else(|| message.pointer("/params/turn/threadId"))
                .or_else(|| message.pointer("/params/turn/thread_id"))
                .and_then(Value::as_str);
            if let Some(thread_id) = thread_id.filter(|thread_id| *thread_id != parent_thread_id) {
                self.active_thread.remove(thread_id);
            }
        }
    }

    fn active_count(&self) -> usize {
        self.active_thread.len()
    }
}

fn message_thread_id(message: &Value) -> Option<&str> {
    message
        .pointer("/params/threadId")
        .or_else(|| message.pointer("/params/thread_id"))
        .and_then(Value::as_str)
}

fn is_parent_wait_message(message: &Value, parent_thread_id: &str) -> bool {
    message_thread_id(message) == Some(parent_thread_id)
        && message
            .pointer("/params/item/tool")
            .and_then(Value::as_str)
            .is_some_and(|tool| tool.eq_ignore_ascii_case("wait"))
}

fn is_parent_activity_message(message: &Value, parent_thread_id: &str) -> bool {
    message_thread_id(message) == Some(parent_thread_id)
        && message
            .get("method")
            .and_then(Value::as_str)
            .is_some_and(|method| method.starts_with("item/"))
}

struct ParentTurn {
    id: String,
    provider_started: bool,
}

/// Coordinates parent turns and descendant lifecycles over one Codex app-server process.
pub(super) struct CodexTurnCoordinator<'a> {
    backend: &'a CodexBackend,
    process: &'a mut CodexJsonRpc,
    output: &'a mut BackendOutput,
    steering: &'a mut ActiveSteering,
    event_sink: Option<BackendEventSink>,
    request: &'a BackendRequest,
    thread_id: &'a str,
    descendant: DescendantTracker,
    waiting: bool,
    finalization_attempt: usize,
}

impl<'a> CodexTurnCoordinator<'a> {
    pub(super) fn new(
        backend: &'a CodexBackend,
        process: &'a mut CodexJsonRpc,
        output: &'a mut BackendOutput,
        steering: &'a mut ActiveSteering,
        event_sink: Option<BackendEventSink>,
        request: &'a BackendRequest,
        thread_id: &'a str,
    ) -> Self {
        Self {
            backend,
            process,
            output,
            steering,
            event_sink,
            request,
            thread_id,
            descendant: DescendantTracker::default(),
            waiting: false,
            finalization_attempt: 0,
        }
    }

    pub(super) async fn run(
        mut self,
        turn_id: String,
        provider_started: bool,
        observed_message_list: Vec<Value>,
    ) -> Result<Value> {
        for message in observed_message_list {
            self.descendant.observe(&message, self.thread_id);
        }
        let mut parent_turn = Some(ParentTurn {
            id: turn_id,
            provider_started,
        });
        let mut pending_command = HashMap::<u64, SteerCommand>::new();
        loop {
            if let Some(active) = parent_turn.as_mut() {
                tokio::select! {
                    Some(command) = self.steering.receive(), if active.provider_started => {
                        self.send_active_command(command, active, &mut pending_command).await;
                    }
                    message = self.process.read_message(self.output) => {
                        let message = message?;
                        self.descendant.observe(&message, self.thread_id);
                        self.complete_pending_command(&message, &mut pending_command)?;
                        if is_parent_wait_message(&message, self.thread_id)
                            && self.descendant.active_count() > 0
                        {
                            if self.waiting { self.refresh_wait(); } else { self.begin_wait(); }
                        } else if self.waiting
                            && (self.descendant.active_count() == 0
                                || is_parent_activity_message(&message, self.thread_id))
                        {
                            self.clear_wait();
                        }
                        if CodexBackend::notification_turn_id(&message, "turn/started") == Some(active.id.as_str()) {
                            active.provider_started = true;
                        }
                        if CodexBackend::notification_matches_turn(
                            &message,
                            "turn/completed",
                            self.thread_id,
                            &active.id,
                        ) {
                            let completion = message.get("params").cloned().unwrap_or(Value::Null);
                            parent_turn = None;
                            if self.descendant.active_count() == 0 {
                                self.clear_wait();
                                return Ok(completion);
                            }
                            if self.waiting {
                                self.refresh_wait();
                            } else {
                                self.begin_wait();
                            }
                        }
                    }
                }
                continue;
            }

            tokio::select! {
                Some(command) = self.steering.receive() => {
                    if command.target.is_some() || command.operation == ActiveTurnOperation::Interrupt {
                        self.send_waiting_target_command(command, &mut pending_command).await;
                    } else {
                        let text = command.text.clone();
                        match self.start_parent_turn(text).await {
                            Ok(turn) => {
                                self.clear_wait();
                                command.complete(Ok(()));
                                parent_turn = Some(turn);
                            }
                            Err(error) => command.complete(Err(error)),
                        }
                    }
                }
                message = self.process.read_message(self.output) => {
                    let message = message?;
                    let had_active = self.descendant.active_count() > 0;
                    self.descendant.observe(&message, self.thread_id);
                    self.complete_pending_command(&message, &mut pending_command)?;
                    if self.descendant.active_count() > 0 {
                        if self.waiting { self.refresh_wait(); } else { self.begin_wait(); }
                        continue;
                    }
                    if !had_active {
                        continue;
                    }
                    self.clear_wait();
                    self.finalization_attempt += 1;
                    anyhow::ensure!(
                        self.finalization_attempt <= MAX_FINALIZATION_ATTEMPT,
                        "Codex child synthesis exceeded {MAX_FINALIZATION_ATTEMPT} attempts"
                    );
                    parent_turn = Some(self.start_parent_turn(
                        "All child agents have finished. Synthesize their results and answer the user. Do not merely report that the children completed.".into()
                    ).await?);
                }
            }
        }
    }

    async fn start_parent_turn(&mut self, text: String) -> Result<ParentTurn> {
        let (turn, provider_started, observed_message_list) = self.backend.start_turn(
            self.process,
            self.output,
            self.request,
            self.thread_id,
            CodexBackend::with_model(CodexBackend::secure(json!({
                "threadId": self.thread_id,
                "input": [{ "type": "text", "text": text }],
                "cwd": self.request.workspace,
                "effort": self.request.effort,
                "serviceTier": if self.request.fast_mode { Value::String("fast".into()) } else { Value::Null }
            }), self.request), &self.request.model),
        ).await?;
        for message in observed_message_list {
            self.descendant.observe(&message, self.thread_id);
        }
        let id = turn
            .pointer("/turn/id")
            .or_else(|| turn.get("turnId"))
            .or_else(|| turn.get("turn_id"))
            .and_then(Value::as_str)
            .context("Codex follow-up turn omitted turn id")?
            .to_owned();
        Ok(ParentTurn {
            id,
            provider_started,
        })
    }

    async fn send_active_command(
        &mut self,
        command: SteerCommand,
        active: &ParentTurn,
        pending_command: &mut HashMap<u64, SteerCommand>,
    ) {
        let thread_id = command
            .target
            .as_ref()
            .map_or(self.thread_id, |target| target.thread_id.as_str())
            .to_owned();
        let turn_id = command
            .target
            .as_ref()
            .map_or(active.id.as_str(), |target| target.turn_id.as_str())
            .to_owned();
        self.send_command(command, &thread_id, &turn_id, pending_command)
            .await;
    }

    async fn send_waiting_target_command(
        &mut self,
        command: SteerCommand,
        pending_command: &mut HashMap<u64, SteerCommand>,
    ) {
        let Some(target) = command.target.clone() else {
            command.complete(Err(anyhow::anyhow!("no active parent turn to interrupt")));
            return;
        };
        self.send_command(command, &target.thread_id, &target.turn_id, pending_command)
            .await;
    }

    async fn send_command(
        &mut self,
        command: SteerCommand,
        thread_id: &str,
        turn_id: &str,
        pending_command: &mut HashMap<u64, SteerCommand>,
    ) {
        let (method, params) = match command.operation {
            ActiveTurnOperation::Steer => (
                "turn/steer",
                json!({
                    "threadId": thread_id,
                    "input": [{ "type": "text", "text": command.text.clone() }],
                    "expectedTurnId": turn_id,
                }),
            ),
            ActiveTurnOperation::Interrupt => (
                "turn/interrupt",
                json!({ "threadId": thread_id, "turnId": turn_id }),
            ),
        };
        match self.process.send_request(method, params).await {
            Ok(request_id) => {
                pending_command.insert(request_id, command);
            }
            Err(error) => command.complete(Err(error)),
        }
    }

    fn complete_pending_command(
        &self,
        message: &Value,
        pending_command: &mut HashMap<u64, SteerCommand>,
    ) -> Result<()> {
        let Some(request_id) = message.get("id").and_then(Value::as_u64) else {
            return Ok(());
        };
        let Some(command) = pending_command.remove(&request_id) else {
            return Ok(());
        };
        let result = CodexJsonRpc::request_result(message, request_id, "active turn command")
            .context("Codex active-turn command response omitted its result")?
            .map(drop);
        command.complete(result);
        Ok(())
    }

    fn begin_wait(&mut self) {
        self.waiting = true;
        self.emit_boundary("wait_started");
    }

    fn refresh_wait(&self) {
        self.emit_boundary("wait_updated");
    }

    fn clear_wait(&mut self) {
        if self.waiting {
            self.waiting = false;
            self.emit_boundary("wait_ended");
        }
    }

    fn emit_boundary(&self, boundary: &str) {
        if let Some(event_sink) = self.event_sink.as_ref() {
            let _ = event_sink.send(BackendEvent {
                kind: "parent_boundary".into(),
                text: None,
                data: json!({
                    "boundary": boundary,
                    "agent_count": self.descendant.active_count(),
                }),
                activity: None,
                summary: None,
                task_update: None,
            });
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn retains_a_child_across_parent_completion_until_the_child_finishes() {
        let mut tracker = DescendantTracker::default();
        tracker.observe(
            &json!({
                "method": "item/completed",
                "params": {
                    "threadId": "parent-thread",
                    "turnId": "parent-turn",
                    "item": {
                        "id": "spawn-one",
                        "type": "collabAgentToolCall",
                        "tool": "spawnAgent",
                        "receiverThreadIds": ["child-thread"],
                        "status": "completed"
                    }
                }
            }),
            "parent-thread",
        );
        assert_eq!(tracker.active_count(), 1);

        tracker.observe(
            &json!({
                "method": "turn/completed",
                "params": { "threadId": "parent-thread", "turn": { "id": "parent-turn" } }
            }),
            "parent-thread",
        );
        assert_eq!(tracker.active_count(), 1);

        tracker.observe(
            &json!({
                "method": "turn/completed",
                "params": { "threadId": "child-thread", "turn": { "id": "child-turn" } }
            }),
            "parent-thread",
        );
        assert_eq!(tracker.active_count(), 0);
    }

    #[test]
    fn replaces_child_state_from_wait_lifecycle_updates() {
        let mut tracker = DescendantTracker::default();
        tracker.observe(
            &json!({
                "method": "item/completed",
                "params": {
                    "threadId": "parent-thread",
                    "item": {
                        "type": "collabAgentToolCall",
                        "tool": "wait",
                        "receiverThreadIds": ["child-one", "child-two"],
                        "agentsStates": {
                            "child-one": { "status": "completed" },
                            "child-two": { "status": "running" }
                        },
                        "status": "completed"
                    }
                }
            }),
            "parent-thread",
        );
        assert_eq!(tracker.active_count(), 1);
        assert!(tracker.active_thread.contains("child-two"));
    }

    #[test]
    fn never_tracks_the_parent_thread_as_a_descendant() {
        let mut tracker = DescendantTracker::default();
        tracker.observe(
            &json!({
                "method": "item/completed",
                "params": {
                    "threadId": "child-thread",
                    "item": {
                        "type": "collabAgentToolCall",
                        "tool": "spawnAgent",
                        "receiverThreadIds": ["parent-thread"],
                        "status": "completed"
                    }
                }
            }),
            "parent-thread",
        );

        assert_eq!(tracker.active_count(), 0);
    }

    #[test]
    fn distinguishes_parent_wait_items_from_resumed_parent_activity() {
        let wait = json!({
            "method": "item/started",
            "params": {
                "threadId": "parent-thread",
                "item": { "type": "collabAgentToolCall", "tool": "wait" }
            }
        });
        let response = json!({
            "method": "item/agentMessage/delta",
            "params": { "threadId": "parent-thread", "delta": "Synthesizing." }
        });
        assert!(is_parent_wait_message(&wait, "parent-thread"));
        assert!(!is_parent_wait_message(&response, "parent-thread"));
        assert!(is_parent_activity_message(&response, "parent-thread"));
    }
}
