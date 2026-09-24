use super::CodexBackend;
use super::json_rpc::{CodexJsonRpc, agent_lifecycle_list, native_goal_state};
use crate::backend::steering::{ActiveSteering, ActiveTurnOperation, SteerCommand};
use crate::backend::{BackendEvent, BackendEventSink, BackendOutput, BackendRequest};
use anyhow::{Context, Result};
use serde_json::{Value, json};
use std::collections::{HashMap, HashSet};

const MAX_FINALIZATION_ATTEMPT: usize = 3;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// Keeps the provider reader across turns until the owning native goal stops.
enum NativeGoalLifetime {
    Disabled,
    Active,
    Settled,
}

impl NativeGoalLifetime {
    /// Accept only the owning thread's explicit goal lifecycle notifications.
    fn observe(&mut self, message: &Value, thread_id: &str) {
        if *self == Self::Disabled {
            return;
        }
        if let Some(state) = native_goal_state(message, thread_id) {
            *self = if state == crate::goal::GoalState::Active {
                Self::Active
            } else {
                Self::Settled
            };
        }
    }
}

/// Encode child relay instructions without replacing acknowledged user input.
fn steering_prompt(command: &SteerCommand) -> String {
    let Some(target) = command.target.as_ref() else {
        return command.text.clone();
    };
    let correction = json!({
        "thread_id": target.thread_id,
        "turn_id": target.turn_id,
        "text": command.text,
    });
    format!(
        "Relay this user correction to the existing child agent identified below using the agent messaging tool. Send the text exactly as provided. Do not perform the child's task yourself or spawn another agent. Continue coordinating the existing task after relaying it.\n\n{correction}"
    )
}

#[derive(Default)]
struct DescendantTracker {
    active_turn_by_thread: HashMap<String, Option<String>>,
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
                self.active_turn_by_thread.entry(thread_id).or_default();
            } else {
                self.active_turn_by_thread.remove(&thread_id);
            }
        }
        if message.get("method").and_then(Value::as_str) == Some("turn/started")
            && let (Some(thread_id), Some(turn_id)) =
                (message_thread_id(message), message_turn_id(message))
            && thread_id != parent_thread_id
        {
            self.active_turn_by_thread
                .insert(thread_id.into(), Some(turn_id.into()));
        }
        if message.get("method").and_then(Value::as_str) == Some("turn/completed") {
            let thread_id = message
                .pointer("/params/threadId")
                .or_else(|| message.pointer("/params/thread_id"))
                .or_else(|| message.pointer("/params/turn/threadId"))
                .or_else(|| message.pointer("/params/turn/thread_id"))
                .and_then(Value::as_str);
            if let Some(thread_id) = thread_id.filter(|thread_id| *thread_id != parent_thread_id)
                && self
                    .active_turn_by_thread
                    .get(thread_id)
                    .is_some_and(|active| {
                        active
                            .as_deref()
                            .is_none_or(|turn_id| Some(turn_id) == message_turn_id(message))
                    })
            {
                self.active_turn_by_thread.remove(thread_id);
            }
        }
    }

    fn active_count(&self) -> usize {
        self.active_turn_by_thread.len()
    }

    fn active_thread_list(&self) -> Vec<String> {
        self.active_turn_by_thread.keys().cloned().collect()
    }

    fn target_for_thread(&self, thread_id: &str) -> Option<crate::backend::SteerTarget> {
        self.active_turn_by_thread
            .get(thread_id)
            .and_then(Option::as_ref)
            .map(|turn_id| crate::backend::SteerTarget {
                thread_id: thread_id.into(),
                turn_id: turn_id.clone(),
            })
    }
}

fn message_thread_id(message: &Value) -> Option<&str> {
    message
        .pointer("/params/threadId")
        .or_else(|| message.pointer("/params/thread_id"))
        .and_then(Value::as_str)
}

fn message_turn_id(message: &Value) -> Option<&str> {
    message
        .pointer("/params/turn/id")
        .or_else(|| message.pointer("/params/turnId"))
        .or_else(|| message.pointer("/params/turn_id"))
        .and_then(Value::as_str)
}

struct DescendantCleanup {
    command: SteerCommand,
    thread: HashSet<String>,
    target: HashSet<(String, String)>,
    pending_request: HashSet<u64>,
    deadline: tokio::time::Instant,
    parent: Option<crate::backend::SteerTarget>,
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
    completion: Option<Value>,
}

impl ParentTurn {
    /// Retain terminal evidence so admission cannot reopen a completed parent turn.
    fn observe(&mut self, message: &Value, thread_id: &str) -> bool {
        if self.completion.is_some() { return false; }
        if let Some(started_turn_id) = CodexBackend::notification_thread_turn_id(message, "turn/started", thread_id) {
            self.id = started_turn_id.to_owned();
            self.provider_started = true;
            return true;
        }
        if CodexBackend::notification_matches_turn(message, "turn/completed", thread_id, &self.id) {
            self.completion = Some(message.get("params").cloned().unwrap_or(Value::Null));
            return true;
        }
        if is_parent_activity_message(message, thread_id)
            && message_turn_id(message) == Some(self.id.as_str()) {
            self.provider_started = true;
        }
        false
    }
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
    interrupted: Vec<(tokio::time::Instant, SteerCommand)>,
    completed_turn: HashSet<(String, String)>,
    cleanup: Option<DescendantCleanup>,
    stopping: bool,
    native_goal: NativeGoalLifetime,
    provider_goal_active: bool,
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
        let provider_goal_active = output.evidence.native_state == Some(crate::goal::GoalState::Active);
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
            interrupted: Vec::new(),
            completed_turn: HashSet::new(),
            cleanup: None,
            stopping: false,
            provider_goal_active,
            native_goal: if request.mode == crate::backend::PromptMode::GoalContinuation {
                NativeGoalLifetime::Active
            } else {
                NativeGoalLifetime::Disabled
            },
        }
    }

    pub(super) async fn run(
        mut self,
        turn_id: String,
        provider_started: bool,
        observed_message_list: Vec<Value>,
    ) -> Result<Value> {
        let mut last_completion = Value::Null;
        let mut parent_turn = Some(ParentTurn {
            id: turn_id,
            provider_started,
            completion: None,
        });
        for message in observed_message_list {
            self.descendant.observe(&message, self.thread_id);
            self.observe_goal(&message);
            if let Some(turn_id) =
                CodexBackend::notification_thread_turn_id(&message, "turn/started", self.thread_id)
            {
                parent_turn = Some(ParentTurn {
                    id: turn_id.to_owned(),
                    provider_started: true,
                    completion: None,
                });
            }
            if parent_turn.as_ref().is_some_and(|active| {
                CodexBackend::notification_matches_turn(
                    &message,
                    "turn/completed",
                    self.thread_id,
                    &active.id,
                )
            }) {
                last_completion = message.get("params").cloned().unwrap_or(Value::Null);
                parent_turn = None;
            }
        }
        let mut pending_command = HashMap::<u64, SteerCommand>::new();
        loop {
            if parent_turn.is_none()
                && (self.stopping || self.native_goal != NativeGoalLifetime::Active)
                && self.cleanup.is_none()
                && self.descendant.active_count() == 0
                && pending_command.is_empty()
            {
                return Ok(last_completion);
            }
            if let Some(active) = parent_turn.as_mut() {
                tokio::select! {
                    _ = tokio::time::sleep(std::time::Duration::from_millis(200)), if !self.interrupted.is_empty() || self.cleanup.is_some() => {
                        settle_interruptions(&mut self.interrupted, &self.completed_turn, tokio::time::Instant::now()).await;
                        self.settle_cleanup(tokio::time::Instant::now()).await;
                    }
                    Some(command) = self.steering.receive(), if active.provider_started => {
                        if command.operation == ActiveTurnOperation::CleanupExecution {
                            self.start_cleanup(command, Some(&active.id)).await;
                        } else if self.stopping {
                            command.complete(Err(anyhow::anyhow!("execution cleanup is pending"))).await;
                        } else {
                            self.send_active_command(command, active, &mut pending_command).await;
                        }
                    }
                    message = self.process.receive_message() => {
                        let message = message?;
                        if message_thread_id(&message) == Some(self.thread_id)
                            && message_turn_id(&message).is_some_and(|turn_id| turn_id != active.id) {
                            self.process.set_activity_publication(false);
                            self.process.publish_message(message, self.output).await?;
                            self.process.set_activity_publication(true);
                            continue;
                        }
                        let message = self.process.publish_message(message, self.output).await?;
                        self.observe_goal(&message);
                        self.descendant.observe(&message, self.thread_id);
                        self.admit_cleanup_targets().await;
                        self.complete_pending_command(&message, &mut pending_command).await?;
                        self.complete_cleanup_request(&message).await?;
                        if is_parent_wait_message(&message, self.thread_id)
                            && self.descendant.active_count() > 0
                        {
                            if self.waiting { self.refresh_wait().await; } else { self.begin_wait().await; }
                        } else if self.waiting
                            && (self.descendant.active_count() == 0
                                || is_parent_activity_message(&message, self.thread_id))
                        {
                            self.clear_wait().await;
                        }
                        active.observe(&message, self.thread_id);
                        if let Some(completion) = active.completion.take() {
                            last_completion = completion;
                            parent_turn = None;
                            if self.descendant.active_count() == 0 && pending_command.is_empty() && self.cleanup.is_none() && (self.stopping || self.native_goal != NativeGoalLifetime::Active) {
                                self.clear_wait().await;
                                return Ok(last_completion);
                            }
                            if self.descendant.active_count() == 0 {
                                self.clear_wait().await;
                            } else if self.waiting {
                                self.refresh_wait().await;
                            } else {
                                self.begin_wait().await;
                            }
                        }
                    }
                }
                continue;
            }

            tokio::select! {
                _ = tokio::time::sleep(std::time::Duration::from_millis(200)), if !self.interrupted.is_empty() || self.cleanup.is_some() => {
                    settle_interruptions(&mut self.interrupted, &self.completed_turn, tokio::time::Instant::now()).await;
                    self.settle_cleanup(tokio::time::Instant::now()).await;
                }
                Some(command) = self.steering.receive() => {
                    if command.operation == ActiveTurnOperation::CleanupExecution {
                        self.start_cleanup(command, None).await;
                    } else if self.stopping {
                        command.complete(Err(anyhow::anyhow!("execution cleanup is pending"))).await;
                    } else if matches!(command.operation, ActiveTurnOperation::PauseGoal | ActiveTurnOperation::ClearGoal) {
                        self.send_command(command, self.thread_id, "", &mut pending_command).await;
                    } else if command.operation == ActiveTurnOperation::Interrupt {
                        self.send_waiting_target_command(command, &mut pending_command).await;
                    } else {
                        let text = steering_prompt(&command);
                        match self.start_parent_turn(text).await {
                            Ok(mut turn) => {
                                self.clear_wait().await;
                                command.complete(Ok(())).await;
                                if let Some(completion) = turn.completion.take() {
                                    last_completion = completion;
                                    parent_turn = None;
                                } else {
                                    parent_turn = Some(turn);
                                }
                            }
                            Err(error) => command.complete(Err(error)).await,
                        }
                    }
                }
                message = self.process.receive_message() => {
                    let message = self.process.publish_message(message?, self.output).await?;
                    self.observe_goal(&message);
                    let had_active = self.descendant.active_count() > 0;
                    self.descendant.observe(&message, self.thread_id);
                    self.admit_cleanup_targets().await;
                    self.complete_pending_command(&message, &mut pending_command).await?;
                    self.complete_cleanup_request(&message).await?;
                    if let Some(turn_id) = CodexBackend::notification_thread_turn_id(&message, "turn/started", self.thread_id) {
                        parent_turn = Some(ParentTurn { id: turn_id.to_owned(), provider_started: true, completion: None });
                        self.clear_wait().await;
                        continue;
                    }
                    if self.descendant.active_count() > 0 {
                        if self.waiting { self.refresh_wait().await; } else { self.begin_wait().await; }
                        continue;
                    }
                    if self.native_goal == NativeGoalLifetime::Active && !self.stopping {
                        self.clear_wait().await;
                        continue;
                    }
                    if self.native_goal == NativeGoalLifetime::Settled {
                        self.clear_wait().await;
                        if pending_command.is_empty() { return Ok(last_completion); }
                        continue;
                    }
                    if !had_active {
                        continue;
                    }
                    self.clear_wait().await;
                    if self.stopping {
                        return Ok(Value::Null);
                    }
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
            true,
            CodexBackend::with_model(CodexBackend::secure(json!({
                "threadId": self.thread_id,
                "input": [{ "type": "text", "text": text }],
                "cwd": self.request.workspace,
                "effort": self.request.effort,
                "serviceTier": if self.request.fast_mode { Value::String("fast".into()) } else { Value::Null }
            }), self.request), &self.request.model),
        ).await?;
        let id = turn
            .pointer("/turn/id")
            .or_else(|| turn.get("turnId"))
            .or_else(|| turn.get("turn_id"))
            .and_then(Value::as_str)
            .context("Codex follow-up turn omitted turn id")?
            .to_owned();
        let mut parent = ParentTurn {
            id,
            provider_started,
            completion: None,
        };
        for message in observed_message_list {
            self.descendant.observe(&message, self.thread_id);
            self.native_goal.observe(&message, self.thread_id);
            parent.observe(&message, self.thread_id);
        }
        Ok(parent)
    }

    async fn send_active_command(
        &mut self,
        command: SteerCommand,
        active: &ParentTurn,
        pending_command: &mut HashMap<u64, SteerCommand>,
    ) {
        let direct_target = command
            .target
            .as_ref()
            .filter(|_| command.operation == ActiveTurnOperation::Interrupt);
        let thread_id = direct_target
            .map_or(self.thread_id, |target| target.thread_id.as_str())
            .to_owned();
        let turn_id = direct_target
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
            command
                .complete(Err(anyhow::anyhow!("no active parent turn to interrupt")))
                .await;
            return;
        };
        self.send_command(command, &target.thread_id, &target.turn_id, pending_command)
            .await;
    }

    async fn send_command(
        &mut self,
        mut command: SteerCommand,
        thread_id: &str,
        turn_id: &str,
        pending_command: &mut HashMap<u64, SteerCommand>,
    ) {
        let (method, params) = match command.operation {
            ActiveTurnOperation::PauseGoal | ActiveTurnOperation::ClearGoal => {
                if self.native_goal == NativeGoalLifetime::Disabled {
                    command.complete(Ok(())).await;
                    return;
                }
                if command.operation == ActiveTurnOperation::ClearGoal {
                    ("thread/goal/clear", json!({ "threadId": thread_id }))
                } else {
                    (
                        "thread/goal/set",
                        json!({ "threadId": thread_id, "status": "paused" }),
                    )
                }
            }
            ActiveTurnOperation::Steer => (
                "turn/steer",
                json!({
                    "threadId": thread_id,
                    "input": [{ "type": "text", "text": steering_prompt(&command) }],
                    "expectedTurnId": turn_id,
                }),
            ),
            ActiveTurnOperation::Interrupt => (
                "turn/interrupt",
                json!({ "threadId": thread_id, "turnId": turn_id }),
            ),
            ActiveTurnOperation::CleanupExecution => {
                command
                    .complete(Err(anyhow::anyhow!(
                        "descendant cleanup must use the cleanup coordinator"
                    )))
                    .await;
                return;
            }
        };
        if command.operation == ActiveTurnOperation::Interrupt {
            command.target = Some(crate::backend::SteerTarget {
                thread_id: thread_id.into(),
                turn_id: turn_id.into(),
            });
        }
        match self.process.send_request(method, params).await {
            Ok(request_id) => {
                pending_command.insert(request_id, command);
            }
            Err(error) => command.complete(Err(error)).await,
        }
    }

    async fn complete_pending_command(
        &mut self,
        message: &Value,
        pending_command: &mut HashMap<u64, SteerCommand>,
    ) -> Result<()> {
        if message.get("method").and_then(Value::as_str) == Some("turn/completed") {
            if let (Some(thread), Some(turn)) = (
                message_thread_id(message),
                message.pointer("/params/turn/id").and_then(Value::as_str),
            ) {
                self.completed_turn.insert((thread.into(), turn.into()));
            }
        }
        settle_interruptions(
            &mut self.interrupted,
            &self.completed_turn,
            tokio::time::Instant::now(),
        )
        .await;
        let Some(request_id) = message.get("id").and_then(Value::as_u64) else {
            return Ok(());
        };
        let Some(command) = pending_command.remove(&request_id) else {
            return Ok(());
        };
        let result = CodexJsonRpc::request_result(message, request_id, "active turn command")
            .context("Codex active-turn command response omitted its result")?
            .map(drop);
        if command.operation == ActiveTurnOperation::Interrupt && result.is_ok() {
            self.interrupted.push((
                tokio::time::Instant::now() + std::time::Duration::from_secs(30),
                command,
            ));
            settle_interruptions(
                &mut self.interrupted,
                &self.completed_turn,
                tokio::time::Instant::now(),
            )
            .await;
        } else {
            command.complete(result).await;
        }
        Ok(())
    }

    async fn start_cleanup(&mut self, command: SteerCommand, parent_turn_id: Option<&str>) {
        if self.cleanup.is_some() {
            command
                .complete(Err(anyhow::anyhow!(
                    "Codex descendant cleanup is already running"
                )))
                .await;
            return;
        }
        let thread = self.descendant.active_thread_list();
        self.stopping = true;
        if thread.is_empty() && parent_turn_id.is_none() && !self.provider_goal_active {
            command.complete(Ok(())).await;
            return;
        }
        self.cleanup = Some(DescendantCleanup {
            command,
            thread: thread.into_iter().collect(),
            target: HashSet::new(),
            pending_request: HashSet::new(),
            deadline: tokio::time::Instant::now() + std::time::Duration::from_secs(30),
            parent: parent_turn_id.map(|turn_id| crate::backend::SteerTarget {
                thread_id: self.thread_id.to_owned(),
                turn_id: turn_id.to_owned(),
            }),
        });
        if self.provider_goal_active {
            match self.process.send_request("thread/goal/set", json!({
                "threadId": self.thread_id, "status": "paused"
            })).await {
                Ok(request_id) => {
                    self.cleanup.as_mut().expect("cleanup exists").pending_request.insert(request_id);
                }
                Err(error) => {
                    self.cleanup.take().expect("cleanup exists").command.complete(Err(error)).await;
                    return;
                }
            }
        }
        self.admit_cleanup_targets().await;
    }

    fn observe_goal(&mut self, message: &Value) {
        if let Some(state) = native_goal_state(message, self.thread_id) {
            self.provider_goal_active = state == crate::goal::GoalState::Active;
        }
        self.native_goal.observe(message, self.thread_id);
    }

    async fn admit_cleanup_targets(&mut self) {
        let Some(cleanup) = self.cleanup.as_mut() else {
            return;
        };
        cleanup.thread.extend(self.descendant.active_thread_list());
        let target_list = cleanup
            .thread
            .iter()
            .filter_map(|thread_id| self.descendant.target_for_thread(thread_id))
            .chain(cleanup.parent.clone())
            .filter(|target| {
                !cleanup
                    .target
                    .contains(&(target.thread_id.clone(), target.turn_id.clone()))
            })
            .collect::<Vec<_>>();
        for target in target_list {
            match self
                .process
                .send_request(
                    "turn/interrupt",
                    json!({ "threadId": target.thread_id, "turnId": target.turn_id }),
                )
                .await
            {
                Ok(request_id) => {
                    let cleanup = self.cleanup.as_mut().expect("cleanup exists");
                    cleanup
                        .target
                        .insert((target.thread_id.clone(), target.turn_id.clone()));
                    cleanup.pending_request.insert(request_id);
                }
                Err(error) => {
                    let cleanup = self.cleanup.take().expect("cleanup exists");
                    cleanup.command.complete(Err(error)).await;
                    return;
                }
            }
        }
        self.settle_cleanup(tokio::time::Instant::now()).await;
    }

    async fn complete_cleanup_request(&mut self, message: &Value) -> Result<()> {
        let Some(request_id) = message.get("id").and_then(Value::as_u64) else {
            self.settle_cleanup(tokio::time::Instant::now()).await;
            return Ok(());
        };
        let Some(cleanup) = self.cleanup.as_mut() else {
            return Ok(());
        };
        if !cleanup.pending_request.remove(&request_id) {
            return Ok(());
        }
        if let Err(error) = CodexJsonRpc::request_result(message, request_id, "child cleanup")
            .context("Codex child cleanup response omitted its result")?
        {
            let cleanup = self.cleanup.take().expect("cleanup exists");
            cleanup.command.complete(Err(error)).await;
            return Ok(());
        }
        self.settle_cleanup(tokio::time::Instant::now()).await;
        Ok(())
    }

    async fn settle_cleanup(&mut self, now: tokio::time::Instant) {
        let Some(cleanup) = self.cleanup.as_ref() else {
            return;
        };
        if cleanup.pending_request.is_empty() && cleanup.parent.as_ref().is_none_or(|parent| {
            self.completed_turn
                .contains(&(parent.thread_id.clone(), parent.turn_id.clone()))
        }) && cleanup.thread.iter().all(|thread_id| {
            !self
                .descendant
                .active_turn_by_thread
                .contains_key(thread_id)
        }) {
            let cleanup = self.cleanup.take().expect("cleanup exists");
            cleanup.command.complete(Ok(())).await;
        } else if now >= cleanup.deadline {
            let cleanup = self.cleanup.take().expect("cleanup exists");
            cleanup
                .command
                .complete(Err(anyhow::anyhow!(
                    "Codex descendant cleanup did not reach terminal state within 30 seconds"
                )))
                .await;
        }
    }

    async fn begin_wait(&mut self) {
        self.waiting = true;
        self.emit_boundary("wait_started").await;
    }

    async fn refresh_wait(&self) {
        self.emit_boundary("wait_updated").await;
    }

    async fn clear_wait(&mut self) {
        if self.waiting {
            self.waiting = false;
            self.emit_boundary("wait_ended").await;
        }
    }

    async fn emit_boundary(&self, boundary: &str) {
        if let Some(event_sink) = self.event_sink.as_ref() {
            let _ = event_sink
                .send_wait(BackendEvent {
                    address: None,
                    turn_boundary: None,
                    kind: "parent_boundary".into(),
                    text: None,
                    data: json!({
                        "boundary": boundary,
                        "agent_count": self.descendant.active_count(),
                    }),
                    activity: None,
                    summary: None,
                    task_update: None,
                })
                .await;
        }
    }
}

/// Resolve interruption only from terminal provider evidence or an explicit deadline failure.
async fn settle_interruptions(
    interrupted: &mut Vec<(tokio::time::Instant, SteerCommand)>,
    completed_turn: &HashSet<(String, String)>,
    now: tokio::time::Instant,
) {
    let mut waiting = Vec::new();
    for (deadline, command) in interrupted.drain(..) {
        let completed = command.target.as_ref().is_some_and(|target| {
            completed_turn.contains(&(target.thread_id.clone(), target.turn_id.clone()))
        });
        if completed {
            command.complete(Ok(())).await;
        } else if now >= deadline {
            command
                .complete(Err(anyhow::anyhow!(
                    "Codex child interruption did not reach terminal state within 30 seconds"
                )))
                .await;
        } else {
            waiting.push((deadline, command));
        }
    }
    *interrupted = waiting;
}

#[cfg(test)]
mod test {
    use super::*;

    #[tokio::test]
    async fn chat_cancellation_pauses_provider_goal_and_waits_for_acknowledgements() -> Result<()> {
        use crate::backend::{BackendInput, PromptMode};
        use crate::backend::steering::SteeringLane;
        use crate::backend::approval::PermissionCoordinator;
        use crate::session::ExecutionMode;
        use crate::trace::TraceStore;
        use futures_util::{SinkExt, StreamExt};
        use std::sync::Arc;

        let directory = tempfile::tempdir()?;
        let workspace = directory.path().to_string_lossy().into_owned();
        let permission = PermissionCoordinator::transient(&workspace)?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let backend = CodexBackend::new_with_permission_coordinator(
            vec!["unused".into()], permission.clone(), trace.clone(),
        )?;
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            let pause: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
            assert_eq!(pause["method"], "thread/goal/set");
            assert_eq!(pause["params"]["status"], "paused");
            let interrupt: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
            assert_eq!(interrupt["method"], "turn/interrupt");
            for message in [
                json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"current","status":"interrupted"}}}),
                json!({"id":interrupt["id"],"result":{}}),
                json!({"id":pause["id"],"result":{}}),
            ] {
                socket.send(tokio_tungstenite::tungstenite::Message::Text(message.to_string().into())).await.unwrap();
            }
            let _ = socket.next().await;
        });
        let mut process = CodexJsonRpc::connect(&endpoint, &workspace, ExecutionMode::Read,
            permission, None, trace, "session".into()).await?;
        let request = BackendRequest {
            harness_session_id: "session".into(), workspace,
            input: BackendInput::from_text("plan out a new one"), mode: PromptMode::Chat,
            model: "gpt-5.6-terra".into(), effort: "medium".into(), context_window: None,
            fast_mode: false, execution_mode: ExecutionMode::Read,
            backend_session_id: Some("parent".into()), control_context: None,
        };
        let lane = SteeringLane::default();
        let mut steering = lane.activate(None)?;
        let cleanup = tokio::spawn(async move { lane.cleanup_execution().await });
        let mut output = BackendOutput::default();
        let result = tokio::time::timeout(std::time::Duration::from_secs(3),
            CodexTurnCoordinator::new(&backend, &mut process, &mut output, &mut steering,
                None, &request, "parent").run("current".into(), true, vec![
                    json!({"method":"thread/goal/updated","params":{"threadId":"parent","goal":{"status":"active"}}})
                ])).await??;
        assert_eq!(result["turn"]["status"], "interrupted");
        tokio::time::timeout(std::time::Duration::from_secs(3), cleanup).await???;
        drop(process);
        tokio::time::timeout(std::time::Duration::from_secs(3), server).await??;
        Ok(())
    }

    #[tokio::test]
    async fn parent_restart_retains_child_progress_and_early_completion() -> Result<()> {
        use crate::backend::approval::PermissionCoordinator;
        use crate::backend::{BackendInput, PromptMode};
        use crate::session::ExecutionMode;
        use crate::trace::TraceStore;
        use futures_util::{SinkExt, StreamExt};
        use std::sync::Arc;
        let directory = tempfile::tempdir()?;
        let workspace = directory.path().to_string_lossy().into_owned();
        let permission = PermissionCoordinator::transient(&workspace)?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let backend = CodexBackend::new_with_permission_coordinator(vec!["unused".into()], permission.clone(), trace.clone())?;
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            let request: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
            assert_eq!(request["method"], "turn/start");
            for message in [
                json!({"method":"item/completed","params":{"threadId":"child","turnId":"child-turn","item":{"id":"child-answer","type":"agentMessage","phase":"final_answer","text":"CHILD-PROGRESS"}}}),
                json!({"method":"turn/completed","params":{"threadId":"child","turn":{"id":"child-turn","status":"completed"}}}),
                json!({"method":"turn/started","params":{"threadId":"parent","turn":{"id":"second","status":"inProgress"}}}),
                json!({"method":"item/completed","params":{"threadId":"parent","turnId":"second","item":{"id":"parent-answer","type":"agentMessage","phase":"final_answer","text":"PARENT-DONE"}}}),
                json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"second","status":"completed"}}}),
                json!({"id":request["id"],"result":{"turn":{"id":"second"}}}),
            ] {
                socket.send(tokio_tungstenite::tungstenite::Message::Text(message.to_string().into())).await.unwrap();
            }
            let _ = socket.next().await;
        });
        let mut process = CodexJsonRpc::connect(&endpoint, &workspace, ExecutionMode::Read, permission, None, trace, "session".into()).await?;
        let request = BackendRequest {
            harness_session_id: "session".into(), workspace, input: BackendInput::from_text("parent"),
            mode: PromptMode::Chat, model: "gpt-5.6-terra".into(), effort: "medium".into(),
            context_window: None, fast_mode: false, execution_mode: ExecutionMode::Read,
            backend_session_id: Some("parent".into()), control_context: None,
        };
        let lane = crate::backend::steering::SteeringLane::default();
        let mut steering = lane.activate(None)?;
        let mut output = BackendOutput { backend_session_id: Some("parent".into()), ..Default::default() };
        let observed = vec![
            json!({"method":"turn/started","params":{"threadId":"child","turn":{"id":"child-turn","status":"inProgress"}}}),
            json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"first","status":"completed"}}}),
        ];
        let coordinator = CodexTurnCoordinator::new(&backend, &mut process, &mut output, &mut steering, None, &request, "parent");
        let (completed, acknowledged) = tokio::time::timeout(std::time::Duration::from_secs(2), async {
            tokio::join!(coordinator.run("first".into(), true, observed), lane.steer("continue".into()))
        }).await?;
        acknowledged?;
        assert_eq!(completed?["turn"]["id"], "second");
        let encoded = serde_json::to_string(&output.event)?;
        assert!(encoded.contains("CHILD-PROGRESS"));
        assert!(encoded.contains("PARENT-DONE"));
        drop(process);
        tokio::time::timeout(std::time::Duration::from_secs(2), server).await??;
        Ok(())
    }

    #[tokio::test]
    async fn native_goal_reader_spans_provider_turns_until_explicit_settlement() -> Result<()> {
        use crate::backend::approval::PermissionCoordinator;
        use crate::backend::steering::SteeringLane;
        use crate::backend::{BackendInput, PromptMode};
        use crate::session::ExecutionMode;
        use crate::trace::TraceStore;
        use futures_util::{SinkExt, StreamExt};
        use std::sync::Arc;
        for (status, before_completion) in [
            "complete",
            "paused",
            "blocked",
            "usageLimited",
            "budgetLimited",
            "cleared",
        ]
        .into_iter()
        .flat_map(|status| [false, true].map(move |before| (status, before)))
        {
            let directory = tempfile::tempdir()?;
            let workspace = directory.path().to_string_lossy().into_owned();
            let permission = PermissionCoordinator::transient(&workspace)?;
            let trace = Arc::new(TraceStore::open(directory.path())?);
            let backend = CodexBackend::new_with_permission_coordinator(
                vec!["unused".into()],
                Arc::clone(&permission),
                Arc::clone(&trace),
            )?;
            let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
            let endpoint = format!("ws://{}", listener.local_addr()?);
            let server = tokio::spawn(async move {
                let (stream, _) = listener.accept().await.unwrap();
                let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
                let mut messages = vec![
                    json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"first","status":"completed"}}}),
                    json!({"method":"thread/goal/updated","params":{"threadId":"child","goal":{"status":"complete"}}}),
                    json!({"method":"turn/started","params":{"threadId":"parent","turn":{"id":"second","status":"inProgress"}}}),
                    json!({"method":"item/completed","params":{"threadId":"parent","turnId":"second","item":{"id":"answer","type":"agentMessage","phase":"final_answer","text":"second turn retained"}}}),
                    json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"second","status":"completed"}}}),
                ];
                messages.push(if status == "cleared" {
                    json!({"method":"thread/goal/cleared","params":{"threadId":"parent"}})
                } else {
                    json!({"method":"thread/goal/updated","params":{"threadId":"parent","goal":{"status":status}}})
                });
                if before_completion {
                    let count = messages.len();
                    messages.swap(count - 1, count - 2);
                }
                let controlled = status == "paused" || status == "cleared";
                let mut control_id = None;
                let count = messages.len();
                for (index, message) in messages.into_iter().enumerate() {
                    if controlled && index == count - 2 {
                        let request: Value = serde_json::from_str(
                            socket.next().await.unwrap().unwrap().to_text().unwrap(),
                        )
                        .unwrap();
                        assert_eq!(
                            request["method"],
                            if status == "cleared" {
                                "thread/goal/clear"
                            } else {
                                "thread/goal/set"
                            }
                        );
                        assert_eq!(request["params"]["threadId"], "parent");
                        control_id = Some(request["id"].clone());
                    }
                    socket
                        .send(tokio_tungstenite::tungstenite::Message::Text(
                            message.to_string().into(),
                        ))
                        .await
                        .unwrap();
                }
                if let Some(id) = control_id {
                    socket
                        .send(tokio_tungstenite::tungstenite::Message::Text(
                            json!({"id":id,"result":{}}).to_string().into(),
                        ))
                        .await
                        .unwrap();
                }
                let _ = socket.next().await;
            });
            let mut process = CodexJsonRpc::connect(
                &endpoint,
                &workspace,
                ExecutionMode::Read,
                permission,
                None,
                trace,
                "session".into(),
            )
            .await?;
            let request = BackendRequest {
                harness_session_id: "session".into(),
                workspace,
                input: BackendInput::from_text("goal"),
                mode: PromptMode::GoalContinuation,
                model: "gpt-5.6-terra".into(),
                effort: "medium".into(),
                context_window: None,
                fast_mode: false,
                execution_mode: ExecutionMode::Read,
                backend_session_id: Some("parent".into()),
                control_context: None,
            };
            let lane = SteeringLane::default();
            let mut steering = lane.activate(None)?;
            let control = if status == "paused" || status == "cleared" {
                let lane = lane.clone();
                Some(tokio::spawn(async move {
                    lane.stop_goal(status == "cleared").await
                }))
            } else {
                None
            };
            let mut output = BackendOutput::default();
            output.backend_session_id = Some("parent".into());
            let result = tokio::time::timeout(
                std::time::Duration::from_secs(5),
                CodexTurnCoordinator::new(
                    &backend,
                    &mut process,
                    &mut output,
                    &mut steering,
                    None,
                    &request,
                    "parent",
                )
                .run("first".into(), true, vec![]),
            )
            .await??;
            server.abort();
            if let Some(control) = control {
                control.await??;
            }
            assert_eq!(
                result.pointer("/turn/id").and_then(Value::as_str),
                Some("second"),
                "{status}"
            );
            assert!(
                output
                    .event
                    .iter()
                    .any(|event| event.text.as_deref() == Some("second turn retained"))
            );
            assert_eq!(
                output.evidence.native_state,
                Some(match status {
                    "complete" => crate::goal::GoalState::Complete,
                    "paused" => crate::goal::GoalState::Paused,
                    "blocked" => crate::goal::GoalState::Blocked,
                    "usageLimited" => crate::goal::GoalState::UsageLimited,
                    "budgetLimited" => crate::goal::GoalState::BudgetLimited,
                    "cleared" => crate::goal::GoalState::Cleared,
                    _ => panic!("unknown fixture status"),
                })
            );
        }
        Ok(())
    }

    #[tokio::test]
    async fn child_relay_acknowledges_original_input_only_after_provider_acceptance() {
        use crate::backend::steering::{SteerTarget, SteeringLane};
        let lane = SteeringLane::default();
        let (event_sink, mut event_stream) = crate::backend::events::channel();
        let mut active = lane.activate(Some(event_sink)).unwrap();
        let original = "Use \"updated\"\nand preserve newlines";
        for accepted in [true, false] {
            let mut request = Box::pin(lane.steer_target(
                original.into(),
                SteerTarget {
                    thread_id: "child".into(),
                    turn_id: "child-turn".into(),
                },
            ));
            assert!(futures_util::poll!(request.as_mut()).is_pending());
            let command = active.receive().await.unwrap();
            let prompt = steering_prompt(&command);
            let payload: Value =
                serde_json::from_str(prompt.rsplit_once("\n\n").unwrap().1).unwrap();
            assert_eq!(
                payload,
                json!({"thread_id":"child", "turn_id":"child-turn", "text":original})
            );
            assert!(event_stream.try_recv().is_err());
            command
                .complete(if accepted {
                    Ok(())
                } else {
                    Err(anyhow::anyhow!("rejected"))
                })
                .await;
            assert_eq!(request.await.is_ok(), accepted);
            if accepted {
                let event = event_stream.recv().await.unwrap().unwrap();
                assert_eq!(event.kind, "steering_input");
                assert_eq!(event.text.as_deref(), Some(original));
            }
            assert!(event_stream.try_recv().is_err());
        }
    }

    #[tokio::test]
    async fn interruption_requires_exact_terminal_evidence_or_fails_at_deadline() {
        use crate::backend::steering::{SteerTarget, SteeringLane};
        for expires in [false, true] {
            let lane = SteeringLane::default();
            let mut active = lane.activate(None).unwrap();
            let target = SteerTarget {
                thread_id: "child".into(),
                turn_id: "turn".into(),
            };
            let request = tokio::spawn(async move { lane.interrupt_target(target).await });
            let command = active.receive().await.unwrap();
            let now = tokio::time::Instant::now();
            let mut pending = vec![(now + std::time::Duration::from_secs(30), command)];
            let mut completed = HashSet::from([("child".into(), "different-turn".into())]);
            settle_interruptions(&mut pending, &completed, now).await;
            assert_eq!(
                pending.len(),
                1,
                "acknowledgement or unrelated turn completion released interruption"
            );
            assert!(!request.is_finished());
            if expires {
                settle_interruptions(
                    &mut pending,
                    &completed,
                    now + std::time::Duration::from_secs(30),
                )
                .await;
                let error = request.await.unwrap().unwrap_err();
                assert!(
                    error
                        .to_string()
                        .contains("terminal state within 30 seconds")
                );
            } else {
                completed.insert(("child".into(), "turn".into()));
                settle_interruptions(&mut pending, &completed, now).await;
                request.await.unwrap().unwrap();
            }
            assert!(pending.is_empty());
        }
    }

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
        assert!(tracker.active_turn_by_thread.contains_key("child-two"));
    }

    #[test]
    fn descendant_cleanup_targets_the_exact_started_child_turn() {
        let mut tracker = DescendantTracker::default();
        tracker.observe(
            &json!({
                "method": "item/completed",
                "params": {
                    "threadId": "parent-thread",
                    "item": {
                        "type": "collabAgentToolCall",
                        "tool": "spawnAgent",
                        "receiverThreadIds": ["child-thread"],
                        "status": "completed"
                    }
                }
            }),
            "parent-thread",
        );
        assert!(tracker.target_for_thread("child-thread").is_none());

        tracker.observe(
            &json!({
                "method": "turn/started",
                "params": {
                    "threadId": "child-thread",
                    "turn": { "id": "child-turn" }
                }
            }),
            "parent-thread",
        );
        let target = tracker.target_for_thread("child-thread").unwrap();
        assert_eq!(target.thread_id, "child-thread");
        assert_eq!(target.turn_id, "child-turn");
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

    #[test]
    fn adopts_the_same_thread_started_id_without_adopting_child_turns() {
        let mut parent = ParentTurn {
            id: "initial-turn".into(),
            provider_started: false,
            completion: None,
        };
        let authoritative_start = json!({
            "method": "turn/started",
            "params": {
                "threadId": "parent-thread",
                "turn": { "id": "authoritative-turn" }
            }
        });
        let child = json!({
            "method": "turn/started",
            "params": {
                "threadId": "child-thread",
                "turn": { "id": "child-turn" }
            }
        });

        assert!(parent.observe(&authoritative_start, "parent-thread"));
        assert_eq!(parent.id, "authoritative-turn");
        assert!(parent.provider_started);
        assert!(!parent.observe(&child, "parent-thread"));
        assert_eq!(parent.id, "authoritative-turn");
    }

    #[test]
    fn acknowledged_turn_waits_for_matching_activity_before_accepting_steering() {
        let mut parent = ParentTurn { id: "current".into(), provider_started: false, completion: None };
        for (thread_id, turn_id) in [("child", "current"), ("parent", "cancelled")] {
            parent.observe(&json!({"method":"item/started","params":{
                "threadId":thread_id,"turnId":turn_id
            }}), "parent");
            assert!(!parent.provider_started);
        }
        parent.observe(&json!({"method":"item/started","params":{
            "threadId":"parent","turnId":"current"
        }}), "parent");
        assert!(parent.provider_started);
    }
}
#[test]
fn stale_child_completion_cannot_settle_a_newer_turn() {
    let mut tracker = DescendantTracker::default();
    tracker.observe(
        &json!({"method":"turn/started", "params": {
            "threadId":"child", "turn":{"id":"new"}
        }}),
        "parent",
    );
    tracker.observe(
        &json!({"method":"turn/completed", "params": {
            "threadId":"child", "turn":{"id":"old"}
        }}),
        "parent",
    );
    assert_eq!(tracker.target_for_thread("child").unwrap().turn_id, "new");
    tracker.observe(
        &json!({"method":"turn/completed", "params": {
            "threadId":"child", "turn":{"id":"new"}
        }}),
        "parent",
    );
    assert_eq!(tracker.active_count(), 0);
}
