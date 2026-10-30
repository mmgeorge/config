use super::CodexBackend;
use super::json_rpc::{CodexJsonRpc, agent_lifecycle_list, native_goal_state};
use crate::backend::steering::{ActiveSteering, ActiveTurnOperation, SteerCommand};
use crate::backend::{BackendEvent, BackendEventSink, BackendOutput, BackendRequest};
use anyhow::{Context, Result};
use serde_json::{Value, json};
use std::collections::{HashMap, HashSet};

const MAX_FINALIZATION_ATTEMPT: usize = 3;

/// Keeps the provider reader across turns until the owning native goal stops.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum NativeGoalLifetime {
    Disabled,
    Active,
    Settled,
}

/// Drains native continuation after an admitted Harness goal termination.
struct NativeGoalSettlement {
    request_id: u64,
    termination: crate::goal::GoalTermination,
    acknowledged: bool,
    terminal_evidence: Option<crate::goal::GoalState>,
    interrupted_turn: HashSet<String>,
    pending_interrupt: HashMap<u64, String>,
}

impl NativeGoalSettlement {
    /// Keep the reader until native cleanup and every raced turn have settled.
    fn drained(&self) -> bool {
        self.acknowledged && self.interrupted_turn.is_empty() && self.pending_interrupt.is_empty()
    }
}

impl NativeGoalLifetime {
    /// Retain the reader only when the admitted task owns a native goal.
    fn for_request(request: &BackendRequest) -> Self {
        if request
            .control_context
            .as_ref()
            .is_some_and(|context| context.native_goal.is_some())
        {
            Self::Active
        } else {
            Self::Disabled
        }
    }

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
    owned_thread: HashSet<String>,
    observed_turn: HashMap<String, ObservedDescendantTurn>,
}

/// Retain early child turn evidence without treating it as proof of ancestry.
enum ObservedDescendantTurn {
    Active(String),
    Completed(String),
}

impl ObservedDescendantTurn {
    /// Get the exact turn named by either lifecycle observation.
    fn id(&self) -> &str {
        match self {
            Self::Active(id) | Self::Completed(id) => id,
        }
    }
}

impl DescendantTracker {
    /// Admit child lifecycle evidence only through trusted parent relationships.
    fn observe(&mut self, message: &Value, parent_thread_id: &str) {
        // Buffer transport observations until a trusted parent establishes child ownership.
        if let (Some(thread_id), Some(turn_id)) =
            (message_thread_id(message), message_turn_id(message))
            && thread_id != parent_thread_id
        {
            match message.get("method").and_then(Value::as_str) {
                Some("turn/started") => {
                    self.observed_turn.insert(
                        thread_id.into(),
                        ObservedDescendantTurn::Active(turn_id.into()),
                    );
                }
                Some("turn/completed")
                    if self
                        .observed_turn
                        .get(thread_id)
                        .is_none_or(|turn| turn.id() == turn_id) =>
                {
                    self.observed_turn.insert(
                        thread_id.into(),
                        ObservedDescendantTurn::Completed(turn_id.into()),
                    );
                }
                _ => {}
            }
        }
        // Admit relationships only through the root or an already owned ancestor.
        for lifecycle in agent_lifecycle_list(message) {
            let trusted_parent = lifecycle.parent_thread_id.as_deref().is_some_and(|parent| {
                parent == parent_thread_id || self.owned_thread.contains(parent)
            });
            if !trusted_parent {
                continue;
            }
            let Some(thread_id) = lifecycle.provider_thread_id else {
                continue;
            };
            if thread_id == parent_thread_id {
                continue;
            }
            // A retained child can rejoin through an explicit operation from its trusted parent.
            let resumes_child = lifecycle.status.is_active()
                && matches!(
                    lifecycle.operation.to_ascii_lowercase().as_str(),
                    "sendinput" | "wait" | "resumeagent"
                );
            if lifecycle.starts_child || resumes_child {
                self.owned_thread.insert(thread_id.clone());
            }
            if !self.owned_thread.contains(&thread_id) {
                continue;
            }
            if lifecycle.status.is_active() {
                match self.observed_turn.get(&thread_id) {
                    Some(ObservedDescendantTurn::Active(turn_id)) => {
                        self.active_turn_by_thread
                            .insert(thread_id, Some(turn_id.clone()));
                    }
                    Some(ObservedDescendantTurn::Completed(_)) if resumes_child => {
                        self.active_turn_by_thread.insert(thread_id, None);
                    }
                    Some(ObservedDescendantTurn::Completed(_)) => {
                        self.active_turn_by_thread.remove(&thread_id);
                    }
                    None => {
                        self.active_turn_by_thread.entry(thread_id).or_default();
                    }
                }
            } else {
                self.active_turn_by_thread.remove(&thread_id);
            }
        }
        if message.get("method").and_then(Value::as_str) == Some("turn/started")
            && let (Some(thread_id), Some(turn_id)) =
                (message_thread_id(message), message_turn_id(message))
            && thread_id != parent_thread_id
            && self.owned_thread.contains(thread_id)
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
    pending_request: HashMap<u64, CleanupRequest>,
    deadline: tokio::time::Instant,
    parent: Option<crate::backend::SteerTarget>,
    command_item: HashMap<String, HashSet<crate::backend::ToolId>>,
    terminal_verified: HashSet<String>,
    terminal_found: HashSet<String>,
    terminal_cursor: HashMap<String, HashSet<String>>,
}

/// Keep native request acknowledgments bound to the cleanup operation that issued them.
enum CleanupRequest {
    GoalPause,
    TurnInterrupt { target: crate::backend::SteerTarget },
    TerminalList { thread_id: String },
    TerminalTerminate { thread_id: String },
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
        if self.completion.is_some() {
            return false;
        }
        if let Some(started_turn_id) =
            CodexBackend::notification_thread_turn_id(message, "turn/started", thread_id)
        {
            self.id = started_turn_id.to_owned();
            self.provider_started = true;
            return true;
        }
        if CodexBackend::notification_matches_turn(message, "turn/completed", thread_id, &self.id) {
            self.completion = Some(message.get("params").cloned().unwrap_or(Value::Null));
            return true;
        }
        if is_parent_activity_message(message, thread_id)
            && message_turn_id(message) == Some(self.id.as_str())
        {
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
    goal_settlement: Option<NativeGoalSettlement>,
    owned_turn: HashSet<(String, crate::backend::ProviderTurnId)>,
    command_item: HashMap<String, HashSet<crate::backend::ToolId>>,
    observed_command:
        HashMap<(String, crate::backend::ProviderTurnId), HashSet<crate::backend::ToolId>>,
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
        let provider_goal_active =
            output.completion.evidence.native_state == Some(crate::goal::GoalState::Active);
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
            goal_settlement: None,
            native_goal: NativeGoalLifetime::for_request(request),
            owned_turn: HashSet::new(),
            command_item: HashMap::new(),
            observed_command: HashMap::new(),
        }
    }

    pub(super) async fn run(
        mut self,
        turn_id: String,
        provider_started: bool,
        observed_message_list: Vec<Value>,
    ) -> Result<Value> {
        let result = self
            .run_inner(turn_id, provider_started, observed_message_list)
            .await;
        let cleanup = self.process.cancel_requests(self.output).await;
        match (result, cleanup) {
            (result, Ok(())) => result,
            (Ok(_), Err(error)) => Err(error),
            (Err(error), Err(cleanup)) => {
                Err(error.context(format!("provider request cleanup failed: {cleanup:#}")))
            }
        }
    }

    async fn run_inner(
        &mut self,
        turn_id: String,
        provider_started: bool,
        observed_message_list: Vec<Value>,
    ) -> Result<Value> {
        self.owned_turn
            .insert((self.thread_id.into(), turn_id.clone().into()));
        let mut last_completion = Value::Null;
        let mut parent_turn = Some(ParentTurn {
            id: turn_id,
            provider_started,
            completion: None,
        });
        for message in observed_message_list {
            self.descendant.observe(&message, self.thread_id);
            self.observe_native_state(&message)?;
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
            self.synchronize_goal_control().await?;
            if self.goal_settlement.is_some() && !self.goal_control_is_current() {
                return Err(crate::task::StaleTask.into());
            }
            if parent_turn.is_none()
                && (self.stopping || self.native_goal != NativeGoalLifetime::Active)
                && self.cleanup.is_none()
                && self.descendant.active_count() == 0
                && pending_command.is_empty()
                && self
                    .goal_settlement
                    .as_ref()
                    .is_none_or(NativeGoalSettlement::drained)
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
                        if message_thread_id(message.message()) == Some(self.thread_id)
                            && message_turn_id(message.message()).is_some_and(|turn_id| turn_id != active.id) {
                            self.process.set_activity_publication(false);
                            let message = self.process.publish_received(message, self.output).await?;
                            self.process.set_activity_publication(true);
                            self.observe_native_state(&message)?;
                            self.drain_settled_goal_turn(&message).await?;
                            continue;
                        }
                        let message = self.process.publish_received(message, self.output).await?;
                        self.descendant.observe(&message, self.thread_id);
                        self.observe_native_state(&message)?;
                        self.drain_settled_goal_turn(&message).await?;
                        self.admit_cleanup_targets().await;
                        self.complete_pending_command(&message, &mut pending_command).await?;
                        self.complete_cleanup_request(&message).await?;
                        if is_parent_wait_message(&message, self.thread_id)
                            && self.descendant.active_count() > 0
                        {
                            if self.waiting { self.refresh_wait().await?; } else { self.begin_wait().await?; }
                        } else if self.waiting
                            && (self.descendant.active_count() == 0
                                || is_parent_activity_message(&message, self.thread_id))
                        {
                            self.clear_wait().await?;
                        }
                        active.observe(&message, self.thread_id);
                        if let Some(completion) = active.completion.take() {
                            last_completion = completion;
                            parent_turn = None;
                            if self.descendant.active_count() == 0 && pending_command.is_empty() && self.cleanup.is_none() && (self.stopping || self.native_goal != NativeGoalLifetime::Active) && self.goal_settlement.as_ref().is_none_or(NativeGoalSettlement::drained) {
                                self.clear_wait().await?;
                                return Ok(last_completion);
                            }
                            if self.descendant.active_count() == 0 {
                                self.clear_wait().await?;
                            } else if self.waiting {
                                self.refresh_wait().await?;
                            } else {
                                self.begin_wait().await?;
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
                        match self.start_parent_turn(text, command.submission.clone()).await {
                            Ok(mut turn) => {
                                self.clear_wait().await?;
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
                    let message = self.process.publish_received(message?, self.output).await?;
                    let had_active = self.descendant.active_count() > 0;
                    self.descendant.observe(&message, self.thread_id);
                    self.observe_native_state(&message)?;
                    if self.drain_settled_goal_turn(&message).await? {
                        continue;
                    }
                    self.admit_cleanup_targets().await;
                    self.complete_pending_command(&message, &mut pending_command).await?;
                    self.complete_cleanup_request(&message).await?;
                    if let Some(turn_id) = CodexBackend::notification_thread_turn_id(&message, "turn/started", self.thread_id) {
                        parent_turn = Some(ParentTurn { id: turn_id.to_owned(), provider_started: true, completion: None });
                        self.clear_wait().await?;
                        continue;
                    }
                    if self.descendant.active_count() > 0 {
                        if self.waiting { self.refresh_wait().await?; } else { self.begin_wait().await?; }
                        continue;
                    }
                    if self.native_goal == NativeGoalLifetime::Active && !self.stopping {
                        self.clear_wait().await?;
                        continue;
                    }
                    if self.native_goal == NativeGoalLifetime::Settled {
                        self.clear_wait().await?;
                        if self.cleanup.is_none() && pending_command.is_empty() && self.goal_settlement.as_ref().is_none_or(NativeGoalSettlement::drained) { return Ok(last_completion); }
                        continue;
                    }
                    if !had_active {
                        continue;
                    }
                    self.clear_wait().await?;
                    if self.stopping {
                        if self.cleanup.is_none() && pending_command.is_empty() && self.goal_settlement.as_ref().is_none_or(NativeGoalSettlement::drained) { return Ok(Value::Null); }
                        continue;
                    }
                    self.finalization_attempt += 1;
                    anyhow::ensure!(
                        self.finalization_attempt <= MAX_FINALIZATION_ATTEMPT,
                        "Codex child synthesis exceeded {MAX_FINALIZATION_ATTEMPT} attempts"
                    );
                    parent_turn = Some(self.start_parent_turn(
                        "All child agents have finished. Synthesize their results and answer the user. Do not merely report that the children completed.".into(), None
                    ).await?);
                }
            }
        }
    }

    /// Admit a follow-up turn and retain native activity received before its acknowledgment.
    async fn start_parent_turn(
        &mut self,
        text: String,
        submission: Option<std::sync::Arc<super::super::submission::Submission>>,
    ) -> Result<ParentTurn> {
        // Prepare the next turn without borrowing command authority from its predecessor.
        let mut request = self.request.clone();
        request.submission = submission;
        let input = crate::attachment::codex_input(text, self.request.input.attachments()).await?;
        let (turn, provider_started, observed_message_list) = self
            .backend
            .start_turn(
                self.process,
                self.output,
                &request,
                self.thread_id,
                true,
                CodexBackend::with_model(
                    CodexBackend::secure(
                        json!({
                            "threadId": self.thread_id,
                            "input": input,
                            "cwd": self.request.workspace,
                            "effort": self.request.effort,
                            "serviceTier": self.request.service_tier
                        }),
                        self.request,
                    ),
                    &self.request.model,
                ),
            )
            .await?;
        let id = turn
            .pointer("/turn/id")
            .or_else(|| turn.get("turnId"))
            .or_else(|| turn.get("turn_id"))
            .and_then(Value::as_str)
            .context("Codex follow-up turn omitted turn id")?
            .to_owned();
        // Bind early provider activity to the acknowledged turn before replaying it.
        self.owned_turn
            .insert((self.thread_id.into(), id.clone().into()));
        let mut parent = ParentTurn {
            id,
            provider_started,
            completion: None,
        };
        for message in observed_message_list {
            self.descendant.observe(&message, self.thread_id);
            self.observe_native_state(&message)?;
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
        let input = if command.operation == ActiveTurnOperation::Steer {
            match crate::attachment::codex_input(steering_prompt(&command), &command.attachments)
                .await
            {
                Ok(input) => Some(input),
                Err(error) => {
                    command.complete(Err(error)).await;
                    return;
                }
            }
        } else {
            None
        };
        let (method, mut params) = match command.operation {
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
                    "input": input,
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
        if let Some(submission) = &command.submission {
            params["clientUserMessageId"] = json!(submission.id());
            let target = super::super::submission::SubmissionTarget::Codex {
                thread_id: thread_id.into(),
            };
            if let Err(error) = submission
                .persist(move |submission| submission.sending(target))
                .await
            {
                command.complete(Err(error)).await;
                return;
            }
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
        if message.get("method").and_then(Value::as_str) == Some("turn/completed")
            && let (Some(thread), Some(turn)) = (
                message_thread_id(message),
                message.pointer("/params/turn/id").and_then(Value::as_str),
            )
        {
            self.completed_turn.insert((thread.into(), turn.into()));
        }
        settle_interruptions(
            &mut self.interrupted,
            &self.completed_turn,
            tokio::time::Instant::now(),
        )
        .await;
        let Some(request_id) = CodexJsonRpc::response_id(message) else {
            return Ok(());
        };
        let Some(command) = pending_command.remove(&request_id) else {
            return Ok(());
        };
        let mut result = CodexJsonRpc::request_result(message, request_id, "active turn command")
            .context("Codex active-turn command response omitted its result")?
            .map(drop);
        if let Some(submission) = &command.submission {
            match &result {
                Ok(()) => {
                    let receipt = format!("turn/steer:{message}");
                    result = submission
                        .persist(move |submission| submission.accepted(receipt))
                        .await;
                }
                Err(error)
                    if matches!(
                        message.pointer("/error/code").and_then(Value::as_i64),
                        Some(-32602..=-32600)
                    ) =>
                {
                    let error = error.to_string();
                    submission
                        .persist(move |submission| submission.rejected(error))
                        .await?;
                }
                Err(_) => {}
            }
        }
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

    /// Collect native turns and their exact command processes before releasing execution ownership.
    async fn start_cleanup(&mut self, command: SteerCommand, parent_turn_id: Option<&str>) {
        if self.cleanup.is_some() {
            command
                .complete(Err(anyhow::anyhow!(
                    "Codex descendant cleanup is already running"
                )))
                .await;
            return;
        }
        // Capture the owned execution tree before requesting native interruption.
        let thread = self.descendant.active_thread_list();
        self.stopping = true;
        self.process.cancel_request_work();
        if thread.is_empty()
            && parent_turn_id.is_none()
            && !self.provider_goal_active
            && self.command_item.is_empty()
        {
            command
                .complete(self.process.collect_requests(self.output).await)
                .await;
            return;
        }
        let mut cleanup = DescendantCleanup {
            command,
            thread: thread.into_iter().collect(),
            target: HashSet::new(),
            pending_request: HashMap::new(),
            deadline: tokio::time::Instant::now() + std::time::Duration::from_secs(30),
            parent: parent_turn_id.map(|turn_id| crate::backend::SteerTarget {
                thread_id: self.thread_id.to_owned(),
                turn_id: turn_id.to_owned(),
            }),
            command_item: self.command_item.clone(),
            terminal_verified: HashSet::new(),
            terminal_found: HashSet::new(),
            terminal_cursor: HashMap::new(),
        };
        // Suspend native continuation before collecting turn and command acknowledgments.
        if self.provider_goal_active {
            match self
                .process
                .send_request(
                    "thread/goal/set",
                    json!({
                        "threadId": self.thread_id, "status": "paused"
                    }),
                )
                .await
            {
                Ok(request_id) => {
                    cleanup
                        .pending_request
                        .insert(request_id, CleanupRequest::GoalPause);
                }
                Err(error) => {
                    cleanup.command.complete(Err(error)).await;
                    return;
                }
            }
        }
        self.cleanup = Some(cleanup);
        self.admit_cleanup_targets().await;
        if let Err(error) = self.process.collect_requests(self.output).await
            && let Some(cleanup) = self.cleanup.take()
        {
            cleanup.command.complete(Err(error)).await;
        }
    }

    /// Validate the accepted control against the current task before stopping its native goal.
    fn goal_control_is_current(&self) -> bool {
        let Some(control) = self.output.completion.goal_control.as_ref() else {
            return false;
        };
        self.request
            .control_context
            .as_ref()
            .and_then(|context| context.task_controller.as_ref())
            .and_then(|controller| controller.current().ok().flatten())
            .is_some_and(|task| control.permit.validate(Some(&task)).is_ok())
    }

    /// Stop native auto-continuation only after canonical goal control was accepted.
    async fn synchronize_goal_control(&mut self) -> Result<()> {
        if self.native_goal != NativeGoalLifetime::Active
            || self.goal_settlement.is_some()
            || !self.goal_control_is_current()
        {
            return Ok(());
        }
        let termination = self
            .output
            .completion
            .goal_control
            .as_ref()
            .unwrap()
            .termination;
        let request_id = self
            .process
            .send_request(
                "thread/goal/set",
                json!({
                    "threadId":self.thread_id,
                    "status":match termination {
                        crate::goal::GoalTermination::Complete => "complete",
                        crate::goal::GoalTermination::Blocked => "blocked",
                    },
                    "origin":"automatic",
                }),
            )
            .await?;
        self.goal_settlement = Some(NativeGoalSettlement {
            request_id,
            termination,
            acknowledged: false,
            terminal_evidence: None,
            interrupted_turn: HashSet::new(),
            pending_interrupt: HashMap::new(),
        });
        Ok(())
    }

    /// Interrupt raced automatic turns while retaining their terminal acknowledgments.
    async fn drain_settled_goal_turn(&mut self, message: &Value) -> Result<bool> {
        let current = self.goal_control_is_current();
        let Some(settlement) = self.goal_settlement.as_mut() else {
            return Ok(false);
        };
        if let Some(turn_id) =
            CodexBackend::notification_thread_turn_id(message, "turn/started", self.thread_id)
        {
            if !current {
                return Ok(true);
            }
            if settlement.interrupted_turn.insert(turn_id.into()) {
                let request_id = self
                    .process
                    .send_request(
                        "turn/interrupt",
                        json!({
                            "threadId":self.thread_id, "turnId":turn_id,
                        }),
                    )
                    .await?;
                settlement
                    .pending_interrupt
                    .insert(request_id, turn_id.into());
            }
            return Ok(true);
        }
        if let Some(turn_id) =
            CodexBackend::notification_thread_turn_id(message, "turn/completed", self.thread_id)
        {
            return Ok(settlement.interrupted_turn.remove(turn_id));
        }
        Ok(false)
    }

    /// Retain owned native command and goal evidence before acknowledging terminal cleanup.
    fn observe_native_state(&mut self, message: &Value) -> Result<()> {
        // Retain exact command ownership even after the provider ends its turn.
        if let (Some(thread_id), Some(turn_id)) =
            (message_thread_id(message), message_turn_id(message))
        {
            if message.get("method").and_then(Value::as_str) == Some("turn/completed")
                && let Some(cleanup) = self.cleanup.as_mut()
                && cleanup.command_item.contains_key(thread_id)
            {
                cleanup.terminal_verified.remove(thread_id);
            }
            if message.get("method").and_then(Value::as_str) == Some("turn/started")
                && (thread_id == self.thread_id
                    || self
                        .descendant
                        .active_turn_by_thread
                        .contains_key(thread_id))
            {
                self.owned_turn.insert((thread_id.into(), turn_id.into()));
            }
            if message.get("method").and_then(Value::as_str) == Some("item/started")
                && message.pointer("/params/item/type").and_then(Value::as_str)
                    == Some("commandExecution")
                && let Some(item_id) = message.pointer("/params/item/id").and_then(Value::as_str)
            {
                self.observed_command
                    .entry((thread_id.into(), turn_id.into()))
                    .or_default()
                    .insert(item_id.into());
            }
        }
        // Promote early observations only after trusted ancestry identifies their exact turn.
        for thread_id in &self.descendant.owned_thread {
            if let Some(turn) = self.descendant.observed_turn.get(thread_id) {
                self.owned_turn
                    .insert((thread_id.clone(), turn.id().into()));
            }
        }
        for (owner, item) in &self.observed_command {
            if !self.owned_turn.contains(owner) {
                continue;
            }
            let retained = self.command_item.entry(owner.0.clone()).or_default();
            for item_id in item {
                if retained.insert(item_id.clone())
                    && let Some(cleanup) = self.cleanup.as_mut()
                {
                    cleanup
                        .command_item
                        .entry(owner.0.clone())
                        .or_default()
                        .insert(item_id.clone());
                    cleanup.terminal_verified.remove(&owner.0);
                }
            }
        }
        // Reconcile goal settlement independently from native command ownership.
        let observed_state = native_goal_state(message, self.thread_id);
        if let Some(state) = observed_state {
            self.provider_goal_active = state == crate::goal::GoalState::Active;
        }
        self.native_goal.observe(message, self.thread_id);
        let current = self.goal_control_is_current();
        let Some(settlement) = self.goal_settlement.as_mut() else {
            return Ok(());
        };
        if let Some(state) = observed_state.filter(|state| *state != crate::goal::GoalState::Active)
        {
            settlement.terminal_evidence = Some(state);
        }
        if let Some(result) = CodexJsonRpc::request_result(
            message,
            settlement.request_id,
            "persist explicitly terminated native goal",
        ) {
            result?;
            settlement.acknowledged = true;
            self.native_goal = NativeGoalLifetime::Settled;
            self.provider_goal_active = false;
        }
        if let Some(request_id) = CodexJsonRpc::response_id(message)
            && let Some(turn_id) = settlement.pending_interrupt.remove(&request_id)
        {
            let result = CodexJsonRpc::request_result(
                message,
                request_id,
                "interrupt raced native goal turn",
            )
            .context("native goal interrupt response omitted its result")?;
            // Natural completion can win the interrupt race; only this exact turn's evidence settles it.
            if settlement.interrupted_turn.contains(&turn_id) {
                result?;
            }
        }
        // Acknowledgment persists the admitted outcome even when its notification arrives later.
        if settlement.acknowledged && current {
            self.native_goal = NativeGoalLifetime::Settled;
            self.provider_goal_active = false;
            self.output.completion.evidence.native_state = Some(
                settlement
                    .terminal_evidence
                    .unwrap_or(match settlement.termination {
                        crate::goal::GoalTermination::Complete => crate::goal::GoalState::Complete,
                        crate::goal::GoalTermination::Blocked => crate::goal::GoalState::Blocked,
                    }),
            );
        }
        Ok(())
    }

    async fn admit_cleanup_targets(&mut self) {
        let Some(mut cleanup) = self.cleanup.take() else {
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
                    cleanup
                        .target
                        .insert((target.thread_id.clone(), target.turn_id.clone()));
                    cleanup
                        .pending_request
                        .insert(request_id, CleanupRequest::TurnInterrupt { target });
                }
                Err(error) => {
                    cleanup.command.complete(Err(error)).await;
                    return;
                }
            }
        }
        self.cleanup = Some(cleanup);
        self.settle_cleanup(tokio::time::Instant::now()).await;
    }

    /// Decode cleanup acknowledgments and verify owned process absence independently of turn completion.
    async fn complete_cleanup_request(&mut self, message: &Value) -> Result<()> {
        // Match the acknowledgment to its exact native cleanup request.
        let Some(request_id) = CodexJsonRpc::response_id(message) else {
            self.settle_cleanup(tokio::time::Instant::now()).await;
            return Ok(());
        };
        let Some(mut cleanup) = self.cleanup.take() else {
            return Ok(());
        };
        let Some(request) = cleanup.pending_request.remove(&request_id) else {
            self.cleanup = Some(cleanup);
            return Ok(());
        };
        // Advance only the inventory or termination operation that owns this acknowledgment.
        match CodexJsonRpc::request_result(message, request_id, "child cleanup")
            .context("Codex child cleanup response omitted its result")
        {
            Ok(Ok(result)) => {
                match request {
                    CleanupRequest::GoalPause | CleanupRequest::TurnInterrupt { .. } => {}
                    CleanupRequest::TerminalTerminate { thread_id } => {
                        // A false response can mean natural exit; only another inventory proves absence.
                        anyhow::ensure!(
                            result.get("terminated").and_then(Value::as_bool).is_some(),
                            "native terminal termination omitted its outcome"
                        );
                        cleanup.terminal_verified.remove(&thread_id);
                    }
                    CleanupRequest::TerminalList { thread_id } => {
                        let rows = result
                            .get("data")
                            .and_then(Value::as_array)
                            .context("native terminal inventory is missing")?;
                        for row in rows {
                            let item_id = row
                                .get("itemId")
                                .and_then(Value::as_str)
                                .context("native terminal item identity is missing")?;
                            if !cleanup.command_item.get(&thread_id).is_some_and(|owned| {
                                owned.contains(&crate::backend::ToolId::from(item_id))
                            }) {
                                continue;
                            }
                            let process_id = row
                                .get("processId")
                                .and_then(Value::as_str)
                                .context("native terminal process identity is missing")?;
                            cleanup.terminal_found.insert(thread_id.clone());
                            let request_id = self
                                .process
                                .send_request(
                                    "thread/backgroundTerminals/terminate",
                                    json!({"threadId":thread_id,"processId":process_id}),
                                )
                                .await?;
                            cleanup.pending_request.insert(
                                request_id,
                                CleanupRequest::TerminalTerminate {
                                    thread_id: thread_id.clone(),
                                },
                            );
                        }
                        if let Some(cursor) = result.get("nextCursor").and_then(Value::as_str) {
                            anyhow::ensure!(
                                cleanup
                                    .terminal_cursor
                                    .entry(thread_id.clone())
                                    .or_default()
                                    .insert(cursor.into()),
                                "native terminal inventory repeated a pagination cursor"
                            );
                            let request_id = self
                                .process
                                .send_request(
                                    "thread/backgroundTerminals/list",
                                    json!({"threadId":thread_id,"cursor":cursor}),
                                )
                                .await?;
                            cleanup
                                .pending_request
                                .insert(request_id, CleanupRequest::TerminalList { thread_id });
                        } else if !cleanup.terminal_found.contains(&thread_id) {
                            cleanup.terminal_verified.insert(thread_id);
                        }
                    }
                }
                self.cleanup = Some(cleanup);
            }
            Ok(Err(error)) => {
                // Natural completion settles only the exact turn targeted by this rejected interrupt.
                let completed = match request {
                    CleanupRequest::TurnInterrupt { target } => self
                        .completed_turn
                        .contains(&(target.thread_id, target.turn_id)),
                    CleanupRequest::GoalPause
                    | CleanupRequest::TerminalList { .. }
                    | CleanupRequest::TerminalTerminate { .. } => false,
                };
                if completed {
                    self.cleanup = Some(cleanup);
                } else {
                    cleanup.command.complete(Err(error)).await;
                    return Ok(());
                }
            }
            Err(error) => {
                self.cleanup = Some(cleanup);
                return Err(error);
            }
        }
        self.settle_cleanup(tokio::time::Instant::now()).await;
        Ok(())
    }

    /// Release cleanup only after native turns and every owned command inventory have settled.
    async fn settle_cleanup(&mut self, now: tokio::time::Instant) {
        let Some(mut cleanup) = self.cleanup.take() else {
            return;
        };
        // Reconcile only command identities observed under this invocation's admitted turns.
        let scan_thread = cleanup
            .command_item
            .keys()
            .filter(|thread_id| {
                !cleanup.terminal_verified.contains(*thread_id)
                    && !cleanup
                        .pending_request
                        .values()
                        .any(|request| match request {
                            CleanupRequest::GoalPause | CleanupRequest::TurnInterrupt { .. } => {
                                false
                            }
                            CleanupRequest::TerminalList { thread_id: pending }
                            | CleanupRequest::TerminalTerminate { thread_id: pending } => {
                                pending == *thread_id
                            }
                        })
            })
            .cloned()
            .collect::<Vec<_>>();
        for thread_id in scan_thread {
            cleanup.terminal_found.remove(&thread_id);
            cleanup.terminal_cursor.remove(&thread_id);
            match self
                .process
                .send_request(
                    "thread/backgroundTerminals/list",
                    json!({"threadId":thread_id}),
                )
                .await
            {
                Ok(request_id) => {
                    cleanup
                        .pending_request
                        .insert(request_id, CleanupRequest::TerminalList { thread_id });
                }
                Err(error) => {
                    cleanup.command.complete(Err(error)).await;
                    return;
                }
            }
        }
        // Commit completion only after every native effect has positive terminal evidence.
        if cleanup.pending_request.is_empty()
            && cleanup
                .command_item
                .keys()
                .all(|thread_id| cleanup.terminal_verified.contains(thread_id))
            && cleanup.parent.as_ref().is_none_or(|parent| {
                self.completed_turn
                    .contains(&(parent.thread_id.clone(), parent.turn_id.clone()))
            })
            && cleanup.thread.iter().all(|thread_id| {
                !self
                    .descendant
                    .active_turn_by_thread
                    .contains_key(thread_id)
            })
        {
            cleanup.command.complete(Ok(())).await;
        } else if now >= cleanup.deadline {
            cleanup
                .command
                .complete(Err(anyhow::anyhow!(
                    "Codex descendant cleanup did not reach terminal state within 30 seconds"
                )))
                .await;
        } else {
            self.cleanup = Some(cleanup);
        }
    }

    async fn begin_wait(&mut self) -> Result<()> {
        self.waiting = true;
        self.emit_boundary(crate::backend::ParentBoundaryUpdate::WaitStarted {
            agent_count: self.descendant.active_count(),
        })
        .await
    }

    async fn refresh_wait(&self) -> Result<()> {
        self.emit_boundary(crate::backend::ParentBoundaryUpdate::WaitUpdated {
            agent_count: self.descendant.active_count(),
        })
        .await
    }

    async fn clear_wait(&mut self) -> Result<()> {
        if self.waiting {
            self.waiting = false;
            self.emit_boundary(crate::backend::ParentBoundaryUpdate::WaitFinished)
                .await?;
        }
        Ok(())
    }

    async fn emit_boundary(&self, update: crate::backend::ParentBoundaryUpdate) -> Result<()> {
        if let Some(event_sink) = self.event_sink.as_ref() {
            event_sink
                .send_wait(BackendEvent::ParentBoundary {
                    scope: event_sink.invocation_scope()?.clone(),
                    update,
                })
                .await?;
        }
        Ok(())
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

    #[derive(Clone, Copy)]
    enum CommandStartTiming {
        Initial,
        BeforeFollowupAcknowledgment,
        ChildBeforeSpawnAcknowledgment,
    }

    #[derive(Clone, Copy)]
    enum InterruptReply {
        Acknowledged,
        AcknowledgedWithServerRequests,
        RejectedAfterCompletion,
        RejectedAfterUnrelatedCompletion,
    }

    /// Keep cancellation pending until exact owned commands disappear, preserving older terminals.
    #[tokio::test]
    async fn cancellation_collects_owned_native_commands_after_turn_completion() -> Result<()> {
        assert_owned_command_cleanup(CommandStartTiming::Initial, InterruptReply::Acknowledged)
            .await
    }

    /// Preserve commands received before a follow-up turn acknowledges its identity.
    #[tokio::test]
    async fn cancellation_collects_commands_started_before_followup_acknowledgment() -> Result<()> {
        assert_owned_command_cleanup(
            CommandStartTiming::BeforeFollowupAcknowledgment,
            InterruptReply::Acknowledged,
        )
        .await
    }

    /// Accept a rejected interrupt only after its exact target has already completed.
    #[tokio::test]
    async fn cancellation_accepts_interrupt_rejection_after_exact_turn_completion() -> Result<()> {
        assert_owned_command_cleanup(
            CommandStartTiming::Initial,
            InterruptReply::RejectedAfterCompletion,
        )
        .await
    }

    /// Preserve an interrupt failure when only another turn has completed.
    #[tokio::test]
    async fn cancellation_rejects_interrupt_error_after_unrelated_turn_completion() -> Result<()> {
        assert_owned_command_cleanup(
            CommandStartTiming::Initial,
            InterruptReply::RejectedAfterUnrelatedCompletion,
        )
        .await
    }

    /// Preserve cleanup acknowledgment ownership when server requests reuse client request IDs.
    #[tokio::test]
    async fn cancellation_ignores_server_requests_reusing_cleanup_response_ids() -> Result<()> {
        assert_owned_command_cleanup(
            CommandStartTiming::Initial,
            InterruptReply::AcknowledgedWithServerRequests,
        )
        .await
    }

    /// Collect a completed child's early command only after its parent proves the child relationship.
    #[tokio::test]
    async fn cancellation_collects_early_child_commands_after_trusted_spawn() -> Result<()> {
        assert_owned_command_cleanup(
            CommandStartTiming::ChildBeforeSpawnAcknowledgment,
            InterruptReply::Acknowledged,
        )
        .await
    }

    /// Retain the reader for an earlier steering acknowledgment after child cleanup finishes.
    #[tokio::test]
    async fn cleanup_retains_reader_until_pending_steer_acknowledgment() -> Result<()> {
        use crate::backend::approval::PermissionCoordinator;
        use crate::backend::steering::SteeringLane;
        use crate::session::ApprovalMode;
        use crate::trace::TraceStore;
        use futures_util::{SinkExt, StreamExt};
        use std::sync::Arc;
        let directory = tempfile::tempdir()?;
        let workspace = directory.path().to_string_lossy().into_owned();
        let permission = PermissionCoordinator::transient(&workspace)?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let backend = CodexBackend::new_with_permission_coordinator(
            vec!["unused".into()],
            permission.clone(),
            trace.clone(),
            None,
        )?;
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let (steer_seen, observed_steer) = tokio::sync::oneshot::channel();
        let (release_steer, released_steer) = tokio::sync::oneshot::channel();
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await?;
            let mut socket = tokio_tungstenite::accept_async(stream).await?;
            let steer: Value =
                serde_json::from_str(socket.next().await.context("steer missing")??.to_text()?)?;
            assert_eq!(steer["method"], "turn/steer");
            socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":steer["id"],"method":"item/tool/call","params":{"threadId":"parent","turnId":"current","callId":"colliding-steer-call","tool":"goal_status","arguments":{}}}).to_string().into())).await?;
            let server_response: Value = serde_json::from_str(
                socket
                    .next()
                    .await
                    .context("colliding server request was not answered")??
                    .to_text()?,
            )?;
            assert_eq!(server_response["id"], steer["id"]);
            steer_seen.send(()).unwrap();
            let mut interrupt = Vec::new();
            for _ in 0..2 {
                let request: Value = serde_json::from_str(
                    socket
                        .next()
                        .await
                        .context("cleanup interrupt missing")??
                        .to_text()?,
                )?;
                assert_eq!(request["method"], "turn/interrupt");
                interrupt.push(request);
            }
            for request in interrupt {
                socket
                    .send(tokio_tungstenite::tungstenite::Message::Text(
                        json!({"id":request["id"],"result":{}}).to_string().into(),
                    ))
                    .await?;
            }
            for message in [
                json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"current","status":"interrupted"}}}),
                json!({"method":"turn/completed","params":{"threadId":"child","turn":{"id":"child-turn","status":"interrupted"}}}),
            ] {
                socket
                    .send(tokio_tungstenite::tungstenite::Message::Text(
                        message.to_string().into(),
                    ))
                    .await?;
            }
            released_steer.await?;
            socket
                .send(tokio_tungstenite::tungstenite::Message::Text(
                    json!({"id":steer["id"],"result":{}}).to_string().into(),
                ))
                .await?;
            let _ = socket.next().await;
            Ok::<_, anyhow::Error>(())
        });
        let mut process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Ask,
            permission,
            None,
            trace,
            "session".into(),
        )
        .await?;
        process
            .normalization
            .bind(Some(crate::backend::InvocationScope::new(
                crate::backend::ExecutionOwner::root("session"),
                "exchange",
            )));
        let request = BackendRequest {
            execution_owner: None,
            harness_session_id: "session".into(),
            workspace,
            input: crate::backend::BackendInput::from_text("run child"),
            mode: crate::backend::PromptMode::Chat,
            model: "fixture-model".into(),
            effort: "low".into(),
            context_window: None,
            access: Default::default(),
            service_tier: crate::backend::ServiceTier::Standard,
            approval_mode: ApprovalMode::Ask,
            backend_session_id: Some("parent".into()),
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        let lane = SteeringLane::default();
        let mut steering = lane.activate(None)?;
        let finished = Arc::new(std::sync::atomic::AtomicBool::new(false));
        let observed_finished = finished.clone();
        let mut output = BackendOutput::default();
        let run = async {
            let result = CodexTurnCoordinator::new(&backend, &mut process, &mut output, &mut steering, None, &request, "parent").run("current".into(), true, vec![
                json!({"method":"item/completed","params":{"threadId":"parent","item":{"type":"collabAgentToolCall","tool":"spawnAgent","receiverThreadIds":["child"],"status":"completed"}}}),
                json!({"method":"turn/started","params":{"threadId":"child","turn":{"id":"child-turn"}}}),
            ]).await?;
            finished.store(true, std::sync::atomic::Ordering::Release);
            Ok::<_, anyhow::Error>(result)
        };
        let control = async {
            let steer_lane = lane.clone();
            let steer = tokio::spawn(async move { steer_lane.steer("correction".into()).await });
            observed_steer.await?;
            lane.cleanup_execution().await?;
            tokio::task::yield_now().await;
            assert!(
                !observed_finished.load(std::sync::atomic::Ordering::Acquire),
                "cleanup dropped the reader before the earlier steering acknowledgment"
            );
            assert!(
                !steer.is_finished(),
                "steering completed without its native acknowledgment"
            );
            release_steer.send(()).unwrap();
            steer.await??;
            Ok::<_, anyhow::Error>(())
        };
        let (result, ()) = tokio::time::timeout(std::time::Duration::from_secs(3), async {
            tokio::try_join!(run, control)
        })
        .await??;
        assert_eq!(result["turn"]["status"], "interrupted");
        drop(process);
        server.await??;
        Ok(())
    }

    /// Exercise native command ownership and acknowledgment order over the real Codex transport.
    async fn assert_owned_command_cleanup(
        timing: CommandStartTiming,
        reply: InterruptReply,
    ) -> Result<()> {
        use crate::backend::approval::PermissionCoordinator;
        use crate::backend::steering::SteeringLane;
        use crate::session::ApprovalMode;
        use crate::trace::TraceStore;
        use futures_util::{SinkExt, StreamExt};
        use std::sync::Arc;
        let directory = tempfile::tempdir()?;
        let workspace = directory.path().to_string_lossy().into_owned();
        let permission = PermissionCoordinator::transient(&workspace)?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let backend = CodexBackend::new_with_permission_coordinator(
            vec!["unused".into()],
            permission.clone(),
            trace.clone(),
            None,
        )?;
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let completed = Arc::new(std::sync::atomic::AtomicBool::new(false));
        let observed_completed = completed.clone();
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await?;
            let mut socket = tokio_tungstenite::accept_async(stream).await?;
            if let CommandStartTiming::BeforeFollowupAcknowledgment = timing {
                let start: Value = serde_json::from_str(
                    socket
                        .next()
                        .await
                        .context("follow-up start missing")??
                        .to_text()?,
                )?;
                assert_eq!(start["method"], "turn/start");
                for message in [
                    json!({"method":"item/started","params":{"threadId":"parent","turnId":"earlier-turn","item":{"id":"earlier-command","type":"commandExecution","processId":"unrelated","command":"keep running","cwd":".","status":"inProgress"}}}),
                    json!({"method":"item/started","params":{"threadId":"parent","turnId":"current","item":{"id":"owned-command","type":"commandExecution","processId":null,"command":"slow command","cwd":".","status":"inProgress"}}}),
                    json!({"id":start["id"],"result":{"turn":{"id":"current"}}}),
                ] {
                    socket
                        .send(tokio_tungstenite::tungstenite::Message::Text(
                            message.to_string().into(),
                        ))
                        .await?;
                }
            }
            let interrupt: Value = serde_json::from_str(
                socket
                    .next()
                    .await
                    .context("interrupt missing")??
                    .to_text()?,
            )?;
            assert_eq!(interrupt["method"], "turn/interrupt");
            assert_eq!(interrupt["params"]["threadId"], "parent");
            let inventory: Value = serde_json::from_str(
                socket
                    .next()
                    .await
                    .context("inventory missing")??
                    .to_text()?,
            )?;
            assert_eq!(inventory["method"], "thread/backgroundTerminals/list");
            let expected_thread = match timing {
                CommandStartTiming::ChildBeforeSpawnAcknowledgment => "child",
                CommandStartTiming::Initial | CommandStartTiming::BeforeFollowupAcknowledgment => {
                    "parent"
                }
            };
            assert_eq!(inventory["params"]["threadId"], expected_thread);
            if let InterruptReply::AcknowledgedWithServerRequests = reply {
                for request in [&interrupt, &inventory] {
                    socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":request["id"],"method":"item/tool/call","params":{"threadId":"parent","turnId":"current","callId":"colliding-server-call","tool":"goal_status","arguments":{}}}).to_string().into())).await?;
                    let response: Value = serde_json::from_str(
                        socket
                            .next()
                            .await
                            .context("server request response missing")??
                            .to_text()?,
                    )?;
                    assert_eq!(response["id"], request["id"]);
                    assert!(
                        response.get("error").is_some(),
                        "cancelled server tool request was not rejected normally"
                    );
                }
            }
            if let InterruptReply::RejectedAfterUnrelatedCompletion = reply {
                for message in [
                    json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"unrelated-turn","status":"completed"}}}),
                    json!({"id":interrupt["id"],"error":{"code":-32600,"message":"interrupt failed"}}),
                    json!({"id":inventory["id"],"result":{"data":[],"nextCursor":null}}),
                    json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"current","status":"interrupted"}}}),
                ] {
                    socket
                        .send(tokio_tungstenite::tungstenite::Message::Text(
                            message.to_string().into(),
                        ))
                        .await?;
                }
                let _ = socket.next().await;
                return Ok::<_, anyhow::Error>(());
            }
            for message in [
                json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"current","status":"interrupted"}}}),
                match reply {
                    InterruptReply::Acknowledged
                    | InterruptReply::AcknowledgedWithServerRequests => {
                        json!({"id":interrupt["id"],"result":{}})
                    }
                    InterruptReply::RejectedAfterCompletion => {
                        json!({"id":interrupt["id"],"error":{"code":-32600,"message":"turn already completed"}})
                    }
                    InterruptReply::RejectedAfterUnrelatedCompletion => {
                        unreachable!("rejected interrupt fixture completed above")
                    }
                },
                json!({"id":inventory["id"],"result":{"data":[{"itemId":"earlier-command","processId":"unrelated","command":"keep running","cwd":"."}],"nextCursor":"owned-page"}}),
            ] {
                socket
                    .send(tokio_tungstenite::tungstenite::Message::Text(
                        message.to_string().into(),
                    ))
                    .await?;
            }
            let page: Value = serde_json::from_str(
                socket
                    .next()
                    .await
                    .context("inventory page missing")??
                    .to_text()?,
            )?;
            assert_eq!(page["params"]["cursor"], "owned-page");
            socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":page["id"],"result":{"data":[{"itemId":"owned-command","processId":"owned-process","command":"slow command","cwd":"."}],"nextCursor":null}}).to_string().into())).await?;
            let terminate: Value = serde_json::from_str(
                socket
                    .next()
                    .await
                    .context("termination missing")??
                    .to_text()?,
            )?;
            assert_eq!(terminate["method"], "thread/backgroundTerminals/terminate");
            assert_eq!(terminate["params"]["processId"], "owned-process");
            assert_eq!(terminate["params"]["threadId"], expected_thread);
            assert!(
                !observed_completed.load(std::sync::atomic::Ordering::Acquire),
                "turn completion released cleanup before native command collection"
            );
            // Natural exit may race termination; absence, rather than the boolean alone, settles it.
            socket
                .send(tokio_tungstenite::tungstenite::Message::Text(
                    json!({"id":terminate["id"],"result":{"terminated":false}})
                        .to_string()
                        .into(),
                ))
                .await?;
            let verify: Value = serde_json::from_str(
                socket
                    .next()
                    .await
                    .context("verification missing")??
                    .to_text()?,
            )?;
            assert_eq!(verify["method"], "thread/backgroundTerminals/list");
            assert!(verify["params"].get("cursor").is_none());
            socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":verify["id"],"result":{"data":[{"itemId":"earlier-command","processId":"unrelated","command":"keep running","cwd":"."}],"nextCursor":null}}).to_string().into())).await?;
            let _ = socket.next().await;
            Ok::<_, anyhow::Error>(())
        });
        let mut process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Ask,
            permission,
            None,
            trace,
            "session".into(),
        )
        .await?;
        process
            .normalization
            .bind(Some(crate::backend::InvocationScope::new(
                crate::backend::ExecutionOwner::root("session"),
                "exchange",
            )));
        let request = BackendRequest {
            execution_owner: None,
            harness_session_id: "session".into(),
            workspace,
            input: crate::backend::BackendInput::from_text("run command"),
            mode: crate::backend::PromptMode::Chat,
            model: "fixture-model".into(),
            effort: "low".into(),
            context_window: None,
            access: Default::default(),
            service_tier: crate::backend::ServiceTier::Standard,
            approval_mode: ApprovalMode::Ask,
            backend_session_id: Some("parent".into()),
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        let lane = SteeringLane::default();
        let mut steering = lane.activate(None)?;
        let cleanup = tokio::spawn(async move {
            lane.cleanup_execution().await?;
            completed.store(true, std::sync::atomic::Ordering::Release);
            Ok::<_, anyhow::Error>(())
        });
        let mut output = BackendOutput::default();
        let mut coordinator = CodexTurnCoordinator::new(
            &backend,
            &mut process,
            &mut output,
            &mut steering,
            None,
            &request,
            "parent",
        );
        let mut observed = vec![
            json!({"method":"turn/started","params":{"threadId":"foreign","turn":{"id":"foreign-turn"}}}),
            json!({"method":"item/started","params":{"threadId":"foreign","turnId":"foreign-turn","item":{"id":"foreign-command","type":"commandExecution","command":"unrelated","cwd":".","status":"inProgress"}}}),
            json!({"method":"item/completed","params":{"threadId":"foreign","item":{"type":"collabAgentToolCall","tool":"spawnAgent","receiverThreadIds":["foreign-child"],"status":"completed"}}}),
            json!({"method":"turn/started","params":{"threadId":"foreign-child","turn":{"id":"foreign-child-turn"}}}),
            json!({"method":"item/started","params":{"threadId":"foreign-child","turnId":"foreign-child-turn","item":{"id":"foreign-child-command","type":"commandExecution","command":"unrelated child","cwd":".","status":"inProgress"}}}),
        ];
        match timing {
            CommandStartTiming::Initial => observed.extend([
                json!({"method":"item/started","params":{"threadId":"parent","turnId":"earlier-turn","item":{"id":"earlier-command","type":"commandExecution","processId":"unrelated","command":"keep running","cwd":".","status":"inProgress"}}}),
                json!({"method":"item/started","params":{"threadId":"parent","turnId":"current","item":{"id":"owned-command","type":"commandExecution","processId":null,"command":"slow command","cwd":".","status":"inProgress"}}}),
            ]),
            CommandStartTiming::ChildBeforeSpawnAcknowledgment => observed.extend([
                json!({"method":"turn/started","params":{"threadId":"child","turn":{"id":"child-turn"}}}),
                json!({"method":"item/started","params":{"threadId":"child","turnId":"child-turn","item":{"id":"owned-command","type":"commandExecution","processId":null,"command":"slow child command","cwd":".","status":"inProgress"}}}),
                json!({"method":"turn/completed","params":{"threadId":"child","turn":{"id":"child-turn","status":"completed"}}}),
                json!({"method":"item/completed","params":{"threadId":"parent","item":{"type":"collabAgentToolCall","tool":"spawnAgent","receiverThreadIds":["child"],"status":"completed"}}}),
            ]),
            CommandStartTiming::BeforeFollowupAcknowledgment => {},
        }
        let result = tokio::time::timeout(std::time::Duration::from_secs(3), async {
            match timing {
                CommandStartTiming::Initial
                | CommandStartTiming::ChildBeforeSpawnAcknowledgment => {
                    coordinator.run("current".into(), true, observed).await
                }
                CommandStartTiming::BeforeFollowupAcknowledgment => {
                    let parent = coordinator
                        .start_parent_turn("follow-up".into(), None)
                        .await?;
                    coordinator
                        .run(parent.id, parent.provider_started, observed)
                        .await
                }
            }
        })
        .await??;
        assert_eq!(result["turn"]["status"], "interrupted");
        let cleanup_result = cleanup.await?;
        match reply {
            InterruptReply::RejectedAfterUnrelatedCompletion => assert!(
                cleanup_result.is_err(),
                "unrelated completion suppressed a genuine interrupt failure"
            ),
            InterruptReply::Acknowledged
            | InterruptReply::AcknowledgedWithServerRequests
            | InterruptReply::RejectedAfterCompletion => cleanup_result?,
        }
        drop(process);
        server.await??;
        Ok(())
    }

    #[test]
    fn harness_goal_does_not_enable_native_reader_lifetime() {
        let mut request = BackendRequest {
            execution_owner: None,
            harness_session_id: "session".into(),
            workspace: String::new(),
            input: crate::backend::BackendInput::from_text("Continue the harness goal"),
            mode: crate::backend::PromptMode::GoalContinuation,
            model: "test".into(),
            effort: "low".into(),
            context_window: None,
            service_tier: crate::backend::ServiceTier::Standard,
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            backend_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: Some(crate::control_tools::ControlTurnContext::inactive(
                crate::backend::PromptMode::GoalContinuation,
            )),
        };
        assert_eq!(
            NativeGoalLifetime::for_request(&request),
            NativeGoalLifetime::Disabled
        );
        request.control_context.as_mut().unwrap().native_goal = Some("Native objective".into());
        assert_eq!(
            NativeGoalLifetime::for_request(&request),
            NativeGoalLifetime::Active
        );
    }

    #[tokio::test]
    async fn pending_lsp_request_keeps_events_and_cleanup_responsive() -> Result<()> {
        use crate::backend::approval::PermissionCoordinator;
        use crate::backend::steering::SteeringLane;
        use crate::backend::{BackendInput, PromptMode};
        use crate::control_tools::{ControlTurnContext, lsp::LspService};
        use crate::session::ApprovalMode;
        use crate::trace::TraceStore;
        use futures_util::{SinkExt, StreamExt};
        use std::sync::{Arc, atomic::AtomicBool};
        use std::time::{Duration, Instant};

        let directory = tempfile::tempdir()?;
        let workspace = directory.path().to_string_lossy().into_owned();
        std::fs::write(directory.path().join("source.rs"), "pub fn source() {}\n")?;
        let permission = PermissionCoordinator::transient(&workspace)?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let backend = CodexBackend::new_with_permission_coordinator(
            vec!["unused".into()],
            permission.clone(),
            trace.clone(),
            None,
        )?;
        let lsp = Arc::new(LspService::default());
        let (sender, mut editor) = forge_protocol::outbound::channel();
        lsp.attach(sender);
        lsp.set_enabled("session", true);
        let policy = Arc::new(
            crate::permissions::matcher::CompiledPermissionDocument::compile(
                crate::permissions::document::default_permission_document(),
                directory.path(),
            ),
        );
        let (scope, _lease) = lsp.scope(
            "session".into(),
            "exchange".into(),
            directory.path().into(),
            policy,
            Arc::new(AtomicBool::new(false)),
        );
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        context.lsp = Some(scope);

        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await?;
            let mut socket = tokio_tungstenite::accept_async(stream).await?;
            for message in [
                json!({"id":99,"method":"item/tool/call","params":{"threadId":"thread","turnId":"turn",
                    "callId":"pending-lsp","tool":"lsp_context","arguments":{"path":"source.rs","line":1,"column":1}}}),
                json!({"method":"item/agentMessage/delta","params":{"threadId":"thread","turnId":"turn",
                    "itemId":"progress","delta":"Provider events continue while LSP is pending."}}),
            ] {
                socket
                    .send(tokio_tungstenite::tungstenite::Message::Text(
                        message.to_string().into(),
                    ))
                    .await?;
            }
            let mut cancelled = false;
            let mut interrupted = false;
            loop {
                let frame = tokio::time::timeout(Duration::from_secs(2), socket.next())
                    .await?
                    .context("provider connection closed before interrupt")??;
                let message: Value = serde_json::from_str(frame.to_text()?)?;
                if message["id"] == 99 {
                    assert!(
                        message.get("error").is_some(),
                        "pending tool reported success during cleanup"
                    );
                    cancelled = true;
                } else if message["method"] == "turn/interrupt" {
                    assert!(
                        !cancelled,
                        "native interrupt waited for tool-result collection"
                    );
                    interrupted = true;
                    for response in [
                        json!({"id":message["id"],"result":{}}),
                        json!({"method":"turn/completed","params":{"threadId":"thread","turn":{"id":"turn","status":"interrupted"}}}),
                    ] {
                        socket
                            .send(tokio_tungstenite::tungstenite::Message::Text(
                                response.to_string().into(),
                            ))
                            .await?;
                    }
                }
                if cancelled && interrupted {
                    break;
                }
            }
            Ok::<_, anyhow::Error>(())
        });
        let (events, mut event_stream) =
            crate::backend::events::channel(crate::backend::InvocationScope::new(
                crate::backend::ExecutionOwner::root("session"),
                "exchange",
            ));
        let mut process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Ask,
            permission,
            Some(events.clone()),
            trace,
            "session".into(),
        )
        .await?;
        process.set_control_context(context).await;
        let request = BackendRequest {
            execution_owner: None,
            harness_session_id: "session".into(),
            workspace,
            input: BackendInput::from_text("wait for diagnostics"),
            mode: PromptMode::Chat,
            model: "fixture".into(),
            effort: "low".into(),
            context_window: None,
            access: Default::default(),
            service_tier: Default::default(),
            approval_mode: ApprovalMode::Ask,
            backend_session_id: Some("thread".into()),
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        let lane = SteeringLane::default();
        let mut steering = lane.activate(None)?;
        let mut output = BackendOutput::default();
        let run = CodexTurnCoordinator::new(
            &backend,
            &mut process,
            &mut output,
            &mut steering,
            Some(events),
            &request,
            "thread",
        )
        .run("turn".into(), true, Vec::new());
        let stop = async {
            let query = editor.recv().await?.context("LSP request was not sent")?;
            let query: Value = serde_json::from_slice(query.bytes())?;
            assert_eq!(query["event"], "lsp.request");
            loop {
                let event = event_stream
                    .recv()
                    .await?
                    .context("provider events stopped behind LSP")?;
                if matches!(event, BackendEvent::Message { .. }) {
                    break;
                }
            }
            let started = Instant::now();
            lane.cleanup_execution().await?;
            assert!(
                started.elapsed() < Duration::from_secs(1),
                "cleanup waited for LSP timeout"
            );
            let cancelled = editor
                .recv()
                .await?
                .context("editor cancellation was not sent")?;
            let cancelled: Value = serde_json::from_slice(cancelled.bytes())?;
            assert_eq!(cancelled["event"], "lsp.cancel");
            assert!(
                lsp.respond(
                    "session",
                    query["payload"]["id"].as_str().unwrap(),
                    json!({}),
                    None
                )
                .is_err(),
                "a late LSP response retained authority after cleanup"
            );
            Ok::<_, anyhow::Error>(())
        };
        let (result, ()) = tokio::time::timeout(Duration::from_secs(3), async {
            tokio::try_join!(run, stop)
        })
        .await??;
        assert_eq!(result["turn"]["status"], "interrupted");
        server.await??;
        Ok(())
    }

    #[tokio::test]
    async fn chat_cancellation_pauses_provider_goal_and_waits_for_acknowledgements() -> Result<()> {
        use crate::backend::approval::PermissionCoordinator;
        use crate::backend::steering::SteeringLane;
        use crate::backend::{BackendInput, PromptMode};
        use crate::session::ApprovalMode;
        use crate::trace::TraceStore;
        use futures_util::{SinkExt, StreamExt};
        use std::sync::Arc;

        let directory = tempfile::tempdir()?;
        let workspace = directory.path().to_string_lossy().into_owned();
        let permission = PermissionCoordinator::transient(&workspace)?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let backend = CodexBackend::new_with_permission_coordinator(
            vec!["unused".into()],
            permission.clone(),
            trace.clone(),
            None,
        )?;
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            let pause: Value =
                serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                    .unwrap();
            assert_eq!(pause["method"], "thread/goal/set");
            assert_eq!(pause["params"]["status"], "paused");
            let interrupt: Value =
                serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                    .unwrap();
            assert_eq!(interrupt["method"], "turn/interrupt");
            for message in [
                json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"current","status":"interrupted"}}}),
                json!({"id":interrupt["id"],"result":{}}),
                json!({"id":pause["id"],"result":{}}),
            ] {
                socket
                    .send(tokio_tungstenite::tungstenite::Message::Text(
                        message.to_string().into(),
                    ))
                    .await
                    .unwrap();
            }
            let _ = socket.next().await;
        });
        let mut process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Ask,
            permission,
            None,
            trace,
            "session".into(),
        )
        .await?;
        process
            .normalization
            .bind(Some(crate::backend::InvocationScope::new(
                crate::backend::ExecutionOwner::root("session"),
                "exchange",
            )));
        let request = BackendRequest {
            execution_owner: None,
            harness_session_id: "session".into(),
            workspace,
            input: BackendInput::from_text("plan out a new one"),
            mode: PromptMode::Chat,
            model: "gpt-5.6-terra".into(),
            effort: "medium".into(),
            context_window: None,
            access: Default::default(),
            service_tier: crate::backend::ServiceTier::Standard,
            approval_mode: ApprovalMode::Ask,
            backend_session_id: Some("parent".into()),
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
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
        use crate::session::ApprovalMode;
        use crate::trace::TraceStore;
        use futures_util::{SinkExt, StreamExt};
        use std::sync::Arc;
        let directory = tempfile::tempdir()?;
        let workspace = directory.path().to_string_lossy().into_owned();
        let permission = PermissionCoordinator::transient(&workspace)?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let backend = CodexBackend::new_with_permission_coordinator(
            vec!["unused".into()],
            permission.clone(),
            trace.clone(),
            None,
        )?;
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            let request: Value =
                serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                    .unwrap();
            assert_eq!(request["method"], "turn/start");
            assert_eq!(request["params"]["serviceTier"], "ultrafast");
            for message in [
                json!({"method":"item/completed","params":{"threadId":"child","turnId":"child-turn","item":{"id":"child-answer","type":"agentMessage","phase":"final_answer","text":"CHILD-PROGRESS"}}}),
                json!({"method":"turn/completed","params":{"threadId":"child","turn":{"id":"child-turn","status":"completed"}}}),
                json!({"method":"turn/started","params":{"threadId":"parent","turn":{"id":"second","status":"inProgress"}}}),
                json!({"method":"item/completed","params":{"threadId":"parent","turnId":"second","item":{"id":"parent-answer","type":"agentMessage","phase":"final_answer","text":"PARENT-DONE"}}}),
                json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"second","status":"completed"}}}),
                json!({"id":request["id"],"result":{"turn":{"id":"second"}}}),
            ] {
                socket
                    .send(tokio_tungstenite::tungstenite::Message::Text(
                        message.to_string().into(),
                    ))
                    .await
                    .unwrap();
            }
            let _ = socket.next().await;
        });
        let mut process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Ask,
            permission,
            None,
            trace,
            "session".into(),
        )
        .await?;
        process
            .normalization
            .bind(Some(crate::backend::InvocationScope::new(
                crate::backend::ExecutionOwner::root("session"),
                "exchange",
            )));
        let request = BackendRequest {
            execution_owner: None,
            harness_session_id: "session".into(),
            workspace,
            input: BackendInput::from_text("parent"),
            mode: PromptMode::Chat,
            model: "gpt-5.6-terra".into(),
            effort: "medium".into(),
            access: Default::default(),
            context_window: None,
            service_tier: crate::backend::ServiceTier::Ultrafast,
            approval_mode: ApprovalMode::Ask,
            backend_session_id: Some("parent".into()),
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        let lane = crate::backend::steering::SteeringLane::default();
        let mut steering = lane.activate(None)?;
        let mut output = BackendOutput {
            event: Vec::new(),
            completion: crate::backend::BackendCompletion {
                backend_session_id: Some("parent".into()),
                ..Default::default()
            },
        };
        let observed = vec![
            json!({"method":"item/completed","params":{"threadId":"parent","item":{"type":"collabAgentToolCall","tool":"spawnAgent","receiverThreadIds":["child"],"status":"completed"}}}),
            json!({"method":"turn/started","params":{"threadId":"child","turn":{"id":"child-turn","status":"inProgress"}}}),
            json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"first","status":"completed"}}}),
        ];
        let coordinator = CodexTurnCoordinator::new(
            &backend,
            &mut process,
            &mut output,
            &mut steering,
            None,
            &request,
            "parent",
        );
        let (completed, acknowledged) =
            tokio::time::timeout(std::time::Duration::from_secs(2), async {
                tokio::join!(
                    coordinator.run("first".into(), true, observed),
                    lane.steer("continue".into())
                )
            })
            .await?;
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
    async fn accepted_goal_controls_persist_native_termination_and_drain_raced_turns() -> Result<()>
    {
        use crate::backend::approval::PermissionCoordinator;
        use crate::backend::steering::SteeringLane;
        use crate::backend::{BackendInput, PromptMode};
        use crate::session::ApprovalMode;
        use crate::trace::TraceStore;
        use futures_util::{SinkExt, StreamExt};
        use std::sync::Arc;

        for (tool, termination) in [
            ("goal_complete", crate::goal::GoalState::Complete),
            ("goal_blocked", crate::goal::GoalState::Blocked),
        ] {
            for (notification_first, interrupt_succeeds, terminal_evidence, ownership_replaced) in [
                (false, true, true, false),
                (true, true, true, false),
                (false, false, true, false),
                (false, false, false, false),
                (false, true, false, true),
            ] {
                let (directory, mut store, controller) =
                    crate::task::SessionTaskController::test_goal();
                let workspace = directory.path().to_string_lossy().into_owned();
                let permission = PermissionCoordinator::transient(&workspace)?;
                let trace = Arc::new(TraceStore::open(directory.path())?);
                let backend = CodexBackend::new_with_permission_coordinator(
                    vec!["unused".into()],
                    Arc::clone(&permission),
                    Arc::clone(&trace),
                    None,
                )?;
                let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
                let endpoint = format!("ws://{}", listener.local_addr()?);
                let server_controller = controller.clone();
                let server = tokio::spawn(async move {
                    let (stream, _) = listener.accept().await.unwrap();
                    let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
                    socket
                        .send(tokio_tungstenite::tungstenite::Message::Text(
                            json!({
                                "id":700, "method":"item/tool/call", "params":{
                                    "threadId":"parent", "turnId":"first", "callId":"finish",
                                    "tool":tool, "arguments":if tool == "goal_complete" {
                                        json!({"summary":"Fixture finished"})
                                    } else {
                                        json!({"reason":"Fixture blocked"})
                                    },
                                }
                            })
                            .to_string()
                            .into(),
                        ))
                        .await
                        .unwrap();
                    let response: Value = serde_json::from_str(
                        socket.next().await.unwrap().unwrap().to_text().unwrap(),
                    )
                    .unwrap();
                    assert_eq!(response["id"], 700);
                    assert_eq!(response["result"]["success"], true, "{response}");
                    let settlement_request: Value = serde_json::from_str(
                        socket.next().await.unwrap().unwrap().to_text().unwrap(),
                    )
                    .unwrap();
                    assert_eq!(settlement_request["method"], "thread/goal/set");
                    assert_eq!(settlement_request["params"]["threadId"], "parent");
                    let status = if tool == "goal_complete" {
                        "complete"
                    } else {
                        "blocked"
                    };
                    assert_eq!(settlement_request["params"]["status"], status);
                    assert_eq!(settlement_request["params"]["origin"], "automatic");
                    if ownership_replaced {
                        let current = server_controller.current().unwrap().unwrap();
                        server_controller
                            .change(
                                &mut store,
                                Some(&current.revision),
                                crate::task::TaskChange::Install(Box::new(
                                    crate::task::ActiveTask::goal(
                                        "New user task".into(),
                                        Default::default(),
                                        true,
                                        2,
                                    ),
                                )),
                                2,
                            )
                            .unwrap();
                        socket
                            .send(tokio_tungstenite::tungstenite::Message::Text(
                                json!({
                                    "method":"turn/started","params":{"threadId":"parent",
                                        "turn":{"id":"new-user-turn","status":"inProgress"}}
                                })
                                .to_string()
                                .into(),
                            ))
                            .await
                            .unwrap();
                        if let Some(Ok(message)) = socket.next().await {
                            assert!(
                                !message.is_text(),
                                "stale owner interrupted the new user turn: {message}"
                            );
                        }
                        return;
                    }
                    // A provider turn can race the settlement acknowledgment after the original ends.
                    for message in [
                        json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"first","status":"completed"}}}),
                        json!({"method":"turn/started","params":{"threadId":"parent","turn":{"id":"raced","status":"inProgress"}}}),
                    ] {
                        socket
                            .send(tokio_tungstenite::tungstenite::Message::Text(
                                message.to_string().into(),
                            ))
                            .await
                            .unwrap();
                    }
                    let notification = json!({"method":"thread/goal/updated","params":{
                        "threadId":"parent","goal":{"status":status}
                    }});
                    let acknowledgment =
                        json!({"id":settlement_request["id"],"result":{"goal":{"status":status}}});
                    let ordered = if notification_first {
                        [notification, acknowledgment]
                    } else {
                        [acknowledgment, notification]
                    };
                    for message in ordered {
                        socket
                            .send(tokio_tungstenite::tungstenite::Message::Text(
                                message.to_string().into(),
                            ))
                            .await
                            .unwrap();
                    }
                    let interrupt: Value = serde_json::from_str(
                        socket.next().await.unwrap().unwrap().to_text().unwrap(),
                    )
                    .unwrap();
                    assert_eq!(interrupt["method"], "turn/interrupt");
                    assert_eq!(interrupt["params"]["turnId"], "raced");
                    socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({
                        "id":interrupt["id"],"method":"item/tool/call","params":{
                            "threadId":"parent","turnId":"raced","callId":"colliding-goal-interrupt",
                            "tool":"goal_status","arguments":{}
                        }
                    }).to_string().into())).await.unwrap();
                    let server_response: Value = serde_json::from_str(
                        socket.next().await.unwrap().unwrap().to_text().unwrap(),
                    )
                    .unwrap();
                    assert_eq!(
                        server_response["id"], interrupt["id"],
                        "colliding server call was not handled independently"
                    );
                    let completed_turn = if terminal_evidence {
                        "raced"
                    } else {
                        "unrelated"
                    };
                    let interrupt_response = if interrupt_succeeds {
                        json!({"id":interrupt["id"],"result":{}})
                    } else {
                        json!({"id":interrupt["id"],"error":{"code":-32602,"message":"turn is not active"}})
                    };
                    for message in [
                        json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":completed_turn,"status":"completed"}}}),
                        interrupt_response,
                    ] {
                        socket
                            .send(tokio_tungstenite::tungstenite::Message::Text(
                                message.to_string().into(),
                            ))
                            .await
                            .unwrap();
                    }
                    let _ = socket.next().await;
                });
                let mut process = CodexJsonRpc::connect(
                    &endpoint,
                    &workspace,
                    ApprovalMode::Ask,
                    permission,
                    None,
                    trace,
                    "session".into(),
                )
                .await?;
                process
                    .normalization
                    .bind(Some(crate::backend::InvocationScope::new(
                        crate::backend::ExecutionOwner::root("session"),
                        "exchange",
                    )));
                let mut context = crate::control_tools::ControlTurnContext::inactive(
                    PromptMode::GoalContinuation,
                );
                context.native_goal = Some("Fixture objective".into());
                context.task_controller = Some(controller.clone());
                process.set_control_context(context.clone()).await;
                let request = BackendRequest {
                    execution_owner: None,
                    harness_session_id: "session".into(),
                    workspace,
                    input: BackendInput::from_text("Finish fixture"),
                    mode: PromptMode::GoalContinuation,
                    model: "test".into(),
                    effort: "low".into(),
                    context_window: None,
                    service_tier: crate::backend::ServiceTier::Standard,
                    access: Default::default(),
                    approval_mode: ApprovalMode::Ask,
                    backend_session_id: Some("parent".into()),
                    submission: None,
                    recovered_input: Vec::new(),
                    control_context: Some(context),
                };
                let lane = SteeringLane::default();
                let mut steering = lane.activate(None)?;
                let mut output = BackendOutput::default();
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
                .await?;
                if ownership_replaced {
                    assert!(result.unwrap_err().is::<crate::task::StaleTask>());
                } else if interrupt_succeeds || terminal_evidence {
                    assert_eq!(result?["turn"]["id"], "first");
                    assert_eq!(output.completion.evidence.native_state, Some(termination));
                } else {
                    assert!(
                        result
                            .unwrap_err()
                            .to_string()
                            .contains("turn is not active")
                    );
                }
                if !ownership_replaced {
                    output
                        .completion
                        .goal_control
                        .as_ref()
                        .unwrap()
                        .permit
                        .validate(controller.current()?.as_ref())?;
                }
                drop(process);
                tokio::time::timeout(std::time::Duration::from_secs(2), server).await??;
            }
        }
        Ok(())
    }

    #[tokio::test]
    async fn rejected_and_stale_goal_controls_preserve_native_continuation() -> Result<()> {
        use crate::backend::approval::PermissionCoordinator;
        use crate::backend::steering::SteeringLane;
        use crate::backend::{BackendInput, PromptMode};
        use crate::session::ApprovalMode;
        use crate::trace::TraceStore;
        use futures_util::{SinkExt, StreamExt};
        use std::sync::Arc;

        for stale in [false, true] {
            let (directory, mut store, controller) =
                crate::task::SessionTaskController::test_goal();
            let previous = controller.current()?.unwrap();
            let mut output = BackendOutput::default();
            output.completion.backend_session_id = Some("parent".into());
            if stale {
                output.completion.goal_control = Some(crate::goal::GoalControl {
                    permit: previous.permit(),
                    termination: crate::goal::GoalTermination::Complete,
                });
                controller.change(
                    &mut store,
                    Some(&previous.revision),
                    crate::task::TaskChange::Pause,
                    2,
                )?;
            }
            let workspace = directory.path().to_string_lossy().into_owned();
            let permission = PermissionCoordinator::transient(&workspace)?;
            let trace = Arc::new(TraceStore::open(directory.path())?);
            let backend = CodexBackend::new_with_permission_coordinator(
                vec!["unused".into()],
                Arc::clone(&permission),
                Arc::clone(&trace),
                None,
            )?;
            let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
            let endpoint = format!("ws://{}", listener.local_addr()?);
            let server = tokio::spawn(async move {
                let (stream, _) = listener.accept().await.unwrap();
                let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
                if !stale {
                    socket
                        .send(tokio_tungstenite::tungstenite::Message::Text(
                            json!({
                                "id":701,"method":"item/tool/call","params":{
                                    "threadId":"parent","turnId":"first","callId":"invalid",
                                    "tool":"goal_complete","arguments":{},
                                }
                            })
                            .to_string()
                            .into(),
                        ))
                        .await
                        .unwrap();
                    let response: Value = serde_json::from_str(
                        socket.next().await.unwrap().unwrap().to_text().unwrap(),
                    )
                    .unwrap();
                    assert_eq!(response["result"]["success"], false);
                }
                for message in [
                    json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"first","status":"completed"}}}),
                    json!({"method":"turn/started","params":{"threadId":"parent","turn":{"id":"second","status":"inProgress"}}}),
                    json!({"method":"thread/goal/updated","params":{"threadId":"parent","goal":{"status":"complete"}}}),
                    json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"second","status":"completed"}}}),
                ] {
                    socket
                        .send(tokio_tungstenite::tungstenite::Message::Text(
                            message.to_string().into(),
                        ))
                        .await
                        .unwrap();
                }
                if let Some(Ok(message)) = socket.next().await {
                    assert!(
                        !message.is_text(),
                        "rejected or stale control stopped native continuation: {message}"
                    );
                }
            });
            let mut process = CodexJsonRpc::connect(
                &endpoint,
                &workspace,
                ApprovalMode::Ask,
                permission,
                None,
                trace,
                "session".into(),
            )
            .await?;
            process
                .normalization
                .bind(Some(crate::backend::InvocationScope::new(
                    crate::backend::ExecutionOwner::root("session"),
                    "exchange",
                )));
            let mut context =
                crate::control_tools::ControlTurnContext::inactive(PromptMode::GoalContinuation);
            context.native_goal = Some("Fixture objective".into());
            context.task_controller = Some(controller);
            process.set_control_context(context.clone()).await;
            let request = BackendRequest {
                execution_owner: None,
                harness_session_id: "session".into(),
                workspace,
                input: BackendInput::from_text("Continue"),
                mode: PromptMode::GoalContinuation,
                model: "test".into(),
                effort: "low".into(),
                context_window: None,
                service_tier: crate::backend::ServiceTier::Standard,
                access: Default::default(),
                approval_mode: ApprovalMode::Ask,
                backend_session_id: Some("parent".into()),
                submission: None,
                recovered_input: Vec::new(),
                control_context: Some(context),
            };
            let lane = SteeringLane::default();
            let mut steering = lane.activate(None)?;
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
            assert_eq!(result["turn"]["id"], "second");
            assert_eq!(
                output.completion.evidence.native_state,
                Some(crate::goal::GoalState::Complete)
            );
            drop(process);
            tokio::time::timeout(std::time::Duration::from_secs(2), server).await??;
        }
        Ok(())
    }

    #[tokio::test]
    async fn native_goal_reader_spans_provider_turns_until_explicit_settlement() -> Result<()> {
        use crate::backend::approval::PermissionCoordinator;
        use crate::backend::steering::SteeringLane;
        use crate::backend::{BackendInput, PromptMode};
        use crate::session::ApprovalMode;
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
                None,
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
                ApprovalMode::Ask,
                permission,
                None,
                trace,
                "session".into(),
            )
            .await?;
            process
                .normalization
                .bind(Some(crate::backend::InvocationScope::new(
                    crate::backend::ExecutionOwner::root("session"),
                    "exchange",
                )));
            let request = BackendRequest {
                execution_owner: None,
                harness_session_id: "session".into(),
                workspace,
                input: BackendInput::from_text("goal"),
                mode: PromptMode::GoalContinuation,
                model: "gpt-5.6-terra".into(),
                effort: "medium".into(),
                context_window: None,
                service_tier: crate::backend::ServiceTier::Standard,
                access: Default::default(),
                approval_mode: ApprovalMode::Ask,
                backend_session_id: Some("parent".into()),
                submission: None,
                recovered_input: Vec::new(),
                control_context: Some({
                    let mut context = crate::control_tools::ControlTurnContext::inactive(
                        PromptMode::GoalContinuation,
                    );
                    context.native_goal = Some("Native reader objective".into());
                    context
                }),
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
            let mut output = BackendOutput {
                event: Vec::new(),
                completion: crate::backend::BackendCompletion {
                    backend_session_id: Some("parent".into()),
                    ..Default::default()
                },
            };
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
                    .any(|event| matches!(event, BackendEvent::Message { update, .. } if update.text() == Some("second turn retained")))
            );
            assert_eq!(
                output.completion.evidence.native_state,
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
    async fn interruption_requires_exact_terminal_evidence_or_fails_at_deadline() {
        use crate::backend::steering::{SteerTarget, SteeringLane};
        for expires in [false, true] {
            let lane = SteeringLane::default();
            let mut active = lane.activate(None).unwrap();
            let target = SteerTarget {
                thread_id: "child".into(),
                turn_id: "turn".into(),
            };
            let request = tokio::spawn(async move { lane.cleanup_execution().await });
            let mut command = active.receive().await.unwrap();
            command.target = Some(target);
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
        tracker.observe(&json!({"method":"item/completed","params":{"threadId":"parent-thread","item":{"type":"collabAgentToolCall","tool":"spawnAgent","receiverThreadIds":["child-one","child-two"],"status":"completed"}}}), "parent-thread");
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

    /// Admit descendants only through a trusted parent, retaining ancestry after completion.
    #[test]
    fn descendant_ownership_rejects_foreign_lifecycle_and_retains_completed_parent_ancestry() {
        let mut tracker = DescendantTracker::default();
        tracker.observe(&json!({"method":"turn/started","params":{"threadId":"foreign","turn":{"id":"foreign-turn"}}}), "parent");
        tracker.observe(&json!({"method":"item/completed","params":{"threadId":"foreign","item":{"type":"collabAgentToolCall","tool":"spawnAgent","receiverThreadIds":["foreign-child"],"status":"completed"}}}), "parent");
        assert!(tracker.owned_thread.is_empty());
        assert_eq!(tracker.active_count(), 0);
        tracker.observe(&json!({"method":"item/completed","params":{"threadId":"parent","item":{"type":"collabAgentToolCall","tool":"spawnAgent","receiverThreadIds":["child"],"status":"completed"}}}), "parent");
        tracker.observe(&json!({"method":"turn/started","params":{"threadId":"child","turn":{"id":"child-turn"}}}), "parent");
        tracker.observe(&json!({"method":"turn/completed","params":{"threadId":"child","turn":{"id":"child-turn"}}}), "parent");
        assert_eq!(tracker.active_count(), 0);
        tracker.observe(&json!({"method":"item/completed","params":{"threadId":"child","item":{"type":"collabAgentToolCall","tool":"spawnAgent","receiverThreadIds":["grandchild"],"status":"completed"}}}), "parent");
        assert!(tracker.owned_thread.contains("grandchild"));
        tracker.observe(&json!({"method":"item/completed","params":{"threadId":"foreign","item":{"type":"collabAgentToolCall","tool":"closeAgent","receiverThreadIds":["grandchild"],"status":"completed"}}}), "parent");
        assert!(tracker.active_turn_by_thread.contains_key("grandchild"));
    }

    /// Reattach a retained child only when the active root explicitly sends or waits for it.
    #[test]
    fn descendant_ownership_accepts_reused_child_from_trusted_parent_operation() {
        for operation in ["sendInput", "wait", "resumeAgent"] {
            let mut tracker = DescendantTracker::default();
            tracker.observe(&json!({"method":"turn/started","params":{"threadId":"retained-child","turn":{"id":"new-child-turn"}}}), "parent");
            assert_eq!(tracker.active_count(), 0);
            tracker.observe(&json!({"method":"item/completed","params":{"threadId":"parent","item":{"type":"collabAgentToolCall","tool":operation,"receiverThreadIds":["retained-child"],"agentsStates":{"retained-child":{"status":"running"}},"status":"completed"}}}), "parent");
            assert_eq!(
                tracker.target_for_thread("retained-child").unwrap().turn_id,
                "new-child-turn"
            );
        }
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
        let mut parent = ParentTurn {
            id: "current".into(),
            provider_started: false,
            completion: None,
        };
        for (thread_id, turn_id) in [("child", "current"), ("parent", "cancelled")] {
            parent.observe(
                &json!({"method":"item/started","params":{
                    "threadId":thread_id,"turnId":turn_id
                }}),
                "parent",
            );
            assert!(!parent.provider_started);
        }
        parent.observe(
            &json!({"method":"item/started","params":{
                "threadId":"parent","turnId":"current"
            }}),
            "parent",
        );
        assert!(parent.provider_started);
    }
}
/// Ignore a previous child turn's completion after its trusted successor starts.
#[test]
fn stale_child_completion_cannot_settle_a_newer_turn() {
    let mut tracker = DescendantTracker::default();
    tracker.observe(
        &json!({"method":"item/completed","params":{"threadId":"parent","item":{"type":"collabAgentToolCall","tool":"spawnAgent","receiverThreadIds":["child"],"status":"completed"}}}),
        "parent",
    );
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
