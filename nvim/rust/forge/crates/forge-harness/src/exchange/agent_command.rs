//! Admit user control through the parent FIFO while child coordinators retain execution.

use std::sync::Arc;

use serde::{Deserialize, Serialize};

use super::command::{Command, CommandFailure};
use super::queue::QueuedCommand;
use super::services::OwnerServices;
use crate::agent::{AgentCoordinator, AgentOperation, AgentRequest, AgentSource, AgentTarget};

/// Control one child without holding the parent slot for the child's answer.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "action", content = "request", rename_all = "snake_case")]
pub(crate) enum AgentCommand {
    /// Admit and start an independent child.
    Start(AgentStartRequest),
    /// Deliver informational input without requiring another child answer.
    SendMessage(AgentMessage),
    /// Admit a new task in the selected child conversation.
    FollowUp(AgentMessage),
    /// Collect the captured child execution or all children present at execution time.
    Interrupt(AgentTargets),
}

impl AgentCommand {
    /// Get whether public admission must reserve a pending child-task slot before FIFO entry.
    pub(crate) fn creates_task(&self) -> bool {
        match self {
            Self::Start(_) | Self::FollowUp(_) => true,
            Self::SendMessage(_) | Self::Interrupt(_) => false,
        }
    }

    /// Capture stable child scope at the caller boundary.
    pub(crate) fn capture(
        operation: AgentOperation,
        children: &AgentCoordinator,
    ) -> Result<Self, AgentFailure> {
        let capture = |target: &str| {
            children
                .capture_target(target)
                .map_err(|_| AgentFailure::StaleTarget)
        };
        match operation {
            AgentOperation::Spawn(request) => {
                if request.text.trim().is_empty() {
                    return Err(AgentFailure::EmptyMessage);
                }
                Ok(Self::Start(AgentStartRequest::try_from(
                    StoredAgentStart {
                        profile: request.profile,
                        text: request.text,
                    },
                )?))
            }
            AgentOperation::Message { target, message } => {
                if message.trim().is_empty() {
                    return Err(AgentFailure::EmptyMessage);
                }
                Ok(Self::SendMessage(AgentMessage::try_from(
                    StoredAgentMessage {
                        target: capture(&target)?,
                        selector: target,
                        text: message,
                    },
                )?))
            }
            AgentOperation::Followup { target, message } => {
                if message.trim().is_empty() {
                    return Err(AgentFailure::EmptyMessage);
                }
                Ok(Self::FollowUp(AgentMessage::try_from(
                    StoredAgentMessage {
                        target: capture(&target)?,
                        selector: target,
                        text: message,
                    },
                )?))
            }
            AgentOperation::Interrupt { target } => Ok(Self::Interrupt(AgentTargets::One {
                target: capture(&target)?,
                selector: target,
            })),
            AgentOperation::InterruptAll => Ok(Self::Interrupt(AgentTargets::All)),
            AgentOperation::List { .. }
            | AgentOperation::Activity { .. }
            | AgentOperation::ToolOutput { .. } => Err(AgentFailure::InspectionOnly),
        }
    }

    /// Compare retry intent against the original selector without recapturing mutable child state.
    pub(crate) fn matches_operation(&self, operation: &AgentOperation) -> bool {
        match (self, operation) {
            (Self::Start(start), AgentOperation::Spawn(request)) => {
                start.profile == request.profile && start.text == request.text
            }
            (Self::SendMessage(saved), AgentOperation::Message { target, message })
            | (Self::FollowUp(saved), AgentOperation::Followup { target, message }) => {
                (saved.selector == *target || saved.target.agent_id() == target)
                    && saved.text == *message
            }
            (
                Self::Interrupt(AgentTargets::One {
                    target: saved,
                    selector,
                }),
                AgentOperation::Interrupt { target },
            ) => selector == target || saved.agent_id() == target,
            (Self::Interrupt(AgentTargets::All), AgentOperation::InterruptAll) => true,
            _ => false,
        }
    }

    /// Get the stable command name used by correlated owner logs.
    pub(crate) fn name(&self) -> &'static str {
        match self {
            Self::Start(_) => "agent_start",
            Self::SendMessage(_) => "agent_message",
            Self::FollowUp(_) => "agent_followup",
            Self::Interrupt(_) => "agent_interrupt",
        }
    }
}

/// Preserve explicit child startup input without inheriting the parent transcript.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(try_from = "StoredAgentStart", into = "StoredAgentStart")]
pub(crate) struct AgentStartRequest {
    profile: Option<String>,
    text: String,
}

/// Retain informational or follow-up input with its captured child identity.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(try_from = "StoredAgentMessage", into = "StoredAgentMessage")]
pub(crate) struct AgentMessage {
    target: AgentTarget,
    selector: String,
    text: String,
}

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct StoredAgentStart {
    profile: Option<String>,
    text: String,
}

impl TryFrom<StoredAgentStart> for AgentStartRequest {
    type Error = AgentFailure;
    /// Validate new and recovered startup requests through the same boundary.
    fn try_from(stored: StoredAgentStart) -> Result<Self, Self::Error> {
        let StoredAgentStart { profile, text } = stored;
        if text.trim().is_empty() {
            return Err(AgentFailure::EmptyMessage);
        }
        if profile
            .as_ref()
            .is_some_and(|profile| profile.trim().is_empty())
        {
            return Err(AgentFailure::InvalidProfile);
        }
        Ok(Self { profile, text })
    }
}

impl From<AgentStartRequest> for StoredAgentStart {
    /// Preserve validated startup intent in durable command records.
    fn from(request: AgentStartRequest) -> Self {
        let AgentStartRequest { profile, text } = request;
        Self { profile, text }
    }
}

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct StoredAgentMessage {
    target: AgentTarget,
    selector: String,
    text: String,
}

impl TryFrom<StoredAgentMessage> for AgentMessage {
    type Error = AgentFailure;
    /// Reject empty input even when the command came from serialized recovery data.
    fn try_from(stored: StoredAgentMessage) -> Result<Self, Self::Error> {
        let StoredAgentMessage {
            target,
            selector,
            text,
        } = stored;
        if text.trim().is_empty() {
            return Err(AgentFailure::EmptyMessage);
        }
        if selector.trim().is_empty() {
            return Err(AgentFailure::StaleTarget);
        }
        Ok(Self {
            target,
            selector,
            text,
        })
    }
}

impl From<AgentMessage> for StoredAgentMessage {
    /// Preserve the validated child target and input without recapturing current state.
    fn from(message: AgentMessage) -> Self {
        let AgentMessage {
            target,
            selector,
            text,
        } = message;
        Self {
            target,
            selector,
            text,
        }
    }
}

/// Select child collection scope independently of mutable picker names.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(try_from = "StoredAgentTargets", into = "StoredAgentTargets")]
pub(crate) enum AgentTargets {
    /// Collect only the child task captured before admission.
    One {
        target: AgentTarget,
        selector: String,
    },
    /// Capture every child when this command starts.
    All,
}

#[derive(Deserialize, Serialize)]
#[serde(
    tag = "scope",
    content = "target",
    rename_all = "snake_case",
    deny_unknown_fields
)]
enum StoredAgentTargets {
    One {
        target: AgentTarget,
        selector: String,
    },
    All,
}

impl TryFrom<StoredAgentTargets> for AgentTargets {
    type Error = AgentFailure;
    /// Validate retained selectors before restoring an interruption command.
    fn try_from(stored: StoredAgentTargets) -> Result<Self, Self::Error> {
        match stored {
            StoredAgentTargets::One { target, selector } => {
                if selector.trim().is_empty() {
                    return Err(AgentFailure::StaleTarget);
                }
                Ok(Self::One { target, selector })
            }
            StoredAgentTargets::All => Ok(Self::All),
        }
    }
}

impl From<AgentTargets> for StoredAgentTargets {
    /// Preserve the original selector and captured child authority for retries.
    fn from(targets: AgentTargets) -> Self {
        match targets {
            AgentTargets::One { target, selector } => Self::One { target, selector },
            AgentTargets::All => Self::All,
        }
    }
}

/// Preserve finite completion evidence instead of a child's eventual answer.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "outcome", content = "result", rename_all = "snake_case")]
pub(crate) enum AgentOutcome {
    /// Child task startup or queued follow-up admission completed.
    Admitted {
        agent: Box<crate::agent::Agent>,
        task: Box<crate::agent::AgentTask>,
    },
    /// Informational input reached its durable child inbox.
    Message {
        agent_id: String,
        message: crate::agent::AgentMessage,
    },
    /// Every selected target has its own collection disposition.
    Interrupted { targets: Vec<AgentInterruption> },
}

/// Keep partial interruption failures alongside successful target collection.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct AgentInterruption {
    target: AgentTarget,
    result: Result<(), AgentFailure>,
}

/// Classify failures at the child-coordinator boundary without interpreting provider text.
#[derive(Clone, Debug, Deserialize, Serialize, thiserror::Error)]
#[serde(tag = "failure", content = "detail", rename_all = "snake_case")]
pub(crate) enum AgentFailure {
    /// The selected child or its task changed before execution.
    #[error("the selected child task changed before the command applied")]
    StaleTarget,
    /// Child input must contain nonempty text.
    #[error("child input must not be empty")]
    EmptyMessage,
    /// A supplied profile must identify a configured child profile.
    #[error("child profile must not be empty")]
    InvalidProfile,
    /// Read-only child queries do not enter the command queue.
    #[error("child inspection is not a mutating command")]
    InspectionOnly,
    /// The child owner rejected admission or delivery.
    #[error("the child coordinator rejected the operation")]
    Rejected,
    /// Retained child cleanup did not establish collection.
    #[error("child cleanup remains unresolved")]
    CleanupFailed,
    /// Observation timed out while the original child operation remains owned.
    #[error("child operation is still unconfirmed; its original work remains owned")]
    OutcomeUncertain,
    /// A child coordinator response violated its adapter contract.
    #[error("child coordinator returned an invalid completion")]
    InvalidCompletion,
    /// The asynchronous command worker ended without a result.
    #[error("child command worker ended without completion")]
    WorkerLost,
}

impl AgentFailure {
    /// Get the stable public failure code for this child operation.
    pub(crate) fn code(&self) -> &'static str {
        match self {
            Self::StaleTarget => "agent_target_changed",
            Self::EmptyMessage => "agent_empty_message",
            Self::InvalidProfile => "agent_invalid_profile",
            Self::InspectionOnly => "agent_inspection_only",
            Self::Rejected => "agent_rejected",
            Self::CleanupFailed => "agent_cleanup_failed",
            Self::OutcomeUncertain => "agent_outcome_unconfirmed",
            Self::InvalidCompletion => "agent_invalid_completion",
            Self::WorkerLost => "agent_worker_lost",
        }
    }
}

/// Retain one child-control worker independently of transient command observers.
pub(super) struct AgentCommandRun {
    pub(super) command: QueuedCommand,
    worker: RetainedAgentCommand,
    observation_deadline: Option<tokio::time::Instant>,
}

impl AgentCommandRun {
    /// Start finite child work only after the parent command's active record commits.
    pub(super) fn start(mut command: QueuedCommand, services: &OwnerServices) -> Self {
        let Command::Agent(request) = &command.record.request.command else {
            unreachable!("agent stages only own agent commands")
        };
        let request = request.clone();
        let children = Arc::clone(&services.children);
        let operation_id = command.record.request.id.as_str().to_owned();
        let task_admission = command.task_admission.take();
        let worker = RetainedAgentCommand {
            worker: Some(tokio::spawn(run(
                request,
                children,
                operation_id,
                task_admission,
            ))),
        };
        Self {
            command,
            worker,
            observation_deadline: Some(
                tokio::time::Instant::now() + std::time::Duration::from_secs(35),
            ),
        }
    }

    /// Observe a deadline once without abandoning work or releasing the FIFO slot.
    pub(super) async fn next(&mut self) -> AgentCommandEvent {
        if let Some(deadline) = self.observation_deadline {
            tokio::select! {
                result = self.worker.collect() => return AgentCommandEvent::Completed(result.map_err(CommandFailure::from)),
                _ = tokio::time::sleep_until(deadline) => {
                    self.observation_deadline = None;
                    return AgentCommandEvent::ObservationDeadline;
                }
            }
        }
        AgentCommandEvent::Completed(self.worker.collect().await.map_err(CommandFailure::from))
    }
}

/// Distinguish an observation deadline from the original operation's actual completion.
pub(super) enum AgentCommandEvent {
    /// The child coordinator established the finite operation's result.
    Completed(Result<AgentOutcome, CommandFailure>),
    /// Keep the command active and inspectable until its retained worker completes.
    ObservationDeadline,
}

/// Preserve the sole completion observer during owner shutdown or storage failure.
pub(super) struct RetainedAgentCommand {
    worker: Option<tokio::task::JoinHandle<Result<AgentOutcome, AgentFailure>>>,
}

impl RetainedAgentCommand {
    /// Collect the original operation once; cancellation of this wait retains the handle.
    pub(super) async fn collect(&mut self) -> Result<AgentOutcome, AgentFailure> {
        let Some(worker) = &mut self.worker else {
            return std::future::pending().await;
        };
        let result = worker.await.map_err(|_| AgentFailure::WorkerLost);
        self.worker = None;
        result?
    }
}

/// Translate the typed command to the existing child-owner input path.
async fn run(
    command: AgentCommand,
    children: Arc<AgentCoordinator>,
    operation_id: String,
    task_admission: Option<tokio::sync::OwnedSemaphorePermit>,
) -> Result<AgentOutcome, AgentFailure> {
    match command {
        AgentCommand::Interrupt(targets) => {
            let targets = match targets {
                AgentTargets::One {
                    target,
                    selector: _,
                } => vec![target],
                AgentTargets::All => children
                    .snapshot(None)
                    .iter()
                    .map(|(agent, _)| {
                        children
                            .capture_target(&agent.id)
                            .map_err(|_| AgentFailure::StaleTarget)
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            };
            let mut outcomes = Vec::with_capacity(targets.len());
            for (index, target) in targets.into_iter().enumerate() {
                let result = interrupt(&children, &target, format!("{operation_id}:{index}")).await;
                outcomes.push(AgentInterruption { target, result });
            }
            Ok(AgentOutcome::Interrupted { targets: outcomes })
        }
        AgentCommand::Start(start) => {
            let value = request(
                &children,
                &operation_id,
                AgentOperation::Spawn(crate::agent::SpawnAgentRequest {
                    profile: start.profile,
                    text: start.text,
                }),
                None,
                task_admission,
            )
            .await?;
            decode_admission(value)
        }
        AgentCommand::FollowUp(message) => {
            let value = request(
                &children,
                &operation_id,
                AgentOperation::Followup {
                    target: message.target.agent_id().into(),
                    message: message.text,
                },
                Some(message.target),
                task_admission,
            )
            .await?;
            decode_admission(value)
        }
        AgentCommand::SendMessage(message) => {
            let agent_id = message.target.agent_id().to_owned();
            let value = request(
                &children,
                &operation_id,
                AgentOperation::Message {
                    target: agent_id.clone(),
                    message: message.text,
                },
                Some(message.target),
                task_admission,
            )
            .await?;
            let message =
                serde_json::from_value(value).map_err(|_| AgentFailure::InvalidCompletion)?;
            Ok(AgentOutcome::Message { agent_id, message })
        }
    }
}

/// Revalidate captured targets before handing work to the child coordinator.
async fn request(
    children: &AgentCoordinator,
    operation_id: &str,
    operation: AgentOperation,
    target: Option<AgentTarget>,
    task_admission: Option<tokio::sync::OwnedSemaphorePermit>,
) -> Result<serde_json::Value, AgentFailure> {
    if let Some(target) = &target {
        children
            .validate_target(target)
            .map_err(|_| AgentFailure::StaleTarget)?;
    }
    children
        .sender()
        .request_captured(
            AgentRequest {
                operation_id: operation_id.into(),
                source: AgentSource::User,
                operation,
            },
            target,
            task_admission,
        )
        .await
        .map_err(|_| AgentFailure::Rejected)
}

/// Decode one finite child admission at the child-owner adapter boundary.
fn decode_admission(value: serde_json::Value) -> Result<AgentOutcome, AgentFailure> {
    #[derive(Deserialize)]
    struct Admission {
        agent: crate::agent::Agent,
        task: crate::agent::AgentTask,
    }
    let admission: Admission =
        serde_json::from_value(value).map_err(|_| AgentFailure::InvalidCompletion)?;
    Ok(AgentOutcome::Admitted {
        agent: Box::new(admission.agent),
        task: Box::new(admission.task),
    })
}

/// Observe the fixed child's collection without holding any lifecycle lock.
async fn interrupt(
    children: &AgentCoordinator,
    target: &AgentTarget,
    operation_id: String,
) -> Result<(), AgentFailure> {
    let mut activity = children.activity();
    request(
        children,
        &operation_id,
        AgentOperation::Interrupt {
            target: target.agent_id().into(),
        },
        Some(target.clone()),
        None,
    )
    .await?;
    loop {
        if children.cleanup_task(target.agent_id()).is_some() {
            return Err(AgentFailure::CleanupFailed);
        }
        if children
            .target_collected(target)
            .map_err(|_| AgentFailure::StaleTarget)?
        {
            return Ok(());
        }
        activity
            .changed()
            .await
            .map_err(|_| AgentFailure::WorkerLost)?;
    }
}
