use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::goal::GoalState;
use crate::plan::{PlanExecutionRecord, PlanExecutionState, PlanRecord, PlanState};
use crate::session::continuation::ContinuationBudget;

/// Identifies one admitted task contract, independently of exchange and provider identities.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(transparent)]
pub struct TaskRevision(String);

impl TaskRevision {
    pub fn fresh() -> Self {
        Self(uuid::Uuid::new_v4().to_string())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Carries the only selected workflow in a session. Transcript entries do not own this state.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ActiveTask {
    pub revision: TaskRevision,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub entry: Option<TaskEntry>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub turn: Option<TaskTurn>,
    #[serde(flatten)]
    pub workflow: TaskWorkflow,
}

/// Identifies a task-generated provider input across delivery retries.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(transparent)]
pub struct TaskInputId(String);

impl TaskInputId {
    pub fn fresh() -> Self {
        Self(uuid::Uuid::new_v4().to_string())
    }
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Orders completed root turns without retaining prior exchanges in task state.
#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
pub struct TaskTurnId {
    pub exchange_ordinal: u64,
    pub turn_ordinal: usize,
}

/// Retains the latest observed turn and its durable continuation delivery intent.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TaskTurn {
    pub id: TaskTurnId,
    pub continuation: Option<TaskContinuation>,
}

/// Owns the exact instruction and provider acknowledgement for one continuation.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TaskContinuation {
    pub id: TaskInputId,
    pub instruction: String,
    pub reason: String,
    pub delivery: TaskInputDelivery,
    #[serde(default)]
    pub work: TaskContinuationWork,
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TaskContinuationWork {
    #[default]
    Provider,
    Checks,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TaskEntry {
    pub id: TaskInputId,
    pub instruction: String,
    pub delivery: TaskInputDelivery,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub execution: Option<ExecutionEntry>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ExecutionEntry {
    pub approval_mode: crate::session::ApprovalMode,
    pub executor: crate::session::PlanExecutor,
    pub compact: bool,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TaskInputDelivery {
    Pending,
    Accepted,
    Cancelled,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TaskWorkflow {
    Goal { goal: GoalTask },
    Plan { plan: PlanTask },
    Execute { execution: ExecutionTask },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GoalTask {
    pub objective: String,
    pub state: GoalState,
    pub continuation: ContinuationBudget,
    pub native: bool,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanTask {
    pub artifact: PlanRecord,
    pub paused: bool,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ExecutionTask {
    pub title: String,
    pub record: PlanExecutionRecord,
    pub continuation: ContinuationBudget,
    pub native: bool,
}

/// Defines the next action after provider execution settles under the current task contract.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TaskDecision {
    Continue { instruction: String, reason: String },
    Wait { reason: String },
    Advance { instruction: String, reason: String },
    Complete { summary: String },
    Block { reason: String },
}

/// Retains a terminal task result without retaining a task owner in the transcript.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct TaskOutcome {
    pub kind: super::TaskKind,
    pub title: String,
    pub state: TaskOutcomeState,
    pub summary: String,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TaskOutcomeState {
    Complete,
    Cancelled,
}

/// Binds a task-specific control call to the contract that admitted it.
#[derive(Clone, Debug)]
pub struct TaskPermit {
    revision: TaskRevision,
}

#[derive(Debug)]
pub(crate) struct StaleTask;

impl std::fmt::Display for StaleTask {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("task changed before the control operation completed")
    }
}

impl std::error::Error for StaleTask {}

impl TaskPermit {
    pub(crate) fn revision(&self) -> &TaskRevision {
        &self.revision
    }

    pub fn validate(&self, task: Option<&ActiveTask>) -> Result<()> {
        if task.is_none_or(|task| task.revision != self.revision) {
            return Err(StaleTask.into());
        }
        Ok(())
    }
}

impl ActiveTask {
    pub fn goal(
        objective: String,
        continuation: ContinuationBudget,
        native: bool,
        now_ms: i64,
    ) -> Self {
        Self {
            revision: TaskRevision::fresh(),
            created_at_ms: now_ms,
            updated_at_ms: now_ms,
            entry: None,
            turn: None,
            workflow: TaskWorkflow::Goal {
                goal: GoalTask {
                    objective,
                    state: GoalState::Active,
                    continuation,
                    native,
                },
            },
        }
    }

    pub fn plan(artifact: PlanRecord, now_ms: i64) -> Self {
        Self {
            revision: TaskRevision::fresh(),
            created_at_ms: now_ms,
            updated_at_ms: now_ms,
            entry: None,
            turn: None,
            workflow: TaskWorkflow::Plan {
                plan: PlanTask {
                    artifact,
                    paused: false,
                },
            },
        }
    }

    pub fn execute(
        title: String,
        record: PlanExecutionRecord,
        continuation: ContinuationBudget,
        native: bool,
        now_ms: i64,
    ) -> Self {
        Self {
            revision: TaskRevision::fresh(),
            created_at_ms: now_ms,
            updated_at_ms: now_ms,
            entry: None,
            turn: None,
            workflow: TaskWorkflow::Execute {
                execution: ExecutionTask {
                    title,
                    record,
                    continuation,
                    native,
                },
            },
        }
    }

    pub fn permit(&self) -> TaskPermit {
        TaskPermit {
            revision: self.revision.clone(),
        }
    }

    /// Retains entry intent until the matching provider receipt activates control admission.
    pub(crate) fn request_entry(&mut self, instruction: String, now_ms: i64) -> Result<()> {
        anyhow::ensure!(
            !instruction.trim().is_empty(),
            "task entry instruction is required"
        );
        self.renew(now_ms);
        self.entry = Some(TaskEntry {
            id: TaskInputId::fresh(),
            instruction,
            delivery: TaskInputDelivery::Pending,
            execution: None,
        });
        Ok(())
    }

    pub(crate) fn entry_accepted(&self) -> bool {
        self.entry
            .as_ref()
            .is_none_or(|entry| entry.delivery == TaskInputDelivery::Accepted)
    }

    pub fn title(&self) -> &str {
        match &self.workflow {
            TaskWorkflow::Goal { goal } => &goal.objective,
            TaskWorkflow::Plan { plan } => &plan.artifact.title,
            TaskWorkflow::Execute { execution } => &execution.title,
        }
    }

    pub fn kind(&self) -> super::TaskKind {
        match self.workflow {
            TaskWorkflow::Goal { .. } => super::TaskKind::Goal,
            TaskWorkflow::Plan { .. } => super::TaskKind::Plan,
            TaskWorkflow::Execute { .. } => super::TaskKind::Execute,
        }
    }

    pub fn status(&self) -> super::TaskStatus {
        use super::TaskStatus;
        match &self.workflow {
            TaskWorkflow::Goal { goal } => match goal.state {
                GoalState::Active => TaskStatus::Running,
                GoalState::Paused => TaskStatus::Paused,
                GoalState::Complete => TaskStatus::Completed,
                GoalState::Cleared => TaskStatus::Cancelled,
                _ => TaskStatus::Blocked,
            },
            TaskWorkflow::Plan { plan } if plan.paused => TaskStatus::Paused,
            TaskWorkflow::Plan { plan } => match plan.artifact.state {
                PlanState::ConfiguringChecks
                | PlanState::AwaitingInput
                | PlanState::AwaitingReview => TaskStatus::Waiting,
                PlanState::Failed => TaskStatus::Failed,
                PlanState::Cancelled | PlanState::Rejected => TaskStatus::Cancelled,
                PlanState::Accepted => TaskStatus::Completed,
                _ => TaskStatus::Running,
            },
            TaskWorkflow::Execute { execution } => match execution.record.state {
                PlanExecutionState::Active => TaskStatus::Running,
                PlanExecutionState::Complete => TaskStatus::Completed,
                PlanExecutionState::Paused => TaskStatus::Paused,
                PlanExecutionState::Cancelled => TaskStatus::Cancelled,
                _ => TaskStatus::Blocked,
            },
        }
    }

    pub(crate) fn same_contract(&self, other: &Self) -> bool {
        match (&self.workflow, &other.workflow) {
            (TaskWorkflow::Goal { goal }, TaskWorkflow::Goal { goal: other }) => {
                goal.objective == other.objective
                    && goal.state == other.state
                    && goal.native == other.native
                    && goal.continuation.max_turn_count == other.continuation.max_turn_count
                    && goal.continuation.max_consecutive_no_progress
                        == other.continuation.max_consecutive_no_progress
            }
            (TaskWorkflow::Plan { plan }, TaskWorkflow::Plan { plan: other }) => {
                plan.artifact.id == other.artifact.id
                    && plan.artifact.state == other.artifact.state
                    && plan.paused == other.paused
            }
            (TaskWorkflow::Execute { execution }, TaskWorkflow::Execute { execution: other }) => {
                execution.record.id == other.record.id
                    && execution.record.plan_id == other.record.plan_id
                    && execution.record.revision == other.record.revision
                    && execution.record.generation == other.record.generation
                    && execution.record.phase == other.record.phase
                    && execution.record.state == other.record.state
                    && execution.record.review_paused == other.record.review_paused
            }
            _ => false,
        }
    }

    /// Change admission identity whenever the task's allowed operations change.
    pub fn renew(&mut self, now_ms: i64) {
        self.revision = TaskRevision::fresh();
        self.updated_at_ms = now_ms;
        if let Some(continuation) = self
            .turn
            .as_mut()
            .and_then(|turn| turn.continuation.as_mut())
        {
            continuation.delivery = TaskInputDelivery::Cancelled;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn task_replacement_rejects_stale_control_results() {
        let mut task = ActiveTask::goal("First".into(), ContinuationBudget::default(), false, 1);
        let permit = task.permit();
        permit.validate(Some(&task)).unwrap();
        task.renew(2);
        assert!(permit.validate(Some(&task)).is_err());
        assert!(permit.validate(None).is_err());
        let replacement =
            ActiveTask::goal("Second".into(), ContinuationBudget::default(), false, 3);
        assert!(permit.validate(Some(&replacement)).is_err());
    }
}
