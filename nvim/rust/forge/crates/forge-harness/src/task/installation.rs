//! Prepare a task installation with an explicit model-input or awaiting-decisions postcondition.

use super::{Task, TaskInputDelivery, TaskInputId, TaskNextAction, WorkflowSnapshot};
use crate::{
    exchange::command::{PlanId, TaskCommandFailure},
    storage::TaskEffects,
};

/// Distinguish work requiring initial dispatch from a plan already awaiting review or decisions.
pub(crate) enum TaskInstallationWork {
    /// Retain the original pending model-entry identity through dispatch preparation.
    Model(TaskInputId),
    /// Commit attachment without manufacturing model input or a dispatch acknowledgment.
    Waiting(PlanId),
}

/// Own a validated task and its initial input until their installation commits atomically.
pub(crate) struct TaskInstallation {
    task: Task,
    work: TaskInstallationWork,
    effects: TaskEffects,
}

impl TaskInstallation {
    /// Create an installation only for a live model entry or a plan awaiting user decisions.
    pub(crate) fn new(task: Task, effects: TaskEffects) -> Result<Self, TaskCommandFailure> {
        if task.status().terminal() {
            return Err(TaskCommandFailure::PreparationUnavailable);
        }
        let work = if let Some(entry) = task.entry.as_ref().filter(|entry| {
            entry.delivery == TaskInputDelivery::Pending
                && task.next_action() == TaskNextAction::Model
        }) {
            TaskInstallationWork::Model(entry.id.clone())
        } else if task.entry.is_none() && task.next_action() == TaskNextAction::Wait {
            match &task.workflow {
                WorkflowSnapshot::Plan { plan }
                    if matches!(
                        plan.artifact.state,
                        crate::plan::PlanState::AwaitingInput
                            | crate::plan::PlanState::AwaitingReview
                    ) =>
                {
                    TaskInstallationWork::Waiting(
                        PlanId::new(plan.artifact.id.clone())
                            .map_err(|_| TaskCommandFailure::InvalidPlanSelection)?,
                    )
                }
                WorkflowSnapshot::Plan { .. }
                | WorkflowSnapshot::Goal { .. }
                | WorkflowSnapshot::Execute { .. } => {
                    return Err(TaskCommandFailure::PreparationUnavailable);
                }
            }
        } else {
            return Err(TaskCommandFailure::PreparationUnavailable);
        };
        Ok(Self {
            task,
            work,
            effects,
        })
    }

    /// Get the prepared task without allowing its initial input identity to change.
    pub(crate) fn task(&self) -> &Task {
        &self.task
    }

    /// Consume the validated installation into its atomic task/checkpoint transaction.
    pub(crate) fn into_parts(self) -> (Task, TaskInstallationWork, TaskEffects) {
        let Self {
            task,
            work,
            effects,
        } = self;
        (task, work, effects)
    }
}
