use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};

use super::active::TaskSnapshot;
use super::{TaskStatus, WorkflowSnapshot};
use crate::plan::{PlanExecutionState, PlanPhase, PlanState};

/// Own the selected workflow independently of interruption and terminal outcomes.
#[derive(Clone, Debug)]
pub enum TaskLifecycle {
    Active(WorkflowState),
    Suspended(SuspendedTask),
    Finished(FinishedTask),
}

/// Restrict each task type to the phases defined by its workflow.
#[derive(Clone, Debug)]
pub enum WorkflowState {
    Plan(PlanningState),
    Execute(ExecutionState),
    Goal(GoalState),
}

/// Separate editable planning from an immutable submitted review.
#[derive(Clone, Debug)]
pub enum PlanningState {
    Draft(PlanDraft),
    Review(PlanReview),
}

/// Separate implementation authority, local checks, repair, and manual evidence.
#[derive(Clone, Debug)]
pub enum ExecutionState {
    Implement(Implementation),
    Check(ExecutionCheck),
    Resolve(Resolution),
    Verify(Verification),
}

/// Keep goal continuation free of execution-specific gates.
#[derive(Clone, Debug)]
pub enum GoalState {
    Work(GoalWork),
}

/// Describe the workflow's current phase without exposing mutation authority.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TaskPhase {
    Draft,
    Review,
    Implement,
    Check,
    Resolve,
    Verify,
    Work,
}

/// Select work from the committed phase rather than from caller-supplied flags.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum TaskNextAction {
    Model,
    Checks,
    Wait,
    Finished,
}

/// Preserve the selected phase and the concrete condition preventing continuation.
#[derive(Clone, Debug)]
pub struct SuspendedTask {
    workflow: WorkflowState,
    reason: SuspensionReason,
}

/// Preserve the concrete interruption independently from the workflow's phase.
#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum SuspensionReason {
    Paused,
    NoProgress {
        consecutive_turn_count: u8,
    },
    TurnBudget {
        turn_count: u32,
        max_turn_count: u32,
    },
    ProviderUsageLimited,
    ProviderBudgetLimited,
    GoalReportedBlocked,
    ExecutionBlocked {
        findings: Vec<String>,
    },
    PlanningFailed,
    Stopping,
}

impl SuspendedTask {
    /// Inspect the interruption without losing its preserved workflow phase.
    pub fn reason(&self) -> &SuspensionReason {
        &self.reason
    }
}

impl SuspensionReason {
    fn status(&self) -> TaskStatus {
        match self {
            Self::Paused => TaskStatus::Paused,
            Self::PlanningFailed => TaskStatus::Failed,
            Self::Stopping => TaskStatus::Stopping,
            Self::NoProgress { .. }
            | Self::TurnBudget { .. }
            | Self::ProviderUsageLimited
            | Self::ProviderBudgetLimited
            | Self::GoalReportedBlocked
            | Self::ExecutionBlocked { .. } => TaskStatus::Blocked,
        }
    }
}

/// Preserve the final workflow while its outcome is atomically archived.
#[derive(Clone, Debug)]
pub struct FinishedTask {
    workflow: WorkflowState,
    status: TaskStatus,
}

/// Retain a mutable draft and its artifact-owned question or setup operation.
#[derive(Clone, Debug)]
pub struct PlanDraft {
    snapshot: TaskSnapshot,
}

/// Retain the submitted artifact and exact review identity.
#[derive(Clone, Debug)]
pub struct PlanReview {
    snapshot: TaskSnapshot,
}

/// Retain the accepted implementation contract before the automated gate.
#[derive(Clone, Debug)]
pub struct Implementation {
    snapshot: TaskSnapshot,
}

/// Retain the accepted gate and its locally executed check attempts.
#[derive(Clone, Debug)]
pub struct ExecutionCheck {
    contract: ExecutionContract,
}

/// Retain corrective findings and any proposed contract revision under review.
#[derive(Clone, Debug)]
pub struct Resolution {
    contract: ExecutionContract,
}

/// Retain manual verification work and the automated evidence preceding it.
#[derive(Clone, Debug)]
pub struct Verification {
    contract: ExecutionContract,
}

/// Prove that gated execution owns an accepted revision and an immutable workspace baseline.
/// Its private constructor accepts only a validated snapshot; no caller can remove either field.
#[derive(Clone, Debug)]
pub struct ExecutionContract {
    snapshot: TaskSnapshot,
}

/// Retain one objective throughout continuation and recovery.
#[derive(Clone, Debug)]
pub struct GoalWork {
    snapshot: TaskSnapshot,
}

impl TaskLifecycle {
    /// Create an encapsulated workflow from a validated persistence snapshot.
    pub(super) fn from_snapshot(snapshot: TaskSnapshot) -> Self {
        let status = snapshot_status(&snapshot.workflow);
        let workflow = match &snapshot.workflow {
            WorkflowSnapshot::Plan { plan } => {
                WorkflowState::Plan(if plan.artifact.state == PlanState::AwaitingReview {
                    PlanningState::Review(PlanReview { snapshot })
                } else {
                    PlanningState::Draft(PlanDraft { snapshot })
                })
            }
            WorkflowSnapshot::Execute { execution } => {
                WorkflowState::Execute(match execution.record.phase {
                    PlanPhase::Implement => ExecutionState::Implement(Implementation { snapshot }),
                    PlanPhase::Checking => ExecutionState::Check(ExecutionCheck {
                        contract: ExecutionContract { snapshot },
                    }),
                    PlanPhase::Resolve => ExecutionState::Resolve(Resolution {
                        contract: ExecutionContract { snapshot },
                    }),
                    PlanPhase::Verify => ExecutionState::Verify(Verification {
                        contract: ExecutionContract { snapshot },
                    }),
                })
            }
            WorkflowSnapshot::Goal { .. } => {
                WorkflowState::Goal(GoalState::Work(GoalWork { snapshot }))
            }
        };
        match status {
            TaskStatus::Running | TaskStatus::Waiting | TaskStatus::Ready => Self::Active(workflow),
            TaskStatus::Stopping
            | TaskStatus::Paused
            | TaskStatus::Blocked
            | TaskStatus::Failed => {
                let reason = suspension_reason(&workflow.snapshot().workflow, status);
                Self::Suspended(SuspendedTask { workflow, reason })
            }
            TaskStatus::Completed | TaskStatus::Cancelled => {
                Self::Finished(FinishedTask { workflow, status })
            }
        }
    }

    /// Get the selected workflow without granting mutation of its phase payload.
    pub fn workflow(&self) -> &WorkflowState {
        match self {
            Self::Active(workflow) => workflow,
            Self::Suspended(task) => &task.workflow,
            Self::Finished(task) => &task.workflow,
        }
    }

    /// Get an artifact snapshot for transport and archive adapters.
    pub(super) fn snapshot(&self) -> &TaskSnapshot {
        self.workflow().snapshot()
    }

    /// Get the lifecycle condition while preserving its workflow phase.
    pub fn status(&self) -> TaskStatus {
        match self {
            Self::Active(workflow) => snapshot_status(&workflow.snapshot().workflow),
            Self::Suspended(task) => task.reason.status(),
            Self::Finished(task) => task.status,
        }
    }

    /// Derive the only class of work legal in the current state.
    pub(crate) fn next_action(&self) -> TaskNextAction {
        match self {
            Self::Suspended(_) => TaskNextAction::Wait,
            Self::Finished(_) => TaskNextAction::Finished,
            Self::Active(workflow) => match workflow {
                WorkflowState::Plan(PlanningState::Review(_)) => TaskNextAction::Wait,
                WorkflowState::Plan(PlanningState::Draft(draft)) => {
                    let WorkflowSnapshot::Plan { plan } = &draft.snapshot.workflow else {
                        unreachable!()
                    };
                    if plan.artifact.state == PlanState::AwaitingInput {
                        TaskNextAction::Wait
                    } else {
                        TaskNextAction::Model
                    }
                }
                WorkflowState::Execute(ExecutionState::Check(_)) => TaskNextAction::Checks,
                WorkflowState::Execute(
                    ExecutionState::Implement(_) | ExecutionState::Verify(_),
                ) => TaskNextAction::Model,
                WorkflowState::Execute(ExecutionState::Resolve(resolution)) => {
                    let WorkflowSnapshot::Execute { execution } =
                        &resolution.contract.snapshot.workflow
                    else {
                        unreachable!()
                    };
                    if execution.record.pending_revision_reason.is_some() {
                        TaskNextAction::Wait
                    } else {
                        TaskNextAction::Model
                    }
                }
                WorkflowState::Goal(GoalState::Work(_)) => TaskNextAction::Model,
            },
        }
    }
}

impl WorkflowState {
    /// Get the user-visible phase directly from its state variant.
    pub fn phase(&self) -> TaskPhase {
        match self {
            Self::Plan(PlanningState::Draft(_)) => TaskPhase::Draft,
            Self::Plan(PlanningState::Review(_)) => TaskPhase::Review,
            Self::Execute(ExecutionState::Implement(_)) => TaskPhase::Implement,
            Self::Execute(ExecutionState::Check(_)) => TaskPhase::Check,
            Self::Execute(ExecutionState::Resolve(_)) => TaskPhase::Resolve,
            Self::Execute(ExecutionState::Verify(_)) => TaskPhase::Verify,
            Self::Goal(GoalState::Work(_)) => TaskPhase::Work,
        }
    }

    /// Get the immutable adapter snapshot held by the selected phase.
    fn snapshot(&self) -> &TaskSnapshot {
        match self {
            Self::Plan(PlanningState::Draft(phase)) => &phase.snapshot,
            Self::Plan(PlanningState::Review(phase)) => &phase.snapshot,
            Self::Execute(ExecutionState::Implement(phase)) => &phase.snapshot,
            Self::Execute(ExecutionState::Check(phase)) => &phase.contract.snapshot,
            Self::Execute(ExecutionState::Resolve(phase)) => &phase.contract.snapshot,
            Self::Execute(ExecutionState::Verify(phase)) => &phase.contract.snapshot,
            Self::Goal(GoalState::Work(phase)) => &phase.snapshot,
        }
    }
}

impl ExecutionState {
    /// Expose a required accepted contract only after execution admission has captured its baseline.
    pub fn contract(&self) -> Option<&ExecutionContract> {
        match self {
            Self::Implement(_) => None,
            Self::Check(phase) => Some(&phase.contract),
            Self::Resolve(phase) => Some(&phase.contract),
            Self::Verify(phase) => Some(&phase.contract),
        }
    }
}

impl ExecutionContract {
    /// Get the immutable baseline required by every gate-bearing phase.
    pub fn baseline_checkpoint(&self) -> &str {
        self.record()
            .baseline_checkpoint
            .as_deref()
            .expect("validated execution baseline")
    }

    /// Get the accepted revision whose checks and manual actions this phase must satisfy.
    pub fn revision(&self) -> u32 {
        self.record().revision
    }

    fn record(&self) -> &crate::plan::PlanExecutionRecord {
        let WorkflowSnapshot::Execute { execution } = &self.snapshot.workflow else {
            unreachable!()
        };
        &execution.record
    }
}

/// Reject incomplete phase payloads before they acquire a lifecycle variant.
pub(super) fn validate_snapshot(snapshot: &TaskSnapshot) -> Result<()> {
    match &snapshot.workflow {
        WorkflowSnapshot::Plan { plan } if plan.artifact.state == PlanState::AwaitingReview => {
            ensure!(
                plan.artifact.model_revision > 0
                    && plan
                        .artifact
                        .review_digest
                        .as_ref()
                        .is_some_and(|digest| !digest.is_empty()),
                "Review requires an exact artifact revision and frozen review digest"
            );
        }
        WorkflowSnapshot::Execute { execution } => {
            let record = &execution.record;
            ensure!(
                record
                    .baseline_checkpoint
                    .as_ref()
                    .is_none_or(|checkpoint| !checkpoint.is_empty()),
                "captured execution baseline identity must not be empty"
            );
            ensure!(
                !record.id.is_empty()
                    && !record.plan_id.is_empty()
                    && !record.session_id.is_empty()
                    && record.original_revision > 0
                    && record.revision >= record.original_revision,
                "execution requires a valid accepted contract identity"
            );
            ensure!(
                record.phase == PlanPhase::Implement
                    || record
                        .baseline_checkpoint
                        .as_ref()
                        .is_some_and(|checkpoint| !checkpoint.is_empty()),
                "execution gates require the admitted workspace baseline"
            );
            ensure!(
                record.pending_revision_reason.is_none() || record.phase == PlanPhase::Resolve,
                "contract revision review requires Resolve"
            );
            if record.phase == PlanPhase::Verify {
                ensure!(
                    !record.manual_verification.is_empty(),
                    "Verify requires accepted manual actions"
                );
                validate_passing_gate(record)?;
            }
        }
        WorkflowSnapshot::Goal { .. } | WorkflowSnapshot::Plan { .. } => {}
    }
    Ok(())
}

/// Require passing local evidence for this contract before manual work or completion.
pub(super) fn validate_passing_gate(record: &crate::plan::PlanExecutionRecord) -> Result<()> {
    ensure!(
        record
            .check_runs
            .last()
            .is_some_and(|run| run.revision == record.revision
                && run.state == crate::checks::CheckState::Passed
                && run.outcome() == crate::plan::execution::VerificationOutcome::Passed),
        "execution requires passing automated evidence for the accepted revision"
    );
    Ok(())
}

/// Translate artifact outcomes once, at construction of the owning suspension payload.
fn suspension_reason(workflow: &WorkflowSnapshot, status: TaskStatus) -> SuspensionReason {
    if status == TaskStatus::Paused {
        return SuspensionReason::Paused;
    }
    if status == TaskStatus::Stopping {
        return SuspensionReason::Stopping;
    }
    let stalled = |budget: &crate::session::continuation::ContinuationBudget| {
        if budget.turn_count >= budget.max_turn_count {
            SuspensionReason::TurnBudget {
                turn_count: budget.turn_count,
                max_turn_count: budget.max_turn_count,
            }
        } else {
            SuspensionReason::NoProgress {
                consecutive_turn_count: budget.consecutive_no_progress,
            }
        }
    };
    match workflow {
        WorkflowSnapshot::Goal { goal } => match goal.state {
            crate::goal::GoalState::Stalled => stalled(&goal.continuation),
            crate::goal::GoalState::UsageLimited => SuspensionReason::ProviderUsageLimited,
            crate::goal::GoalState::BudgetLimited => SuspensionReason::ProviderBudgetLimited,
            _ => SuspensionReason::GoalReportedBlocked,
        },
        WorkflowSnapshot::Plan { plan }
            if plan.artifact.generation.budget.turn_count
                >= plan.artifact.generation.budget.max_turn_count
                || plan.artifact.generation.budget.consecutive_no_progress
                    >= plan.artifact.generation.budget.max_consecutive_no_progress =>
        {
            stalled(&plan.artifact.generation.budget)
        }
        WorkflowSnapshot::Plan { .. } => SuspensionReason::PlanningFailed,
        WorkflowSnapshot::Execute { execution }
            if execution.record.state == PlanExecutionState::Stalled =>
        {
            stalled(&execution.continuation)
        }
        WorkflowSnapshot::Execute { execution } => SuspensionReason::ExecutionBlocked {
            findings: execution.record.findings.clone(),
        },
    }
}

/// Interpret legacy artifact statuses only at the snapshot boundary.
pub(super) fn snapshot_status(snapshot: &WorkflowSnapshot) -> TaskStatus {
    match snapshot {
        WorkflowSnapshot::Goal { goal } => match goal.state {
            crate::goal::GoalState::Active => TaskStatus::Running,
            crate::goal::GoalState::Paused => TaskStatus::Paused,
            crate::goal::GoalState::Complete => TaskStatus::Completed,
            crate::goal::GoalState::Cleared => TaskStatus::Cancelled,
            crate::goal::GoalState::Blocked
            | crate::goal::GoalState::UsageLimited
            | crate::goal::GoalState::BudgetLimited
            | crate::goal::GoalState::Stalled => TaskStatus::Blocked,
        },
        WorkflowSnapshot::Plan { plan } if plan.paused => {
            if plan.artifact.generation.budget.turn_count
                >= plan.artifact.generation.budget.max_turn_count
                || plan.artifact.generation.budget.consecutive_no_progress
                    >= plan.artifact.generation.budget.max_consecutive_no_progress
            {
                TaskStatus::Blocked
            } else {
                TaskStatus::Paused
            }
        }
        WorkflowSnapshot::Plan { plan } => match plan.artifact.state {
            PlanState::AwaitingInput | PlanState::AwaitingReview => TaskStatus::Waiting,
            PlanState::Failed
                if plan.artifact.generation.budget.turn_count
                    >= plan.artifact.generation.budget.max_turn_count
                    || plan.artifact.generation.budget.consecutive_no_progress
                        >= plan.artifact.generation.budget.max_consecutive_no_progress =>
            {
                TaskStatus::Blocked
            }
            PlanState::Failed => TaskStatus::Failed,
            PlanState::Cancelled | PlanState::Rejected => TaskStatus::Cancelled,
            PlanState::Accepted => TaskStatus::Completed,
            PlanState::ConfiguringChecks | PlanState::Generating | PlanState::Revising => {
                TaskStatus::Running
            }
        },
        WorkflowSnapshot::Execute { execution } => match execution.record.state {
            PlanExecutionState::Active if execution.record.pending_revision_reason.is_some() => {
                TaskStatus::Waiting
            }
            PlanExecutionState::Active => TaskStatus::Running,
            PlanExecutionState::Complete => TaskStatus::Completed,
            PlanExecutionState::Paused => TaskStatus::Paused,
            PlanExecutionState::Cancelled => TaskStatus::Cancelled,
            PlanExecutionState::Stalled | PlanExecutionState::Blocked => TaskStatus::Blocked,
        },
    }
}
