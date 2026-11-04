use crate::plan::{PlanExecutionRecord, PlanRecord};

/// Report a completed domain operation without exposing mutable task state.
#[derive(Clone, Debug)]
pub(crate) enum TaskEvent {
    PlanArtifactUpdated(Box<PlanRecord>),
    PlanGenerationFailed,
    ExecutionUpdated(Box<PlanExecutionRecord>),
    ExecutionBaselineCaptured(String),
    GoalWorkspaceObserved(String),
    CheckPreparationFailed(String),
    VerificationInputsChanged(super::TaskTurnId),
    EntryPreparationFinished,
}
