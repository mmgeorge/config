//! Capture workflow continuation authority independently of the transport route.

use serde::{Deserialize, Serialize};

use super::{
    PlanExecutionSelection, PlanQuestionSetSelection, PlanSelection, TaskCommandFailure,
    TaskSelection,
};
use crate::{
    exchange::{Exchange, ExchangeState},
    task::{Task, TaskTurnId},
};

/// Continue exactly the workflow object selected at admission.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct WorkflowContinuationRequest {
    /// Captured source and revision; execution must never select a newer object.
    pub(crate) target: WorkflowContinuationTarget,
    /// Explicit caller instruction for the resumed workflow, when supplied.
    pub(crate) message: Option<String>,
}

/// Distinguish saved-plan approval, completed work, and explicitly scoped question completion.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "owner", content = "selection", rename_all = "snake_case")]
pub(crate) enum WorkflowContinuationTarget {
    /// Reuse the captured execution approval gate for a submitted plan.
    Plan(PlanExecutionSelection),
    /// Advance only the captured completed exchange and its current task.
    Exchange(ExchangeContinuationSelection),
    /// Revalidate the question-set revision before advancing its workflow.
    Questions(QuestionContinuationSelection),
}

impl WorkflowContinuationRequest {
    /// Get plan approval intent when this continuation passes through ExecutePlan preparation.
    pub(crate) fn execution(&self) -> Option<&PlanExecutionSelection> {
        match &self.target {
            WorkflowContinuationTarget::Plan(selection) => Some(selection),
            WorkflowContinuationTarget::Questions(QuestionContinuationSelection {
                workflow: QuestionWorkflow::PlanReview(selection),
                ..
            }) => Some(selection),
            WorkflowContinuationTarget::Exchange(_) | WorkflowContinuationTarget::Questions(_) => {
                None
            }
        }
    }
}

/// Capture source identity and terminal-turn revision without exposing mutable latest selection.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ExchangeContinuationSelection {
    id: crate::provider::ExchangeId,
    task: Option<TaskSelection>,
    completed_turn: TaskTurnId,
    state: ExchangeState,
}

impl ExchangeContinuationSelection {
    /// Capture exchange state and the task authority selected by the same admission snapshot.
    pub(crate) fn capture(
        exchange: &Exchange,
        task: Option<&Task>,
    ) -> Result<Self, TaskCommandFailure> {
        let id = exchange.id.clone().into();
        Ok(Self {
            id,
            task: task.map(TaskSelection::capture),
            completed_turn: TaskTurnId {
                exchange_ordinal: exchange.ordinal,
                turn_ordinal: exchange.turn.len(),
            },
            state: exchange.state(),
        })
    }

    /// Get the exact exchange selected by the caller.
    pub(crate) fn id(&self) -> &crate::provider::ExchangeId {
        &self.id
    }

    /// Get the completed provider-turn boundary captured at admission.
    pub(crate) fn completed_turn(&self) -> &TaskTurnId {
        &self.completed_turn
    }

    /// Reject changed exchange or task authority before reserving or dispatching continuation.
    pub(crate) fn validate(
        &self,
        exchange: &Exchange,
        task: Option<&Task>,
    ) -> Result<(), TaskCommandFailure> {
        if self.id.as_str() != exchange.id
            || self.state != exchange.state()
            || self.completed_turn.exchange_ordinal != exchange.ordinal
            || self.completed_turn.turn_ordinal != exchange.turn.len()
        {
            return Err(TaskCommandFailure::Changed);
        }
        match &self.task {
            Some(selected) => selected.validate(task),
            None if task.is_none() => Ok(()),
            None => Err(TaskCommandFailure::Changed),
        }
    }
}

/// Bind an explicit question continuation to the exact question revision and owning workflow.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct QuestionContinuationSelection {
    /// Committed content and answer revision selected by the question UI.
    pub(crate) questions: PlanQuestionSetSelection,
    /// Workflow source captured alongside the selected question state.
    pub(crate) workflow: QuestionWorkflow,
}

/// Preserve the distinction between review decisions, planning questions, and exchange questions.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "owner", content = "selection", rename_all = "snake_case")]
pub(crate) enum QuestionWorkflow {
    /// Approval decisions reuse ExecutePlan and do not approve automatically when answers arrive.
    PlanReview(Box<PlanExecutionSelection>),
    /// Planning decisions advance only the selected plan and its current task.
    Plan {
        source: PlanSelection,
        task: TaskSelection,
    },
    /// Clarification decisions belong to the selected existing exchange.
    Exchange(ExchangeContinuationSelection),
}
