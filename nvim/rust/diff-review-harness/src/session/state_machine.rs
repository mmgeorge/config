use serde::Serialize;

use crate::{
    broker::{ActiveElicitation, ElicitationOwner},
    interaction::ActiveWait,
    plan::{PlanRecord, PlanState},
};

/// Represents the user-visible work class rendered by the timeline status.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum WorkflowActivity {
    Working,
    Planning,
}

/// Represents the authoritative workflow phase exposed to one session timeline.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum SessionPhase {
    #[default]
    Idle,
    Working {
        started_at_ms: i64,
        activity: WorkflowActivity,
    },
    AwaitingInput {
        owner: ElicitationOwner,
        plan_id: Option<String>,
        interaction_id: Option<String>,
    },
    AwaitingPlanReview {
        plan_id: String,
        revision: u32,
    },
    RetryingPlanGeneration {
        plan_id: String,
        turn: u32,
        max_turn: u32,
        started_at_ms: i64,
    },
    PlanningFailed {
        plan_id: String,
        turn_count: u32,
    },
    WaitingForAgent {
        agent_count: usize,
    },
}

impl SessionPhase {
    /// Resolve one deterministic workflow phase from the session's durable owners.
    pub fn resolve(
        active_plan: Option<&PlanRecord>,
        active_elicitation: Option<&ActiveElicitation>,
        active_wait: Option<&ActiveWait>,
        working: Option<(i64, WorkflowActivity)>,
    ) -> Self {
        if let Some(plan) = active_plan.filter(|plan| plan.state == PlanState::Failed) {
            return Self::PlanningFailed {
                plan_id: plan.id.clone(),
                turn_count: plan.generation.budget.turn_count,
            };
        }
        if let Some(elicitation) = active_elicitation {
            return Self::AwaitingInput {
                owner: elicitation.owner,
                plan_id: elicitation.plan_id.clone(),
                interaction_id: elicitation.interaction_id.clone(),
            };
        }
        if let Some(plan) = active_plan.filter(|plan| plan.state == PlanState::AwaitingReview) {
            return Self::AwaitingPlanReview {
                plan_id: plan.id.clone(),
                revision: plan.model_revision,
            };
        }
        if let (Some(plan), Some((started_at_ms, WorkflowActivity::Planning))) = (
            active_plan.filter(|plan| {
                matches!(plan.state, PlanState::Generating | PlanState::Revising)
                    && plan.generation.budget.turn_count > 0
            }),
            working,
        ) {
            return Self::RetryingPlanGeneration {
                plan_id: plan.id.clone(),
                turn: plan.generation.budget.turn_count + 1,
                max_turn: plan.generation.budget.max_turn_count,
                started_at_ms,
            };
        }
        if let Some(wait) = active_wait {
            return Self::WaitingForAgent {
                agent_count: wait.agent_count,
            };
        }
        if let Some((started_at_ms, activity)) = working {
            return Self::Working {
                started_at_ms,
                activity,
            };
        }
        Self::Idle
    }

    /// Report whether the structural idle phase should remain invisible.
    pub fn visible(&self) -> bool {
        !matches!(self, Self::Idle)
    }
}

#[cfg(test)]
mod test {
    use super::{SessionPhase, WorkflowActivity};
    use crate::{
        broker::{ActiveElicitation, ElicitationOwner},
        plan::{PlanRecord, PlanState},
    };

    fn plan(state: PlanState) -> PlanRecord {
        PlanRecord {
            id: "plan".into(),
            session_id: "session".into(),
            request: "plan".into(),
            title: "Plan".into(),
            state,
            working_path: String::new(),
            document_version: 1,
            model_revision: 0,
            submitted_version: None,
            accepted_revision: None,
            user_revision: 0,
            review_digest: None,
            accepted_digest: None,
            elicitation: None,
            acceptance: None,
            question_ledger: Default::default(),
            generation: Default::default(),
            created_at_ms: 0,
            updated_at_ms: 0,
        }
    }

    #[test]
    fn working_phase_preempts_stale_waiting_state() {
        assert_eq!(
            SessionPhase::resolve(None, None, None, Some((42, WorkflowActivity::Planning))),
            SessionPhase::Working {
                started_at_ms: 42,
                activity: WorkflowActivity::Planning,
            }
        );
    }

    #[test]
    fn idle_phase_remains_structural_and_invisible() {
        assert!(!SessionPhase::Idle.visible());
    }

    #[test]
    fn planning_failure_preempts_stale_input_projection() {
        let plan = plan(PlanState::Failed);
        let elicitation = ActiveElicitation {
            owner: ElicitationOwner::Plan,
            plan_id: Some(plan.id.clone()),
            interaction_id: None,
            elicitation: crate::plan::PlanElicitation::new(
                crate::plan::PlanQuestionSet::freeform("Old question".into())
                    .normalize()
                    .unwrap(),
            ),
        };
        assert!(matches!(
            SessionPhase::resolve(Some(&plan), Some(&elicitation), None, None),
            SessionPhase::PlanningFailed { .. }
        ));
    }

    #[test]
    fn retry_projection_exposes_authoritative_budget_position() {
        let mut plan = plan(PlanState::Generating);
        plan.generation.budget.observe(false);
        assert_eq!(
            SessionPhase::resolve(
                Some(&plan),
                None,
                None,
                Some((42, WorkflowActivity::Planning))
            ),
            SessionPhase::RetryingPlanGeneration {
                plan_id: "plan".into(),
                turn: 2,
                max_turn: 20,
                started_at_ms: 42,
            }
        );
    }
}
