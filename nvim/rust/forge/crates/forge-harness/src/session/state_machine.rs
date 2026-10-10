use serde::Serialize;

use crate::{
    broker::ElicitationOwner,
    exchange::{ActiveWait, Exchange, ExchangeKind, ExchangeState},
    plan::{PlanPhase, PlanRecord, PlanState, execution::SemanticProgress},
};

/// Represents the user-visible work class rendered by the timeline status.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum WorkflowActivity {
    Working,
    Planning,
    Revising,
    Implementing,
    Checking,
    Verifying,
    Resolving,
    Goal,
}

impl WorkflowActivity {
    /// Label an active exchange and its live status with the same activity.
    pub fn active_label(self) -> &'static str {
        match self {
            Self::Working | Self::Goal => "Working",
            Self::Planning => "Planning",
            Self::Revising => "Revising plan",
            Self::Implementing => "Implementing",
            Self::Checking => "Checking",
            Self::Verifying => "Verifying",
            Self::Resolving => "Resolving",
        }
    }

    /// Label an exchange whose activity finished.
    pub fn completed_label(self) -> &'static str {
        match self {
            Self::Working | Self::Goal => "Thought",
            Self::Planning | Self::Revising => "Planned",
            Self::Implementing => "Implemented",
            Self::Checking => "Checked",
            Self::Verifying => "Verified",
            Self::Resolving => "Resolved",
        }
    }

    /// Name the task phase independently of its transition.
    pub fn lifecycle_label(self) -> &'static str {
        match self {
            Self::Working => "Work",
            Self::Planning => "Planning",
            Self::Revising => "Plan revision",
            Self::Implementing => "Plan implementation",
            Self::Checking => "Checks",
            Self::Verifying => "Plan verification",
            Self::Resolving => "Plan resolution",
            Self::Goal => "Goal",
        }
    }

    /// Render a persisted task lifecycle event.
    pub fn event(self, action: LifecycleAction) -> String {
        format!("{} {}", self.lifecycle_label(), action.label())
    }

    /// Resolve activity from the exchange kind and execution phase.
    pub fn from_exchange(exchange: &Exchange) -> Self {
        match exchange.kind {
            ExchangeKind::PlanDraft => Self::Planning,
            ExchangeKind::PlanRevision => Self::Revising,
            ExchangeKind::PlanExecution => exchange.execution_phase.as_ref()
                .map_or(Self::Implementing, |phase| phase.phase.into()),
            ExchangeKind::Chat if exchange.goal_id.is_some() => Self::Goal,
            ExchangeKind::Chat => Self::Working,
        }
    }
}

impl From<PlanPhase> for WorkflowActivity {
    fn from(phase: PlanPhase) -> Self {
        match phase {
            PlanPhase::Implement => Self::Implementing,
            PlanPhase::Checking => Self::Checking,
            PlanPhase::Verify => Self::Verifying,
            PlanPhase::Resolve => Self::Resolving,
        }
    }
}

#[derive(Clone, Copy)]
/// Distinguishes entry, automatic continuation, and explicit resumption.
pub enum LifecycleAction { Started, Continued, Resumed }

impl LifecycleAction {
    fn label(self) -> &'static str {
        match self {
            Self::Started => "started",
            Self::Continued => "continued",
            Self::Resumed => "resumed",
        }
    }
}

/// Represents the authoritative workflow phase exposed to one session timeline.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum SessionPhase {
    #[default]
    Idle,
    Paused,
    Finalizing {
        exchange_id: String,
        error: Option<String>,
    },
    Working {
        started_at_ms: i64,
        activity: WorkflowActivity,
        reasoning_summary: Option<String>,
        execution: Option<ExecutionStatus>,
    },
    AwaitingInput {
        owner: ElicitationOwner,
        plan_id: Option<String>,
        exchange_id: Option<String>,
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
        activity: WorkflowActivity,
    },
    PlanningFailed {
        plan_id: String,
        turn_count: u32,
        reason: String,
    },
    WaitingForAgent {
        agent_count: usize,
    },
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
/// Retains file comparison evidence independently of the model's reasoning summary.
pub struct ExecutionStatus {
    /// Execution identity used to preserve detail folds across progress updates.
    pub id: String,
    /// Current gate of the accepted plan's execution.
    pub phase: PlanPhase,
    /// Latest comparison of the accepted design and workspace files.
    pub progress: SemanticProgress,
}

impl SessionPhase {
    /// Resolve one deterministic workflow phase from the session's durable owners.
    pub fn resolve(
        active_plan: Option<&PlanRecord>,
        active_wait: Option<&ActiveWait>,
        exchange: Option<&Exchange>,
    ) -> Self {
        let executing = exchange.is_some_and(|exchange| {
            exchange.state == ExchangeState::Running && exchange.execution_started_at_ms.is_some()
        });
        if let Some(exchange) =
            exchange.filter(|exchange| exchange.state == ExchangeState::Finalizing)
        {
            return Self::Finalizing {
                exchange_id: exchange.id.clone(),
                error: exchange.finalization_error.clone(),
            };
        }
        if let Some(plan) = active_plan.filter(|plan| plan.state == PlanState::Failed) {
            return Self::PlanningFailed {
                plan_id: plan.id.clone(),
                turn_count: plan.generation.budget.turn_count,
                reason: if plan.generation.budget.turn_count >= plan.generation.budget.max_turn_count {
                    "continuation limit reached"
                } else if plan.generation.budget.consecutive_no_progress >= plan.generation.budget.max_consecutive_no_progress {
                    "no progress limit reached"
                } else {
                    "planning could not finish"
                }.into(),
            };
        }
        if let Some(plan) = active_plan
            .filter(|plan| !executing && (plan.acceptance.is_some() || plan.elicitation.is_some()))
        {
            return Self::AwaitingInput {
                owner: if plan.acceptance.is_some() {
                    ElicitationOwner::PlanAcceptance
                } else {
                    ElicitationOwner::Plan
                },
                plan_id: Some(plan.id.clone()),
                exchange_id: None,
            };
        }
        if let Some(exchange) = exchange.filter(|exchange| {
            exchange.state == ExchangeState::Running
                && !executing
                && (exchange.awaiting_input || exchange.elicitation.is_some())
        }) {
            return Self::AwaitingInput {
                owner: ElicitationOwner::Interaction,
                plan_id: exchange.plan_id.clone(),
                exchange_id: Some(exchange.id.clone()),
            };
        }
        if let Some(plan) =
            active_plan.filter(|plan| !executing && plan.state == PlanState::AwaitingReview)
        {
            return Self::AwaitingPlanReview {
                plan_id: plan.id.clone(),
                revision: plan.model_revision,
            };
        }
        if exchange.is_some_and(|exchange| {
            exchange.state == ExchangeState::Running
                && exchange.execution_started_at_ms.is_none()
                && exchange.turn.last().is_some_and(|turn| {
                    matches!(
                        turn.state(),
                        crate::turn::TurnState::Finished {
                            outcome: crate::turn::TurnOutcome::Interrupted
                                | crate::turn::TurnOutcome::Cancelled
                        }
                    )
                })
        }) {
            return Self::Paused;
        }
        let working = exchange
            .filter(|exchange| exchange.state == ExchangeState::Running)
            .map(|exchange| {
                let activity = WorkflowActivity::from_exchange(exchange);
                (
                    exchange
                        .execution_started_at_ms
                        .unwrap_or(exchange.created_at_ms),
                    activity,
                    exchange.latest_reasoning_summary().map(str::to_owned),
                )
            });
        if let (Some(plan), Some((started_at_ms, activity @ (WorkflowActivity::Planning | WorkflowActivity::Revising), _))) = (
            active_plan.filter(|plan| {
                matches!(plan.state, PlanState::Generating | PlanState::Revising)
                    && plan.generation.budget.turn_count > 0
            }),
            working.as_ref(),
        ) {
            return Self::RetryingPlanGeneration {
                plan_id: plan.id.clone(),
                turn: plan.generation.budget.turn_count + 1,
                max_turn: plan.generation.budget.max_turn_count,
                started_at_ms: *started_at_ms,
                activity: *activity,
            };
        }
        if let Some(wait) = active_wait {
            return Self::WaitingForAgent {
                agent_count: wait.agent_count,
            };
        }
        if let Some((started_at_ms, activity, reasoning_summary)) = working {
            return Self::Working {
                started_at_ms,
                activity,
                reasoning_summary,
                execution: None,
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
        agent::Delegation,
        exchange::{Exchange, ExchangeKind, ExchangeState},
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
            validation_warning: Vec::new(),
            created_at_ms: 0,
            updated_at_ms: 0,
        }
    }

    fn exchange() -> Exchange {
        let mut exchange = Exchange::delegated(
            "session",
            &Delegation {
                id: "delegation".into(),
                parent_exchange_id: "parent".into(),
                parent_turn_id: None,
                child_agent_id: "agent".into(),
                child_exchange_id: "exchange".into(),
                task: "task".into(),
                created_at_ms: 42,
            },
            1,
        );
        exchange.state = ExchangeState::Running;
        exchange.kind = ExchangeKind::PlanDraft;
        exchange.resume(42).unwrap();
        exchange
    }

    #[test]
    fn planning_retries_preserve_creation_and_revision_activity() {
        for (kind, state, activity) in [
            (ExchangeKind::PlanDraft, PlanState::Generating, WorkflowActivity::Planning),
            (ExchangeKind::PlanRevision, PlanState::Revising, WorkflowActivity::Revising),
        ] {
            let mut exchange = exchange();
            exchange.kind = kind;
            let mut plan = plan(state);
            plan.generation.budget.observe(true);
            assert!(matches!(SessionPhase::resolve(Some(&plan), None, Some(&exchange)),
                SessionPhase::RetryingPlanGeneration { activity: actual, .. } if actual == activity));
            plan.state = PlanState::Failed;
            plan.generation.budget.turn_count = plan.generation.budget.max_turn_count;
            assert!(matches!(SessionPhase::resolve(Some(&plan), None, Some(&exchange)),
                SessionPhase::PlanningFailed { reason, .. } if reason == "continuation limit reached"));
            plan.generation.budget.turn_count = 2;
            plan.generation.budget.consecutive_no_progress = plan.generation.budget.max_consecutive_no_progress;
            assert!(matches!(SessionPhase::resolve(Some(&plan), None, Some(&exchange)),
                SessionPhase::PlanningFailed { reason, .. } if reason == "no progress limit reached"));
        }
    }

    #[test]
    fn running_exchange_always_has_working_status() {
        assert_eq!(
            SessionPhase::resolve(None, None, Some(&exchange()),),
            SessionPhase::Working {
                started_at_ms: 42,
                activity: WorkflowActivity::Planning,
                execution: None,
                reasoning_summary: None,
            }
        );
    }

    #[test]
    fn question_wait_yields_to_execution_and_returns_after_the_reply() {
        for planning in [false, true] {
            let elicitation = crate::plan::PlanElicitation::new(
                crate::plan::PlanQuestionSet::freeform("Which scope?".into())
                    .normalize()
                    .unwrap(),
            );
            let mut plan = plan(PlanState::AwaitingInput);
            let mut exchange = exchange();
            if planning {
                plan.elicitation = Some(elicitation);
            } else {
                exchange.elicitation = Some(elicitation);
            }
            let active_plan = planning.then_some(&plan);
            exchange.awaiting_input = true;
            exchange.pause(50);
            assert!(matches!(
                SessionPhase::resolve(active_plan, None, Some(&exchange)),
                SessionPhase::AwaitingInput { .. }
            ));
            exchange.resume(60).unwrap();
            assert!(matches!(
                SessionPhase::resolve(active_plan, None, Some(&exchange)),
                SessionPhase::Working {
                    started_at_ms: 60,
                    ..
                }
            ));
            exchange.pause(70);
            assert!(matches!(
                SessionPhase::resolve(active_plan, None, Some(&exchange)),
                SessionPhase::AwaitingInput { .. }
            ));
        }
        let plan = plan(PlanState::AwaitingReview);
        let mut unrelated = exchange();
        unrelated.kind = ExchangeKind::Chat;
        assert!(matches!(
            SessionPhase::resolve(Some(&plan), None, Some(&unrelated)),
            SessionPhase::Working { .. }
        ));
        unrelated.finish(ExchangeState::Complete, 70).unwrap();
        assert!(matches!(
            SessionPhase::resolve(Some(&plan), None, Some(&unrelated)),
            SessionPhase::AwaitingPlanReview { .. }
        ));
    }

    #[test]
    fn idle_phase_remains_structural_and_invisible() {
        assert!(!SessionPhase::Idle.visible());
    }

    #[test]
    fn planning_failure_preempts_stale_input_projection() {
        let mut plan = plan(PlanState::Failed);
        plan.elicitation = Some(crate::plan::PlanElicitation::new(
            crate::plan::PlanQuestionSet::freeform("Old question".into())
                .normalize()
                .unwrap(),
        ));
        assert!(matches!(
            SessionPhase::resolve(Some(&plan), None, None),
            SessionPhase::PlanningFailed { .. }
        ));
    }

    #[test]
    fn retry_projection_exposes_authoritative_budget_position() {
        let mut plan = plan(PlanState::Generating);
        plan.generation.budget.observe(false);
        assert_eq!(
            SessionPhase::resolve(Some(&plan), None, Some(&exchange())),
            SessionPhase::RetryingPlanGeneration {
                plan_id: "plan".into(),
                turn: 2,
                max_turn: 20,
                activity: WorkflowActivity::Planning,
                started_at_ms: 42,
            }
        );
    }

    #[test]
    fn clarification_resume_and_terminal_phases_follow_exchange_state() {
        let mut exchange = exchange();
        exchange.kind = ExchangeKind::Chat;
        exchange.awaiting_input = true;
        exchange.execution_started_at_ms = None;
        assert!(matches!(
            SessionPhase::resolve(None, None, Some(&exchange)),
            SessionPhase::AwaitingInput { .. }
        ));
        exchange.resume(100).unwrap();
        assert_eq!(
            SessionPhase::resolve(None, None, Some(&exchange)),
            SessionPhase::Working {
                started_at_ms: 100,
                activity: WorkflowActivity::Working,
                execution: None,
                reasoning_summary: None,
            }
        );
        exchange.state = ExchangeState::Finalizing;
        assert!(matches!(
            SessionPhase::resolve(None, None, Some(&exchange)),
            SessionPhase::Finalizing { .. }
        ));
        for state in [
            ExchangeState::Complete,
            ExchangeState::Failed,
            ExchangeState::Cancelled,
            ExchangeState::Interrupted,
        ] {
            exchange.state = state;
            assert_eq!(
                SessionPhase::resolve(None, None, Some(&exchange)),
                SessionPhase::Idle
            );
        }
    }

    #[test]
    fn running_exchange_without_timer_cannot_become_idle() {
        let mut exchange = exchange();
        exchange.execution_started_at_ms = None;
        assert!(matches!(
            SessionPhase::resolve(None, None, Some(&exchange)),
            SessionPhase::Working {
                started_at_ms: 42,
                ..
            }
        ));
    }

    #[test]
    fn interrupted_restart_remains_paused_until_execution_resumes() {
        let mut exchange = exchange();
        exchange
            .start_turn(
                crate::backend::ProviderAddress {
                    thread_id: "thread".into(),
                    turn_id: "turn".into(),
                },
                42,
            )
            .unwrap();
        exchange.pause_for_restart(100).unwrap();
        assert_eq!(
            SessionPhase::resolve(None, None, Some(&exchange)),
            SessionPhase::Paused
        );
        exchange.resume(120).unwrap();
        assert!(matches!(
            SessionPhase::resolve(None, None, Some(&exchange)),
            SessionPhase::Working { .. }
        ));
    }
}
