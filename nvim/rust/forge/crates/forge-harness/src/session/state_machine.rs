use serde::Serialize;

use crate::{
    broker::ElicitationOwner,
    exchange::{ActiveWait, Exchange, ExchangeActivity, ExchangeKind, ExchangeState},
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
    Compacting {
        started_at_ms: i64,
    },
    Finalizing {
        exchange_id: String,
        error: Option<String>,
    },
    Working {
        /// Accumulated execution intervals, excluding user waits.
        duration_ms: u64,
        /// Current execution interval, absent while processing a settled provider result.
        started_at_ms: Option<i64>,
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
    AwaitingApproval {
        /// Exchange whose permission request owns the wait.
        exchange_id: String,
    },
    RetryingPlanGeneration {
        plan_id: String,
        turn: u32,
        max_turn: u32,
        /// Accumulated execution intervals retained across planning continuations.
        duration_ms: u64,
        /// Current execution interval, excluding approval and question waits.
        started_at_ms: Option<i64>,
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

impl ExecutionStatus {
    /// Reports recorded comparison evidence without treating an empty result as running work.
    pub(crate) fn has_comparison(&self) -> bool {
        !self.progress.matched.is_empty() || !self.progress.missing.is_empty()
            || !self.progress.different.is_empty() || !self.progress.unverified.is_empty()
            || !self.progress.declaration_changes.is_empty()
    }
}

impl SessionPhase {
    /// Resolve one deterministic workflow phase from the session's durable owners.
    pub fn resolve(
        active_plan: Option<&PlanRecord>,
        active_wait: Option<&ActiveWait>,
        exchange: Option<&Exchange>,
    ) -> Self {
        if let Some(started_at_ms) = exchange.and_then(Exchange::compaction_started_at_ms) {
            return Self::Compacting { started_at_ms };
        }
        let active_wait = active_wait.filter(|wait| exchange.is_some_and(|exchange|
            exchange.id == wait.exchange_id && matches!(exchange.activity(),
                ExchangeActivity::Active | ExchangeActivity::WaitingForAgent)));
        match exchange.map(Exchange::activity) {
            Some(ExchangeActivity::Paused) => return Self::Paused,
            Some(ExchangeActivity::AwaitingApproval) => return Self::AwaitingApproval {
                exchange_id: exchange.unwrap().id.clone(),
            },
            Some(ExchangeActivity::Compacting { started_at_ms }) => return Self::Compacting { started_at_ms },
            Some(ExchangeActivity::WaitingForAgent) => return Self::WaitingForAgent {
                agent_count: active_wait.map_or(0, |wait| wait.agent_count),
            },
            _ => {}
        }
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
        let working = exchange
            .filter(|exchange| exchange.activity() == ExchangeActivity::Active)
            .map(|exchange| {
                let activity = WorkflowActivity::from_exchange(exchange);
                (
                    exchange.execution_started_at_ms,
                    exchange.duration_ms,
                    activity,
                    exchange.latest_reasoning_summary().map(str::to_owned),
                )
            });
        if let Some(wait) = active_wait {
            return Self::WaitingForAgent {
                agent_count: wait.agent_count,
            };
        }
        if let (Some(plan), Some((started_at_ms, duration_ms, activity @ (WorkflowActivity::Planning | WorkflowActivity::Revising), _))) = (
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
                duration_ms: *duration_ms,
                activity: *activity,
            };
        }
        if let Some((started_at_ms, duration_ms, activity, reasoning_summary)) = working {
            return Self::Working {
                started_at_ms,
                duration_ms,
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

    /// Computes elapsed time from recorded intervals without inventing a running clock.
    pub(crate) fn elapsed_ms(&self, now_ms: i64) -> u64 {
        match self {
            Self::Working { duration_ms, started_at_ms, .. }
            | Self::RetryingPlanGeneration { duration_ms, started_at_ms, .. } =>
                duration_ms.saturating_add(started_at_ms.map_or(0,
                    |started| now_ms.saturating_sub(started).max(0) as u64)),
            Self::Compacting { started_at_ms } => now_ms.saturating_sub(*started_at_ms).max(0) as u64,
            _ => 0,
        }
    }

    /// Selects animation from lifecycle ownership rather than text or clock presence.
    pub(crate) fn animated(&self) -> bool {
        matches!(self, Self::Working { .. } | Self::RetryingPlanGeneration { .. }
            | Self::Compacting { .. } | Self::WaitingForAgent { .. } | Self::Finalizing { error: None, .. })
    }

    /// Selects periodic text refresh only when elapsed time can advance.
    pub(crate) fn ticking(&self) -> bool {
        matches!(self, Self::Working { started_at_ms: Some(_), .. }
            | Self::RetryingPlanGeneration { started_at_ms: Some(_), .. } | Self::Compacting { .. })
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
            manual_changes: Vec::new(),
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
                duration_ms: 0, started_at_ms: Some(42),
                activity: WorkflowActivity::Planning,
                execution: None,
                reasoning_summary: None,
            }
        );
    }

    #[test]
    fn compaction_restores_workflow_status_after_every_terminal_outcome() {
        for state in [crate::backend::CompactionState::Completed,
            crate::backend::CompactionState::Failed, crate::backend::CompactionState::Unconfirmed] {
            let mut exchange = exchange();
            let original = SessionPhase::resolve(None, None, Some(&exchange));
            let mut compaction = crate::backend::Compaction {
                id: "compaction".into(), thread_id: "provider".into(),
                state: crate::backend::CompactionState::Running, manual: false,
                started_at_ms: None, completed_at_ms: None, result: Default::default(),
            };
            exchange.observe_compaction(compaction.clone(), 50);
            assert_eq!(SessionPhase::resolve(None, None, Some(&exchange)),
                SessionPhase::Compacting { started_at_ms: 50 });
            compaction.state = state;
            exchange.observe_compaction(compaction, 60);
            assert_eq!(SessionPhase::resolve(None, None, Some(&exchange)), original);
        }
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
                    started_at_ms: Some(60),
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
                duration_ms: 0, started_at_ms: Some(42),
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
                duration_ms: 0, started_at_ms: Some(100),
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
                started_at_ms: None,
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
                    owner: None,
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

    #[test]
    fn terminal_exchanges_release_approval_status_without_hiding_background_compaction() {
        for state in [ExchangeState::Finalizing, ExchangeState::Complete, ExchangeState::Failed,
            ExchangeState::Cancelled, ExchangeState::Interrupted] {
            let mut exchange = exchange();
            exchange.observe_compaction(crate::backend::Compaction {
                id:"compaction".into(), thread_id:"thread".into(),
                state:crate::backend::CompactionState::Running, manual:false,
                started_at_ms:Some(42), completed_at_ms:None, result:Default::default(),
            }, 42);
            exchange.observe_blocker("approval:command".into(),true,50);
            exchange.state = state;
            assert_eq!(exchange.activity(),crate::exchange::ExchangeActivity::Settled);
            let status = SessionPhase::resolve(None,None,Some(&exchange));
            assert_eq!(status,SessionPhase::Compacting {started_at_ms:42});
            exchange.node_list.clear();
            let status = SessionPhase::resolve(None,None,Some(&exchange));
            assert!(matches!(status,SessionPhase::Finalizing { .. } | SessionPhase::Idle),"{state:?}: {status:?}");
        }
        let mut exchange = exchange();
        exchange.start_turn(crate::backend::ProviderAddress {
            owner:None, thread_id:"thread".into(), turn_id:"turn".into(),
        },42).unwrap();
        exchange.observe_blocker("approval:command".into(),true,50);
        exchange.pause_for_restart(60).unwrap();
        assert_eq!(SessionPhase::resolve(None,None,Some(&exchange)),SessionPhase::Paused);
    }
}
