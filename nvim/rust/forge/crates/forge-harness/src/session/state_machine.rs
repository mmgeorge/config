use serde::{Deserialize, Serialize};

use crate::{
    broker::ElicitationOwner,
    exchange::{ActiveWait, Exchange, ExchangeActivity, ExchangeState},
    plan::{PlanPhase, PlanRecord, PlanState, execution::SemanticProgress},
};

/// Represents the user-visible work class rendered by the timeline status.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum WorkflowActivity {
    Working,
    /// A finished turn's queued successor is about to start. Status shows the work continuing,
    /// and the editor reads the hand-off to leave the finished exchange where it is.
    HandingOff,
    /// A finished Implement turn's check run is reserved but has no record yet. Status keeps
    /// reading like the finished turn until the run's rows exist, then becomes `Checking` with
    /// those rows in one publication, so entering Check changes the status height once.
    ChecksStarting,
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
            Self::Working | Self::HandingOff | Self::ChecksStarting | Self::Goal => "Working",
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
            Self::Working | Self::HandingOff | Self::ChecksStarting | Self::Goal => "Thought",
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
            Self::Working | Self::HandingOff => "Work",
            Self::Planning => "Planning",
            Self::Revising => "Plan revision",
            Self::Implementing => "Plan implementation",
            Self::Checking | Self::ChecksStarting => "Checks",
            Self::Verifying => "Plan verification",
            Self::Resolving => "Plan resolution",
            Self::Goal => "Goal",
        }
    }

    /// Render a persisted task lifecycle event.
    pub fn event(self, action: LifecycleAction) -> String {
        format!("{} {}", self.lifecycle_label(), action.label())
    }

    /// Keeps historical exchanges independent of the session's active task.
    pub fn from_exchange(_exchange: &Exchange) -> Self {
        Self::Working
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
pub enum LifecycleAction {
    Started,
    Continued,
    Resumed,
}

impl LifecycleAction {
    fn label(self) -> &'static str {
        match self {
            Self::Started => "started",
            Self::Continued => "continued",
            Self::Resumed => "resumed",
        }
    }
}

/// A decision that waits for the user while its task is paused.
#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum PausedWait {
    /// A submitted plan revision awaits review.
    PlanReview { plan_id: String, revision: u32 },
}

impl SessionPhase {
    /// Pause the task this phase belongs to, keeping a plan review that waits for the user.
    ///
    /// Only a phase in which nothing runs yields. A turn still stopping keeps that phase,
    /// because its pause has not taken effect and showing Paused first would flash before
    /// Stopping returns. A turn that runs while the task is paused, such as an ordinary prompt
    /// in a session whose plan a restart paused, keeps its own phase, so the status row shows
    /// its elapsed time and interrupt key instead of a pause that describes only the task.
    pub(crate) fn paused(self) -> Self {
        let waiting = match self {
            running @ (Self::Stopping { .. }
            | Self::Compacting { .. }
            | Self::Finalizing { .. }
            | Self::Working { .. }
            | Self::AwaitingApproval { .. }
            | Self::AwaitingInput {
                exchange_id: Some(_),
                ..
            }
            | Self::RetryingPlanGeneration { .. }
            | Self::RetryingProvider { .. }
            | Self::WaitingForAgent { .. }) => return running,
            Self::AwaitingPlanReview { plan_id, revision } => {
                Some(PausedWait::PlanReview { plan_id, revision })
            }
            Self::Paused { waiting } => waiting,
            _ => None,
        };
        Self::Paused { waiting }
    }

    /// Name a blocked task's cause in place of an idle phase.
    ///
    /// Only an idle phase yields: a turn still stopping or finalizing, or queued input handing
    /// off to its turn, describes what happens next better than the block it replaces.
    pub(crate) fn blocked_by(self, task: Option<&crate::task::Task>) -> Self {
        match (self, task.and_then(crate::task::Task::blocked_cause)) {
            (Self::Idle, Some(reason)) => Self::Blocked { reason },
            (phase, _) => phase,
        }
    }

    /// Name a forked session's provider preparation in place of an idle phase.
    ///
    /// A prompt sent to a fork before the provider has copied the source conversation waits on
    /// that copy, which can take many seconds under load. Without this phase the status split
    /// stayed empty while the prompt read `[submitting]`.
    pub(crate) fn preparing_fork(self, fork_state: &crate::session::ProviderForkState) -> Self {
        match (self, fork_state) {
            (Self::Idle, crate::session::ProviderForkState::Preparing { .. }) => {
                Self::PreparingFork
            }
            (phase, _) => phase,
        }
    }
}

/// Represents the authoritative workflow phase exposed to one session timeline.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum SessionPhase {
    #[default]
    Idle,
    /// The task is paused; `waiting` names a decision that still waits for the user, so the
    /// status can say what resuming leads to.
    Paused {
        #[serde(skip_serializing_if = "Option::is_none")]
        waiting: Option<PausedWait>,
    },
    /// A Goal or Execute task stopped continuing on its own; `reason` says why, so the status
    /// can name it beside the commands that resume or end the task.
    Blocked {
        reason: String,
    },
    /// The provider is still copying the source conversation into this forked session, and
    /// provider-bound input waits until it finishes.
    PreparingFork,
    Stopping {
        collection: crate::exchange::ExchangeCollection,
    },
    Compacting {
        started_at_ms: i64,
        /// A turn of an unfinished exchange waits for this compaction: the turn it stopped runs
        /// again, or a running turn continues, once it ends. A compaction between turns, such as
        /// one while a plan waits in review, holds no turn.
        turn_waiting: bool,
    },
    Finalizing {
        exchange_id: String,
        outcome: Option<crate::exchange::ExchangeOutcome>,
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
    RetryingProvider {
        retry: crate::provider::retry::ProviderRetry,
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
        !self.progress.matched.is_empty()
            || !self.progress.missing.is_empty()
            || !self.progress.different.is_empty()
            || !self.progress.unverified.is_empty()
            || !self.progress.declaration_changes.is_empty()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PlanStatus {
    pub id: String,
    pub state: PlanState,
    pub model_revision: u32,
    pub review_questions_pending: bool,
    pub elicitation: bool,
    pub generation: crate::plan::PlanGeneration,
}

impl From<&PlanRecord> for PlanStatus {
    fn from(plan: &PlanRecord) -> Self {
        Self {
            id: plan.id.clone(),
            state: plan.state,
            model_revision: plan.model_revision,
            review_questions_pending: plan
                .review_questions()
                .is_some_and(|questions| questions.has_pending_decisions()),
            elicitation: plan.elicitation.is_some(),
            generation: plan.generation.clone(),
        }
    }
}

#[derive(Clone, Debug, Deserialize)]
pub(crate) struct ExecutionStatusSnapshot {
    pub id: String,
    pub state: crate::plan::PlanExecutionState,
    pub phase: PlanPhase,
    pub progress: SemanticProgress,
    pub check_run: Option<crate::checks::CheckRun>,
}

impl From<&crate::plan::PlanExecutionRecord> for ExecutionStatusSnapshot {
    fn from(execution: &crate::plan::PlanExecutionRecord) -> Self {
        Self {
            id: execution.id.clone(),
            state: execution.state,
            phase: execution.phase,
            progress: execution.progress.clone(),
            check_run: execution.check_runs.last().cloned(),
        }
    }
}

impl ExecutionStatusSnapshot {
    pub(crate) fn apply(&self, status: &mut SessionPhase) {
        if matches!(status, SessionPhase::Idle)
            && self.state == crate::plan::PlanExecutionState::Paused
        {
            *status = SessionPhase::Paused { waiting: None };
            return;
        }
        if !matches!(status, SessionPhase::Working { .. } | SessionPhase::Idle) {
            return;
        }
        if matches!(status, SessionPhase::Idle)
            && self.phase == PlanPhase::Checking
            && self.state == crate::plan::PlanExecutionState::Active
            && let Some(activity) = self
                .check_run
                .as_ref()
                .and_then(crate::checks::CheckActivity::from_run)
        {
            *status = SessionPhase::Working {
                duration_ms: activity.duration_ms,
                started_at_ms: activity.started_at_ms,
                activity: WorkflowActivity::Checking,
                reasoning_summary: None,
                execution: None,
            };
        }
        if let SessionPhase::Working { execution, .. } = status {
            *execution = Some(ExecutionStatus {
                id: self.id.clone(),
                phase: self.phase,
                progress: self.progress.clone(),
            });
        }
    }
}

impl SessionPhase {
    /// Resolve one deterministic workflow phase from the session's durable owners.
    pub fn resolve(
        active_plan: Option<&PlanRecord>,
        active_wait: Option<&ActiveWait>,
        exchange: Option<&Exchange>,
    ) -> Self {
        Self::resolve_status(
            active_plan.map(PlanStatus::from).as_ref(),
            active_wait,
            exchange,
        )
    }

    pub(crate) fn resolve_status(
        active_plan: Option<&PlanStatus>,
        active_wait: Option<&ActiveWait>,
        exchange: Option<&Exchange>,
    ) -> Self {
        if let Some(collection) = exchange.and_then(|exchange| exchange.collection) {
            return Self::Stopping { collection };
        }
        if let Some(exchange) = exchange
            && let Some(started_at_ms) = exchange.compaction_started_at_ms()
        {
            return Self::Compacting {
                started_at_ms,
                turn_waiting: !exchange.is_finished(),
            };
        }
        let active_wait = active_wait.filter(|wait| {
            exchange.is_some_and(|exchange| {
                exchange.id == wait.exchange_id
                    && matches!(
                        exchange.activity(),
                        ExchangeActivity::Active | ExchangeActivity::WaitingForAgent
                    )
            })
        });
        match exchange.map(Exchange::activity) {
            Some(ExchangeActivity::Paused) => return Self::Paused { waiting: None },
            Some(ExchangeActivity::AwaitingApproval) => {
                return Self::AwaitingApproval {
                    exchange_id: exchange.unwrap().id.clone(),
                };
            }
            Some(ExchangeActivity::Compacting { started_at_ms }) => {
                return Self::Compacting {
                    started_at_ms,
                    turn_waiting: true,
                };
            }
            Some(ExchangeActivity::WaitingForAgent) => {
                return Self::WaitingForAgent {
                    agent_count: active_wait.map_or(0, |wait| wait.agent_count),
                };
            }
            _ => {}
        }
        let executing = exchange.is_some_and(|exchange| {
            exchange.state() == ExchangeState::Running
                && exchange.execution_started_at_ms().is_some()
        });
        if let Some(exchange) =
            exchange.filter(|exchange| exchange.state() == ExchangeState::Finalizing)
        {
            return Self::Finalizing {
                exchange_id: exchange.id.clone(),
                outcome: exchange.finalization_outcome(),
                error: exchange.finalization_error.clone(),
            };
        }
        if let Some(plan) = active_plan.filter(|plan| plan.state == PlanState::Failed) {
            return Self::PlanningFailed {
                plan_id: plan.id.clone(),
                turn_count: plan.generation.budget.turn_count,
                reason: if plan.generation.budget.turn_count
                    >= plan.generation.budget.max_turn_count
                {
                    "continuation limit reached"
                } else if plan.generation.budget.consecutive_no_progress
                    >= plan.generation.budget.max_consecutive_no_progress
                {
                    "no progress limit reached"
                } else {
                    "planning could not finish"
                }
                .into(),
            };
        }
        if let Some(plan) = active_plan
            .filter(|plan| !executing && (plan.review_questions_pending || plan.elicitation))
        {
            return Self::AwaitingInput {
                owner: if plan.review_questions_pending {
                    ElicitationOwner::PlanReview
                } else {
                    ElicitationOwner::Plan
                },
                plan_id: Some(plan.id.clone()),
                exchange_id: None,
            };
        }
        // A tool waiting for the user's answer pauses the exchange's clock and reads as
        // `AwaitingInput`; the question lives in the question controller rather than on the
        // exchange, so without the activity the phase fell through to idle while it waited.
        if let Some(exchange) = exchange.filter(|exchange| {
            exchange.state() == ExchangeState::Running
                && (exchange.activity() == ExchangeActivity::AwaitingInput
                    || !executing
                        && (exchange.awaiting_input() || exchange.elicitation().is_some()))
        }) {
            return Self::AwaitingInput {
                owner: ElicitationOwner::Interaction,
                plan_id: None,
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
        if let Some(exchange) =
            exchange.filter(|exchange| exchange.activity() == ExchangeActivity::Active)
            && let Some(retry) = &exchange.provider_retry
        {
            return Self::RetryingProvider {
                retry: retry.clone(),
                activity: WorkflowActivity::from_exchange(exchange),
            };
        }
        let working = exchange
            .filter(|exchange| exchange.activity() == ExchangeActivity::Active)
            .map(|exchange| {
                let activity = WorkflowActivity::from_exchange(exchange);
                (
                    exchange.execution_started_at_ms(),
                    exchange.duration_ms(),
                    activity,
                    exchange.latest_reasoning_summary().map(str::to_owned),
                )
            });
        if let Some(wait) = active_wait {
            return Self::WaitingForAgent {
                agent_count: wait.agent_count,
            };
        }
        if let (
            Some(plan),
            Some((
                started_at_ms,
                duration_ms,
                activity @ (WorkflowActivity::Planning | WorkflowActivity::Revising),
                _,
            )),
        ) = (
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

    /// Computes elapsed time from recorded intervals without inventing a running clock.
    pub(crate) fn elapsed_ms(&self, now_ms: i64) -> u64 {
        match self {
            Self::Working {
                duration_ms,
                started_at_ms,
                ..
            }
            | Self::RetryingPlanGeneration {
                duration_ms,
                started_at_ms,
                ..
            } => duration_ms.saturating_add(
                started_at_ms.map_or(0, |started| now_ms.saturating_sub(started).max(0) as u64),
            ),
            Self::Compacting { started_at_ms, .. } => {
                now_ms.saturating_sub(*started_at_ms).max(0) as u64
            }
            _ => 0,
        }
    }

    /// Selects animation from lifecycle ownership rather than text or clock presence.
    pub(crate) fn animated(&self) -> bool {
        matches!(
            self,
            Self::Working { .. }
                | Self::RetryingPlanGeneration { .. }
                | Self::RetryingProvider { .. }
                | Self::Compacting { .. }
                | Self::WaitingForAgent { .. }
                | Self::PreparingFork
                | Self::Finalizing { error: None, .. }
                | Self::Stopping {
                    collection: crate::exchange::ExchangeCollection::Pending
                        | crate::exchange::ExchangeCollection::Settling
                }
        )
    }

    /// Selects periodic text refresh only when elapsed time can advance.
    pub(crate) fn ticking(&self) -> bool {
        matches!(
            self,
            Self::Working {
                started_at_ms: Some(_),
                ..
            } | Self::RetryingPlanGeneration {
                started_at_ms: Some(_),
                ..
            } | Self::Compacting { .. }
                | Self::RetryingProvider {
                    retry: crate::provider::retry::ProviderRetry {
                        retry_at_ms: Some(_),
                        ..
                    },
                    ..
                }
        )
    }
}

#[cfg(test)]
mod paused_tests {
    use super::{PausedWait, SessionPhase};

    /// Pausing keeps what still waits: a stopping turn stays Stopping, a plan review stays
    /// named, and other phases become a plain pause.
    #[test]
    fn paused_keeps_stopping_and_a_waiting_review() {
        let stopping = SessionPhase::Stopping {
            collection: crate::exchange::ExchangeCollection::Pending,
        };
        assert_eq!(stopping.clone().paused(), stopping);
        assert_eq!(
            SessionPhase::AwaitingPlanReview {
                plan_id: "plan".into(),
                revision: 2
            }
            .paused(),
            SessionPhase::Paused {
                waiting: Some(PausedWait::PlanReview {
                    plan_id: "plan".into(),
                    revision: 2
                })
            }
        );
        assert_eq!(
            SessionPhase::Idle.paused(),
            SessionPhase::Paused { waiting: None }
        );
    }

    /// A turn that runs while its task is paused, such as an ordinary prompt sent to a session
    /// whose plan a restart paused, keeps its own phase: the pause describes the task, and the
    /// status row describes what runs.
    #[test]
    fn paused_keeps_a_running_turn() {
        let working = SessionPhase::Working {
            duration_ms: 15_000,
            started_at_ms: Some(1),
            activity: super::WorkflowActivity::Working,
            reasoning_summary: None,
            execution: None,
        };
        let answering = SessionPhase::AwaitingInput {
            owner: super::ElicitationOwner::Interaction,
            plan_id: None,
            exchange_id: Some("exchange".into()),
        };
        for running in [
            working,
            answering,
            SessionPhase::AwaitingApproval {
                exchange_id: "exchange".into(),
            },
            SessionPhase::WaitingForAgent { agent_count: 1 },
            SessionPhase::Compacting {
                started_at_ms: 1,
                turn_waiting: true,
            },
        ] {
            assert_eq!(running.clone().paused(), running);
        }
        assert_eq!(
            SessionPhase::AwaitingInput {
                owner: super::ElicitationOwner::Plan,
                plan_id: Some("plan".into()),
                exchange_id: None,
            }
            .paused(),
            SessionPhase::Paused { waiting: None }
        );
    }
}
