use std::sync::Arc;

use anyhow::{Context, Result, ensure};

use crate::{
    goal::GoalState,
    plan::PlanExecutionState,
    storage::{SqliteStore, admission::SessionAdmission},
};

use super::{ActiveTask, TaskKind, TaskPermit, TaskRevision, TaskStatus, TaskWorkflow};

pub(crate) enum TaskChange {
    Install(Box<ActiveTask>),
    Update(Box<ActiveTask>),
    Preserve,
    Pause,
    Resume,
    ResumeWithEntry(String),
    Clear,
}

/// Coordinates task mutations against the session's committed admission projection.
pub(crate) struct SessionTaskController {
    session_id: String,
    admission: Arc<SessionAdmission>,
}

impl SessionTaskController {
    pub(crate) fn apply_plan_answer(
        task: &mut ActiveTask,
        answered: crate::plan::PlanRecord,
    ) -> Result<()> {
        let TaskWorkflow::Plan { plan } = &mut task.workflow else {
            anyhow::bail!("answer no longer belongs to the active planning task");
        };
        ensure!(
            plan.artifact.id == answered.id,
            "answer belongs to a replaced planning task"
        );
        plan.artifact = answered;
        Ok(())
    }
    pub(crate) fn new(admission: Arc<SessionAdmission>) -> Result<Self> {
        let session_id = admission.snapshot()?.session.id.clone();
        Ok(Self {
            session_id,
            admission,
        })
    }

    pub(crate) fn current(&self) -> Result<Option<ActiveTask>> {
        Ok(self.admission.runtime()?.active_task.clone())
    }

    /// Applies a task-specific mutation atomically against its admitted contract.
    pub(crate) fn update<Output>(
        &self,
        store: &mut SqliteStore,
        permit: &TaskPermit,
        now_ms: i64,
        change: impl FnOnce(&mut ActiveTask) -> Result<Output>,
    ) -> Result<Output> {
        store.commit_task_change(
            &self.session_id,
            Some(permit.revision()),
            now_ms,
            |active| {
                let task = active.as_mut().context("session has no active task")?;
                permit.validate(Some(task))?;
                let previous = task.clone();
                let output = change(task)?;
                if !task.same_contract(&previous) && task.revision == previous.revision {
                    task.renew(now_ms);
                } else {
                    task.updated_at_ms = now_ms;
                }
                Ok(output)
            },
        )
    }

    /// Reserves one exact continuation after a finalized turn without replaying a prior reservation.
    pub(crate) fn reserve_continuation(
        &self,
        store: &mut SqliteStore,
        permit: &TaskPermit,
        completed_turn: super::TaskTurnId,
        instruction: String,
        reason: String,
        now_ms: i64,
    ) -> Result<bool> {
        ensure!(
            !instruction.trim().is_empty(),
            "continuation instruction is required"
        );
        self.update(store, permit, now_ms, |task| {
            ensure!(
                task.entry_accepted(),
                "task entry has not been acknowledged"
            );
            ensure!(
                task.status() == TaskStatus::Running,
                "task cannot continue in its current state"
            );
            if task.turn.as_ref().is_some_and(|turn| {
                turn.id > completed_turn
                    || turn.id == completed_turn
                        && turn.continuation.as_ref().is_some_and(|input| {
                            !(input.work == super::TaskContinuationWork::Checks
                                && input.delivery == super::TaskInputDelivery::Accepted)
                        })
            }) {
                return Ok(false);
            }
            task.turn = Some(super::TaskTurn {
                id: completed_turn,
                continuation: Some(super::TaskContinuation {
                    id: super::TaskInputId::fresh(),
                    instruction,
                    reason,
                    delivery: super::TaskInputDelivery::Pending,
                    work: super::TaskContinuationWork::Provider,
                }),
            });
            Ok(true)
        })
    }

    /// Schedules Harness-owned verification at the same durable turn boundary as provider work.
    pub(crate) fn reserve_checks(
        &self,
        store: &mut SqliteStore,
        permit: &TaskPermit,
        completed_turn: super::TaskTurnId,
        now_ms: i64,
    ) -> Result<bool> {
        self.update(store, permit, now_ms, |task| {
            ensure!(
                task.entry_accepted(),
                "task entry has not been acknowledged"
            );
            ensure!(
                matches!(&task.workflow, TaskWorkflow::Execute { execution }
                if execution.record.state == PlanExecutionState::Active
                    && execution.record.phase == crate::plan::PlanPhase::Checking),
                "checks require active execution in Checking"
            );
            if task.turn.as_ref().is_some_and(|turn| {
                turn.id > completed_turn || turn.id == completed_turn && turn.continuation.is_some()
            }) {
                return Ok(false);
            }
            task.turn = Some(super::TaskTurn {
                id: completed_turn,
                continuation: Some(super::TaskContinuation {
                    id: super::TaskInputId::fresh(),
                    instruction: String::new(),
                    reason: "Run the accepted plan's verification checks".into(),
                    delivery: super::TaskInputDelivery::Pending,
                    work: super::TaskContinuationWork::Checks,
                }),
            });
            Ok(true)
        })
    }

    /// Commits dependent artifacts and task mutation under one revision fence.
    pub(crate) fn update_with_effects<Output>(
        &self,
        store: &mut SqliteStore,
        permit: &TaskPermit,
        now_ms: i64,
        change: impl FnOnce(&mut ActiveTask) -> Result<(Output, crate::storage::TaskEffects)>,
    ) -> Result<Output> {
        store
            .commit_task_effects(
                &self.session_id,
                Some(permit.revision()),
                now_ms,
                None,
                |active| {
                    let task = active.as_mut().context("session has no active task")?;
                    permit.validate(Some(task))?;
                    let previous = task.clone();
                    let result = change(task)?;
                    if !task.same_contract(&previous) && task.revision == previous.revision {
                        task.renew(now_ms);
                    } else {
                        task.updated_at_ms = now_ms;
                    }
                    Ok(result)
                },
            )?
            .context("task update unexpectedly reused a command")
    }

    /// Publishes a terminal outcome and removes its live owner in the same commit.
    pub(crate) fn finish(
        &self,
        store: &mut SqliteStore,
        permit: &TaskPermit,
        state: super::TaskOutcomeState,
        summary: String,
        now_ms: i64,
    ) -> Result<super::TaskOutcome> {
        store
            .commit_task_effects(
                &self.session_id,
                Some(permit.revision()),
                now_ms,
                None,
                |active| {
                    let task = active.as_ref().context("session has no active task")?;
                    permit.validate(Some(task))?;
                    let outcome = super::TaskOutcome {
                        kind: task.kind(),
                        title: task.title().into(),
                        state,
                        summary,
                    };
                    let effects = outcome_effect(&self.session_id, task, outcome.clone(), now_ms);
                    *active = None;
                    Ok((outcome, effects))
                },
            )?
            .context("task completion unexpectedly reused a command")
    }

    pub(crate) fn admit(&self, kind: TaskKind) -> Result<TaskPermit> {
        let task = self.current()?.context("session has no active task")?;
        ensure!(
            task.entry_accepted(),
            "task entry has not been acknowledged by the provider"
        );
        ensure!(
            task.kind() == kind,
            "operation does not belong to the active task"
        );
        ensure!(
            matches!(task.status(), TaskStatus::Running | TaskStatus::Waiting),
            "active task does not currently admit control operations"
        );
        Ok(task.permit())
    }

    pub(crate) fn admit_goal_control(&self, completing: bool) -> Result<TaskPermit> {
        let task = self.current()?.context("session has no active task")?;
        ensure!(
            task.entry_accepted(),
            "task entry has not been acknowledged by the provider"
        );
        ensure!(
            task.status() == TaskStatus::Running
                && matches!(task.kind(), TaskKind::Goal | TaskKind::Execute),
            "goal control requires an active goal. Resume paused or blocked goals explicitly before reporting completion or blockage"
        );
        ensure!(
            !completing || task.kind() == TaskKind::Goal,
            "plan execution completes only through plan_phase_done in Verify"
        );
        Ok(task.permit())
    }

    /// Activates only the contract whose exact entry received a durable provider receipt.
    pub(crate) fn acknowledge_entry(
        &self,
        store: &mut SqliteStore,
        revision: &TaskRevision,
        id: &super::TaskInputId,
        now_ms: i64,
    ) -> Result<bool> {
        let result = store.commit_task_change(&self.session_id, Some(revision), now_ms, |active| {
            let task = active.as_mut().context("session has no active task")?;
            let Some(entry) = task.entry.as_mut().filter(|entry| &entry.id == id) else {
                return Ok(false);
            };
            if entry.delivery == crate::task::TaskInputDelivery::Accepted {
                return Ok(true);
            }
            entry.delivery = crate::task::TaskInputDelivery::Accepted;
            task.updated_at_ms = now_ms;
            Ok(true)
        });
        match result {
            Err(error) if error.is::<super::StaleTask>() => Ok(false),
            result => result,
        }
    }

    /// Commits one terminal turn's evidence and continuation intent under its admitted revision.
    pub(crate) fn observe_goal(
        &self,
        store: &mut SqliteStore,
        permit: &TaskPermit,
        completed_turn: crate::task::TaskTurnId,
        mut evidence: crate::goal::TurnEvidence,
        now_ms: i64,
    ) -> Result<(super::GoalTask, crate::goal::ContinuationDecision)> {
        store
            .commit_task_effects(
                &self.session_id,
                Some(permit.revision()),
                now_ms,
                None,
                |active| {
                    let task = active.as_mut().context("session has no active task")?;
                    permit.validate(Some(task))?;
                    ensure!(
                        task.entry_accepted(),
                        "task entry has not been acknowledged by the provider"
                    );
                    if task
                        .turn
                        .as_ref()
                        .is_some_and(|turn| turn.id >= completed_turn)
                    {
                        return Ok((
                            (goal_view(task)?, crate::goal::ContinuationDecision::Stop),
                            crate::storage::TaskEffects::default(),
                        ));
                    }
                    let previous = task.clone();
                    let decision = match &mut task.workflow {
                        TaskWorkflow::Goal { goal } => crate::goal::observe(
                            &mut goal.state,
                            &mut goal.continuation,
                            goal.native,
                            evidence,
                        ),
                        TaskWorkflow::Execute { execution } => {
                            evidence.structured_complete = false;
                            let mut state = match execution.record.state {
                                PlanExecutionState::Active => GoalState::Active,
                                PlanExecutionState::Paused => GoalState::Paused,
                                PlanExecutionState::Blocked => GoalState::Blocked,
                                PlanExecutionState::Stalled => GoalState::Stalled,
                                PlanExecutionState::Complete => GoalState::Complete,
                                PlanExecutionState::Cancelled => GoalState::Cleared,
                            };
                            let decision = crate::goal::observe(
                                &mut state,
                                &mut execution.continuation,
                                false,
                                evidence,
                            );
                            execution.record.state = match state {
                                GoalState::Active => PlanExecutionState::Active,
                                GoalState::Paused => PlanExecutionState::Paused,
                                GoalState::Complete => PlanExecutionState::Complete,
                                GoalState::Cleared => PlanExecutionState::Cancelled,
                                GoalState::Stalled => PlanExecutionState::Stalled,
                                _ => PlanExecutionState::Blocked,
                            };
                            decision
                        }
                        TaskWorkflow::Plan { .. } => {
                            anyhow::bail!("goal evidence requires Goal or Execute")
                        }
                    };
                    let goal = goal_view(task)?;
                    if task.same_contract(&previous) {
                        task.updated_at_ms = now_ms;
                    } else {
                        task.renew(now_ms);
                    }
                    let continuation = match (&task.workflow, decision) {
                        (
                            TaskWorkflow::Goal { goal },
                            crate::goal::ContinuationDecision::Continue
                            | crate::goal::ContinuationDecision::RetryNoProgress,
                        ) => Some(super::TaskContinuation {
                            id: super::TaskInputId::fresh(),
                            instruction: if goal.native {
                                "/goal resume".into()
                            } else {
                                format!("Continue working toward this goal: {}", goal.objective)
                            },
                            reason: "The active task requires another turn".into(),
                            delivery: crate::task::TaskInputDelivery::Pending,
                            work: super::TaskContinuationWork::Provider,
                        }),
                        _ => None,
                    };
                    task.turn = Some(super::TaskTurn {
                        id: completed_turn,
                        continuation,
                    });
                    let mut effects = crate::storage::TaskEffects::default();
                    if task.status().terminal() {
                        let outcome = super::TaskOutcome {
                            kind: task.kind(),
                            title: task.title().into(),
                            state: if task.status() == TaskStatus::Completed {
                                super::TaskOutcomeState::Complete
                            } else {
                                super::TaskOutcomeState::Cancelled
                            },
                            summary: match task.status() {
                                TaskStatus::Completed => "Task completed".into(),
                                _ => "Task cleared".into(),
                            },
                        };
                        effects = outcome_effect(&self.session_id, task, outcome, now_ms);
                        *active = None;
                    }
                    Ok(((goal, decision), effects))
                },
            )?
            .context("turn observation unexpectedly reused a command")
    }

    /// Commits planning progress once per completed turn under the current contract.
    pub(crate) fn observe_plan(
        &self,
        store: &mut SqliteStore,
        permit: &TaskPermit,
        completed_turn: crate::task::TaskTurnId,
        canonical_progress: bool,
        now_ms: i64,
    ) -> Result<Option<(crate::plan::PlanRecord, bool)>> {
        store.commit_task_change(
            &self.session_id,
            Some(permit.revision()),
            now_ms,
            |active| {
                let task = active.as_mut().context("session has no active task")?;
                permit.validate(Some(task))?;
                ensure!(
                    task.entry_accepted(),
                    "plan entry has not been acknowledged"
                );
                if task
                    .turn
                    .as_ref()
                    .is_some_and(|turn| turn.id >= completed_turn)
                {
                    return Ok(None);
                }
                let TaskWorkflow::Plan { plan } = &mut task.workflow else {
                    anyhow::bail!("planning progress requires an active plan");
                };
                ensure!(!plan.paused, "planning task is paused");
                if canonical_progress {
                    plan.artifact.generation.canonical_revision = plan
                        .artifact
                        .generation
                        .canonical_revision
                        .saturating_add(1);
                }
                let budget_allows = plan.artifact.generation.observe(canonical_progress);
                let generating = matches!(plan.artifact.state,
                    crate::plan::PlanState::Generating | crate::plan::PlanState::Revising);
                if generating && !budget_allows {
                    crate::plan::state_machine::PlanStateMachine::apply(
                        &mut plan.artifact,
                        crate::plan::state_machine::PlanEvent::TransitionFailed,
                        now_ms,
                    )?;
                }
                let continuing = generating && budget_allows;
                let artifact = plan.artifact.clone();
                task.turn = Some(super::TaskTurn {
                    id: completed_turn,
                    continuation: continuing.then(|| super::TaskContinuation {
                        id: super::TaskInputId::fresh(),
                        instruction: format!(
                            "Continue the active planning task. The previous turn ended without submitting the plan or asking a question. Edit the proposed declaration files if needed, then call plan_submit. Prose alone does not submit a plan. Use question_ask only for a new open decision, and only when the user's question preferences allow it. Do not repeat answered questions. If a tool keeps failing, report the failure instead of repeating the same rejected request.\n\nPlanning continuation: turn {} of {}.",
                            artifact.generation.budget.turn_count + 1,
                            artifact.generation.budget.max_turn_count,
                        ),
                        reason: "The planning turn ended without submitting a plan".into(),
                        delivery: super::TaskInputDelivery::Pending,
                        work: super::TaskContinuationWork::Provider,
                    }),
                });
                if generating && !budget_allows { task.renew(now_ms); } else { task.updated_at_ms = now_ms; }
                Ok(Some((artifact, continuing)))
            },
        )
    }

    /// Records provider acceptance even if later user input cancelled the unsent intent.
    pub(crate) fn acknowledge_continuation(
        &self,
        store: &mut SqliteStore,
        revision: &TaskRevision,
        id: &super::TaskInputId,
        now_ms: i64,
    ) -> Result<bool> {
        let result = store.commit_task_change(&self.session_id, Some(revision), now_ms, |active| {
            let task = active.as_mut().context("session has no active task")?;
            let Some(input) = task
                .turn
                .as_mut()
                .and_then(|turn| turn.continuation.as_mut())
                .filter(|input| &input.id == id)
            else {
                return Ok(false);
            };
            if input.delivery == crate::task::TaskInputDelivery::Accepted {
                return Ok(true);
            }
            input.delivery = crate::task::TaskInputDelivery::Accepted;
            task.updated_at_ms = now_ms;
            Ok(true)
        });
        match result {
            Err(error) if error.is::<super::StaleTask>() => Ok(false),
            result => result,
        }
    }

    /// Invalidates an unacknowledged continuation while preserving the completed-turn watermark.
    pub(crate) fn cancel_continuation(&self, store: &mut SqliteStore, now_ms: i64) -> Result<()> {
        let Some(current) = self.current()? else {
            return Ok(());
        };
        if current
            .turn
            .as_ref()
            .and_then(|turn| turn.continuation.as_ref())
            .is_none_or(|input| input.delivery != crate::task::TaskInputDelivery::Pending)
        {
            return Ok(());
        }
        let result = store.commit_task_change(
            &self.session_id,
            Some(&current.revision),
            now_ms,
            |active| {
                if let Some(input) = active
                    .as_mut()
                    .and_then(|task| task.turn.as_mut())
                    .and_then(|turn| turn.continuation.as_mut())
                {
                    input.delivery = crate::task::TaskInputDelivery::Cancelled;
                }
                Ok(())
            },
        );
        match result {
            Err(error) if error.is::<super::StaleTask>() => Ok(()),
            result => result,
        }
    }

    #[cfg(test)]
    pub(crate) fn test_goal() -> (tempfile::TempDir, SqliteStore, Arc<Self>) {
        use crate::session::SessionStore;
        let root = tempfile::tempdir().unwrap();
        let mut store = SqliteStore::open(root.path()).unwrap();
        let session: crate::session::HarnessSession = serde_json::from_value(serde_json::json!({
            "id":"session", "primary_agent_id":crate::session::HarnessSession::primary_agent_id("session"),
            "name":"Test", "workspace":"workspace", "backend":"mock", "model":"mock",
            "effort":"", "approval_mode":"ask", "created_at_ms":0, "updated_at_ms":0,
            "provider_fork_state":{"state":"ready"}
        })).unwrap();
        store.save_session(&session).unwrap();
        let controller =
            Arc::new(Self::new(store.session_admission(&session.id).unwrap()).unwrap());
        controller
            .change(
                &mut store,
                None,
                TaskChange::Install(Box::new(ActiveTask::goal(
                    "Finish".into(),
                    Default::default(),
                    false,
                    1,
                ))),
                1,
            )
            .unwrap();
        (root, store, controller)
    }

    pub(crate) fn change(
        &self,
        store: &mut SqliteStore,
        expected: Option<&TaskRevision>,
        change: TaskChange,
        now_ms: i64,
    ) -> Result<Option<ActiveTask>> {
        let retire_questions = matches!(
            &change,
            TaskChange::Install(_) | TaskChange::Clear | TaskChange::ResumeWithEntry(_)
        );
        store
            .commit_task_effects(&self.session_id, expected, now_ms, None, |active| {
                let previous = active.clone();
                let changed = Self::apply_change(active, change, now_ms)?;
                let mut effects =
                    clear_effect(&self.session_id, previous.as_ref(), active.as_ref(), now_ms);
                effects.retire_questions = retire_questions;
                Ok((changed, effects))
            })?
            .context("task change unexpectedly reused a command")
    }

    #[cfg(test)]
    pub(crate) fn command(
        &self,
        store: &mut SqliteStore,
        receipt: &super::TaskOperation,
        expected: Option<&TaskRevision>,
        change: TaskChange,
        now_ms: i64,
    ) -> Result<Option<ActiveTask>> {
        self.command_with_effects(store, receipt, expected, change, Default::default(), now_ms)
    }

    pub(crate) fn command_with_effects(
        &self,
        store: &mut SqliteStore,
        receipt: &super::TaskOperation,
        expected: Option<&TaskRevision>,
        change: TaskChange,
        mut effects: crate::storage::TaskEffects,
        now_ms: i64,
    ) -> Result<Option<ActiveTask>> {
        ensure!(
            receipt.session_id == self.session_id,
            "task command belongs to another session"
        );
        let retire_questions = matches!(
            &change,
            TaskChange::Install(_) | TaskChange::Clear | TaskChange::ResumeWithEntry(_)
        );
        store.commit_task_effects(
            &self.session_id,
            expected,
            now_ms,
            Some(receipt),
            |active| {
                let previous = active.clone();
                let changed = Self::apply_change(active, change, now_ms)?;
                let terminal =
                    clear_effect(&self.session_id, previous.as_ref(), active.as_ref(), now_ms);
                effects.event.extend(terminal.event);
                effects.plan.extend(terminal.plan);
                effects.execution.extend(terminal.execution);
                effects.retire_questions |= retire_questions;
                Ok((changed, effects))
            },
        )?;
        self.current()
    }

    fn apply_change(
        active: &mut Option<ActiveTask>,
        change: TaskChange,
        now_ms: i64,
    ) -> Result<Option<ActiveTask>> {
        match change {
            TaskChange::Preserve => {}
            TaskChange::Update(mut next) => {
                let current = active.as_ref().context("session has no active task")?;
                ensure!(
                    current.kind() == next.kind(),
                    "task updates cannot change task kind"
                );
                if !current.same_contract(&next) {
                    let turn = next.turn.take();
                    next.renew(now_ms);
                    next.turn = turn;
                } else {
                    next.updated_at_ms = now_ms;
                }
                *active = Some(*next);
            }
            TaskChange::ResumeWithEntry(instruction) => {
                Self::apply_change(active, TaskChange::Resume, now_ms)?;
                active
                    .as_mut()
                    .context("session has no active task")?
                    .request_entry(instruction, now_ms)?;
            }
            TaskChange::Install(mut next) => {
                ensure!(!next.status().terminal(), "cannot install a terminal task");
                next.renew(now_ms);
                *active = Some(*next);
            }
            TaskChange::Clear => {
                ensure!(active.is_some(), "session has no active task");
                *active = None;
            }
            TaskChange::Pause => {
                let task = active.as_mut().context("session has no active task")?;
                ensure!(!task.status().terminal(), "cannot pause a terminal task");
                match &mut task.workflow {
                    TaskWorkflow::Goal { goal } => goal.state = GoalState::Paused,
                    TaskWorkflow::Plan { plan } => plan.paused = true,
                    TaskWorkflow::Execute { execution } => {
                        execution.record.state = PlanExecutionState::Paused
                    }
                }
                task.renew(now_ms);
            }
            TaskChange::Resume => {
                let task = active.as_mut().context("session has no active task")?;
                ensure!(
                    matches!(
                        task.status(),
                        TaskStatus::Paused | TaskStatus::Blocked | TaskStatus::Failed
                    ),
                    "task is not paused or blocked"
                );
                match &mut task.workflow {
                    TaskWorkflow::Goal { goal } => {
                        goal.state = GoalState::Active;
                        goal.continuation.reset();
                    }
                    TaskWorkflow::Plan { plan } => {
                        if plan.artifact.state == crate::plan::PlanState::Failed {
                            plan.artifact.generation.reset();
                            crate::plan::state_machine::PlanStateMachine::apply(
                                &mut plan.artifact,
                                crate::plan::state_machine::PlanEvent::RetryRequested,
                                now_ms,
                            )?;
                        }
                        plan.paused = false;
                    }
                    TaskWorkflow::Execute { execution } => {
                        execution.record.state = PlanExecutionState::Active;
                        execution.continuation.reset();
                    }
                }
                task.renew(now_ms);
            }
        }
        Ok(active.clone())
    }
}

fn clear_effect(
    session_id: &str,
    previous: Option<&ActiveTask>,
    active: Option<&ActiveTask>,
    now_ms: i64,
) -> crate::storage::TaskEffects {
    if let Some(task) = previous.filter(|_| active.is_none()) {
        let outcome = super::TaskOutcome {
            kind: task.kind(),
            title: task.title().into(),
            state: super::TaskOutcomeState::Cancelled,
            summary: "Task cleared".into(),
        };
        outcome_effect(session_id, task, outcome, now_ms)
    } else {
        crate::storage::TaskEffects::default()
    }
}

fn outcome_effect(
    session_id: &str,
    task: &ActiveTask,
    outcome: super::TaskOutcome,
    now_ms: i64,
) -> crate::storage::TaskEffects {
    let terminal = outcome.state;
    let mut effects = crate::storage::TaskEffects {
        event: vec![crate::timeline::SessionEventRecord {
            id: format!("{session_id}:task-finished:{}", task.revision.as_str()),
            session_id: session_id.into(),
            owner: crate::timeline::TimelineOwner::session(session_id),
            created_at_ms: now_ms,
            detail: crate::timeline::SessionEventKind::TaskFinished { outcome },
        }],
        ..Default::default()
    };
    match &task.workflow {
        TaskWorkflow::Plan { plan } => {
            let mut artifact = plan.artifact.clone();
            artifact.state = match terminal {
                super::TaskOutcomeState::Complete => crate::plan::PlanState::Accepted,
                super::TaskOutcomeState::Cancelled => crate::plan::PlanState::Cancelled,
            };
            artifact.updated_at_ms = now_ms;
            effects.plan.push(artifact);
        }
        TaskWorkflow::Execute { execution } => {
            let mut record = execution.record.clone();
            record.state = match terminal {
                super::TaskOutcomeState::Complete => PlanExecutionState::Complete,
                super::TaskOutcomeState::Cancelled => PlanExecutionState::Cancelled,
            };
            record.completed_at_ms = Some(now_ms);
            effects.execution.push(record);
        }
        TaskWorkflow::Goal { .. } => {}
    }
    effects
}

fn goal_view(task: &ActiveTask) -> Result<super::GoalTask> {
    Ok(match &task.workflow {
        TaskWorkflow::Goal { goal } => goal.clone(),
        TaskWorkflow::Execute { execution } => super::GoalTask {
            objective: execution.title.clone(),
            state: match execution.record.state {
                PlanExecutionState::Active => GoalState::Active,
                PlanExecutionState::Paused => GoalState::Paused,
                PlanExecutionState::Blocked => GoalState::Blocked,
                PlanExecutionState::Stalled => GoalState::Stalled,
                PlanExecutionState::Complete => GoalState::Complete,
                PlanExecutionState::Cancelled => GoalState::Cleared,
            },
            continuation: execution.continuation.clone(),
            native: false,
        },
        TaskWorkflow::Plan { .. } => anyhow::bail!("goal evidence requires Goal or Execute"),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::session::{HarnessSession, SessionStore, continuation::ContinuationBudget};

    #[test]
    fn repeated_task_input_acknowledgements_preserve_first_acceptance() -> Result<()> {
        let (_root, mut store, controller) = SessionTaskController::test_goal();
        let previous = controller.current()?.unwrap();
        let mut task = ActiveTask::goal(
            "Keep first acknowledgement".into(),
            ContinuationBudget::new(20, 2),
            false,
            2,
        );
        task.request_entry("Start once".into(), 2)?;
        let task = controller
            .change(
                &mut store,
                Some(&previous.revision),
                TaskChange::Install(Box::new(task)),
                2,
            )?
            .unwrap();
        let entry_id = task.entry.as_ref().unwrap().id.clone();
        assert!(controller.acknowledge_entry(&mut store, &task.revision, &entry_id, 3)?);
        let accepted = serde_json::to_value(controller.current()?.unwrap())?;
        assert!(controller.acknowledge_entry(&mut store, &task.revision, &entry_id, 100)?);
        assert_eq!(
            serde_json::to_value(controller.current()?.unwrap())?,
            accepted
        );
        assert_eq!(
            serde_json::to_value(store.load_active_task("session")?.unwrap())?,
            accepted
        );

        let task = controller.current()?.unwrap();
        controller.reserve_continuation(
            &mut store,
            &task.permit(),
            super::super::TaskTurnId {
                exchange_ordinal: 1,
                turn_ordinal: 1,
            },
            "Continue once".into(),
            "Remaining work".into(),
            200,
        )?;
        let task = controller.current()?.unwrap();
        let continuation_id = task
            .turn
            .as_ref()
            .unwrap()
            .continuation
            .as_ref()
            .unwrap()
            .id
            .clone();
        assert!(controller.acknowledge_continuation(
            &mut store,
            &task.revision,
            &continuation_id,
            201
        )?);
        let accepted = serde_json::to_value(controller.current()?.unwrap())?;
        assert!(controller.acknowledge_continuation(
            &mut store,
            &task.revision,
            &continuation_id,
            900
        )?);
        assert_eq!(
            serde_json::to_value(controller.current()?.unwrap())?,
            accepted
        );
        assert_eq!(
            serde_json::to_value(store.load_active_task("session")?.unwrap())?,
            accepted
        );
        Ok(())
    }

    #[test]
    fn task_entry_commit_and_acknowledgement_fence_control_admission() -> Result<()> {
        let (_root, mut store, controller) = SessionTaskController::test_goal();
        let previous = controller.current()?.unwrap();
        let mut task = ActiveTask::goal("New goal".into(), Default::default(), false, 2);
        task.request_entry("Work on the new goal".into(), 2)?;
        let command = super::super::TaskOperation {
            revision: 0,
            id: "install".into(),
            session_id: "session".into(),
            action: serde_json::json!({"action":"goal","text":"New goal"}),
            permission_selection: None,
            state: "completed".into(),
            error: None,
            exchange_id: None,
            created_at_ms: 2,
            resume_after_transition: false,
        };
        let installed = controller
            .command(
                &mut store,
                &command,
                Some(&previous.revision),
                TaskChange::Install(Box::new(task.clone())),
                2,
            )?
            .unwrap();
        let entry = installed.entry.as_ref().unwrap();
        assert!(controller.admit_goal_control(true).is_err());
        assert!(!controller.acknowledge_entry(&mut store, &previous.revision, &entry.id, 3)?);
        assert!(controller.acknowledge_entry(&mut store, &installed.revision, &entry.id, 3)?);
        controller.admit_goal_control(true)?;
        let replay = controller
            .command(
                &mut store,
                &command,
                Some(&previous.revision),
                TaskChange::Install(Box::new(task)),
                4,
            )?
            .unwrap();
        assert_eq!(replay.revision, installed.revision);
        assert_eq!(
            replay.entry.unwrap().delivery,
            crate::task::TaskInputDelivery::Accepted
        );
        let mut conflicting = command.clone();
        conflicting.action["text"] = serde_json::json!("Different");
        assert!(
            controller
                .command(
                    &mut store,
                    &conflicting,
                    Some(&installed.revision),
                    TaskChange::Clear,
                    4
                )
                .is_err()
        );
        let mut failed = command;
        failed.id = "stale-clear".into();
        assert!(
            controller
                .command(
                    &mut store,
                    &failed,
                    Some(&previous.revision),
                    TaskChange::Clear,
                    4
                )
                .is_err()
        );
        assert!(
            store
                .load_task_operation("session", "stale-clear")?
                .is_none()
        );
        assert_eq!(controller.current()?.unwrap().revision, installed.revision);
        Ok(())
    }

    #[test]
    fn planning_progress_is_committed_once_and_rejects_stale_revision() -> Result<()> {
        let (_root, mut store, controller) = SessionTaskController::test_goal();
        let previous = controller.current()?.unwrap();
        let artifact: crate::plan::PlanRecord = serde_json::from_value(serde_json::json!({
            "id":"plan", "session_id":"session", "request":"Design the change", "title":"Design",
            "state":"generating", "working_path":"working.md", "model_revision":0,
            "user_revision":0, "review_digest":null, "accepted_digest":null,
            "created_at_ms":1, "updated_at_ms":1
        }))?;
        let task = controller
            .change(
                &mut store,
                Some(&previous.revision),
                TaskChange::Install(Box::new(ActiveTask::plan(artifact, 2))),
                2,
            )?
            .unwrap();
        let completed = crate::task::TaskTurnId {
            exchange_ordinal: 1,
            turn_ordinal: 1,
        };
        let (plan, _) = controller
            .observe_plan(&mut store, &task.permit(), completed.clone(), true, 3)?
            .unwrap();
        assert_eq!(plan.generation.budget.turn_count, 1);
        assert!(
            controller
                .observe_plan(&mut store, &task.permit(), completed.clone(), true, 4)?
                .is_none()
        );
        let saved = controller.current()?.unwrap();
        let TaskWorkflow::Plan { plan } = &saved.workflow else {
            unreachable!()
        };
        assert_eq!(plan.artifact.generation.budget.turn_count, 1);
        assert_eq!(plan.artifact.generation.canonical_revision, 1);
        controller.change(&mut store, Some(&saved.revision), TaskChange::Pause, 5)?;
        assert!(
            controller
                .observe_plan(&mut store, &task.permit(), completed, true, 6)
                .unwrap_err()
                .is::<crate::task::StaleTask>()
        );
        Ok(())
    }

    #[test]
    fn turn_end_schedules_one_typed_continuation() -> Result<()> {
        let (root, mut store, controller) = SessionTaskController::test_goal();
        let permit = controller.admit_goal_control(true)?;
        let completed = crate::task::TaskTurnId {
            exchange_ordinal: 2,
            turn_ordinal: 1,
        };
        controller.observe_goal(
            &mut store,
            &permit,
            completed.clone(),
            crate::goal::TurnEvidence {
                tool_called: true,
                ..Default::default()
            },
            2,
        )?;
        let first = controller.current()?.unwrap();
        let input = first.turn.as_ref().unwrap().continuation.as_ref().unwrap();
        assert_eq!(
            input.instruction,
            "Continue working toward this goal: Finish"
        );
        assert_eq!(input.delivery, crate::task::TaskInputDelivery::Pending);
        let identity = input.id.clone();
        for duplicate in [
            completed.clone(),
            crate::task::TaskTurnId {
                exchange_ordinal: 1,
                turn_ordinal: 8,
            },
        ] {
            let (_, decision) = controller.observe_goal(
                &mut store,
                &permit,
                duplicate,
                crate::goal::TurnEvidence {
                    structured_complete: true,
                    ..Default::default()
                },
                3,
            )?;
            assert_eq!(decision, crate::goal::ContinuationDecision::Stop);
        }
        let reopened = SqliteStore::open(root.path())?;
        let saved = reopened.load_active_task("session")?.unwrap();
        assert_eq!(
            saved
                .turn
                .as_ref()
                .unwrap()
                .continuation
                .as_ref()
                .unwrap()
                .id,
            identity
        );
        let TaskWorkflow::Goal { goal } = &saved.workflow else {
            unreachable!()
        };
        assert_eq!(goal.continuation.turn_count, 1);
        controller.cancel_continuation(&mut store, 4)?;
        let cancelled = controller.current()?.unwrap();
        assert_eq!(
            cancelled
                .turn
                .as_ref()
                .unwrap()
                .continuation
                .as_ref()
                .unwrap()
                .delivery,
            crate::task::TaskInputDelivery::Cancelled
        );
        controller.observe_goal(
            &mut store,
            &permit,
            completed,
            crate::goal::TurnEvidence {
                tool_called: true,
                ..Default::default()
            },
            5,
        )?;
        assert_eq!(
            controller
                .current()?
                .unwrap()
                .turn
                .unwrap()
                .continuation
                .unwrap()
                .delivery,
            crate::task::TaskInputDelivery::Cancelled
        );
        controller.acknowledge_continuation(&mut store, permit.revision(), &identity, 6)?;
        assert_eq!(
            controller
                .current()?
                .unwrap()
                .turn
                .unwrap()
                .continuation
                .unwrap()
                .delivery,
            crate::task::TaskInputDelivery::Accepted
        );
        Ok(())
    }

    #[test]
    fn task_progress_preserves_admitted_control() -> Result<()> {
        let (_root, mut store, controller) = SessionTaskController::test_goal();
        let permit = controller.admit_goal_control(true)?;
        let (_, decision) = controller.observe_goal(
            &mut store,
            &permit,
            crate::task::TaskTurnId {
                exchange_ordinal: 1,
                turn_ordinal: 1,
            },
            crate::goal::TurnEvidence {
                tool_called: true,
                ..Default::default()
            },
            2,
        )?;
        assert_eq!(decision, crate::goal::ContinuationDecision::Continue);
        permit.validate(controller.current()?.as_ref())?;
        let (goal, decision) = controller.observe_goal(
            &mut store,
            &permit,
            crate::task::TaskTurnId {
                exchange_ordinal: 1,
                turn_ordinal: 2,
            },
            crate::goal::TurnEvidence {
                structured_complete: true,
                ..Default::default()
            },
            3,
        )?;
        assert_eq!(goal.state, GoalState::Complete);
        assert_eq!(decision, crate::goal::ContinuationDecision::Complete);
        assert!(controller.current()?.is_none());
        Ok(())
    }

    #[test]
    fn blocked_task_retains_resume_state() -> Result<()> {
        let (_root, mut store, controller) = SessionTaskController::test_goal();
        let permit = controller.admit_goal_control(false)?;
        controller.observe_goal(
            &mut store,
            &permit,
            crate::task::TaskTurnId {
                exchange_ordinal: 1,
                turn_ordinal: 1,
            },
            crate::goal::TurnEvidence {
                structured_blocked: true,
                ..Default::default()
            },
            2,
        )?;
        let blocked = controller.current()?.unwrap();
        assert_eq!(blocked.status(), TaskStatus::Blocked);
        assert!(permit.validate(Some(&blocked)).is_err());
        assert!(controller.admit_goal_control(true).is_err());
        let resumed = controller
            .change(&mut store, Some(&blocked.revision), TaskChange::Resume, 3)?
            .unwrap();
        assert_eq!(resumed.status(), TaskStatus::Running);
        assert_ne!(blocked.revision, resumed.revision);
        Ok(())
    }

    #[test]
    fn stale_goal_evidence_cannot_change_a_replacement() -> Result<()> {
        let (_root, mut store, controller) = SessionTaskController::test_goal();
        let permit = controller.admit_goal_control(true)?;
        let replacement = controller
            .change(
                &mut store,
                Some(permit.revision()),
                TaskChange::Install(Box::new(ActiveTask::goal(
                    "Replacement".into(),
                    Default::default(),
                    false,
                    2,
                ))),
                2,
            )?
            .unwrap();
        let error = controller
            .observe_goal(
                &mut store,
                &permit,
                crate::task::TaskTurnId {
                    exchange_ordinal: 1,
                    turn_ordinal: 1,
                },
                crate::goal::TurnEvidence {
                    structured_complete: true,
                    ..Default::default()
                },
                3,
            )
            .unwrap_err();
        assert!(error.is::<crate::task::StaleTask>());
        assert_eq!(
            controller.current()?.unwrap().revision,
            replacement.revision
        );
        Ok(())
    }

    #[test]
    fn pause_resume_and_clear_invalidate_admitted_task_controls() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut store = SqliteStore::open(root.path())?;
        let session: HarnessSession = serde_json::from_value(serde_json::json!({
            "id":"session", "primary_agent_id":HarnessSession::primary_agent_id("session"),
            "name":"Test", "workspace":"workspace", "backend":"mock", "model":"mock",
            "effort":"", "approval_mode":"ask", "created_at_ms":0, "updated_at_ms":0,
            "provider_fork_state":{"state":"ready"}
        }))?;
        store.save_session(&session)?;
        let controller = SessionTaskController::new(store.session_admission(&session.id)?)?;
        let task = controller
            .change(
                &mut store,
                None,
                TaskChange::Install(Box::new(ActiveTask::goal(
                    "Finish".into(),
                    ContinuationBudget::default(),
                    false,
                    1,
                ))),
                1,
            )?
            .unwrap();
        let permit = controller.admit(TaskKind::Goal)?;
        assert!(controller.admit(TaskKind::Plan).is_err());
        let paused = controller
            .change(&mut store, Some(&task.revision), TaskChange::Pause, 2)?
            .unwrap();
        assert_eq!(paused.status(), TaskStatus::Paused);
        assert!(controller.admit(TaskKind::Goal).is_err());
        assert!(permit.validate(controller.current()?.as_ref()).is_err());
        assert!(
            controller
                .change(&mut store, Some(permit.revision()), TaskChange::Clear, 3)
                .is_err()
        );
        let resumed = controller
            .change(&mut store, Some(&paused.revision), TaskChange::Resume, 4)?
            .unwrap();
        assert_eq!(resumed.status(), TaskStatus::Running);
        assert_ne!(paused.revision, resumed.revision);
        controller.change(&mut store, Some(&resumed.revision), TaskChange::Clear, 5)?;
        assert!(controller.current()?.is_none());
        assert!(
            store
                .load_session(&session.id)?
                .unwrap()
                .active_task
                .is_none()
        );
        assert_eq!(
            store.load_session(&session.id)?.unwrap().approval_mode,
            session.approval_mode
        );
        Ok(())
    }
}
