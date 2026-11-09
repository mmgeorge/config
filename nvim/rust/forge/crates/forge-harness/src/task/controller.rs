use std::sync::Arc;

use anyhow::{Context, Result, ensure};

use crate::{
    goal::GoalState,
    plan::PlanExecutionState,
    storage::{SqliteStore, admission::SessionAdmission},
};

use super::{Task, TaskKind, TaskPermit, TaskRevision, TaskStatus, WorkflowSnapshot};

/// Durable command identity and task authority captured for one owner commit.
pub(crate) struct OwnerTaskCommit<'command> {
    pub id: &'command crate::exchange::command::CommandId,
    pub permit: Option<&'command TaskPermit>,
    pub now_ms: i64,
}

pub(crate) enum TaskChange {
    Install(Box<Task>),
    Update(Box<Task>),
    SetPlanAcceptance(bool),
    Preserve,
    Pause,
    Resume,
    ResumeWithEntry(String),
    ResumeWithChecks(super::TaskTurnId),
    Clear,
}

impl TaskChange {
    /// Keep review revisions within their durable owner; install only a new workflow or identity.
    pub(crate) fn prepared(current: Option<&Task>, next: Task) -> Self {
        if current.is_some_and(|current| current.id == next.id && current.kind() == next.kind()) {
            Self::Update(Box::new(next))
        } else {
            Self::Install(Box::new(next))
        }
    }
}

/// Coordinates task mutations against the session's committed admission projection.
pub(crate) struct SessionTaskController {
    session_id: String,
    admission: Arc<SessionAdmission>,
}

impl SessionTaskController {
    /// Serialize configuration writes with generation retirement.
    pub(crate) fn configuration_lane(&self) -> Arc<tokio::sync::Mutex<()>> {
        self.admission.configuration_lane()
    }

    /// Block admission after an uncertain configuration mutation.
    pub(crate) fn mark_configuration_uncertain(&self) {
        self.admission.mark_configuration_uncertain();
    }

    /// Reject another configuration write until generation recovery completes.
    pub(crate) fn ensure_configuration_certain(&self) -> Result<()> {
        self.admission.ensure_configuration_certain()
    }

    /// Get whether configuration mutation requires generation recovery.
    pub(crate) fn configuration_is_uncertain(&self) -> bool {
        self.admission.configuration_is_uncertain()
    }

    /// Clear uncertainty after the old provider generation has been collected.
    pub(crate) fn reset_configuration_generation(&self) -> Result<()> {
        self.admission.reset_configuration_generation()
    }

    /// Commit a typed observation under the task revision that admitted its operation.
    pub(crate) fn apply(
        &self,
        store: &mut SqliteStore,
        permit: &TaskPermit,
        event: super::TaskEvent,
        now_ms: i64,
    ) -> Result<()> {
        self.apply_with_effects(store, permit, event, Default::default(), now_ms)
    }

    /// Commit a typed observation and dependent artifacts in one transaction.
    pub(crate) fn apply_with_effects(
        &self,
        store: &mut SqliteStore,
        permit: &TaskPermit,
        event: super::TaskEvent,
        effects: crate::storage::TaskEffects,
        now_ms: i64,
    ) -> Result<()> {
        self.update_with_effects(store, permit, now_ms, |task| {
            Self::apply_event(task, event, now_ms)?;
            Ok(((), effects))
        })
    }

    /// Commit a task observation and dependent artifacts while its working document stays fixed.
    pub(crate) fn apply_with_plan_document(
        &self,
        store: &mut SqliteStore,
        permit: &TaskPermit,
        document: crate::plan::WorkingDocumentGuard<'_>,
        event: super::TaskEvent,
        effects: crate::storage::TaskEffects,
        now_ms: i64,
    ) -> Result<()> {
        store.commit_task_effects_with_document(
            &self.session_id,
            permit,
            now_ms,
            document,
            |active| {
                let task = active.as_mut().context("session has no active task")?;
                task.edit(|draft| {
                    let previous = draft.clone();
                    Self::apply_event(draft, event, now_ms)?;
                    if !draft.same_contract(&previous) && draft.revision == previous.revision {
                        draft.renew(now_ms);
                    } else {
                        draft.updated_at_ms = now_ms;
                    }
                    Ok(((), effects))
                })
            },
        )
    }

    /// Interpret an event inside the controller-owned candidate update.
    fn apply_event(
        task: &mut super::TaskSnapshot,
        event: super::TaskEvent,
        now_ms: i64,
    ) -> Result<()> {
        match event {
            super::TaskEvent::PlanArtifactUpdated(artifact) => {
                let WorkflowSnapshot::Plan { plan } = &mut task.workflow else {
                    anyhow::bail!("plan update requires the current planning task");
                };
                ensure!(plan.artifact.id == artifact.id, "planning task changed");
                ensure!(
                    artifact.document_version >= plan.artifact.document_version
                        && artifact.model_revision >= plan.artifact.model_revision,
                    "planning observation cannot roll back the canonical artifact"
                );
                plan.artifact = *artifact;
            }
            super::TaskEvent::PlanGenerationFailed => {
                let WorkflowSnapshot::Plan { plan } = &mut task.workflow else {
                    anyhow::bail!("planning failure requires a planning task");
                };
                if matches!(
                    plan.artifact.state,
                    crate::plan::PlanState::Generating
                        | crate::plan::PlanState::Revising
                        | crate::plan::PlanState::AwaitingInput
                ) {
                    crate::plan::state_machine::PlanStateMachine::apply(
                        &mut plan.artifact,
                        crate::plan::state_machine::PlanEvent::TransitionFailed,
                        now_ms,
                    )?;
                }
            }
            super::TaskEvent::ExecutionUpdated(record) => {
                let WorkflowSnapshot::Execute { execution } = &mut task.workflow else {
                    anyhow::bail!("execution update requires an execution task");
                };
                ensure!(
                    execution.record.state == PlanExecutionState::Active
                        || execution.record.state == record.state,
                    "execution observations cannot resume suspended work"
                );
                validate_execution_update(&execution.record, &record)?;
                execution.record = *record;
            }
            super::TaskEvent::ExecutionBaselineCaptured(checkpoint) => {
                let WorkflowSnapshot::Execute { execution } = &mut task.workflow else {
                    anyhow::bail!("baseline capture requires an execution task");
                };
                ensure!(
                    execution
                        .record
                        .baseline_checkpoint
                        .as_ref()
                        .is_none_or(|baseline| baseline == &checkpoint),
                    "execution baseline cannot change after admission"
                );
                execution.record.baseline_checkpoint = Some(checkpoint);
            }
            super::TaskEvent::GoalWorkspaceObserved(checkpoint) => {
                let WorkflowSnapshot::Goal { goal } = &mut task.workflow else {
                    anyhow::bail!("goal workspace observation requires Goal");
                };
                goal.observed_checkpoint = Some(checkpoint);
            }
            super::TaskEvent::CheckPreparationFailed(diagnostic) => {
                let WorkflowSnapshot::Execute { execution } = &mut task.workflow else {
                    anyhow::bail!("check preparation requires an execution task");
                };
                ensure!(
                    execution.record.phase == crate::plan::PlanPhase::Checking
                        && execution.record.state == PlanExecutionState::Active,
                    "check preparation requires active Check"
                );
                ensure!(
                    !diagnostic.trim().is_empty(),
                    "check preparation requires diagnostics"
                );
                execution.record.phase = crate::plan::PlanPhase::Resolve;
                execution.record.findings = vec![diagnostic];
                execution.record.generation += 1;
            }
            super::TaskEvent::EntryPreparationFinished => {
                task.entry
                    .as_mut()
                    .context("execution entry disappeared")?
                    .execution = None;
            }
        }
        Ok(())
    }

    pub(crate) fn apply_plan_answer(
        task: &mut Task,
        answered: crate::plan::PlanRecord,
    ) -> Result<()> {
        task.edit(|task| {
            let WorkflowSnapshot::Plan { plan } = &mut task.workflow else {
                anyhow::bail!("answer no longer belongs to the active planning task");
            };
            ensure!(
                plan.artifact.id == answered.id,
                "answer belongs to a replaced planning task"
            );
            plan.artifact = answered;
            Ok(())
        })
    }
    pub(crate) fn new(admission: Arc<SessionAdmission>) -> Result<Self> {
        let session_id = admission.snapshot()?.session.id.clone();
        Ok(Self {
            session_id,
            admission,
        })
    }

    pub(crate) fn current(&self) -> Result<Option<Task>> {
        Ok(self.admission.runtime()?.active_task.clone())
    }

    pub(crate) fn callback_snapshot(&self) -> Result<(u64, Option<Task>)> {
        self.admission.callback_snapshot()
    }

    pub(crate) fn with_callback_authority<T>(
        &self,
        generation: u64,
        permit: Option<&TaskPermit>,
        commit: impl FnOnce() -> Result<T>,
    ) -> Result<T> {
        self.admission
            .with_callback_authority(generation, permit, commit)
    }

    /// Commit a prepared plan file write only while its exact task authority remains current.
    ///
    /// Perform rendering before this call. The closure may perform the short file CAS, but
    /// must not await or mutate task storage because task commits acquire the same gate.
    pub(crate) fn commit_plan_write<Output>(
        &self,
        permit: &TaskPermit,
        write: impl FnOnce() -> Result<Output>,
    ) -> Result<Output> {
        self.admission.with_task_mutation(|| {
            permit.validate(self.current()?.as_ref())?;
            write()
        })
    }

    /// Apply a synchronous dependent restore against one current task snapshot.
    ///
    /// The closure must not mutate task state or await. Task replacement, recovery, and
    /// revocation remain excluded until the dependent restore and its publication finish.
    pub(crate) fn with_task_snapshot<Output>(
        &self,
        restore: impl FnOnce(Option<Task>) -> Result<Output>,
    ) -> Result<Output> {
        self.admission
            .with_task_mutation(|| restore(self.current()?))
    }

    /// Clear task authority and replace rollback history in one durable transaction.
    pub(crate) fn clear_for_rollback(
        &self,
        store: &mut SqliteStore,
        expected: Option<&TaskRevision>,
        replacement: &crate::session::HarnessSession,
        roots: &[String],
        now_ms: i64,
    ) -> Result<()> {
        ensure!(
            replacement.id == self.session_id,
            "rollback belongs to another session"
        );
        let rollback = store.prepare_rollback(replacement, roots)?;
        store
            .commit_task_effects(&self.session_id, expected, now_ms, |active| {
                let mut effects = clear_effect(&self.session_id, active.as_ref(), None, now_ms);
                effects.rollback = Some(Box::new(rollback));
                *active = None;
                Ok(((), effects))
            })?
            .context("rollback task commit unexpectedly reused a command")
    }

    /// Applies a task-specific mutation atomically against its admitted contract.
    fn mutate<Output>(
        &self,
        store: &mut SqliteStore,
        permit: &TaskPermit,
        now_ms: i64,
        change: impl FnOnce(&mut super::active::TaskSnapshot) -> Result<Output>,
    ) -> Result<Output> {
        store.commit_task_change(
            &self.session_id,
            Some(permit.revision()),
            now_ms,
            |active| {
                let task = active.as_mut().context("session has no active task")?;
                permit.validate(Some(task))?;
                task.edit(|draft| {
                    let previous = draft.clone();
                    let output = change(draft)?;
                    if !draft.same_contract(&previous) && draft.revision == previous.revision {
                        draft.renew(now_ms);
                    } else {
                        draft.updated_at_ms = now_ms;
                    }
                    Ok(output)
                })
            },
        )
    }

    #[cfg(test)]
    pub(crate) fn update<Output>(
        &self,
        store: &mut SqliteStore,
        permit: &TaskPermit,
        now_ms: i64,
        change: impl FnOnce(&mut super::TaskSnapshot) -> Result<Output>,
    ) -> Result<Output> {
        self.mutate(store, permit, now_ms, change)
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
        self.mutate(store, permit, now_ms, |task| {
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
                            !(input.work.is_checks()
                                && input.delivery == super::TaskInputDelivery::Accepted)
                        })
            }) {
                return Ok(false);
            }
            task.turn = Some(super::TaskTurn {
                id: completed_turn,
                continuation: Some(super::TaskContinuation {
                    id: super::TaskInputId::fresh(),
                    reason,
                    delivery: super::TaskInputDelivery::Pending,
                    work: super::TaskWork::Provider { instruction },
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
        self.mutate(store, permit, now_ms, |task| {
            ensure!(
                task.entry_accepted(),
                "task entry has not been acknowledged"
            );
            ensure!(
                matches!(&task.workflow, WorkflowSnapshot::Execute { execution }
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
                    reason: "Run the accepted plan's verification checks".into(),
                    delivery: super::TaskInputDelivery::Pending,
                    work: super::TaskWork::Checks,
                }),
            });
            Ok(true)
        })
    }

    /// Commits dependent artifacts and task mutation under one revision fence.
    pub(super) fn update_with_effects<Output>(
        &self,
        store: &mut SqliteStore,
        permit: &TaskPermit,
        now_ms: i64,
        change: impl FnOnce(
            &mut super::active::TaskSnapshot,
        ) -> Result<(Output, crate::storage::TaskEffects)>,
    ) -> Result<Output> {
        store
            .commit_task_effects(
                &self.session_id,
                Some(permit.revision()),
                now_ms,
                |active| {
                    let task = active.as_mut().context("session has no active task")?;
                    permit.validate(Some(task))?;
                    task.edit(|draft| {
                        let previous = draft.clone();
                        let result = change(draft)?;
                        if !draft.same_contract(&previous) && draft.revision == previous.revision {
                            draft.renew(now_ms);
                        } else {
                            draft.updated_at_ms = now_ms;
                        }
                        Ok(result)
                    })
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
                |active| {
                    let task = active.as_ref().context("session has no active task")?;
                    permit.validate(Some(task))?;
                    ensure!(
                        !matches!(state, super::TaskOutcomeState::Complete)
                            || task.kind() == TaskKind::Goal
                            || task.status() == TaskStatus::Completed,
                        "workflow completion requires its terminal gate state"
                    );
                    let outcome = super::TaskOutcome {
                        id: task.id.clone(),
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

    /// Exercise strict unscoped admission independently of dispatched callback authority.
    #[cfg(test)]
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

    /// Admit a callback from the task permit already captured for its dispatched invocation.
    /// Native receipt acceptance remains a separate fact committed by the exchange owner.
    pub(crate) fn admit_dispatched(
        &self,
        kind: TaskKind,
        permit: &TaskPermit,
    ) -> Result<TaskPermit> {
        let task = self.current()?.context("session has no active task")?;
        permit.validate(Some(&task))?;
        ensure!(
            task.kind() == kind,
            "operation does not belong to the dispatched task"
        );
        ensure!(
            matches!(task.status(), TaskStatus::Running | TaskStatus::Waiting),
            "dispatched task does not currently admit control operations"
        );
        Ok(permit.clone())
    }

    /// Validate goal controls from an authenticated invocation without fabricating input acceptance.
    pub(crate) fn admit_dispatched_goal(
        &self,
        permit: &TaskPermit,
        completing: bool,
    ) -> Result<TaskPermit> {
        let task = self.current()?.context("session has no active task")?;
        permit.validate(Some(&task))?;
        ensure!(
            task.status() == TaskStatus::Running
                && task.next_action() == super::TaskNextAction::Model
                && matches!(task.kind(), TaskKind::Goal | TaskKind::Execute),
            "goal control requires the dispatched active goal"
        );
        ensure!(
            !completing || task.kind() == TaskKind::Goal,
            "plan execution completes only through plan_phase_done in Verify"
        );
        Ok(permit.clone())
    }

    /// Commit explicit blockage under dispatched authority without marking native work complete.
    pub(crate) fn block_dispatched_goal(
        &self,
        store: &mut SqliteStore,
        permit: &TaskPermit,
        reason: String,
        now_ms: i64,
    ) -> Result<()> {
        self.admit_dispatched_goal(permit, false)?;
        self.update_with_effects(store, permit, now_ms, |task| {
            match &mut task.workflow {
                WorkflowSnapshot::Goal { goal } => {
                    goal.state = GoalState::Blocked;
                    goal.reported_status = Some(reason);
                }
                WorkflowSnapshot::Execute { execution } => {
                    execution.record.state = PlanExecutionState::Blocked;
                    execution.record.findings.push(reason);
                }
                WorkflowSnapshot::Plan { .. } => return Err(super::StaleTask.into()),
            }
            Ok(((), crate::storage::TaskEffects::default()))
        })
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
            task.edit(|task| {
                let Some(entry) = task.entry.as_mut().filter(|entry| &entry.id == id) else {
                    return Ok(false);
                };
                if entry.delivery == crate::task::TaskInputDelivery::Accepted {
                    return Ok(true);
                }
                entry.delivery = crate::task::TaskInputDelivery::Accepted;
                task.updated_at_ms = now_ms;
                Ok(true)
            })
        });
        match result {
            Err(error) if error.is::<super::StaleTask>() => Ok(false),
            result => result,
        }
    }

    /// Apply a real native receipt after the same invocation's logical effects advance task state.
    pub(crate) fn acknowledge_dispatched_input(
        &self,
        store: &mut SqliteStore,
        receipt: &super::TaskReceiptPermit,
        id: &super::TaskInputId,
        kind: super::TaskReceiptInput,
        now_ms: i64,
    ) -> Result<bool> {
        let Some(current) = self.current()? else {
            return Ok(false);
        };
        let permit = match receipt.permit_for(&current) {
            Ok(permit) => permit,
            Err(error) if error.is::<super::StaleTask>() => return Ok(false),
            Err(error) => return Err(error),
        };
        let result = store.commit_task_observation(&self.session_id, &permit, now_ms, |active| {
            let task = active.as_mut().context("dispatched task disappeared")?;
            let acknowledged = task.edit(|task| {
                let input = match kind {
                    super::TaskReceiptInput::Continuation => task
                        .turn
                        .as_mut()
                        .and_then(|turn| turn.continuation.as_mut())
                        .map(|input| (&input.id, &mut input.delivery)),
                    super::TaskReceiptInput::Entry => task
                        .entry
                        .as_mut()
                        .map(|entry| (&entry.id, &mut entry.delivery)),
                };
                let Some((_, delivery)) = input.filter(|(identity, _)| *identity == id) else {
                    return Ok(false);
                };
                *delivery = super::TaskInputDelivery::Accepted;
                task.updated_at_ms = now_ms;
                Ok(true)
            })?;
            Ok((acknowledged, crate::storage::TaskEffects::default()))
        });
        match result {
            Ok(result) => Ok(result.unwrap_or(false)),
            Err(error) if error.is::<super::StaleTask>() => Ok(false),
            Err(error) => Err(error),
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
    ) -> Result<(super::GoalSnapshot, crate::goal::ContinuationDecision)> {
        store
            .commit_task_observation(&self.session_id, permit, now_ms, |active| {
                permit.validate_observation(active.as_ref())?;
                let mut candidate = active
                    .as_ref()
                    .context("session has no active task")?
                    .draft();
                let task = &mut candidate;
                ensure!(
                    task.entry_accepted(),
                    "task entry has not been acknowledged by the provider"
                );
                if task
                    .observed_turn
                    .as_ref()
                    .is_some_and(|observed| observed >= &completed_turn)
                {
                    return Ok((
                        (goal_view(task)?, crate::goal::ContinuationDecision::Stop),
                        crate::storage::TaskEffects::default(),
                    ));
                }
                let previous = task.clone();
                let decision = match &mut task.workflow {
                    WorkflowSnapshot::Goal { goal } => {
                        crate::goal::observe(&mut goal.state, &mut goal.continuation, evidence)
                    }
                    WorkflowSnapshot::Execute { execution } => {
                        evidence.structured_complete = false;
                        let mut state = match execution.record.state {
                            PlanExecutionState::Active => GoalState::Active,
                            PlanExecutionState::Paused => GoalState::Paused,
                            PlanExecutionState::Blocked => GoalState::Blocked,
                            PlanExecutionState::Stalled => GoalState::Stalled,
                            PlanExecutionState::Complete => GoalState::Complete,
                            PlanExecutionState::Cancelled => GoalState::Cleared,
                        };
                        let decision =
                            crate::goal::observe(&mut state, &mut execution.continuation, evidence);
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
                    WorkflowSnapshot::Plan { .. } => {
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
                        WorkflowSnapshot::Goal { goal },
                        crate::goal::ContinuationDecision::Continue
                        | crate::goal::ContinuationDecision::RetryNoProgress,
                    ) => Some(super::TaskContinuation {
                        id: super::TaskInputId::fresh(),
                        work: super::TaskWork::Provider {
                            instruction: format!(
                                "Continue working toward this goal: {}",
                                goal.objective
                            ),
                        },
                        reason: "The active task requires another turn".into(),
                        delivery: crate::task::TaskInputDelivery::Pending,
                    }),
                    _ => None,
                };
                task.observed_turn = Some(completed_turn.clone());
                task.turn = Some(super::TaskTurn {
                    id: completed_turn,
                    continuation,
                });
                let mut effects = crate::storage::TaskEffects::default();
                if task.status().terminal() {
                    let outcome = super::TaskOutcome {
                        id: task.id.clone(),
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
                if active.is_some() {
                    *active = Some(Task::try_from(candidate)?);
                }
                Ok(((goal, decision), effects))
            })?
            .context("turn observation unexpectedly reused a command")
    }

    /// Count a completed response once; interruption and failure never consume the plan budget.
    /// Planning observes only responses accepted by [`latest_turn_completed_normally`].
    pub(crate) fn observe_plan_response(
        &self,
        store: &mut SqliteStore,
        permit: &TaskPermit,
        exchange: &crate::exchange::Exchange,
        canonical_progress: bool,
        draft_dirty: bool,
        now_ms: i64,
    ) -> Result<Option<(crate::plan::PlanRecord, bool)>> {
        if !latest_turn_completed_normally(exchange) {
            return Ok(None);
        }
        ensure!(
            exchange.session_id == self.session_id,
            "planning response belongs to another session"
        );
        let completed_turn = crate::task::TaskTurnId {
            exchange_ordinal: exchange.ordinal,
            turn_ordinal: exchange.turn.len(),
        };
        store.commit_task_change(
            &self.session_id,
            Some(permit.revision()),
            now_ms,
            |active| {
                let task = active.as_mut().context("session has no active task")?;
                permit.validate(Some(task))?;
                task.edit(|task| {
                ensure!(
                    task.entry_accepted(),
                    "plan entry has not been acknowledged"
                );
                if task.observed_turn.as_ref().is_some_and(|observed| observed >= &completed_turn)
                {
                    return Ok(None);
                }
                let WorkflowSnapshot::Plan { plan } = &mut task.workflow else {
                    anyhow::bail!("planning progress requires an active plan");
                };
                ensure!(!plan.paused, "planning task is paused");
                if draft_dirty && plan.artifact.state == crate::plan::PlanState::AwaitingReview {
                    crate::plan::state_machine::PlanStateMachine::apply(
                        &mut plan.artifact, crate::plan::state_machine::PlanEvent::ChangesRequested, now_ms,
                    )?;
                }
                if canonical_progress {
                    plan.artifact.generation.canonical_revision = plan
                        .artifact
                        .generation
                        .canonical_revision
                        .saturating_add(1);
                }
                let budget_allows = plan.artifact.generation.observe(canonical_progress);
                let generating = matches!(plan.artifact.state,
                    crate::plan::PlanState::ConfiguringChecks | crate::plan::PlanState::Generating | crate::plan::PlanState::Revising);
                if generating && !budget_allows {
                    if plan.artifact.state == crate::plan::PlanState::ConfiguringChecks {
                        plan.paused = true;
                    } else { crate::plan::state_machine::PlanStateMachine::apply(
                        &mut plan.artifact,
                        crate::plan::state_machine::PlanEvent::TransitionFailed,
                        now_ms,
                    )?; }
                }
                let continuing = generating && budget_allows;
                let artifact = plan.artifact.clone();
                let reserved = task.turn.as_ref().filter(|turn| turn.id == completed_turn)
                    .and_then(|turn| turn.continuation.clone())
                    .filter(|input| input.delivery == super::TaskInputDelivery::Pending);
                task.observed_turn = Some(completed_turn.clone());
                task.turn = Some(super::TaskTurn {
                    id: completed_turn,
                    continuation: if continuing { reserved.or_else(|| Some(super::TaskContinuation {
                        id: super::TaskInputId::fresh(),
                        work: super::TaskWork::Provider { instruction: if artifact.state == crate::plan::PlanState::ConfiguringChecks {
                            crate::plan::PlanPrompt::checks_setup(&artifact.request)
                        } else { format!(
                            "Continue the active planning task in its existing directory. The previous turn ended without submitting the plan or asking a question. Use ordinary file tools for docs/ and files/, then call plan_submit with the plan ID and no expected version. Prose alone does not submit a plan. Use question_ask only for a new open decision, and only when the user's question preferences allow it. Do not repeat answered questions. If a tool keeps failing, report the failure instead of repeating the same rejected request.\n\nPlanning continuation: turn {} of {}.",
                            artifact.generation.budget.turn_count + 1,
                            artifact.generation.budget.max_turn_count,
                        ) } },
                        reason: "The planning turn ended without submitting a plan".into(),
                        delivery: super::TaskInputDelivery::Pending,
                    })) } else { None },
                });
                if generating && !budget_allows { task.renew(now_ms); } else { task.updated_at_ms = now_ms; }
                Ok(Some((artifact, continuing)))
                })
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
            task.edit(|task| {
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
            })
        });
        match result {
            Err(error) if error.is::<super::StaleTask>() => Ok(false),
            result => result,
        }
    }

    /// Cancel only the observed pending continuation under its original task and input guards.
    pub(crate) fn cancel_pending_continuation(
        &self,
        store: &mut SqliteStore,
        permit: &TaskPermit,
        input: &super::TaskInputId,
        now_ms: i64,
    ) -> std::result::Result<bool, crate::exchange::command::TaskCommandFailure> {
        use crate::exchange::command::TaskCommandFailure;
        store
            .commit_task_change(
                &self.session_id,
                Some(permit.revision()),
                now_ms,
                |active| {
                    permit.validate(active.as_ref())?;
                    let task = active.as_mut().ok_or(TaskCommandFailure::MissingTask)?;
                    task.edit(|draft| {
                        let Some(continuation) = draft
                            .turn
                            .as_mut()
                            .and_then(|turn| turn.continuation.as_mut())
                        else {
                            return Ok(false);
                        };
                        if &continuation.id != input
                            || continuation.delivery != super::TaskInputDelivery::Pending
                        {
                            return Ok(false);
                        }
                        continuation.delivery = super::TaskInputDelivery::Cancelled;
                        draft.updated_at_ms = now_ms;
                        Ok(true)
                    })
                },
            )
            .map_err(|error| {
                if error.is::<super::StaleTask>() {
                    TaskCommandFailure::Changed
                } else {
                    error
                        .downcast::<TaskCommandFailure>()
                        .unwrap_or(TaskCommandFailure::StateUnavailable)
                }
            })
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
                if let Some(task) = active.as_mut() {
                    task.edit(|task| {
                        if let Some(input) = task
                            .turn
                            .as_mut()
                            .and_then(|turn| turn.continuation.as_mut())
                        {
                            input.delivery = crate::task::TaskInputDelivery::Cancelled;
                        }
                        Ok(())
                    })?;
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
            "name":"Test", "workspace":"workspace", "provider":"mock", "model":"mock",
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
                TaskChange::Install(Box::new(Task::goal("Finish".into(), Default::default(), 1))),
                1,
            )
            .unwrap();
        (root, store, controller)
    }

    /// Commit owner task control and its durable result/checkpoint under the same task fence.
    pub(crate) fn apply_owner_command(
        &self,
        store: &mut SqliteStore,
        id: &crate::exchange::command::CommandId,
        command: &crate::exchange::command::TaskCommand,
        permit: &TaskPermit,
        now_ms: i64,
    ) -> std::result::Result<
        crate::exchange::command::TaskCommandOutcome,
        crate::exchange::command::TaskCommandFailure,
    > {
        use crate::exchange::command::{
            TaskCommand, TaskCommandFailure, TaskCommandOutcome, TaskSelection,
        };
        let committed = store.commit_task_observation(&self.session_id, permit, now_ms, |active| {
            let current = active.as_ref().ok_or(TaskCommandFailure::MissingTask)?;
            let previous = current.clone();
            let change = match command {
                TaskCommand::EditPlan(_)
                | TaskCommand::Resume(_)
                | TaskCommand::RequestPlanChanges(_)
                | TaskCommand::CancelPlan(_)
                | TaskCommand::ExecutePlan(_)
                | TaskCommand::StartGoal(_)
                | TaskCommand::StartPlan(_)
                | TaskCommand::AttachPlan(_)
                | TaskCommand::ForkPlan(_)
                | TaskCommand::Replan(_) => {
                    unreachable!("task creation commits its prepared replacement")
                }
                TaskCommand::Pause(_) if current.status().terminal() => TaskChange::Preserve,
                TaskCommand::Pause(_) => TaskChange::Pause,
                TaskCommand::Clear(_) => TaskChange::Clear,
                TaskCommand::SetAutoAccept(policy) => {
                    policy.target.validate(Some(current))?;
                    if !matches!(current.kind(), TaskKind::Plan | TaskKind::Execute) {
                        return Err(TaskCommandFailure::InvalidPolicyTask.into());
                    }
                    TaskChange::SetPlanAcceptance(policy.enabled)
                }
            };
            Self::apply_change(active, change, now_ms)?;
            let outcome = match command {
                TaskCommand::EditPlan(_)
                | TaskCommand::Resume(_)
                | TaskCommand::RequestPlanChanges(_)
                | TaskCommand::CancelPlan(_)
                | TaskCommand::ExecutePlan(_)
                | TaskCommand::StartGoal(_)
                | TaskCommand::StartPlan(_)
                | TaskCommand::AttachPlan(_)
                | TaskCommand::ForkPlan(_)
                | TaskCommand::Replan(_) => {
                    unreachable!("task creation commits its prepared replacement")
                }
                TaskCommand::Pause(_) => {
                    let task = active.as_ref().ok_or(TaskCommandFailure::MissingTask)?;
                    if previous.status().terminal() {
                        TaskCommandOutcome::Unchanged {
                            task: TaskSelection::capture(task),
                        }
                    } else {
                        TaskCommandOutcome::Paused {
                            task: TaskSelection::capture(task),
                        }
                    }
                }
                TaskCommand::Clear(target) => TaskCommandOutcome::Cleared {
                    task: target.id().clone(),
                },
                TaskCommand::SetAutoAccept(policy) => TaskCommandOutcome::AutoAcceptChanged {
                    task: TaskSelection::capture(
                        active.as_ref().ok_or(TaskCommandFailure::MissingTask)?,
                    ),
                    enabled: policy.enabled,
                },
            };
            let mut effects =
                clear_effect(&self.session_id, Some(&previous), active.as_ref(), now_ms);
            effects.retire_questions = matches!(command, TaskCommand::Clear(_));
            effects.owner_task_commit = Some(match command {
                TaskCommand::EditPlan(_)
                | TaskCommand::Resume(_)
                | TaskCommand::RequestPlanChanges(_)
                | TaskCommand::CancelPlan(_)
                | TaskCommand::ExecutePlan(_)
                | TaskCommand::StartGoal(_)
                | TaskCommand::StartPlan(_)
                | TaskCommand::AttachPlan(_)
                | TaskCommand::ForkPlan(_)
                | TaskCommand::Replan(_) => {
                    unreachable!("task creation commits its prepared replacement")
                }
                TaskCommand::SetAutoAccept(_) => {
                    crate::exchange::journal::TaskCommandCommit::Complete {
                        command: id.clone(),
                        outcome: outcome.clone(),
                    }
                }
                TaskCommand::Pause(_) | TaskCommand::Clear(_) => {
                    crate::exchange::journal::TaskCommandCommit::Complete {
                        command: id.clone(),
                        outcome: outcome.clone(),
                    }
                }
            });
            Ok((outcome, effects))
        });
        committed
            .map_err(|error| {
                if error.is::<super::StaleTask>() {
                    return TaskCommandFailure::Changed;
                }
                error
                    .downcast::<TaskCommandFailure>()
                    .unwrap_or(TaskCommandFailure::StateUnavailable)
            })?
            .ok_or(TaskCommandFailure::StateUnavailable)
    }

    /// Confirm current plan ownership without changing the task, its questions, or existing native work.
    pub(crate) fn confirm_owner_attachment(
        &self,
        store: &mut SqliteStore,
        id: &crate::exchange::command::CommandId,
        permit: &TaskPermit,
        source: &crate::exchange::command::PlanSelection,
        now_ms: i64,
    ) -> std::result::Result<
        crate::exchange::command::TaskCommandOutcome,
        crate::exchange::command::TaskCommandFailure,
    > {
        use crate::exchange::command::{
            PlanSourceGuard, TaskCommandFailure, TaskCommandOutcome, TaskSelection,
        };
        source.validate_attachment()?;
        store
            .commit_task_observation(&self.session_id, permit, now_ms, |active| {
                let task = active.as_ref().ok_or(TaskCommandFailure::MissingTask)?;
                let WorkflowSnapshot::Plan { plan } = &task.workflow else {
                    return Err(TaskCommandFailure::Changed.into());
                };
                source.validate(&plan.artifact)?;
                let selected = TaskSelection::capture(task);
                let outcome = TaskCommandOutcome::Attached {
                    task: selected.clone(),
                    plan: source.id().clone(),
                };
                let effects = crate::storage::TaskEffects {
                    source_plan: vec![PlanSourceGuard::Working(source.clone())],
                    owner_task_commit: Some(
                        crate::exchange::journal::TaskCommandCommit::Attachment {
                            command: id.clone(),
                            task: selected,
                            source: source.clone(),
                        },
                    ),
                    ..Default::default()
                };
                Ok((outcome, effects))
            })
            .map_err(|error| {
                if error.is::<super::StaleTask>() {
                    TaskCommandFailure::Changed
                } else if matches!(
                    error.downcast_ref::<crate::exchange::journal::CommandJournalError>(),
                    Some(crate::exchange::journal::CommandJournalError::IdentityConflict)
                ) {
                    TaskCommandFailure::IdentityConflict
                } else {
                    error
                        .downcast::<TaskCommandFailure>()
                        .unwrap_or(TaskCommandFailure::StateUnavailable)
                }
            })?
            .ok_or(TaskCommandFailure::StateUnavailable)
    }

    /// Install a prepared task and checkpoint its initial input or waiting-plan ownership atomically.
    pub(crate) fn apply_owner_creation(
        &self,
        store: &mut SqliteStore,
        id: &crate::exchange::command::CommandId,
        permit: Option<&TaskPermit>,
        prepared: super::TaskInstallation,
        now_ms: i64,
    ) -> std::result::Result<
        crate::exchange::command::TaskCommandCheckpoint,
        crate::exchange::command::TaskCommandFailure,
    > {
        use crate::exchange::command::{TaskCommandCheckpoint, TaskCommandFailure, TaskSelection};
        let (task, work, mut effects) = prepared.into_parts();
        let commit = |active: &mut Option<Task>| -> anyhow::Result<_> {
            if let Some(permit) = permit {
                permit.validate_observation(active.as_ref())?;
            } else if active.is_some() {
                return Err(TaskCommandFailure::Changed.into());
            }
            let previous = active.clone();
            Self::apply_change(active, TaskChange::Install(Box::new(task)), now_ms)?;
            let task = active
                .as_ref()
                .ok_or(TaskCommandFailure::StateUnavailable)?;
            let selected = TaskSelection::capture(task);
            let kind = task.kind();
            let source = match effects.source_plan.as_slice() {
                [] => None,
                [source] => Some(source.clone()),
                _ => return Err(TaskCommandFailure::IdentityConflict.into()),
            };
            let terminal =
                clear_effect(&self.session_id, previous.as_ref(), active.as_ref(), now_ms);
            effects.event.extend(terminal.event);
            effects.plan.extend(terminal.plan);
            effects.execution.extend(terminal.execution);
            effects.retire_questions = true;
            let checkpoint = match work {
                super::TaskInstallationWork::Model(input) => {
                    effects.owner_task_commit =
                        Some(crate::exchange::journal::TaskCommandCommit::AwaitInput {
                            command: id.clone(),
                            task: selected.clone(),
                            input: input.clone(),
                            kind,
                            source,
                        });
                    TaskCommandCheckpoint::InputPending {
                        task: selected,
                        input,
                    }
                }
                super::TaskInstallationWork::Waiting(plan) => {
                    let source = match source {
                        Some(crate::exchange::command::PlanSourceGuard::Working(source))
                            if source.id() == &plan =>
                        {
                            source
                        }
                        Some(
                            crate::exchange::command::PlanSourceGuard::Working(_)
                            | crate::exchange::command::PlanSourceGuard::Historical(_),
                        )
                        | None => return Err(TaskCommandFailure::IdentityConflict.into()),
                    };
                    effects.owner_task_commit =
                        Some(crate::exchange::journal::TaskCommandCommit::Attachment {
                            command: id.clone(),
                            task: selected.clone(),
                            source,
                        });
                    TaskCommandCheckpoint::Control {
                        outcome: Box::new(crate::exchange::command::TaskCommandOutcome::Attached {
                            task: selected,
                            plan,
                        }),
                    }
                }
            };
            Ok((checkpoint, effects))
        };
        let result = match permit {
            Some(permit) => store.commit_task_observation(&self.session_id, permit, now_ms, commit),
            None => store.commit_task_effects(&self.session_id, None, now_ms, commit),
        };
        result
            .map_err(|error| {
                if error.is::<super::StaleTask>() {
                    TaskCommandFailure::Changed
                } else if matches!(
                    error.downcast_ref::<crate::exchange::journal::CommandJournalError>(),
                    Some(crate::exchange::journal::CommandJournalError::IdentityConflict)
                ) {
                    TaskCommandFailure::IdentityConflict
                } else {
                    error
                        .downcast::<TaskCommandFailure>()
                        .unwrap_or(TaskCommandFailure::StateUnavailable)
                }
            })?
            .ok_or(TaskCommandFailure::StateUnavailable)
    }

    /// Commit captured execution effects and their finite result or input checkpoint atomically.
    pub(crate) fn apply_owner_execution(
        &self,
        store: &mut SqliteStore,
        context: OwnerTaskCommit<'_>,
        selection: &crate::exchange::command::PlanExecutionSelection,
        prepared: crate::broker::plan_entry::PreparedTaskChange,
        question_set: Option<crate::exchange::command::PlanQuestionSetSelection>,
    ) -> std::result::Result<
        crate::exchange::command::TaskCommandCheckpoint,
        crate::exchange::command::TaskCommandFailure,
    > {
        let OwnerTaskCommit { id, permit, now_ms } = context;
        use crate::exchange::command::{
            PlanSourceGuard, TaskCommandCheckpoint, TaskCommandFailure, TaskCommandOutcome,
            TaskSelection,
        };
        let crate::broker::plan_entry::PreparedTaskChange {
            change,
            mut effects,
        } = prepared;
        effects
            .source_plan
            .push(PlanSourceGuard::Working(selection.source().clone()));
        let commit = |active: &mut Option<Task>| -> anyhow::Result<_> {
            if let Some(permit) = permit {
                permit.validate_observation(active.as_ref())?;
            } else if active.is_some() {
                return Err(TaskCommandFailure::Changed.into());
            }
            let installing = matches!(&change, TaskChange::Install(_));
            let previous = active.clone();
            Self::apply_change(active, change, now_ms)?;
            if installing {
                let terminal =
                    clear_effect(&self.session_id, previous.as_ref(), active.as_ref(), now_ms);
                effects.event.extend(terminal.event);
                effects.plan.extend(terminal.plan);
                effects.execution.extend(terminal.execution);
                effects.retire_questions = true;
            }
            let checkpoint = if let Some(question_set) = question_set {
                let selected = active
                    .as_ref()
                    .filter(|task| match &task.workflow {
                        WorkflowSnapshot::Plan { plan } => {
                            plan.artifact.id == selection.source().id().as_str()
                        }
                        WorkflowSnapshot::Execute { execution } => {
                            execution.record.plan_id == selection.source().id().as_str()
                        }
                        WorkflowSnapshot::Goal { .. } => false,
                    })
                    .map(TaskSelection::capture);
                let outcome = TaskCommandOutcome::AwaitingInput {
                    task: selected,
                    source: selection.source().clone(),
                    question_set,
                };
                effects.owner_task_commit =
                    Some(crate::exchange::journal::TaskCommandCommit::Complete {
                        command: id.clone(),
                        outcome: outcome.clone(),
                    });
                TaskCommandCheckpoint::Control {
                    outcome: Box::new(outcome),
                }
            } else {
                let task = active.as_ref().ok_or(TaskCommandFailure::MissingTask)?;
                let selected = TaskSelection::capture(task);
                if let Some(entry) = task.entry.as_ref().filter(|entry| {
                    entry.delivery == super::TaskInputDelivery::Pending
                        && task.next_action() == super::TaskNextAction::Model
                }) {
                    effects.owner_task_commit =
                        Some(crate::exchange::journal::TaskCommandCommit::AwaitInput {
                            command: id.clone(),
                            task: selected.clone(),
                            input: entry.id.clone(),
                            kind: task.kind(),
                            source: Some(PlanSourceGuard::Working(selection.source().clone())),
                        });
                    TaskCommandCheckpoint::InputPending {
                        task: selected,
                        input: entry.id.clone(),
                    }
                } else {
                    let outcome = TaskCommandOutcome::ExecutionUpdated {
                        task: selected,
                        source: selection.source().clone(),
                    };
                    effects.owner_task_commit =
                        Some(crate::exchange::journal::TaskCommandCommit::Complete {
                            command: id.clone(),
                            outcome: outcome.clone(),
                        });
                    TaskCommandCheckpoint::Control {
                        outcome: Box::new(outcome),
                    }
                }
            };
            Ok((checkpoint, effects))
        };
        let result = match permit {
            Some(permit) => store.commit_task_observation(&self.session_id, permit, now_ms, commit),
            None => store.commit_task_effects(&self.session_id, None, now_ms, commit),
        };
        result
            .map_err(|error| {
                if error.is::<super::StaleTask>() {
                    TaskCommandFailure::Changed
                } else if matches!(
                    error.downcast_ref::<crate::exchange::journal::CommandJournalError>(),
                    Some(crate::exchange::journal::CommandJournalError::IdentityConflict)
                ) {
                    TaskCommandFailure::IdentityConflict
                } else {
                    error
                        .downcast::<TaskCommandFailure>()
                        .unwrap_or(TaskCommandFailure::StateUnavailable)
                }
            })?
            .ok_or(TaskCommandFailure::StateUnavailable)
    }

    /// Commit a prepared resume or review mutation with its task checkpoint and immutable source guard.
    pub(crate) fn apply_owner_mutation(
        &self,
        store: &mut SqliteStore,
        id: &crate::exchange::command::CommandId,
        intent: &crate::exchange::command::TaskCommand,
        permit: Option<&TaskPermit>,
        prepared: crate::broker::plan_entry::PreparedTaskChange,
        now_ms: i64,
    ) -> std::result::Result<
        crate::exchange::command::TaskCommandCheckpoint,
        crate::exchange::command::TaskCommandFailure,
    > {
        use crate::exchange::command::{
            TaskCommand, TaskCommandCheckpoint, TaskCommandFailure, TaskCommandOutcome,
            TaskSelection,
        };
        let crate::broker::plan_entry::PreparedTaskChange {
            change,
            mut effects,
        } = prepared;
        if let Some(source) = intent.plan_source() {
            effects.source_plan.push(source);
        }
        let commit = |active: &mut Option<Task>| -> anyhow::Result<_> {
            match permit {
                Some(permit) => permit.validate_observation(active.as_ref())?,
                None if active.is_none() => {}
                None => return Err(TaskCommandFailure::Changed.into()),
            }
            let previous = active.clone();
            let finite = matches!(intent, TaskCommand::EditPlan(_))
                || matches!(intent, TaskCommand::CancelPlan(_))
                    && matches!(&change, TaskChange::Preserve);
            let installing = matches!(&change, TaskChange::Install(_));
            Self::apply_change(active, change, now_ms)?;
            if installing {
                let terminal =
                    clear_effect(&self.session_id, previous.as_ref(), active.as_ref(), now_ms);
                effects.event.extend(terminal.event);
                effects.plan.extend(terminal.plan);
                effects.execution.extend(terminal.execution);
                effects.retire_questions = true;
            }
            let task = active.as_ref();
            if matches!(
                intent,
                TaskCommand::Resume(_) | TaskCommand::RequestPlanChanges(_)
            ) && let Some(task) = task
                && task.next_action() == super::TaskNextAction::Model
            {
                let input = task
                    .entry
                    .as_ref()
                    .filter(|entry| entry.delivery == super::TaskInputDelivery::Pending)
                    .map(|entry| entry.id.clone())
                    .or_else(|| {
                        task.turn
                            .as_ref()
                            .and_then(|turn| turn.continuation.as_ref())
                            .filter(|input| {
                                input.delivery == super::TaskInputDelivery::Pending
                                    && !input.work.is_checks()
                            })
                            .map(|input| input.id.clone())
                    });
                if let Some(input) = input {
                    let selected = TaskSelection::capture(task);
                    effects.owner_task_commit =
                        Some(crate::exchange::journal::TaskCommandCommit::AwaitInput {
                            command: id.clone(),
                            task: selected.clone(),
                            input: input.clone(),
                            kind: task.kind(),
                            source: intent.plan_source(),
                        });
                    return Ok((
                        TaskCommandCheckpoint::InputPending {
                            task: selected,
                            input,
                        },
                        effects,
                    ));
                }
            }
            let outcome = match intent {
                TaskCommand::Resume(_) => TaskCommandOutcome::Resumed {
                    task: TaskSelection::capture(task.ok_or(TaskCommandFailure::MissingTask)?),
                },
                TaskCommand::EditPlan(request) => {
                    let plan = effects
                        .plan
                        .iter()
                        .find(|plan| plan.id == request.source().id().as_str())
                        .cloned()
                        .ok_or(TaskCommandFailure::StateUnavailable)?;
                    TaskCommandOutcome::PlanEdited {
                        source: crate::exchange::command::PlanSelection::capture(&plan)?,
                        plan: Box::new(plan),
                        task: task
                            .filter(|task| match &task.workflow {
                                WorkflowSnapshot::Plan { plan } => {
                                    plan.artifact.id == request.source().id().as_str()
                                }
                                WorkflowSnapshot::Execute { execution } => {
                                    execution.record.plan_id == request.source().id().as_str()
                                }
                                WorkflowSnapshot::Goal { .. } => false,
                            })
                            .map(TaskSelection::capture),
                    }
                }
                TaskCommand::RequestPlanChanges(request) => {
                    TaskCommandOutcome::PlanChangesRequested {
                        source: request.selection().source().clone(),
                        task: task.map(TaskSelection::capture),
                    }
                }
                TaskCommand::CancelPlan(request) => TaskCommandOutcome::PlanCancelled {
                    source: request.selection().source().clone(),
                    task: if finite {
                        None
                    } else {
                        task.map(TaskSelection::capture)
                    },
                },
                _ => return Err(TaskCommandFailure::IdentityConflict.into()),
            };
            effects.owner_task_commit =
                Some(crate::exchange::journal::TaskCommandCommit::Complete {
                    command: id.clone(),
                    outcome: outcome.clone(),
                });
            Ok((
                TaskCommandCheckpoint::Control {
                    outcome: Box::new(outcome),
                },
                effects,
            ))
        };
        let result = match permit {
            Some(permit) => store.commit_task_observation(&self.session_id, permit, now_ms, commit),
            None => store.commit_task_effects(&self.session_id, None, now_ms, commit),
        };
        result
            .map_err(|error| {
                if error.is::<super::StaleTask>() {
                    TaskCommandFailure::Changed
                } else {
                    error
                        .downcast::<TaskCommandFailure>()
                        .unwrap_or(TaskCommandFailure::StateUnavailable)
                }
            })?
            .ok_or(TaskCommandFailure::StateUnavailable)
    }

    /// Reserve exactly one captured workflow continuation and its input in the same commit.
    pub(crate) fn apply_owner_continuation(
        &self,
        store: &mut SqliteStore,
        context: OwnerTaskCommit<'_>,
        request: &crate::exchange::command::WorkflowContinuationRequest,
        prepared: crate::broker::plan_entry::PreparedTaskChange,
        instruction: String,
    ) -> std::result::Result<
        crate::exchange::command::TaskCommandCheckpoint,
        crate::exchange::command::TaskCommandFailure,
    > {
        let OwnerTaskCommit { id, permit, now_ms } = context;
        use crate::exchange::command::{
            InputId, TaskCommandCheckpoint, TaskCommandFailure, TaskSelection,
        };
        let crate::broker::plan_entry::PreparedTaskChange {
            change,
            mut effects,
        } = prepared;
        effects.continuation_guard.push(request.clone());
        let commit = |active: &mut Option<Task>| -> anyhow::Result<_> {
            match permit {
                Some(permit) => permit.validate_observation(active.as_ref())?,
                None if active.is_none() => {}
                None => return Err(TaskCommandFailure::Changed.into()),
            }
            Self::apply_change(active, change, now_ms)?;
            let selected = active.as_ref().map(TaskSelection::capture);
            let (input, purpose) = match active.as_ref() {
                Some(task) => {
                    let turn = task
                        .turn
                        .as_ref()
                        .ok_or(TaskCommandFailure::PreparationUnavailable)?;
                    let continuation = turn
                        .continuation
                        .as_ref()
                        .filter(|input| input.delivery == super::TaskInputDelivery::Pending)
                        .ok_or(TaskCommandFailure::PreparationUnavailable)?;
                    if continuation.work.is_checks() {
                        let outcome =
                            crate::exchange::command::TaskCommandOutcome::ContinuationReserved {
                                task: TaskSelection::capture(task),
                            };
                        effects.owner_task_commit =
                            Some(crate::exchange::journal::TaskCommandCommit::Complete {
                                command: id.clone(),
                                outcome: outcome.clone(),
                            });
                        return Ok((
                            TaskCommandCheckpoint::Control {
                                outcome: Box::new(outcome),
                            },
                            effects,
                        ));
                    }
                    (
                        InputId::new(continuation.id.as_str())
                            .map_err(|_| TaskCommandFailure::PreparationUnavailable)?,
                        crate::input::InputPurpose::TaskContinuation {
                            revision: task.revision.clone(),
                            id: continuation.id.clone(),
                            completed_turn_id: turn.id.clone(),
                            reason: continuation.reason.clone(),
                        },
                    )
                }
                None => {
                    let exchange = match &request.target {
                        crate::exchange::command::WorkflowContinuationTarget::Exchange(
                            exchange,
                        ) => exchange,
                        crate::exchange::command::WorkflowContinuationTarget::Questions(
                            crate::exchange::command::QuestionContinuationSelection {
                                workflow:
                                    crate::exchange::command::QuestionWorkflow::Exchange(exchange),
                                ..
                            },
                        ) => exchange,
                        _ => return Err(TaskCommandFailure::IdentityConflict.into()),
                    };
                    (
                        InputId::new(uuid::Uuid::new_v4().to_string())
                            .map_err(|_| TaskCommandFailure::PreparationUnavailable)?,
                        crate::input::InputPurpose::WorkflowContinuation {
                            exchange: exchange.clone(),
                        },
                    )
                }
            };
            let message = crate::input::InputMessage::admitted(crate::input::InputDraft {
                id: input.as_str().into(),
                text: instruction,
                purpose,
                attachments: Vec::new(),
                config: None,
                intent: crate::input::InputIntent::Auto,
            });
            effects.owner_task_commit =
                Some(crate::exchange::journal::TaskCommandCommit::WorkflowInput {
                    command: id.clone(),
                    task: selected.clone(),
                    message: Box::new(message),
                });
            Ok((
                TaskCommandCheckpoint::WorkflowInputPending {
                    task: selected,
                    input,
                },
                effects,
            ))
        };
        let result = match permit {
            Some(permit) => store.commit_task_observation(&self.session_id, permit, now_ms, commit),
            None => store.commit_task_effects(&self.session_id, None, now_ms, commit),
        };
        result
            .map_err(|error| {
                if error.is::<super::StaleTask>() {
                    TaskCommandFailure::Changed
                } else {
                    error
                        .downcast::<TaskCommandFailure>()
                        .unwrap_or(TaskCommandFailure::StateUnavailable)
                }
            })?
            .ok_or(TaskCommandFailure::StateUnavailable)
    }

    /// Complete unresolved question continuation without creating input or changing its workflow.
    pub(crate) fn apply_owner_question_wait(
        &self,
        store: &mut SqliteStore,
        id: &crate::exchange::command::CommandId,
        request: &crate::exchange::command::WorkflowContinuationRequest,
        permit: Option<&TaskPermit>,
        questions: crate::exchange::command::PlanQuestionSetSelection,
        now_ms: i64,
    ) -> std::result::Result<
        crate::exchange::command::TaskCommandOutcome,
        crate::exchange::command::TaskCommandFailure,
    > {
        use crate::exchange::command::{TaskCommandFailure, TaskCommandOutcome, TaskSelection};
        let commit = |active: &mut Option<Task>| -> anyhow::Result<_> {
            let outcome = TaskCommandOutcome::WorkflowAwaitingInput {
                task: active.as_ref().map(TaskSelection::capture),
                question_set: questions,
            };
            Ok((
                outcome.clone(),
                crate::storage::TaskEffects {
                    continuation_guard: vec![request.clone()],
                    owner_task_commit: Some(
                        crate::exchange::journal::TaskCommandCommit::Complete {
                            command: id.clone(),
                            outcome,
                        },
                    ),
                    ..Default::default()
                },
            ))
        };
        let result = match permit {
            Some(permit) => store.commit_task_observation(&self.session_id, permit, now_ms, commit),
            None => store.commit_task_effects(&self.session_id, None, now_ms, commit),
        };
        result
            .map_err(|error| {
                error
                    .downcast::<TaskCommandFailure>()
                    .unwrap_or(TaskCommandFailure::StateUnavailable)
            })?
            .ok_or(TaskCommandFailure::StateUnavailable)
    }

    pub(crate) fn change(
        &self,
        store: &mut SqliteStore,
        expected: Option<&TaskRevision>,
        change: TaskChange,
        now_ms: i64,
    ) -> Result<Option<Task>> {
        let retire_questions = matches!(
            &change,
            TaskChange::Install(_) | TaskChange::Clear | TaskChange::ResumeWithEntry(_)
        );
        store
            .commit_task_effects(&self.session_id, expected, now_ms, |active| {
                let previous = active.clone();
                let changed = Self::apply_change(active, change, now_ms)?;
                let mut effects =
                    clear_effect(&self.session_id, previous.as_ref(), active.as_ref(), now_ms);
                effects.retire_questions = retire_questions;
                Ok((changed, effects))
            })?
            .context("task change unexpectedly reused a command")
    }

    pub(crate) fn change_with_effects(
        &self,
        store: &mut SqliteStore,
        expected: Option<&TaskRevision>,
        change: TaskChange,
        mut effects: crate::storage::TaskEffects,
        now_ms: i64,
    ) -> Result<Option<Task>> {
        let retire_questions = matches!(
            &change,
            TaskChange::Install(_) | TaskChange::Clear | TaskChange::ResumeWithEntry(_)
        );
        store.commit_task_effects(&self.session_id, expected, now_ms, |active| {
            let previous = active.clone();
            let changed = Self::apply_change(active, change, now_ms)?;
            let terminal =
                clear_effect(&self.session_id, previous.as_ref(), active.as_ref(), now_ms);
            effects.event.extend(terminal.event);
            effects.plan.extend(terminal.plan);
            effects.execution.extend(terminal.execution);
            effects.retire_questions |= retire_questions;
            Ok((changed, effects))
        })?;
        self.current()
    }

    /// Resume execution work without changing admission identity or timestamps.
    /// Callers still validate the enclosing task's resume eligibility before committing it.
    pub(crate) fn resume_execution(execution: &mut super::ExecutionSnapshot) {
        execution.record.state = PlanExecutionState::Active;
        if execution.record.pending_revision_reason.is_some() {
            execution.record.resume_after_review = true;
        }
        execution.continuation.reset();
    }

    fn apply_change(
        active: &mut Option<Task>,
        change: TaskChange,
        now_ms: i64,
    ) -> Result<Option<Task>> {
        let invalidate_observation = matches!(
            &change,
            TaskChange::Pause
                | TaskChange::Resume
                | TaskChange::ResumeWithEntry(_)
                | TaskChange::ResumeWithChecks(_)
                | TaskChange::Update(_)
                | TaskChange::Install(_)
        );
        let mut draft = active.as_ref().map(Task::draft);
        Self::apply_draft_change(&mut draft, change, now_ms)?;
        if invalidate_observation && let Some(task) = &mut draft {
            task.observation_revision = TaskRevision::fresh();
        }
        *active = draft.map(Task::try_from).transpose()?;
        Ok(active.clone())
    }

    fn apply_draft_change(
        active: &mut Option<super::active::TaskSnapshot>,
        change: TaskChange,
        now_ms: i64,
    ) -> Result<Option<super::active::TaskSnapshot>> {
        match change {
            TaskChange::Preserve => {}
            TaskChange::SetPlanAcceptance(enabled) => {
                let task = active
                    .as_mut()
                    .context("plan acceptance requires an active task")?;
                ensure!(
                    matches!(
                        task.workflow,
                        WorkflowSnapshot::Plan { .. } | WorkflowSnapshot::Execute { .. }
                    ),
                    "plan acceptance requires a Plan or Execute task"
                );
                task.auto_accept_plan = enabled;
                task.updated_at_ms = now_ms;
            }
            TaskChange::Update(next) => {
                let mut next = Box::new(next.draft());
                let current = active.as_ref().context("session has no active task")?;
                ensure!(
                    current.id == next.id,
                    "task updates cannot replace task identity"
                );
                ensure!(
                    current.kind() == next.kind(),
                    "task updates cannot change task kind"
                );
                if let (
                    WorkflowSnapshot::Execute {
                        execution: previous,
                    },
                    WorkflowSnapshot::Execute {
                        execution: proposed,
                    },
                ) = (&current.workflow, &next.workflow)
                {
                    validate_execution_update(&previous.record, &proposed.record)?;
                }
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
                Self::apply_draft_change(active, TaskChange::Resume, now_ms)?;
                active
                    .as_mut()
                    .context("session has no active task")?
                    .request_resume_entry(instruction, now_ms)?;
            }
            TaskChange::ResumeWithChecks(completed_turn) => {
                Self::apply_draft_change(active, TaskChange::Resume, now_ms)?;
                let task = active.as_mut().context("session has no active task")?;
                ensure!(
                    matches!(&task.workflow, WorkflowSnapshot::Execute { execution }
                        if execution.record.phase == crate::plan::PlanPhase::Checking),
                    "local check resumption requires Check"
                );
                task.turn = Some(super::TaskTurn {
                    id: completed_turn,
                    continuation: Some(super::TaskContinuation {
                        id: super::TaskInputId::fresh(),
                        reason: "Resume required execution checks".into(),
                        delivery: super::TaskInputDelivery::Pending,
                        work: super::TaskWork::Checks,
                    }),
                });
            }
            TaskChange::Install(next) => {
                let mut next = Box::new(next.draft());
                ensure!(!next.status().terminal(), "cannot install a terminal task");
                if let Some(previous) = active.as_ref().filter(|previous| previous.id == next.id) {
                    let (WorkflowSnapshot::Plan { plan }, WorkflowSnapshot::Execute { execution }) =
                        (&previous.workflow, &next.workflow)
                    else {
                        anyhow::bail!(
                            "retaining task identity requires the Plan to Execute handoff"
                        );
                    };
                    ensure!(
                        plan.artifact.id == execution.record.plan_id
                            && execution.record.original_revision >= plan.artifact.model_revision
                            && execution.continuation == plan.artifact.generation.budget,
                        "workflow handoff must preserve its plan contract and consumed allowance"
                    );
                }
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
                    WorkflowSnapshot::Goal { goal } => goal.state = GoalState::Paused,
                    WorkflowSnapshot::Plan { plan } => plan.paused = true,
                    WorkflowSnapshot::Execute { execution } => {
                        execution.record.state = PlanExecutionState::Paused;
                        execution.record.resume_after_review = false;
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
                    WorkflowSnapshot::Goal { goal } => {
                        goal.state = GoalState::Active;
                        goal.continuation.reset();
                    }
                    WorkflowSnapshot::Plan { plan } => {
                        if plan.artifact.state == crate::plan::PlanState::Failed {
                            crate::plan::state_machine::PlanStateMachine::apply(
                                &mut plan.artifact,
                                crate::plan::state_machine::PlanEvent::RetryRequested,
                                now_ms,
                            )?;
                        }
                        plan.artifact.generation.reset();
                        plan.paused = false;
                    }
                    WorkflowSnapshot::Execute { execution } => {
                        Self::resume_execution(execution);
                    }
                }
                task.renew(now_ms);
            }
        }
        Ok(active.clone())
    }
}

/// Reject record proposals that change ownership or bypass the execution transition graph.
pub(super) fn validate_execution_update(
    previous: &crate::plan::PlanExecutionRecord,
    next: &crate::plan::PlanExecutionRecord,
) -> Result<()> {
    use crate::plan::PlanPhase;
    ensure!(
        previous.id == next.id
            && previous.session_id == next.session_id
            && previous.plan_id == next.plan_id
            && previous.original_revision == next.original_revision,
        "execution update cannot replace its owning contract"
    );
    ensure!(
        next.generation >= previous.generation,
        "execution observation is stale"
    );
    ensure!(
        next.revision >= previous.revision,
        "accepted execution revision cannot move backward"
    );
    ensure!(
        previous.baseline_checkpoint.is_none()
            || previous.baseline_checkpoint == next.baseline_checkpoint,
        "execution baseline cannot change after admission"
    );
    ensure!(
        previous.manual_verification == next.manual_verification
            || previous.revision != next.revision,
        "manual actions can change only with an accepted contract revision"
    );
    if previous.phase != next.phase {
        let allowed = match previous.phase {
            PlanPhase::Implement => next.phase == PlanPhase::Checking,
            PlanPhase::Checking => matches!(next.phase, PlanPhase::Resolve | PlanPhase::Verify),
            PlanPhase::Resolve => matches!(next.phase, PlanPhase::Checking | PlanPhase::Verify),
            PlanPhase::Verify => matches!(next.phase, PlanPhase::Resolve | PlanPhase::Checking),
        };
        ensure!(
            allowed,
            "illegal execution transition: {:?} to {:?}",
            previous.phase,
            next.phase
        );
    }
    if next.state == PlanExecutionState::Complete {
        ensure!(
            matches!(
                previous.phase,
                PlanPhase::Checking | PlanPhase::Verify | PlanPhase::Resolve
            ),
            "implementation cannot complete without its verification gates"
        );
        super::state::validate_passing_gate(next)?;
        ensure!(
            next.manual_verification.is_empty()
                || previous.phase == PlanPhase::Verify
                    && next
                        .verification
                        .last()
                        .is_some_and(|evidence| evidence.revision == next.revision
                            && evidence.report.status
                                == crate::plan::execution::VerificationStatus::Passed),
            "completion requires passing evidence for accepted manual actions"
        );
    }
    Ok(())
}

/// Report whether an exchange's latest turn completed normally, the only outcome that may advance
/// planning or check setup. Running, interrupted, failed, limited, and refused turns leave the plan
/// where it stands, so a user stop never starts the retained planning request on its own.
pub(crate) fn latest_turn_completed_normally(exchange: &crate::exchange::Exchange) -> bool {
    use crate::turn::{TurnOutcome, TurnState};

    let Some(response) = exchange.turn.last() else {
        return false;
    };
    match response.state() {
        TurnState::Finished {
            outcome:
                TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                },
        } => true,
        TurnState::Running
        | TurnState::Finished {
            outcome:
                TurnOutcome::Interrupted
                | TurnOutcome::Failed { .. }
                | TurnOutcome::Completed {
                    stop_reason:
                        forge_provider_api::turn::TurnCompletionReason::TokenLimit
                        | forge_provider_api::turn::TurnCompletionReason::RequestLimit
                        | forge_provider_api::turn::TurnCompletionReason::Refusal,
                },
        } => false,
    }
}

/// Archive displaced live task work without changing completed history or a same-task phase transition.
fn clear_effect(
    session_id: &str,
    previous: Option<&Task>,
    active: Option<&Task>,
    now_ms: i64,
) -> crate::storage::TaskEffects {
    if let Some(task) = previous.filter(|previous| {
        !previous.status().terminal() && active.is_none_or(|current| current.id != previous.id)
    }) {
        let outcome = super::TaskOutcome {
            id: task.id.clone(),
            kind: task.kind(),
            title: task.title().into(),
            state: super::TaskOutcomeState::Cancelled,
            summary: if active.is_some() {
                "Task replaced"
            } else {
                "Task cleared"
            }
            .into(),
        };
        outcome_effect(session_id, task, outcome, now_ms)
    } else {
        crate::storage::TaskEffects::default()
    }
}

fn outcome_effect(
    session_id: &str,
    task: &super::active::TaskSnapshot,
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
        WorkflowSnapshot::Plan { plan } => {
            let mut artifact = plan.artifact.clone();
            artifact.state = match terminal {
                super::TaskOutcomeState::Complete => crate::plan::PlanState::Accepted,
                super::TaskOutcomeState::Cancelled => crate::plan::PlanState::Cancelled,
            };
            artifact.updated_at_ms = now_ms;
            effects.plan.push(artifact);
        }
        WorkflowSnapshot::Execute { execution } => {
            let mut record = execution.record.clone();
            record.state = match terminal {
                super::TaskOutcomeState::Complete => PlanExecutionState::Complete,
                super::TaskOutcomeState::Cancelled => PlanExecutionState::Cancelled,
            };
            record.completed_at_ms = Some(now_ms);
            effects.execution.push(record);
        }
        WorkflowSnapshot::Goal { .. } => {}
    }
    effects
}

fn goal_view(task: &super::active::TaskSnapshot) -> Result<super::GoalSnapshot> {
    Ok(match &task.workflow {
        WorkflowSnapshot::Goal { goal } => goal.clone(),
        WorkflowSnapshot::Execute { execution } => super::GoalSnapshot {
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
            observed_checkpoint: execution.record.observed_checkpoint.clone(),
            reported_status: None,
        },
        WorkflowSnapshot::Plan { .. } => anyhow::bail!("goal evidence requires Goal or Execute"),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::session::{HarnessSession, SessionStore, continuation::ContinuationBudget};

    /// Reject task and artifact publication when an equal-version working document changed.
    #[test]
    fn changed_document_cannot_commit_task_effects() -> Result<()> {
        let (root, mut store, controller) = SessionTaskController::test_goal();
        let previous = controller.current()?.unwrap();
        let artifact: crate::plan::PlanRecord = serde_json::from_value(serde_json::json!({
            "id":"document-plan", "session_id":"session", "request":"Design the change",
            "state":"generating", "working_path":"working.json", "model_revision":0,
            "user_revision":0, "created_at_ms":1, "updated_at_ms":1
        }))?;
        let task = controller
            .change(
                &mut store,
                Some(&previous.revision),
                TaskChange::Install(Box::new(Task::plan(artifact.clone(), 2)?)),
                2,
            )?
            .unwrap();
        let files = crate::plan::PlanFileStore::new(root.path(), root.path()).unwrap();
        let original = crate::plan::test_fixture("document-plan", "original");
        files.create_working_document("session", "document-plan", &original)?;
        let mut intervening = original.clone();
        intervening.overview = "new content at the same version".into();
        files.write_working_document_if_unchanged(
            "session",
            "document-plan",
            &original,
            &intervening,
        )?;
        let mut observed = artifact.clone();
        observed.model_revision = 1;
        let error = controller
            .apply_with_plan_document(
                &mut store,
                &task.permit(),
                files.guard_working_document(&original),
                super::super::TaskEvent::PlanArtifactUpdated(Box::new(observed.clone())),
                Default::default(),
                3,
            )
            .unwrap_err();
        assert!(error.is::<crate::plan::WorkingDocumentChanged>());
        assert_eq!(store.load_plan("document-plan")?.unwrap().model_revision, 0);
        assert_eq!(controller.current()?.unwrap().revision, task.revision);

        controller.apply_with_plan_document(
            &mut store,
            &task.permit(),
            files.guard_working_document(&intervening),
            super::super::TaskEvent::PlanArtifactUpdated(Box::new(observed)),
            Default::default(),
            4,
        )?;
        assert_eq!(store.load_plan("document-plan")?.unwrap().model_revision, 1);
        Ok(())
    }

    #[test]
    fn pause_resume_and_clear_invalidate_admitted_task_controls() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut store = SqliteStore::open(root.path())?;
        let session: HarnessSession = serde_json::from_value(serde_json::json!({
            "id":"session", "primary_agent_id":HarnessSession::primary_agent_id("session"),
            "name":"Test", "workspace":"workspace", "provider":"mock", "model":"mock",
            "effort":"", "approval_mode":"ask", "created_at_ms":0, "updated_at_ms":0,
            "provider_fork_state":{"state":"ready"}
        }))?;
        store.save_session(&session)?;
        let controller = SessionTaskController::new(store.session_admission(&session.id)?)?;
        let task = controller
            .change(
                &mut store,
                None,
                TaskChange::Install(Box::new(Task::goal(
                    "Finish".into(),
                    ContinuationBudget::default(),
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
