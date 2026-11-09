use super::*;
use crate::task::command::{ResumeRequest, SessionAction};
use crate::task::{Task, TaskChange, WorkflowSnapshot};

pub(crate) struct PreparedTaskChange {
    pub change: TaskChange,
    pub effects: crate::storage::TaskEffects,
}

pub(crate) enum PlanTaskRequest {
    New { objective: String },
    Attach { plan_id: String },
    Fork { plan_id: String },
    Replan { plan_id: String, revision: u32 },
}

impl HarnessBroker {
    pub(super) fn dispatch_session_command(
        &mut self,
        method: &str,
        mut params: Value,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        if params.is_null() {
            params = json!({});
        }
        if params.get("operation_id").is_none()
            && let Some(id) = &self.active_operation_id
        {
            params["operation_id"] = json!(id);
        }
        let request = Request {
            id: 0,
            method: method.into(),
            params,
        };
        let command = crate::task::command::SessionCommand::parse_request(&request)?
            .context("request is not a session task command")?;
        self.commit_session_command(command)
    }

    pub(super) fn commit_session_command(
        &mut self,
        command: crate::task::command::SessionCommand,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let payload = command.action.to_params()?;
        let current = self.task_controller.current()?;
        let now_ms = self.clock.now_ms();
        let prepared = self.observer.prepare_task_change(
            &command.action,
            current.as_ref(),
            self.goal_max_turns,
            now_ms,
        )?;
        let plan = prepared.effects.plan.last().cloned();
        let lifecycle = prepared.effects.lifecycle.last().cloned();
        let event_name = command
            .action
            .event_name(lifecycle.as_ref().map(|record| record.kind));
        self.task_controller.change_with_effects(
            &mut self.store,
            current.as_ref().map(|task| &task.revision),
            prepared.change,
            prepared.effects,
            now_ms,
        )?;
        self.session.active_task = self.task_controller.current()?;
        let snapshot = serde_json::to_value(self.snapshot()?)?;
        let events = event_name
            .map(|name| self.event(name, json!({"plan":plan,"lifecycle":lifecycle})))
            .transpose()?
            .into_iter()
            .collect();
        let response = match command.response {
            crate::task::command::CommandResponse::Snapshot => snapshot,
            crate::task::command::CommandResponse::Receipt => json!({
                "id":command.operation_id,"state":"completed","action":payload.get("action")
            }),
        };
        Ok((response, events))
    }
}

impl BrokerObserver {
    pub(crate) fn prepare_task_change(
        &self,
        action: &SessionAction,
        current: Option<&Task>,
        goal_max_turns: u32,
        now_ms: i64,
    ) -> Result<PreparedTaskChange> {
        let effects = crate::storage::TaskEffects::default();
        let mut change = match action {
            SessionAction::PlanAccept { enabled } => TaskChange::SetPlanAcceptance(*enabled),
            SessionAction::NewGoal { text } => {
                let mut task = Task::goal(
                    text.clone(),
                    crate::session::continuation::ContinuationBudget::new(goal_max_turns, 5),
                    now_ms,
                );
                task.request_entry(format!("Goal: {text}\nContinue working until the goal is complete. Use goal_complete or goal_blocked to report terminal state."), now_ms)?;
                TaskChange::Install(Box::new(task))
            }
            SessionAction::NewPlan { text } => {
                TaskChange::Install(Box::new(self.prepare_plan_task(
                    PlanTaskRequest::New {
                        objective: text.clone(),
                    },
                    now_ms,
                )?))
            }
            SessionAction::Attach { plan_id } => {
                TaskChange::Install(Box::new(self.prepare_plan_task(
                    PlanTaskRequest::Attach {
                        plan_id: plan_id.clone(),
                    },
                    now_ms,
                )?))
            }
            SessionAction::Fork { plan_id } => {
                TaskChange::Install(Box::new(self.prepare_plan_task(
                    PlanTaskRequest::Fork {
                        plan_id: plan_id.clone(),
                    },
                    now_ms,
                )?))
            }
            SessionAction::Replan { plan_id, revision } => {
                TaskChange::Install(Box::new(self.prepare_plan_task(
                    PlanTaskRequest::Replan {
                        plan_id: plan_id.clone(),
                        revision: *revision,
                    },
                    now_ms,
                )?))
            }
            SessionAction::Execute { selection } => {
                let (task, effects) = self.prepare_execution(selection, goal_max_turns, now_ms)?;
                return Ok(PreparedTaskChange {
                    change: TaskChange::prepared(current, task),
                    effects,
                });
            }
            SessionAction::Pause => {
                if current.is_some_and(|task| !task.status().terminal()) {
                    TaskChange::Pause
                } else {
                    TaskChange::Preserve
                }
            }
            SessionAction::Clear => TaskChange::Clear,
            SessionAction::Resume { request } => {
                self.prepare_resume(current.context("session has no active task")?, request)?
            }
            SessionAction::ReserveContinuation { completed_turn } => self.prepare_continuation(
                current.context("session has no active task")?,
                completed_turn.as_ref(),
                now_ms,
            )?,
            SessionAction::RequestChanges { .. } | SessionAction::CancelPlan { .. } => {
                return self.prepare_review_change(action, current, now_ms);
            }
        };
        if let TaskChange::Install(task) = &mut change
            && task.kind() == crate::task::TaskKind::Plan
        {
            task.edit(|draft| {
                let WorkflowSnapshot::Plan { plan } = &mut draft.workflow else {
                    unreachable!()
                };
                plan.artifact.generation.budget =
                    crate::session::continuation::ContinuationBudget::new(goal_max_turns, 5);
                Ok(())
            })?;
        }
        Ok(PreparedTaskChange { change, effects })
    }

    fn prepare_resume(&self, task: &Task, request: &ResumeRequest) -> Result<TaskChange> {
        if matches!(&task.workflow, WorkflowSnapshot::Execute { execution }
            if execution.record.pending_revision_reason.is_some())
        {
            return Ok(TaskChange::Resume);
        }
        if task
            .entry
            .as_ref()
            .is_some_and(|entry| entry.delivery == crate::task::TaskInputDelivery::Pending)
            && request.message.is_none()
            && request.continuation.is_none()
        {
            return Ok(TaskChange::Resume);
        }
        if matches!(&task.workflow, WorkflowSnapshot::Execute { execution }
            if execution.record.phase == crate::plan::PlanPhase::Checking)
        {
            let store = SqliteStore::read_only(&self.data_root)?;
            let exchange = store
                .latest_exchange(&self.session_id)?
                .context("execution checks require a completed source exchange")?;
            anyhow::ensure!(exchange.is_finished(), "source exchange is not finalized");
            return Ok(TaskChange::ResumeWithChecks(crate::task::TaskTurnId {
                exchange_ordinal: exchange.ordinal,
                turn_ordinal: exchange.turn.len(),
            }));
        }
        if matches!(&task.workflow,WorkflowSnapshot::Plan { plan } if matches!(plan.artifact.state,PlanState::AwaitingInput | PlanState::AwaitingReview))
            && request.message.is_none()
            && request.continuation.is_none()
        {
            return Ok(TaskChange::Resume);
        }
        let mut instruction = match &task.workflow {
            WorkflowSnapshot::Goal { goal } => format!(
                "Resume working toward this goal: {}\nContinue until it is complete. Use goal_complete or goal_blocked to report terminal state.",
                goal.objective
            ),
            WorkflowSnapshot::Plan { plan }
                if plan.artifact.state == PlanState::ConfiguringChecks =>
            {
                PlanPrompt::checks_setup(&plan.artifact.request)
            }
            WorkflowSnapshot::Plan { plan } => {
                let document = self
                    .plan_file
                    .read_working_document(&self.session_id, &plan.artifact.id)?;
                PlanPrompt::with_active_plan(PlanPrompt::draft(&plan.artifact.request), &document)
            }
            WorkflowSnapshot::Execute { execution } => {
                let mut resumed = execution.clone();
                crate::task::SessionTaskController::resume_execution(&mut resumed);
                self.execution_continuation_prompt(&resumed)?
            }
        };
        for text in request.message.iter().chain(request.continuation.iter()) {
            instruction.push_str("\n\n");
            instruction.push_str(text);
        }
        Ok(TaskChange::ResumeWithEntry(instruction))
    }

    pub(super) fn execution_continuation_prompt(
        &self,
        execution: &crate::task::ExecutionSnapshot,
    ) -> Result<String> {
        let store = SqliteStore::read_only(&self.data_root)?;
        let plan = store
            .load_plan(&execution.record.plan_id)?
            .context("accepted plan is missing")?;
        let document = self.plan_file.read_submitted_document(
            &self.session_id,
            &plan.id,
            execution.record.revision,
        )?;
        Ok(PlanPrompt::with_manual_changes(
            PlanPrompt::execution(
                &execution.record,
                PlanExecutionPromptKind::Continue,
                &document.execution_json(execution.record.phase)?,
            ),
            &plan.manual_changes,
        ))
    }

    fn prepare_continuation(
        &self,
        current: &Task,
        completed: Option<&crate::task::TaskTurnId>,
        now_ms: i64,
    ) -> Result<TaskChange> {
        let store = SqliteStore::read_only(&self.data_root)?;
        let exchange = store
            .latest_exchange(&self.session_id)?
            .context("task has no completed exchange")?;
        anyhow::ensure!(exchange.is_finished(), "task exchange has not completed");
        let completed = completed.cloned().unwrap_or(crate::task::TaskTurnId {
            exchange_ordinal: exchange.ordinal,
            turn_ordinal: exchange.turn.len(),
        });
        anyhow::ensure!(
            current.entry_accepted() && current.status() == crate::task::TaskStatus::Running,
            "task cannot continue before its entry is accepted or while paused"
        );
        if current.turn.as_ref().is_some_and(|turn| {
            turn.id > completed || turn.id == completed && turn.continuation.is_some()
        }) {
            return Ok(TaskChange::Preserve);
        }
        let work = match &current.workflow {
            WorkflowSnapshot::Execute { execution }
                if execution.record.phase == crate::plan::PlanPhase::Checking =>
            {
                crate::task::TaskWork::Checks
            }
            WorkflowSnapshot::Execute { execution } => crate::task::TaskWork::Provider {
                instruction: self.execution_continuation_prompt(execution)?,
            },
            WorkflowSnapshot::Goal { goal } => crate::task::TaskWork::Provider {
                instruction: format!("Continue working toward this goal: {}", goal.objective),
            },
            WorkflowSnapshot::Plan { .. } => {
                anyhow::bail!("planning continuation is owned by its terminal turn")
            }
        };
        let mut task = current.draft();
        task.updated_at_ms = now_ms;
        task.turn = Some(crate::task::TaskTurn {
            id: completed,
            continuation: Some(crate::task::TaskContinuation {
                id: crate::task::TaskInputId::fresh(),
                reason: "The active task requires another turn".into(),
                delivery: crate::task::TaskInputDelivery::Pending,
                work,
            }),
        });
        Ok(TaskChange::Update(Box::new(Task::try_from(task)?)))
    }
    pub(crate) fn prepare_plan_task(
        &self,
        request: PlanTaskRequest,
        now_ms: i64,
    ) -> Result<crate::task::Task> {
        let prepared = match request {
            PlanTaskRequest::New { objective } => {
                self.plan_file
                    .prepare_entry(&self.session_id, objective, None, now_ms)?
            }
            PlanTaskRequest::Attach { plan_id } => {
                let store = SqliteStore::read_only(&self.data_root)?;
                let record = store
                    .load_plan(&plan_id)?
                    .context("selected plan is missing")?;
                anyhow::ensure!(
                    record.session_id == self.session_id,
                    "plan belongs to another conversation"
                );
                anyhow::ensure!(
                    record.accepted_revision.is_none(),
                    "accepted plan must be forked before editing"
                );
                let mut task = crate::task::TaskSnapshot::plan(record, now_ms);
                if let crate::task::WorkflowSnapshot::Plan { plan } = &mut task.workflow
                    && !matches!(
                        plan.artifact.state,
                        PlanState::AwaitingInput | PlanState::AwaitingReview
                    )
                {
                    if plan.artifact.state == PlanState::Failed {
                        PlanStateMachine::apply(
                            &mut plan.artifact,
                            PlanEvent::RetryRequested,
                            now_ms,
                        )?;
                        plan.artifact.generation.reset();
                    }
                    let instruction = if plan.artifact.state == PlanState::ConfiguringChecks {
                        PlanPrompt::checks_setup(&plan.artifact.request)
                    } else {
                        let document = self
                            .plan_file
                            .read_working_document(&self.session_id, &plan.artifact.id)?;
                        PlanPrompt::with_active_plan(
                            PlanPrompt::draft(&plan.artifact.request),
                            &document,
                        )
                    };
                    task.request_entry(instruction, now_ms)?;
                }
                return Task::try_from(task);
            }
            PlanTaskRequest::Fork { plan_id } => {
                let store = SqliteStore::read_only(&self.data_root)?;
                let source = store
                    .load_plan(&plan_id)?
                    .context("source plan is missing")?;
                anyhow::ensure!(
                    source.session_id == self.session_id,
                    "source plan belongs to another conversation"
                );
                let mut document = self
                    .plan_file
                    .read_working_document(&self.session_id, &plan_id)?;
                document.plan_id = Uuid::new_v4().to_string();
                document.version = 1;
                let objective = format!(
                    "Fork {}. Assess this saved design against the current repository and create a new plan. Original objective: {}",
                    source.title, source.request
                );
                document.prompt = objective.clone();
                if let Some(design) = document.design.as_mut() {
                    let session = store
                        .load_session(&self.session_id)?
                        .context("session is missing")?;
                    let fresh =
                        crate::plan::DeclarationDesign::open(Path::new(&session.workspace))?;
                    design.baseline = fresh.baseline;
                    design.baseline_calls = fresh.baseline_calls;
                }
                self.plan_file
                    .prepare_entry(&self.session_id, objective, Some(document), now_ms)?
            }
            PlanTaskRequest::Replan { plan_id, revision } => {
                let store = SqliteStore::read_only(&self.data_root)?;
                let source = store
                    .load_plan(&plan_id)?
                    .context("source plan not found")?;
                let owner = store
                    .load_session(&source.session_id)?
                    .context("source session not found")?;
                let session = store
                    .load_session(&self.session_id)?
                    .context("session is missing")?;
                anyhow::ensure!(
                    owner.workspace == session.workspace,
                    "source plan belongs to another workspace"
                );
                anyhow::ensure!(
                    revision > 0 && revision <= source.model_revision,
                    "source revision is unavailable"
                );
                let mut document = self
                    .plan_file
                    .capture_revision_source(&source.session_id, &source.id, revision)?
                    .into_data()
                    .revision
                    .into_parts()
                    .0;
                document.plan_id = Uuid::new_v4().to_string();
                document.version = 1;
                let request = format!(
                    "Replan {} from revision {revision}. Reassess this plan against the current workspace, preserve useful decisions, and ask about necessary changes. Original request: {}",
                    source.title, source.request
                );
                document.prompt = request.clone();
                self.plan_file
                    .prepare_entry(&self.session_id, request, Some(document), now_ms)?
            }
        };
        let mut task = crate::task::TaskSnapshot::plan(prepared.record, now_ms);
        task.request_entry(prepared.instruction, now_ms)?;
        Task::try_from(task)
    }
}
