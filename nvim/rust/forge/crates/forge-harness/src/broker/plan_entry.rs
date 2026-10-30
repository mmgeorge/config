use super::*;
use crate::task::command::{ResumeRequest, SessionAction};
use crate::task::{ActiveTask, TaskChange, TaskWorkflow};

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
        if let Some(previous) = self
            .store
            .load_task_operation(&self.session.id, &command.operation_id)?
        {
            anyhow::ensure!(
                previous.action == payload,
                "operation ID was reused for a different action"
            );
            let response = match command.response {
                crate::task::command::CommandResponse::Snapshot => {
                    serde_json::to_value(self.snapshot()?)?
                }
                crate::task::command::CommandResponse::Receipt => previous.view(),
            };
            return Ok((response, Vec::new()));
        }
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
        let receipt = crate::task::TaskOperation {
            revision: 0,
            id: command.operation_id,
            session_id: self.session.id.clone(),
            action: payload,
            permission_selection: None,
            state: "completed".into(),
            error: None,
            exchange_id: None,
            created_at_ms: now_ms,
            resume_after_transition: false,
        };
        self.task_controller.command_with_effects(
            &mut self.store,
            &receipt,
            current.as_ref().map(|task| &task.revision),
            prepared.change,
            prepared.effects,
            now_ms,
        )?;
        self.session.active_task = self.task_controller.current()?;
        let snapshot = serde_json::to_value(self.snapshot()?)?;
        let events = event_name
            .map(|name| {
                self.event(
                    name,
                    if name == "plan_acceptance_started" {
                        snapshot["active_elicitation"].clone()
                    } else {
                        json!({"plan":plan,"lifecycle":lifecycle})
                    },
                )
            })
            .transpose()?
            .into_iter()
            .collect();
        let response = match command.response {
            crate::task::command::CommandResponse::Snapshot => snapshot,
            crate::task::command::CommandResponse::Receipt => self
                .store
                .load_task_operation(&self.session.id, &receipt.id)?
                .context("task receipt is missing")?
                .view(),
        };
        Ok((response, events))
    }
}

impl BrokerObserver {
    pub(crate) fn prepare_task_change(
        &self,
        action: &SessionAction,
        current: Option<&ActiveTask>,
        goal_max_turns: u32,
        now_ms: i64,
    ) -> Result<PreparedTaskChange> {
        let effects = crate::storage::TaskEffects::default();
        let change = match action {
            SessionAction::PlanAccept { enabled } => TaskChange::SetPlanAcceptance(*enabled),
            SessionAction::NewGoal { text } => {
                let mut task = ActiveTask::goal(
                    text.clone(),
                    crate::session::continuation::ContinuationBudget::new(goal_max_turns, 2),
                    self.native_goal_supported()?,
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
            SessionAction::Execute { .. } => {
                let (task, effects) =
                    self.prepare_execution(&action.to_params()?, goal_max_turns, now_ms)?;
                return Ok(PreparedTaskChange {
                    change: TaskChange::Install(Box::new(task)),
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
            SessionAction::CompleteAcceptance { plan_id, digest } => {
                let task = current.context("session has no active task")?;
                let TaskWorkflow::Plan { plan } = &task.workflow else {
                    anyhow::bail!("acceptance requires the current planning task")
                };
                let acceptance = plan
                    .artifact
                    .acceptance
                    .as_ref()
                    .context("active plan has no acceptance state")?;
                anyhow::ensure!(
                    &plan.artifact.id == plan_id && &acceptance.review_digest == digest,
                    "plan acceptance changed before completion"
                );
                let (task,effects)=self.prepare_execution(&json!({"plan_id":plan.artifact.id,"digest":acceptance.review_digest,"saved_source_digest":acceptance.saved_source_digest,"approval_mode":acceptance.approval_mode()?,"comment":acceptance.overall_comment}),goal_max_turns,now_ms)?;
                return Ok(PreparedTaskChange {
                    change: TaskChange::Install(Box::new(task)),
                    effects,
                });
            }
            SessionAction::BeginAcceptance { .. }
            | SessionAction::CancelAcceptance { .. }
            | SessionAction::RequestChanges { .. }
            | SessionAction::CancelPlan { .. } => {
                return self.prepare_review_change(action, current, now_ms);
            }
        };
        Ok(PreparedTaskChange { change, effects })
    }

    fn prepare_resume(&self, task: &ActiveTask, request: &ResumeRequest) -> Result<TaskChange> {
        if matches!(&task.workflow,TaskWorkflow::Plan { plan } if matches!(plan.artifact.state,PlanState::AwaitingInput | PlanState::AwaitingReview))
            && request.message.is_none()
            && request.continuation.is_none()
        {
            return Ok(TaskChange::Resume);
        }
        let mut instruction = match &task.workflow {
            TaskWorkflow::Goal { goal } => format!(
                "Resume working toward this goal: {}\nContinue until it is complete. Use goal_complete or goal_blocked to report terminal state.",
                goal.objective
            ),
            TaskWorkflow::Plan { plan } if plan.artifact.state == PlanState::ConfiguringChecks => {
                PlanPrompt::checks_setup(&plan.artifact.request)
            }
            TaskWorkflow::Plan { plan } => {
                let document = self
                    .plan_file
                    .read_working_document(&self.session_id, &plan.artifact.id)?;
                PlanPrompt::with_active_document(
                    PlanPrompt::draft(&plan.artifact.request),
                    &document.model_json()?,
                )
            }
            TaskWorkflow::Execute { execution } => self.execution_continuation_prompt(execution)?,
        };
        for text in request.message.iter().chain(request.continuation.iter()) {
            instruction.push_str("\n\n");
            instruction.push_str(text);
        }
        Ok(TaskChange::ResumeWithEntry(instruction))
    }

    pub(super) fn execution_continuation_prompt(
        &self,
        execution: &crate::task::ExecutionTask,
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
        current: &ActiveTask,
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
        let (instruction, work) = match &current.workflow {
            TaskWorkflow::Execute { execution }
                if execution.record.phase == crate::plan::PlanPhase::Checking =>
            {
                (String::new(), crate::task::TaskContinuationWork::Checks)
            }
            TaskWorkflow::Execute { execution } => (
                self.execution_continuation_prompt(execution)?,
                crate::task::TaskContinuationWork::Provider,
            ),
            TaskWorkflow::Goal { goal } => (
                format!("Continue working toward this goal: {}", goal.objective),
                crate::task::TaskContinuationWork::Provider,
            ),
            TaskWorkflow::Plan { .. } => {
                anyhow::bail!("planning continuation is owned by its terminal turn")
            }
        };
        let mut task = current.clone();
        task.updated_at_ms = now_ms;
        task.turn = Some(crate::task::TaskTurn {
            id: completed,
            continuation: Some(crate::task::TaskContinuation {
                id: crate::task::TaskInputId::fresh(),
                instruction,
                reason: "The active task requires another turn".into(),
                delivery: crate::task::TaskInputDelivery::Pending,
                work,
            }),
        });
        Ok(TaskChange::Update(Box::new(task)))
    }
    pub(crate) fn prepare_plan_task(
        &self,
        request: PlanTaskRequest,
        now_ms: i64,
    ) -> Result<crate::task::ActiveTask> {
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
                let mut task = crate::task::ActiveTask::plan(record, now_ms);
                if let crate::task::TaskWorkflow::Plan { plan } = &mut task.workflow
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
                        PlanPrompt::with_active_document(
                            PlanPrompt::draft(&plan.artifact.request),
                            &document.model_json()?,
                        )
                    };
                    task.request_entry(instruction, now_ms)?;
                }
                return Ok(task);
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
        let mut task = crate::task::ActiveTask::plan(prepared.record, now_ms);
        task.request_entry(prepared.instruction, now_ms)?;
        Ok(task)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn pause_without_active_task_preserves_chat_state() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        for id in 1..=2 {
            let result = broker
                .dispatch(Request {
                    id,
                    method: "session.command.apply".into(),
                    params: json!({"operation_id":format!("pause-chat-{id}"),"action":"pause"}),
                })
                .await;
            assert!(
                result.response.error().is_none(),
                "{:?}",
                result.response.error()
            );
            assert!(broker.task_controller.current()?.is_none());
        }
        Ok(())
    }

    #[tokio::test]
    async fn direct_acceptance_replay_preserves_replacement_without_provider_execution()
    -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        let replacement = broker
            .task_controller
            .change(
                &mut broker.store,
                None,
                TaskChange::Install(Box::new(ActiveTask::goal(
                    "Replacement task".into(),
                    Default::default(),
                    false,
                    1,
                ))),
                1,
            )?
            .unwrap();
        let receipt = crate::task::TaskOperation {
            revision: 1,
            id: "accepted-review".into(),
            session_id: broker.session.id.clone(),
            action: json!({"action":"complete_acceptance","plan_id":"earlier-plan","digest":"earlier-digest"}),
            permission_selection: None,
            state: "completed".into(),
            error: None,
            exchange_id: None,
            created_at_ms: 1,
            resume_after_transition: false,
        };
        broker.store.admit_task_operation(&receipt)?;
        let before = broker.store.list_exchange(&broker.session.id)?.len();
        let result = broker
            .dispatch(Request {
                id: 77,
                method: "session.question.continue".into(),
                params: json!({"operation_id":"accepted-review"}),
            })
            .await;
        assert!(
            result.response.error().is_none(),
            "{:?}",
            result.response.error()
        );
        assert_eq!(
            broker.task_controller.current()?.unwrap().revision,
            replacement.revision
        );
        assert_eq!(
            broker.store.list_exchange(&broker.session.id)?.len(),
            before
        );
        Ok(())
    }

    #[tokio::test]
    async fn direct_child_steering_cannot_change_root_task() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        let result = broker
            .dispatch(Request {
                id: 78,
                method: "session.turn.steer".into(),
                params: json!({"text":"/goal Change root","target":{"thread_id":"child-thread"}}),
            })
            .await;
        assert!(result.response.error().is_some());
        assert!(broker.task_controller.current()?.is_none());
        Ok(())
    }
}
