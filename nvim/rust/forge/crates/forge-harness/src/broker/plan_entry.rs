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
                PlanPrompt::with_active_document(
                    PlanPrompt::draft(&plan.artifact.request),
                    &document.model_json()?,
                )
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
                        PlanPrompt::with_active_document(
                            PlanPrompt::draft(&plan.artifact.request),
                            &document.model_json()?,
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

#[cfg(test)]
mod tests {
    use super::*;

    /// Queue active phase guidance from the same workflow state that resume commits.
    #[tokio::test]
    async fn resumed_execution_entry_uses_committed_phase_instead_of_stopped_state() -> Result<()> {
        for (stopped, phase) in [
            (PlanExecutionState::Stalled, crate::plan::PlanPhase::Resolve),
            (PlanExecutionState::Paused, crate::plan::PlanPhase::Resolve),
            (PlanExecutionState::Paused, crate::plan::PlanPhase::Checking),
        ] {
            let workspace = super::super::test::repository();
            let data = tempfile::tempdir()?;
            let mut broker =
                super::super::test::planning_question_broker(workspace.path(), data.path(), false);
            let mut document = crate::plan::test_fixture("resume-plan", "Resume fixture");
            let mut design = crate::plan::DeclarationDesign::default();
            design.document.objective = "Resume existing work".into();
            design.document.background = "A blocked execution".into();
            design.document.design = "Keep the accepted implementation".into();
            document.design = Some(design);
            let mut plan = broker
                .plan_file
                .prepare_entry(
                    &broker.session.id,
                    "Resume fixture".into(),
                    Some(document),
                    1,
                )?
                .record;
            broker
                .plan_file
                .submit_document_revision(&broker.session.id, "resume-plan", 1, 1)?;
            plan.state = PlanState::Accepted;
            plan.model_revision = 1;
            plan.accepted_revision = Some(1);
            plan.submitted_version = Some(plan.document_version);
            broker.store.save_plan(&plan)?;
            let exchange: crate::exchange::Exchange = serde_json::from_value(json!({
                "id":"resume-source", "session_id":broker.session.id,
                "agent_id":broker.session.primary_agent_id, "ordinal":1,
                "prompt":"Implementation", "state":"complete",
                "created_at_ms":1, "completed_at_ms":2,
                "attributed_matches_checkpoint":false, "node_list":[]
            }))?;
            broker.store.save_exchange(&exchange)?;
            let record: PlanExecutionRecord = serde_json::from_value(json!({
                "id":"resume-execution", "session_id":broker.session.id,
                "plan_id":"resume-plan", "state":stopped, "phase":phase, "baseline_checkpoint":"captured",
                "original_revision":1, "revision":1, "generation":7,
                "findings":["Operator gate failed"], "created_at_ms":1,
                "lifecycle":[], "progress":crate::plan::execution::SemanticProgress::default(),
                "verification":[], "revision_history":[], "resume_after_review":false,
                "observed_source":{}, "observed_document_version":7, "observed_check":[],
            }))?;
            let mut task = crate::task::TaskSnapshot::execute(
                "Resume fixture".into(),
                record,
                crate::session::continuation::ContinuationBudget::new(20, 2),
                1,
            );
            let WorkflowSnapshot::Execute { execution } = &mut task.workflow else {
                unreachable!()
            };
            execution.continuation.observe(false);
            let before = execution.clone();
            let mut projected = execution.clone();
            crate::task::SessionTaskController::resume_execution(&mut projected);
            assert_eq!(execution.record.state, stopped);
            assert_eq!(projected.record.state, PlanExecutionState::Active);
            assert_eq!(projected.record.id, before.record.id);
            assert_eq!(projected.record.generation, before.record.generation);
            assert_eq!(projected.record.created_at_ms, before.record.created_at_ms);
            assert_eq!(projected.continuation.turn_count, 0);
            let installed = broker
                .task_controller
                .change(
                    &mut broker.store,
                    None,
                    TaskChange::Install(Box::new(Task::try_from(task)?)),
                    1,
                )?
                .unwrap();
            let prepared = broker.observer.prepare_task_change(
                &SessionAction::Resume {
                    request: ResumeRequest::default(),
                },
                Some(&installed),
                20,
                2,
            )?;
            let resumed = broker
                .task_controller
                .change(
                    &mut broker.store,
                    Some(&installed.revision),
                    prepared.change,
                    2,
                )?
                .unwrap();
            assert_ne!(resumed.revision, installed.revision);
            if phase == crate::plan::PlanPhase::Checking {
                assert!(
                    resumed.entry.is_none(),
                    "Check must not create a provider entry"
                );
                let work = resumed
                    .turn
                    .as_ref()
                    .and_then(|turn| turn.continuation.as_ref())
                    .context("resumed Check must reserve local work")?;
                assert_eq!(work.work, crate::task::TaskWork::Checks);
                continue;
            }
            let entry = resumed.entry.as_ref().context("resume entry missing")?;
            assert!(
                entry
                    .instruction
                    .contains("Resolve the collected verification findings")
            );
            assert!(entry.instruction.contains("plan_phase_done"));
            assert!(
                !entry
                    .instruction
                    .contains("End this turn, and do not continue automatically")
            );
            assert!(!entry.instruction.contains("Execution is Stalled"));
            assert!(!entry.instruction.contains("Execution is Paused"));
            let WorkflowSnapshot::Execute { execution } = &resumed.workflow else {
                unreachable!()
            };
            assert_eq!(execution.record.state, projected.record.state);
            assert_eq!(execution.record.generation, 7);
            assert_eq!(execution.record.findings, before.record.findings);
            assert_eq!(execution.continuation, projected.continuation);
        }
        Ok(())
    }

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
    async fn uncoordinated_acceptance_preserves_replacement_without_provider_execution()
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
                TaskChange::Install(Box::new(Task::goal(
                    "Replacement task".into(),
                    Default::default(),
                    1,
                ))),
                1,
            )?
            .unwrap();
        let before = broker.store.list_exchange(&broker.session.id)?.len();
        let result = broker
            .dispatch(Request {
                id: 77,
                method: "session.question.continue".into(),
                params: json!({"operation_id":"accepted-review"}),
            })
            .await;
        assert!(
            result.response.error().is_some(),
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
