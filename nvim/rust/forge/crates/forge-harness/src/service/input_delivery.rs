use super::*;
use crate::input::{InputDelivery, InputOperation};

impl SessionController {
    /// Keep automatic work behind both the required gate and committed answers awaiting delivery.
    pub(super) fn questions_block_work(&self) -> Result<bool> {
        crate::exchange::questions_block_task_work(
            &self.coordinator.submissions.snapshot()?,
            &self.questions,
        )
        .map_err(Into::into)
    }
    /// Submit caller input mutations to the owner and read committed transport projections.
    pub(super) async fn input_request(
        &self,
        request: Request,
        method: RuntimeMethod,
        sink: &MessageSender,
    ) -> Result<()> {
        ensure!(
            !self.input_stopping.load(Ordering::Acquire),
            "Harness session is closing"
        );
        let result = match method {
            RuntimeMethod::SessionTurnSteer => {
                crate::protocol::validate_root_control(request.params.get("target"))?;
                let command =
                    crate::exchange::command::CommandRequest::from_steering(request.params)?;
                match self.execute_command(command, sink).await {
                    Ok(result) => serde_json::to_value(result)?,
                    Err(error) => {
                        sink.send_response(Response::failure(
                            request.id,
                            error.code(),
                            error.to_string(),
                        ))
                        .await?;
                        return Ok(());
                    }
                }
            }
            RuntimeMethod::SessionInputApply => {
                let target = request
                    .params
                    .get("target")
                    .cloned()
                    .map(serde_json::from_value::<crate::exchange::command::InputSubmissionTarget>)
                    .transpose()?;
                let operation: InputOperation = crate::codec::from_value(request.params)?;
                let command = match target {
                    Some(target) => crate::exchange::command::CommandRequest::from_targeted_input(
                        operation, target,
                    )?,
                    None => crate::exchange::command::CommandRequest::from_input(operation)?,
                };
                let id = command.id.clone();
                let outcome = self.execute_command(command, sink).await;
                match outcome {
                    Ok(_) => {}
                    Err(crate::exchange::command::ExecuteCommandError::Failed(
                        crate::exchange::command::CommandFailure::Input(_)
                        | crate::exchange::command::CommandFailure::LegacyInputRejected,
                    )) if self
                        .coordinator
                        .submissions
                        .operation(id.as_str())?
                        .is_some() => {}
                    Err(error) => {
                        sink.send_response(Response::failure(
                            request.id,
                            error.code(),
                            error.to_string(),
                        ))
                        .await?;
                        return Ok(());
                    }
                }
                serde_json::to_value(
                    self.coordinator
                        .submissions
                        .operation(id.as_str())?
                        .context("committed input command has no domain receipt")?,
                )?
            }
            RuntimeMethod::SessionInputOperationGet => serde_json::to_value(
                self.coordinator.submissions.operation(
                    request.params["id"]
                        .as_str()
                        .context("input operation identity is required")?,
                )?,
            )?,
            RuntimeMethod::SessionInputGet => {
                serde_json::to_value(self.coordinator.submissions.snapshot()?)?
            }
            _ => unreachable!("input routing accepts only input commands and queries"),
        };
        sink.send_response(Response::success(request.id, result)?)
            .await?;
        Ok(())
    }

    pub(super) async fn attach_input(self: &Arc<Self>, sink: &MessageSender) -> Result<()> {
        let mut task = self.input_task.lock().await;
        if task.is_some() {
            return Ok(());
        }
        let owner = Arc::downgrade(self);
        let input = self.coordinator.submissions.clone();
        let admission_changed = self.observer.admission.changed.clone();
        let sink = sink.clone();
        *task = Some(tokio::spawn(async move {
            let mut jobs = JoinSet::new();
            loop {
                let notified = input.changed.notified();
                let state_changed = admission_changed.notified();
                let Some(controller) = owner.upgrade() else {
                    break;
                };
                if controller.input_stopping.load(Ordering::Acquire)
                    || controller.coordinator.execution.is_closed()
                {
                    while jobs.join_next().await.is_some() {}
                    break;
                }
                let progress = controller.admit_input(&sink, &mut jobs).await;
                let wait_execution = match progress {
                    Ok(InputProgress::Started) => continue,
                    Ok(InputProgress::WaitExecution) => true,
                    Ok(InputProgress::Waiting) => false,
                    // Shutdown can close the exchange owner between the stopping check
                    // above and this admission attempt. No input remains deliverable once
                    // shutdown began, so that failure ends the loop without an error.
                    Err(_)
                        if controller.input_stopping.load(Ordering::Acquire)
                            || controller.coordinator.execution.is_closed() =>
                    {
                        while jobs.join_next().await.is_some() {}
                        break;
                    }
                    Err(error) => {
                        let _ = sink.send_event(SessionEvent {
                            session_id: controller.catalog_request.read().await.harness_session_id.clone(),
                            event: "provider_event".into(),
                            payload: json!({"kind":"error","text":format!("Input delivery failed: {error:#}")}),
                        }).await;
                        false
                    }
                };
                let admission = controller.coordinator.execution.clone();
                drop(controller);
                tokio::select! {
                    _ = notified => {},
                    _ = state_changed => {},
                    result = jobs.join_next_with_id(), if !jobs.is_empty() => {
                        match result {
                            Some(Ok(_)) => {}
                            Some(Err(error)) => {
                                let Some(controller) = owner.upgrade() else { break };
                                controller.trace.record(controller.observer.session_id(), "local_work.observation_lost", json!({"job_id":error.id().to_string(),"cancelled":error.is_cancelled(),"panicked":error.is_panic()}));
                                let _ = sink.send_event(SessionEvent {
                                    session_id: controller.catalog_request.read().await.harness_session_id.clone(),
                                    event: "provider_event".into(),
                                    payload: json!({"kind":"error","code":"local_work_observation_lost","text":"Queued local work ended before its outcome could be observed"}),
                                }).await;
                            }
                            None => {}
                        }
                    }
                    permit = admission.acquire_owned(), if wait_execution => { drop(permit); }
                }
            }
        }));
        self.coordinator.submissions.changed.notify_one();
        Ok(())
    }

    async fn admit_input(
        self: &Arc<Self>,
        sink: &MessageSender,
        jobs: &mut JoinSet<()>,
    ) -> Result<InputProgress> {
        if let Some(progress) = self.exchange_owner.task_checks_ready(sink.clone()).await? {
            return Ok(match progress {
                crate::exchange::InputReadiness::Started => InputProgress::Started,
                crate::exchange::InputReadiness::WaitExecution => InputProgress::WaitExecution,
                crate::exchange::InputReadiness::Waiting => InputProgress::Waiting,
            });
        }
        if matches!(
            self.recover_automatic_plan_acceptance(sink).await?,
            AutomaticAcceptanceRecovery::Applied
        ) {
            return Ok(InputProgress::Started);
        }
        self.exchange_owner
            .reconcile_task_input(sink.clone())
            .await?;
        let Ok(control) = self.coordinator.control.clone().try_acquire_owned() else {
            return Ok(InputProgress::Waiting);
        };
        let snapshot = self.observer.input_readiness()?;
        let engine = self
            .broker
            .lock()
            .await
            .execution_registry()
            .existing_owner(&crate::provider::ThreadOwner::root(&snapshot.session.id))?;
        let native_active = engine
            .map(|engine| engine.active_invocation_scope())
            .transpose()?
            .flatten()
            .is_some();
        let steering_available = self.coordinator.execution.available_permits() == 0
            && (self.parent_input.is_active() || snapshot.native_steer && native_active);
        let policy = if steering_available {
            crate::exchange::InputQueuePolicy::AllowSteering
        } else {
            crate::exchange::InputQueuePolicy::Ordered
        };
        let (message, command) = match crate::exchange::select_pending_input(
            self.coordinator.submissions.snapshot()?,
            &self.questions,
            policy,
        )? {
            crate::exchange::PendingInput::Empty => {
                drop(control);
                return self.complete_input_checks(sink, jobs).await;
            }
            crate::exchange::PendingInput::Waiting => return Ok(InputProgress::Waiting),
            crate::exchange::PendingInput::WaitExecution => {
                return Ok(InputProgress::WaitExecution);
            }
            crate::exchange::PendingInput::Ready(selection) => selection.into_parts(),
        };
        if let Some(command) = command {
            drop(control);
            let result = super::task_state::apply_command(
                self,
                &snapshot.session.id,
                &command,
                None,
                self.goal_max_turns,
                sink,
            )
            .await;
            let outcome = result.map_err(|error| {
                use crate::exchange::command::{CommandFailure, ExecuteCommandError};
                if let Some(ExecuteCommandError::Failed(failure)) =
                    error.downcast_ref::<ExecuteCommandError>()
                {
                    return failure.clone();
                }
                if let Some(failure) =
                    error.downcast_ref::<crate::exchange::command::TaskCommandFailure>()
                {
                    return CommandFailure::Task(failure.clone());
                }
                CommandFailure::PrerequisiteFailed
            });
            self.exchange_owner
                .settle_local_input(
                    crate::exchange::command::InputRevision {
                        id: crate::exchange::command::InputId::new(message.id.clone())?,
                        revision: message.revision,
                    },
                    outcome,
                    sink.clone(),
                )
                .await?;
            return Ok(InputProgress::Started);
        }
        drop(control);
        let readiness = self.exchange_owner.input_ready(sink.clone()).await?;
        Ok(match readiness {
            crate::exchange::InputReadiness::Started => InputProgress::Started,
            crate::exchange::InputReadiness::WaitExecution => InputProgress::WaitExecution,
            crate::exchange::InputReadiness::Waiting => InputProgress::Waiting,
        })
    }

    /// Schedule an eligible immutable ExecutePlan intent; the owner revalidates approval and dispatch.
    async fn recover_automatic_plan_acceptance(
        &self,
        sink: &MessageSender,
    ) -> Result<AutomaticAcceptanceRecovery> {
        if self.questions_block_work()? {
            return Ok(AutomaticAcceptanceRecovery::Unchanged);
        }
        let Some(task) = self.task_controller.current()? else {
            return Ok(AutomaticAcceptanceRecovery::Unchanged);
        };
        if !matches!(
            task.status(),
            crate::task::TaskStatus::Running | crate::task::TaskStatus::Waiting
        ) {
            return Ok(AutomaticAcceptanceRecovery::Unchanged);
        }
        let snapshot = self.coordinator.submissions.snapshot()?;
        // Recover complete submissions without replaying provider work.
        if !snapshot.suspended
            && !snapshot.messages.iter().any(|message| {
                matches!(message.purpose, crate::input::InputPurpose::User)
                    && !matches!(
                        message.delivery(),
                        InputDelivery::Accepted | InputDelivery::Delivering
                    )
            })
            && crate::task::command::SessionCommand::approved_revision(&task)?.is_some()
        {
            task.permit()
                .validate(self.task_controller.current()?.as_ref())?;
            let current = self
                .task_controller
                .current()?
                .context("planning task is missing")?;
            let Some(command) = crate::task::command::SessionCommand::approved_revision(&current)?
            else {
                return Ok(AutomaticAcceptanceRecovery::Unchanged);
            };
            super::task_state::apply_command(
                self,
                self.observer.session_id(),
                &command,
                None,
                self.goal_max_turns,
                sink,
            )
            .await?;
            return Ok(AutomaticAcceptanceRecovery::Applied);
        }
        Ok(AutomaticAcceptanceRecovery::Unchanged)
    }

    /// Request deferred checks without acquiring execution or lifecycle authority.
    async fn complete_input_checks(
        self: &Arc<Self>,
        sink: &MessageSender,
        jobs: &mut JoinSet<()>,
    ) -> Result<InputProgress> {
        // The retained request owns queue completion until its reply is observed.
        // Input notifications must not enqueue the same obligation repeatedly.
        if !jobs.is_empty()
            || !self.observer.completion_queue.deferred()?
            || self.checks.active()?.is_some()
        {
            return Ok(InputProgress::Waiting);
        }
        let sender = self.observer.check_owner_sender()?;
        let controller = self.clone();
        let sink = sink.clone();
        jobs.spawn(async move {
            match sender.complete().await {
                Ok(outcome) => {
                    for event in outcome.events {
                        if sink.send_event(event).await.is_err() { break; }
                    }
                }
                Err(error) => {
                    let _ = sink.send_event(SessionEvent {
                        session_id: controller.observer.session_id().to_owned(),
                        event: "provider_event".into(),
                        payload: json!({"kind":"error","text":format!("Queue completion checks failed: {error}")}),
                    }).await;
                }
            }
            controller.coordinator.submissions.changed.notify_one();
        });
        Ok(InputProgress::Started)
    }

    /// Read the same revision-scoped authority that the owner revalidates before dispatch.
    #[cfg(test)]
    pub(super) fn task_input_is_current(
        &self,
        purpose: &crate::input::InputPurpose,
    ) -> Result<bool> {
        crate::input::input_purpose_is_current(
            &self.task_controller,
            &self.questions,
            purpose,
            &self.data_root,
            self.observer.session_id(),
        )
        .map_err(Into::into)
    }
}

enum AutomaticAcceptanceRecovery {
    Applied,
    Unchanged,
}

enum InputProgress {
    Started,
    WaitExecution,
    Waiting,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Repeated wakeups retain one completion request until its result is collected.
    #[tokio::test]
    async fn deferred_checks_keep_one_in_flight_request() -> Result<()> {
        let (_root, controller, _exchange) = super::super::question_event_test::fixture().await?;
        let queue = &controller.observer.completion_queue;
        let input = &controller.observer.input;
        input.apply(InputOperation {
            id: "deferred-check-input".into(), control_revision: 0, after_control: None,
            action: crate::input::InputAction::Submit { message: Box::new(serde_json::from_value(json!({
                "id":"pending-check-input", "text":"Queued input", "intent":"queue", "attachments":[]
            }))?) },
        })?;
        assert!(queue.defer(crate::checks::CheckTiming::AfterQueue, "source")?);
        input.apply(InputOperation {
            id: "remove-check-input".into(),
            control_revision: 0,
            after_control: None,
            action: crate::input::InputAction::Remove {
                id: "pending-check-input".into(),
                revision: 1,
            },
        })?;
        let (sink, _output) = forge_protocol::outbound::channel();
        let mut jobs = JoinSet::new();
        let (release, held) = tokio::sync::oneshot::channel::<()>();
        jobs.spawn(async move {
            let _ = held.await;
        });
        for _ in 0..3 {
            assert!(matches!(
                controller.complete_input_checks(&sink, &mut jobs).await?,
                InputProgress::Waiting
            ));
            assert_eq!(jobs.len(), 1);
        }
        release.send(()).unwrap();
        jobs.join_next().await.unwrap()?;
        controller.finish_exchange_owner().await?;
        Ok(())
    }

    /// Stop readiness after fatal admission closure without draining or dispatching queued input.
    #[tokio::test]
    async fn closed_execution_admission_stops_input_readiness_without_dispatch() -> Result<()> {
        let root = tempfile::tempdir()?;
        let controller =
            SessionController::new(HarnessBroker::initialize(serde_json::from_value(json!({
                "workspace":root.path(),"data_root":root.path().join("data"),
                "client_id":"closed-readiness","provider":{"kind":"mock","command":[]},
            }))?)?)
            .await?;
        controller
            .coordinator
            .submissions
            .apply(serde_json::from_value(json!({
                "id":"queued-before-failure", "action":{"kind":"submit", "message":{
                    "id":"preserved-input","text":"Keep this input for recovery","intent":"queue",
                }},
            }))?)?;
        let before = controller.coordinator.submissions.snapshot()?;
        controller.coordinator.execution.close();
        let (messages, _output) = forge_protocol::outbound::channel();
        controller.attach_input(&messages).await?;
        let task = controller.input_task.lock().await.take().unwrap();
        task.await?;
        let after = controller.coordinator.submissions.snapshot()?;
        assert_eq!(after.revision, before.revision);
        assert_eq!(after.messages.len(), 1);
        assert_eq!(after.messages[0].id, "preserved-input");
        assert_eq!(after.messages[0].delivery(), InputDelivery::Queued);
        assert!(controller.coordinator.execution.is_closed());
        controller.finish_exchange_owner().await?;
        Ok(())
    }
}
