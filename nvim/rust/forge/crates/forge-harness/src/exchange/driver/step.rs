//! Run the driver's per-step phases between intake and wake selection.

use super::*;

/// Report how the driver continues after a step phase.
pub(super) enum StepGate {
    /// Continue with the rest of this step.
    Proceed,
    /// Start the next step immediately.
    Restart,
    /// Shutdown is complete; the driver exits.
    Exit,
}

impl ExchangeDriver {
    /// Run collection effects, exit once shutdown is complete, or restart an idle step early.
    pub(super) fn step_finish_check(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        closed: bool,
        ready_wake: &Option<DriverWake>,
    ) -> StepGate {
        let DriverEnv {
            trace,
            session_id,
            services,
            ..
        } = env;
        let admission = self.admission(services, state, closed);
        let DriverState {
            foreground,
            deferred_publications,
            retained_cancellation,
            failed_work,
            steering_ready,
            automatic_action,
            task_check_completion,
            ..
        } = state;
        // Collection and terminal evidence must progress even when an ordinary effect phase is deferred.
        if let Some(runtime) = foreground.as_mut() {
            runtime.run_effects(ForegroundEffectGate::CollectionOnly);
        }
        if ready_wake.is_none() && admission.finish_shutdown {
            if let Some(FailedWork::Turn(runtime)) = failed_work.as_ref() {
                trace.record(
                    session_id,
                    "exchange.failed_work.retained",
                    serde_json::json!({"work_id":runtime.id,"update_step":step}),
                );
            }
            self.commands.close(trace, session_id, step);
            if !deferred_publications.updates.is_empty() {
                trace.record(
                    session_id,
                    "exchange.publication.shutdown_uncommitted",
                    serde_json::json!({
                        "remaining_updates":deferred_publications.updates.len(),"update_step":step,
                    }),
                );
            }
            drop(retained_cancellation.take());
            return StepGate::Exit;
        }
        if ready_wake.is_none()
            && foreground.is_none()
            && steering_ready.is_none()
            && admission.start_command
        {
            return StepGate::Restart;
        }
        if ready_wake.is_none()
            && (matches!(
                automatic_action,
                Some(crate::broker::OwnedCheckProgress::Ready(_))
            ) || task_check_completion.is_some())
        {
            return StepGate::Restart;
        }
        StepGate::Proceed
    }

    /// Start the next queued turn, then run the active turn's effects under the current gate.
    #[allow(clippy::too_many_arguments)]
    pub(super) async fn turns_advance(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        closed: bool,
        foreground_at_entry: bool,
    ) {
        let DriverEnv {
            broker,
            trace,
            session_id,
            services,
            ..
        } = env;
        if !foreground_at_entry
            && self.admission(services, state, closed).start_turn
            && let Some(prepared) = state.pending.pop_front()
        {
            trace.record(
                session_id,
                "exchange.turn.started",
                serde_json::json!({"work_id":prepared.id,"update_step":step}),
            );
            state.foreground = Some(if closed {
                ActiveTurn::stopping(prepared, services.publications.clone())
            } else {
                ActiveTurn::new(prepared, services.publications.clone())
            });
        }
        // Read admission again, because the turn started above changes its facts.
        let run_turn_effects = self.admission(services, state, closed).run_turn_effects;
        if let Some(runtime) = state.foreground.as_mut() {
            let gate = if run_turn_effects {
                ForegroundEffectGate::Open
            } else {
                ForegroundEffectGate::CollectionOnly
            };
            runtime.dispatch_live_effects(broker, gate).await;
            runtime.run_effects(gate);
        }
    }

    /// Readmit a deferred settings change once the availability it waited on has changed.
    #[allow(clippy::too_many_arguments)]
    pub(super) async fn deferred_configuration_retry(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        closed: bool,
        broker_deferred: &mut bool,
    ) {
        let DriverEnv {
            broker,
            trace,
            session_id,
            services,
            ..
        } = env;
        if self
            .admission(services, state, closed)
            .retry_deferred_configuration
            && self
                .deferred_configuration
                .as_ref()
                .is_some_and(|deferred| {
                    services
                        .observer
                        .session_events()
                        .configuration_availability()
                        .is_some_and(|receipt| Some(receipt) != deferred.after)
                })
        {
            match broker.try_lock() {
                Ok(broker) => {
                    let retry = broker.pending_configuration_retry(
                        &self
                            .deferred_configuration
                            .as_ref()
                            .expect("deferred configuration selected above")
                            .patch,
                    );
                    match retry {
                        Ok(crate::broker::PendingConfigurationRetry::Blocked) => {}
                        Ok(crate::broker::PendingConfigurationRetry::Obsolete) => {
                            self.deferred_configuration = None;
                        }
                        Ok(crate::broker::PendingConfigurationRetry::Ready(patch)) => {
                            self.deferred_configuration = None;
                            let (reply, _response) = oneshot::channel();
                            self.commands.admit(
                                CommandAdmission {
                                    request: CommandRequest {
                                        id: CommandId::new(uuid::Uuid::new_v4().to_string())
                                            .expect("generated retry command identity is nonempty"),
                                        command: Command::ConfigureSession(
                                            crate::exchange::command::ConfigurationChange::Settings(
                                                *patch,
                                            ),
                                        ),
                                        input_control: InputControl::AtExecution,
                                    },
                                    reply,
                                    task_admission: None,
                                },
                                None,
                                trace,
                                session_id,
                                step,
                            );
                        }
                        Err(error) => {
                            trace.record(
                                session_id,
                                "exchange.configuration.retry_failed",
                                serde_json::json!({"update_step":step,"reason":error.to_string()}),
                            );
                            self.commands.retention_failed(trace, session_id, step);
                        }
                    }
                }
                Err(_) => *broker_deferred = true,
            }
        }
    }

    /// Admit the next waiting question tool when its gate is open.
    #[allow(clippy::too_many_arguments)]
    pub(super) async fn question_tools_admit(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        closed: bool,
    ) {
        let DriverEnv {
            trace,
            session_id,
            services,
            ..
        } = env;
        let question_gate_open = self.admission(services, state, closed).open_question_tools;
        let DriverState { foreground, .. } = state;
        if let Some(index) = self.question_tools.iter().position(|request| {
            !question_gate_open || !request.waits_for_capacity(&self.question_waits)
        }) {
            let request = self
                .question_tools
                .remove(index)
                .expect("selected runnable question callback exists");
            let gate_open = question_gate_open;
            let live_scope = foreground
                .as_ref()
                .and_then(|runtime| match &runtime.transaction {
                    TurnTransaction::Running { transaction, .. }
                        if transaction.collection.is_none()
                            && !transaction.failure_cleanup_active =>
                    {
                        runtime
                            .transaction
                            .dispatch_observer()
                            .map(|(scope, _)| scope)
                    }
                    TurnTransaction::Running { .. }
                    | TurnTransaction::Admitting(_)
                    | TurnTransaction::Finalizing { .. }
                    | TurnTransaction::Done(_)
                    | TurnTransaction::Transitioning => None,
                });
            let admission = if gate_open {
                crate::exchange::question_tool::QuestionToolAdmission::Open {
                    scope: live_scope.as_ref(),
                }
            } else {
                crate::exchange::question_tool::QuestionToolAdmission::Closed
            };
            if request.apply(
                admission,
                services,
                &mut self.question_waits,
                trace,
                session_id,
                step,
            ) {
                self.question_inputs.request();
                services.input.changed.notify_one();
            }
        }
    }

    /// Start the next queued command when no other work holds command admission.
    #[allow(clippy::too_many_arguments)]
    pub(super) async fn command_start(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        closed: bool,
        command_at_entry: bool,
        event_status: &TurnStatus,
    ) {
        let DriverEnv {
            broker,
            trace,
            session_id,
            services,
            ..
        } = env;
        let start_command = self.admission(services, state, closed).start_command;
        let DriverState {
            foreground,
            command,
            ..
        } = state;
        // Start only one command per update step, including immediate rejection.
        if !command_at_entry
            && start_command
            && let Some(queued) = self.commands.start(trace, session_id, step)
        {
            // Execution never reopens after a lost stop or restart, so cancel cannot wait for it.
            if matches!(self.execution_state, ExecutionState::Failed)
                && matches!(queued.record.request.command, Command::CancelExchange(_))
            {
                self.commands.finish(
                    queued,
                    Err(CommandFailure::ProviderFailed),
                    trace,
                    session_id,
                    step,
                );
            } else {
                let record = crate::exchange::core_log::CoreRecord::OperationStarted {
                    operation: crate::exchange::core_log::CoreOperation::Command {
                        command: queued.record.request.id.clone(),
                        kind: (&queued.record.request.command).into(),
                    },
                };
                if record_owner_core(&mut self.commands, &record, trace, session_id, step).is_ok() {
                    if matches!(
                        &queued.record.request.command,
                        Command::ConfigureSession(
                            crate::exchange::command::ConfigurationChange::Settings(_)
                        )
                    ) {
                        self.deferred_configuration = None;
                        self.configuration_attempt = Some(ConfigurationAttempt {
                            command: queued.record.request.id.clone(),
                            availability: services
                                .observer
                                .session_events()
                                .configuration_availability(),
                        });
                    }
                    let mut active = ActiveCommand::start(queued, services);
                    let pin = active
                        .pin_cancellation_target(
                            event_status,
                            foreground.as_mut().map(|runtime| &mut runtime.transaction),
                            broker,
                            services,
                        )
                        .await;
                    match pin {
                        Ok((events, input)) => {
                            let id = active.queued_mut().record.request.id.clone();
                            publish_control(events, input.as_ref(), env.context(step), &id);
                        }
                        Err(_) => {
                            self.execution_fail(services);
                            self.commands.retention_failed(trace, session_id, step);
                        }
                    }
                    *command = Some(active);
                } else {
                    let mut queued = queued;
                    self.commands
                        .lose_observation(&mut queued, trace, session_id, step);
                }
            }
        }
    }

    /// Advance a provider restart once the owner is idle.
    #[allow(clippy::too_many_arguments)]
    pub(super) async fn restart_advance(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        closed: bool,
    ) {
        let DriverEnv {
            broker,
            trace,
            session_id,
            services,
            ..
        } = env;
        // A closed observation stream ends its provider generation, and a failed one reports its
        // failure to every later reader. Restart now rather than on the next prompt, so queued
        // commands and later turns never read the failed stream.
        let admission = self.admission(services, state, closed);
        if admission.request_restart
            && self
                .observation_recovery
                .request_after_failure(services.observer.session_events())
        {
            trace.record(
                session_id,
                "exchange.restart.requested",
                serde_json::json!({"update_step":step,"kind":"observation_closed"}),
            );
        }
        if self.observation_recovery.active() {
            let progress = if self.commands.storage_unavailable() {
                self.observation_recovery.fail_storage();
                self.execution_fail(services);
                Ok(false)
            } else if admission.restart_idle || self.observation_recovery.started() {
                let mut state = broker.lock().await;
                self.observation_recovery
                    .advance(admission.restart_idle, &mut state, services)
            } else {
                Ok(false)
            };
            match progress {
                // The request finished without restarting. For a failed turn this means another
                // restart already replaced its generation, so execution may reopen.
                Ok(true) if self.execution_state == ExecutionState::Restarting => {
                    self.restart_succeeded(env, state, step)
                }
                Ok(true) => {}
                Ok(false)
                    if self.observation_recovery.started()
                        && !self.commands.storage_unavailable() =>
                {
                    self.execution_restart_begin(services)
                }
                Ok(false) => {}
                Err(error) => self.restart_failed(env, step, error),
            }
        }
    }

    /// Advance question input, publication delivery, reconnect, and the steering preview.
    pub(super) async fn deliveries_advance(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        closed: bool,
    ) {
        let DriverEnv {
            broker,
            trace,
            session_id,
            services,
            ..
        } = env;
        let steering_status = self.turn_status(state);
        let DriverState {
            command_messages,
            publication_delivery,
            steering_ready,
            pending_wake,
            ..
        } = state;
        if !closed && !self.commands.storage_unavailable() {
            match self
                .question_inputs
                .update(services, command_messages.as_ref())
            {
                Ok(Some(progress)) => {
                    trace.record(session_id, "exchange.question_input.applied", serde_json::json!({
                    "question_revision":progress.revision,"admitted":progress.admitted,"remaining":progress.remaining,"update_step":step,
                }));
                }
                Ok(None) => {}
                Err(error) => {
                    trace.record(
                        session_id,
                        "exchange.question_input.failed",
                        serde_json::json!({
                            "error_code":error.code(),"reason":error.to_string(),"update_step":step,
                        }),
                    );
                }
            }
        }
        let publication_transport = self
            .reconnect_requests
            .is_empty()
            .then_some(command_messages.as_ref())
            .flatten();
        match publication_delivery.update(&services.publications, publication_transport) {
            Ok(Some(progress)) => {
                use crate::exchange::publication::PublicationProgress;
                let (sequence, delivery) = match progress {
                    PublicationProgress::Offering(sequence) => (sequence, "offering"),
                    PublicationProgress::Admitted(sequence) => (sequence, "admitted"),
                    PublicationProgress::Uncertain(sequence) => (sequence, "uncertain"),
                };
                trace.record(session_id, "exchange.publication.delivery", serde_json::json!({
                    "publication_sequence":sequence.get(),"delivery":delivery,"update_step":step,
                }));
            }
            Ok(None) => {}
            Err(error) => {
                trace.record(
                    session_id,
                    "exchange.publication.delivery_failed",
                    serde_json::json!({
                        "update_step":step,"reason":error.to_string(),
                    }),
                );
            }
        }
        self.advance_reconnect(broker, env.context(step)).await;
        if steering_ready.is_none()
            && !matches!(
                &pending_wake,
                Some(DriverWake::InputDelivery(
                    InputDeliveryTarget::Steering(_),
                    _
                ))
            )
        {
            *steering_ready = next_steering(&mut self.steering, steering_status).now_or_never();
        }
        if closed {
            self.observation_recovery.close_pending();
        }
    }

    /// Stop admitting work after owner storage fails, and fail the work it can no longer commit.
    ///
    /// The active command loses its observation, the active turn is collected as failed, queued
    /// turns are rejected, and queued question tools close once the inbox or storage is gone.
    pub(super) async fn storage_failure_apply(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        closed: bool,
    ) {
        let DriverEnv {
            broker,
            trace,
            session_id,
            services,
            ..
        } = env;
        let DriverState {
            pending,
            foreground,
            command,
            tasks:
                RunningTasks {
                    failed_turns: failed_foregrounds,
                    input_stops: input_cleanup,
                    ..
                },
            ..
        } = state;
        if self.commands.storage_unavailable()
            && let Some(command) = command.as_mut()
        {
            self.commands
                .lose_observation(command.queued_mut(), trace, session_id, step);
        }
        if self.commands.storage_unavailable()
            && let Some(runtime) = foreground.take()
        {
            self.execution_stop_begin(services);
            let (provider, permissions) = {
                let broker = broker.lock().await;
                (broker.provider_handle(), broker.permission_coordinator())
            };
            failed_foregrounds.spawn(collect_failed_foreground(
                runtime,
                anyhow::anyhow!("foreground state cannot be committed after owner storage failure"),
                provider,
                permissions,
                session_id.clone(),
                trace.clone(),
            ));
        }
        if self.commands.storage_unavailable() {
            services.execution.close();
            // The closed execution gate already prevents new foreground work. Collection is
            // lost only while delivery work that may own native execution is still retained;
            // an idle owner stays collectable so shutdown can confirm quiescence.
            if matches!(self.execution_state, ExecutionState::Ready)
                && (self.input_delivery.is_some()
                    || !self.steering.is_empty()
                    || !input_cleanup.is_empty())
            {
                self.execution_fail(services);
            }
            queued_turns_reject(
                pending,
                "foreground admission is unavailable after owner storage failure",
                env.context(step),
            );
        }
        if (closed || self.commands.storage_unavailable()) && !self.question_tools.is_empty() {
            for _ in 0..OWNER_INTAKE_BUDGET {
                let Some(request) = self.question_tools.pop_front() else {
                    break;
                };
                request.apply(
                    crate::exchange::question_tool::QuestionToolAdmission::Closed,
                    services,
                    &mut self.question_waits,
                    trace,
                    session_id,
                    step,
                );
            }
        }
    }

    /// Retry publication storage recovery when due, then flush deferred publications.
    pub(super) async fn publications_flush(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
    ) {
        let DriverEnv {
            trace,
            session_id,
            services,
            ..
        } = env;
        let DriverState {
            command_messages,
            publication_recovery_at,
            deferred_publications,
            ..
        } = state;
        if services.publications.storage_failed() {
            self.commands.retention_failed(trace, session_id, step);
        }
        if services.publications.has_recovery()
            && publication_recovery_at
                .is_none_or(|deadline| tokio::time::Instant::now() >= deadline)
        {
            if let Err(error) = services.publications.retry_recovery() {
                trace.record(
                    session_id,
                    "exchange.publication.recovery_failed",
                    serde_json::json!({"reason":error.to_string(),"update_step":step}),
                );
            }
            *publication_recovery_at = services
                .publications
                .has_recovery()
                .then(|| tokio::time::Instant::now() + std::time::Duration::from_millis(250));
        }
        if !deferred_publications.updates.is_empty() {
            publish_updates(
                deferred_publications,
                &mut self.commands,
                Vec::new(),
                command_messages.as_ref(),
                env.context(step),
            )
            .await;
        }
    }

    /// Receive a bounded batch of owner events, and record when the owner inbox closes.
    ///
    /// A step that continues its event phase, or holds a wake for the next pass, receives
    /// nothing, so ready events finish before new commands queue behind them.
    pub(super) async fn events_receive(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        continuing_event_phase: bool,
        closed: &mut bool,
    ) {
        let DriverEnv { broker, .. } = env;
        let DriverState {
            pending,
            foreground,
            command,
            command_messages,
            pending_wake,
            ..
        } = state;
        let intake = if continuing_event_phase || pending_wake.is_some() {
            0
        } else {
            OWNER_INTAKE_BUDGET.min(OWNER_QUEUE_CAPACITY - pending.len())
        };
        for _ in 0..intake {
            match self.receiver.try_recv() {
                Ok(event) => {
                    self.receive(
                        event,
                        pending,
                        OwnerIntake {
                            active: command.as_mut(),
                            steering: SteeringAdmission::from(foreground.as_ref()),
                        },
                        command_messages,
                        broker,
                        env.context(step),
                    )
                    .await
                }
                Err(mpsc::error::TryRecvError::Empty) => break,
                Err(mpsc::error::TryRecvError::Disconnected) => {
                    *closed = true;
                    break;
                }
            }
        }
    }

    /// Publish the owner's activity, start shutdown once the inbox closes, and retire question waits.
    pub(super) fn step_begin(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        closed: bool,
    ) {
        let DriverEnv {
            trace,
            session_id,
            services,
            ..
        } = env;
        let DriverState {
            pending,
            foreground,
            command,
            command_messages,
            shutdown,
            check_runs,
            pending_automatic_checks,
            automatic_checks,
            automatic_action,
            automatic_repair,
            task_verification,
            tasks:
                RunningTasks {
                    failed_turns: failed_foregrounds,
                    stops: running_stops,
                    rollbacks: rollback_cleanup,
                    input_stops: input_cleanup,
                    ..
                },
            ..
        } = state;
        let activity = match self.execution_state {
            ExecutionState::Failed => crate::exchange::ActivityPhase::Unavailable,
            ExecutionState::Stopping | ExecutionState::Restarting => {
                crate::exchange::ActivityPhase::Working
            }
            ExecutionState::Ready
                if foreground.is_some()
                    || command.is_some()
                    || !pending.is_empty()
                    || self.commands.has_pending()
                    || self.input_delivery.is_some()
                    || !self.steering.is_empty()
                    || !input_cleanup.is_empty()
                    || !failed_foregrounds.is_empty()
                    || !running_stops.is_empty()
                    || !rollback_cleanup.is_empty()
                    || automatic_checks.is_some()
                    || automatic_repair.is_some()
                    || task_verification.is_some()
                    || !check_runs.is_empty() =>
            {
                crate::exchange::ActivityPhase::Working
            }
            ExecutionState::Ready => crate::exchange::ActivityPhase::Idle,
        };
        if let Err(error) = publish_activity(
            activity,
            command_messages.as_ref(),
            services,
            trace,
            session_id,
            step,
        ) {
            self.commands.retention_failed(trace, session_id, step);
            services.execution.close();
            trace.record(
                session_id,
                "exchange.activity.publication_failed",
                serde_json::json!({"reason":error.to_string(),"update_step":step}),
            );
        }
        if closed && command.is_none() && shutdown.is_none() && !self.observation_recovery.active()
        {
            *shutdown = Some(crate::exchange::shutdown::OwnerShutdown::start(services));
        }
        if closed {
            self.question_waits.close();
            for reply in self.task_check_requests.drain(..) {
                let _ = reply.send(Err(InputOfferError::OwnerClosed));
            }
            let _ = check_runs.cancel_all();
            if let Some(checks) = automatic_checks.as_mut() {
                let _ = checks.cancel();
            }
            if let Some(checks) = task_verification.as_mut() {
                let _ = checks.run.cancel();
            }
            *automatic_action = None;
            for request in pending_automatic_checks.drain(..) {
                let _ = request
                    .reply
                    .send(Err(crate::checks::owner::CheckOwnerFailure::Unavailable));
            }
        }
        if let Err(error) = self
            .question_waits
            .update(services, trace, session_id, step)
        {
            if error.error().requires_storage_fence() {
                self.commands.retention_failed(trace, session_id, step);
                services.execution.close();
            }
            trace.record(session_id, "exchange.question_wait.retirement_failed", serde_json::json!({
                "wait_id":error.wait_id().to_string(),"error_code":error.error().code(),"reason":error.to_string(),"update_step":step,
            }));
        }
    }
}
