//! Apply command wakes for the exchange driver loop.

use super::super::*;

impl ExchangeDriver {
    /// Apply one active-command fact.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn command_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        foreground_status: &TurnStatus,
        closed: &mut bool,
        wake: CommandEvent,
    ) -> WakeEnd {
        let DriverEnv {
            broker,
            trace,
            session_id,
            services,
            ..
        } = env;
        let task_delivery = self.admission(services, state, *closed).task_delivery;
        let DriverState {
            pending,
            foreground,
            command,
            command_messages,
            deferred_publications,
            tasks:
                RunningTasks {
                    stops: running_stops,
                    rollbacks: rollback_cleanup,
                    internal_completions,
                    ..
                },
            ..
        } = state;
        match wake {
            CommandEvent::Input(prepared) => {
                let Some(ActiveCommand::Input(run)) = command.take() else {
                    unreachable!("input evidence belongs to the active input command")
                };
                self.finish_input(run, prepared, command_messages.as_ref(), env.context(step));
            }
            CommandEvent::Configuration => {
                let Some(ActiveCommand::Configuration(queued)) = command.take() else {
                    unreachable!(
                        "configuration readiness belongs to the active configuration command"
                    )
                };
                if self.commands.storage_unavailable() {
                    let mut queued = queued;
                    self.commands
                        .lose_observation(&mut queued, trace, session_id, step);
                } else {
                    match crate::exchange::configuration::apply_configuration(&queued, services) {
                        Ok(outcome) => self.commands.finish(
                            queued,
                            Ok(CommandOutcome::Configured(outcome)),
                            trace,
                            session_id,
                            step,
                        ),
                        Err(
                            crate::exchange::configuration::ConfigurationCommitError::Rejected(
                                reason,
                            ),
                        ) => {
                            self.commands.finish(
                                queued,
                                Err(CommandFailure::ConfigurationRejected(reason)),
                                trace,
                                session_id,
                                step,
                            );
                        }
                        Err(error) => {
                            trace.record(session_id, "exchange.configuration.commit_failed", serde_json::json!({
                                    "command_id":queued.record.request.id.as_str(),"update_step":step,"reason":error.to_string(),
                                }));
                            self.commands.uncommitted(
                                queued,
                                CommandJournalError::DomainCommit,
                                trace,
                                session_id,
                                step,
                            );
                        }
                    }
                }
            }
            CommandEvent::Agent(event) => {
                let Some(ActiveCommand::Agent(run)) = command.take() else {
                    unreachable!("child coordinator facts belong to the active agent command")
                };
                let id = run.command.record.request.id.clone();
                match event {
                    crate::exchange::agent_command::AgentCommandEvent::Completed(outcome) => {
                        self.commands.finish(
                            run.command,
                            outcome.map(CommandOutcome::Agent),
                            trace,
                            session_id,
                            step,
                        );
                    }
                    crate::exchange::agent_command::AgentCommandEvent::ObservationDeadline => {
                        trace.record(
                            session_id,
                            "exchange.command.observation_deadline",
                            serde_json::json!({
                                "command_id":id.as_str(),"sequence":run.command.record.sequence,
                                "source":"agent","update_step":step,
                            }),
                        );
                        *command = Some(ActiveCommand::Agent(run));
                    }
                }
            }
            CommandEvent::Rollback(event) => {
                let Some(ActiveCommand::Rollback(mut run)) = command.take() else {
                    unreachable!("rollback facts belong to the retained rollback command")
                };
                let id = run.command.record.request.id.clone();
                trace.record(
                    session_id,
                    "exchange.command.event",
                    serde_json::json!({
                        "command_id":id.as_str(),"sequence":run.command.record.sequence,
                        "source":"rollback","event":event.name(),"update_step":step,
                    }),
                );
                let was_initializing = run.initializing();
                let (progress, catalog) = {
                    let mut broker = broker.lock().await;
                    let progress = run.update(
                        event,
                        foreground_status.clone(),
                        &mut broker,
                        services,
                        !self.commands.storage_unavailable(),
                    );
                    let catalog = if matches!(
                        &progress,
                        Ok(RollbackProgress::Finished { outcome: Ok(_), .. })
                    ) {
                        *services
                            .provider
                            .write()
                            .unwrap_or_else(std::sync::PoisonError::into_inner) =
                            broker.provider_handle();
                        Some(broker.provider_catalog_request())
                    } else {
                        None
                    };
                    (progress, catalog)
                };
                collection_signals_move(foreground.as_mut(), || run.take_collection_signal());
                if let Some(catalog) = catalog {
                    match catalog {
                        Ok(catalog) => {
                            services.catalog_publish(catalog).await;
                        }
                        Err(error) => {
                            self.execution_fail(services);
                            trace.record(session_id, "exchange.rollback.catalog_failed", serde_json::json!({
                                    "command_id":id.as_str(),"reason":error.to_string(),"update_step":step,
                                }));
                        }
                    }
                }
                match progress {
                    Ok(RollbackProgress::Waiting { events, input }) => {
                        if run.initializing() {
                            self.execution_stop_begin(services);
                        }
                        publish_control(events, input.as_deref(), env.context(step), &id);
                        *command = Some(ActiveCommand::Rollback(run));
                    }
                    Ok(RollbackProgress::Finished { outcome, events }) => {
                        if was_initializing
                            && outcome.is_ok()
                            && matches!(self.execution_state, ExecutionState::Stopping)
                        {
                            self.execution_reopen(services);
                        }
                        if matches!(
                            &outcome,
                            Err(crate::exchange::rollback::RollbackFailure::RecoveryRequired)
                        ) {
                            self.execution_stop_begin(services);
                            publish_control(events, None, env.context(step), &id);
                            let (queued, recovery) = run.into_recovery();
                            self.commands.finish(
                                queued,
                                outcome
                                    .map(CommandOutcome::Rollback)
                                    .map_err(CommandFailure::Rollback),
                                trace,
                                session_id,
                                step,
                            );
                            rollback_cleanup.spawn(async move { (id, recovery.collect().await) });
                            return WakeEnd::RestartStep;
                        }
                        publish_control(events, None, env.context(step), &id);
                        self.commands.finish(
                            run.command,
                            outcome
                                .map(CommandOutcome::Rollback)
                                .map_err(CommandFailure::Rollback),
                            trace,
                            session_id,
                            step,
                        );
                    }
                    Err(error) => {
                        trace.record(session_id, "exchange.command.effect_failed", serde_json::json!({
                                "command_id":id.as_str(),"code":error.code(),"reason":error.diagnostic(),"update_step":step,
                            }));
                        let (queued, recovery) = run.into_recovery();
                        self.execution_stop_begin(services);
                        self.stopped_command_finish(env, step, queued, error);
                        rollback_cleanup.spawn(async move { (id, recovery.collect().await) });
                    }
                }
            }
            CommandEvent::Task(event) => {
                let Some(ActiveCommand::Task(mut run)) = command.take() else {
                    unreachable!("task facts belong to the retained active task command")
                };
                let id = run.command.record.request.id.clone();
                let source_plan = run.captured_plan();
                let task = run.captured_task();
                trace.record(session_id, "exchange.event.read", serde_json::json!({
                        "source":"task_command","command_id":id.as_str(),"sequence":run.command.record.sequence,
                        "task_id":task.map(|task| task.id().as_str()),"task_revision":task.map(|task| task.revision().as_str()),
                        "source_plan_id":source_plan.as_ref().map(|source| source.id().as_str()),
                        "prepared_task_id":event.prepared_task().map(|task| task.id.as_str()),
                        "prepared_plan_id":event.prepared_task().and_then(|task| match &task.workflow {
                            crate::task::WorkflowSnapshot::Plan { plan } => Some(plan.artifact.id.as_str()),
                            crate::task::WorkflowSnapshot::Goal { .. } | crate::task::WorkflowSnapshot::Execute { .. } => None,
                        }),
                        "event":event.name(),"update_step":step,
                    }));
                let was_preparing = run.preparing();
                let progress = match self.input_receipts.observe_acceptance(services) {
                    Ok(()) => run.update(
                        event,
                        foreground_status.clone(),
                        &mut *broker.lock().await,
                        services,
                        !self.commands.storage_unavailable(),
                        task_delivery == crate::exchange::task_command::TaskDeliveryGate::Open,
                    ),
                    Err(error) => {
                        run.consume_completed_worker(&event);
                        Err(OperationFailure::Domain(error.into()))
                    }
                };
                if !was_preparing && run.preparing() {
                    trace.record(session_id, "exchange.command.effect_requested", serde_json::json!({
                            "command_id":id.as_str(),"effect":"task_creation_preparation","update_step":step,
                        }));
                }
                collection_signals_move(foreground.as_mut(), || run.take_collection_signal());
                match progress {
                    Ok(TaskCommandProgress::DeliverInput(start)) => {
                        let start = *start;
                        let output = command_messages.clone();
                        let input = InputRevision {
                            id: crate::exchange::command::InputId::new(start.message.id.clone())
                                .expect("task input identity was validated before its claim"),
                            revision: start.message.revision,
                        };
                        services.cancellation.arm();
                        self.input_delivery = Some(InputDeliveryRun::start(
                            start.message,
                            start.capabilities.execution,
                            start.capabilities.control,
                            output.clone(),
                            services,
                        ));
                        // Retain native preparation before notifications can fail or observers disappear.
                        for update in start.updates {
                            publish_input_observation(
                                update,
                                output.as_ref(),
                                services,
                                trace,
                                session_id,
                                step,
                            );
                        }
                        if let Ok(snapshot) = services.input.snapshot() {
                            publish_input_observation(
                                crate::input::InputUpdate {
                                    revision: snapshot.revision,
                                    suspended: snapshot.suspended,
                                    control_revision: snapshot.control_revision,
                                    snapshot: Some(snapshot),
                                    operation_id: Some(id.as_str().into()),
                                    upsert: Vec::new(),
                                    removed: Vec::new(),
                                    error: None,
                                    error_code: None,
                                    prepend: false,
                                    retracted: None,
                                },
                                output.as_ref(),
                                services,
                                trace,
                                session_id,
                                step,
                            );
                        }
                        trace.record(session_id, "exchange.command.effect_requested", serde_json::json!({
                                "command_id":id.as_str(),"effect":"task_input_delivery","input_id":input.id.as_str(),
                                "input_revision":input.revision,"update_step":step,
                            }));
                        *command = Some(ActiveCommand::Task(run));
                    }
                    Ok(TaskCommandProgress::Waiting { events, input }) => {
                        publish_control(events, input.as_ref(), env.context(step), &id);
                        *command = Some(ActiveCommand::Task(run));
                    }
                    Ok(TaskCommandProgress::Finished { outcome, events }) => {
                        // A failed creation cannot become an implicit readiness retry after releasing its command slot.
                        if outcome.is_err()
                            && matches!(
                                run.checkpoint(),
                                Some(
                                    crate::exchange::command::TaskCommandCheckpoint::InputPending { .. }
                                )
                            )
                        {
                            match services.input.suspend(true, Some(id.as_str())) {
                                Ok(update) => publish_input_observation(
                                    update,
                                    command_messages.as_ref(),
                                    services,
                                    trace,
                                    session_id,
                                    step,
                                ),
                                Err(_) => {
                                    self.commands.uncommitted(
                                        run.command,
                                        CommandJournalError::DomainCommit,
                                        trace,
                                        session_id,
                                        step,
                                    );
                                    return WakeEnd::RestartStep;
                                }
                            }
                        }
                        publish_task_state(
                            command_messages.as_ref(),
                            services,
                            trace,
                            session_id,
                            &id,
                            step,
                        );
                        publish_control(events, None, env.context(step), &id);
                        match self.commands.committed(&id) {
                            Ok(Some(record)) if record.completion().is_some() => self
                                .commands
                                .complete(run.command, record, trace, session_id, step),
                            Ok(Some(_)) | Ok(None) => self.commands.finish(
                                run.command,
                                outcome.map(CommandOutcome::Task),
                                trace,
                                session_id,
                                step,
                            ),
                            Err(error) => self.commands.uncommitted(
                                run.command,
                                error,
                                trace,
                                session_id,
                                step,
                            ),
                        }
                    }
                    Err(OperationFailure::Transition(TransitionFailure::Rejected(failure))) => {
                        let (queued, retained) = run.into_retained();
                        drop(retained);
                        self.commands
                            .finish(queued, Err(failure), trace, session_id, step);
                    }
                    Err(error) => {
                        trace.record(session_id, "exchange.command.effect_failed", serde_json::json!({"command_id":id.as_str(),"code":error.code(),"reason":error.diagnostic(),"update_step":step}));
                        let checkpoint = run.checkpoint().cloned();
                        let preparation_overdue =
                            run.preparing() && matches!(&error, OperationFailure::JobDeadline);
                        let (queued, retained) = run.into_retained();
                        self.execution_stop_begin(services);
                        if preparation_overdue {
                            self.commands.finish(
                                    queued,
                                    Err(CommandFailure::Task(
                                        crate::exchange::command::TaskCommandFailure::PreparationTimedOut,
                                    )),
                                    trace,
                                    session_id,
                                    step,
                                );
                        } else if let Some(committed) =
                            checkpoint.filter(|_| matches!(&error, OperationFailure::DispatchLost))
                        {
                            self.commands.finish(queued, Err(CommandFailure::TaskSynchronization {
                                    committed, failure: crate::exchange::command::TaskSynchronizationFailure::DispatchLost,
                                }), trace, session_id, step);
                        } else {
                            self.stopped_command_finish(env, step, queued, error);
                        }
                        stop_collect_spawn(running_stops, id, retained, services);
                    }
                }
                services.input.changed.notify_one();
            }
            CommandEvent::NativeCommand(event) => {
                let Some(ActiveCommand::NativeCommand(mut run)) = command.take() else {
                    unreachable!("native policy evidence belongs to the active policy command")
                };
                let id = run.command.record.request.id.clone();
                trace.record(session_id, "exchange.command.event", serde_json::json!({
                        "command_id":id.as_str(),"sequence":run.command.record.sequence,"event":event.name(),"update_step":step,"compaction_id":run.compaction_id().map(|id| id.as_str()),
                    }));
                let mut progress = match self.input_receipts.observe_acceptance(services) {
                    Ok(()) => run.update(
                        event,
                        foreground_status.clone(),
                        &mut *broker.lock().await,
                        services,
                        !self.commands.storage_unavailable(),
                    ),
                    Err(error) => {
                        match event {
                            crate::exchange::native_command::NativeCommandEvent::Transition(
                                event,
                            ) => run.retain_transition_event(event),
                            event => run.consume_completed_worker(&event),
                        }
                        Err(OperationFailure::Domain(error.into()))
                    }
                };
                if matches!(
                    &progress,
                    Ok(NativeCommandProgress::Finished(CommandOutcome::Configured(
                        _
                    )))
                ) {
                    let record = crate::exchange::core_log::CoreRecord::TransitionCommitted {
                        command: Some(id.clone()),
                        invocation: None,
                        kind: crate::exchange::core_log::CoreTransitionKind::ConfigurationCommitted,
                        outcome: crate::exchange::core_log::CoreOutcome::Succeeded,
                    };
                    if let Err(error) =
                        record_owner_core(&mut self.commands, &record, trace, session_id, step)
                    {
                        progress = Err(OperationFailure::Transition(error));
                    }
                }
                collection_signals_move(foreground.as_mut(), || run.take_collection_signal());
                match progress {
                    Ok(NativeCommandProgress::Waiting { events, updates }) => {
                        publish_control(events, None, env.context(step), &id);
                        publish_updates(
                            deferred_publications,
                            &mut self.commands,
                            updates,
                            command_messages.as_ref(),
                            env.context(step),
                        )
                        .await;
                        *command = Some(ActiveCommand::NativeCommand(run));
                    }
                    Ok(NativeCommandProgress::Replacement(replacement)) => {
                        let messages = command_messages.clone();
                        let (completion, response) = oneshot::channel::<Result<DispatchResult>>();
                        let id = replacement.id;
                        let output = messages.clone();
                        let publications = services.publications.clone();
                        internal_completions.spawn(async move {
                            let outcome = async {
                                if let Ok(Ok(result)) = response.await {
                                    for event in result.event {
                                        publications.publish(event, output.as_ref())?;
                                    }
                                }
                                Ok::<(), crate::exchange::publication::PublicationError>(())
                            }
                            .await;
                            (id, outcome)
                        });
                        pending.push_back(QueuedTurn {
                            id,
                            transaction: replacement.transaction,
                            messages,
                            initial: None,
                            completion: ForegroundCompletion::Caller(completion),
                            updates: Some(replacement.updates),
                            execution: Some(replacement.execution),
                        });
                        *command = Some(ActiveCommand::NativeCommand(run));
                    }
                    Ok(NativeCommandProgress::Finished(outcome)) => {
                        let run = *run;
                        self.commands
                            .finish(run.command, Ok(outcome), trace, session_id, step);
                    }
                    Err(error) => {
                        trace.record(session_id, "exchange.configuration.effect_failed", serde_json::json!({
                                "command_id":id.as_str(),"code":error.code(),"reason":error.diagnostic(),"update_step":step,
                            }));
                        if let OperationFailure::Validation(error) = error {
                            if error
                                .downcast_ref::<crate::provider::ProviderError>()
                                .is_some_and(configuration_waits_for_child)
                                && let Command::ConfigureSession(
                                    crate::exchange::command::ConfigurationChange::Settings(patch),
                                ) = &run.command.record.request.command
                                && let Some(attempt) = self
                                    .configuration_attempt
                                    .take()
                                    .filter(|attempt| attempt.command == id)
                            {
                                self.deferred_configuration = Some(DeferredConfiguration {
                                    patch: patch.clone(),
                                    after: attempt.availability,
                                });
                            }

                            let reason = error
                                    .downcast_ref::<ConfigurationRejection>()
                                    .copied()
                                    .or_else(|| {
                                        error.downcast_ref::<crate::provider::ProviderError>().map(|error| {
                                            ConfigurationRejection::Provider(
                                                crate::provider::lifecycle_error::diagnostic(
                                                    forge_provider_api::ProviderOperation::ThreadConfigure,
                                                    error,
                                                ).kind(),
                                            )
                                        })
                                    })
                                    .unwrap_or(ConfigurationRejection::Unavailable);
                            trace.record(session_id, "exchange.configuration.rejected", serde_json::json!({
                                    "command_id":id.as_str(),"update_step":step,"reason":error.to_string(),
                                }));
                            self.commands.finish(
                                run.command,
                                Err(CommandFailure::ConfigurationRejected(reason)),
                                trace,
                                session_id,
                                step,
                            );
                        } else if let OperationFailure::Transition(TransitionFailure::Rejected(
                            failure,
                        )) = error
                        {
                            let (queued, _) = run.into_retained();
                            self.commands
                                .finish(queued, Err(failure), trace, session_id, step);
                        } else {
                            self.execution_stop_begin(services);
                            let (queued, retained) = run.into_retained();
                            let command_id = queued.record.request.id.clone();
                            match error {
                                OperationFailure::Compaction { failure, cause } => {
                                    trace.record(session_id, "exchange.compaction.failed", serde_json::json!({
                                            "command_id":command_id.as_str(),"update_step":step,"code":failure.code(),"reason":cause.to_string(),
                                        }));
                                    self.commands.finish(
                                        queued,
                                        Err(CommandFailure::Compaction(failure)),
                                        trace,
                                        session_id,
                                        step,
                                    );
                                }
                                OperationFailure::Reconciliation(error) => {
                                    let diagnostic = crate::provider::lifecycle_error::diagnostic(
                                        forge_provider_api::ProviderOperation::ThreadConfigure,
                                        &error,
                                    );
                                    trace.record(session_id, "exchange.configuration.reconciliation_failed", serde_json::json!({
                                            "command_id":command_id.as_str(),"update_step":step,"kind":diagnostic.kind(),
                                        }));
                                    self.commands.finish(
                                        queued,
                                        Err(CommandFailure::ConfigurationReconcileFailed {
                                            kind: diagnostic.kind(),
                                        }),
                                        trace,
                                        session_id,
                                        step,
                                    );
                                }
                                OperationFailure::InputReconciliation(error) => {
                                    trace.record(session_id, "exchange.input.reconciliation_failed", serde_json::json!({"command_id":command_id.as_str(),"update_step":step,"reason":error.to_string()}));
                                    self.commands.finish(
                                        queued,
                                        Err(CommandFailure::StopFailed),
                                        trace,
                                        session_id,
                                        step,
                                    );
                                }
                                OperationFailure::ExecutionUnavailable => self.commands.finish(
                                    queued,
                                    Err(CommandFailure::OwnershipUnavailable),
                                    trace,
                                    session_id,
                                    step,
                                ),
                                OperationFailure::JobDeadline => self.commands.finish(
                                    queued,
                                    Err(CommandFailure::ConfigurationTimedOut),
                                    trace,
                                    session_id,
                                    step,
                                ),
                                OperationFailure::JobObservationLost(error) => {
                                    trace.record(session_id, "exchange.configuration.worker_lost", serde_json::json!({
                                            "command_id":command_id.as_str(),"update_step":step,"reason":error.to_string(),
                                        }));
                                    self.commands.finish(
                                        queued,
                                        Err(CommandFailure::StopFailed),
                                        trace,
                                        session_id,
                                        step,
                                    );
                                }
                                OperationFailure::Transition(TransitionFailure::Overdue) => {
                                    self.commands.finish(
                                        queued,
                                        Err(CommandFailure::StopTimedOut),
                                        trace,
                                        session_id,
                                        step,
                                    )
                                }
                                OperationFailure::Transition(TransitionFailure::Uncertain) => {
                                    self.commands.finish(
                                        queued,
                                        Err(CommandFailure::StopFailed),
                                        trace,
                                        session_id,
                                        step,
                                    )
                                }
                                OperationFailure::DispatchLost => self.commands.finish(
                                    queued,
                                    Err(CommandFailure::DispatchLost),
                                    trace,
                                    session_id,
                                    step,
                                ),
                                OperationFailure::Domain(error)
                                | OperationFailure::Transition(TransitionFailure::Domain(error)) => {
                                    trace.record(session_id, "exchange.configuration.commit_failed", serde_json::json!({
                                            "command_id":command_id.as_str(),"update_step":step,"reason":error.to_string(),
                                        }));
                                    self.commands.uncommitted(
                                        queued,
                                        CommandJournalError::DomainCommit,
                                        trace,
                                        session_id,
                                        step,
                                    );
                                }
                                OperationFailure::StorageUncommitted => self.commands.uncommitted(
                                    queued,
                                    CommandJournalError::DomainCommit,
                                    trace,
                                    session_id,
                                    step,
                                ),
                                OperationFailure::Transition(TransitionFailure::Rejected(_)) => {
                                    unreachable!("safe rejection is handled separately")
                                }
                                OperationFailure::Validation(_) => unreachable!(
                                    "preflight rejection is handled without fencing execution"
                                ),
                            }
                            stop_collect_spawn(running_stops, command_id, retained, services);
                        }
                    }
                }
            }
            CommandEvent::Cancel(event) => {
                let Some(ActiveCommand::Cancel {
                    command: queued,
                    mut transition,
                }) = command.take()
                else {
                    unreachable!("cancellation evidence belongs to the active cancellation command")
                };
                let command_id = queued.record.request.id.clone();
                trace.record(
                    session_id,
                    "exchange.command.event",
                    serde_json::json!({
                        "command_id":command_id.as_str(),"sequence":queued.record.sequence,
                        "update_step":step,"event":event.name(),
                    }),
                );
                let core_transition = event.core_transition();
                let mut progress = match self.input_receipts.observe_acceptance(services) {
                    Ok(()) if !self.commands.storage_unavailable() => transition.update(
                        event,
                        foreground_status.clone(),
                        &mut *broker.lock().await,
                        services,
                    ),
                    Ok(()) => {
                        transition.retain_uncommitted(event);
                        Err(TransitionFailure::Domain(anyhow::anyhow!(
                            "owner storage is unavailable"
                        )))
                    }
                    Err(error) => {
                        transition.retain_uncommitted(event);
                        Err(TransitionFailure::Domain(error.into()))
                    }
                };
                if let Some((kind, outcome)) = core_transition
                    && let Err(error) = record_owner_core(
                        &mut self.commands,
                        &transition.core_transition(kind, outcome),
                        trace,
                        session_id,
                        step,
                    )
                {
                    progress = Err(error);
                }
                if let Ok(TransitionProgress::Finished { .. }) = &progress {
                    let record = transition.core_transition(
                        crate::exchange::core_log::CoreTransitionKind::SettlementCompleted,
                        crate::exchange::core_log::CoreOutcome::Succeeded,
                    );
                    if let Err(error) =
                        record_owner_core(&mut self.commands, &record, trace, session_id, step)
                    {
                        progress = Err(error);
                    }
                }
                collection_signals_move(foreground.as_mut(), || {
                    transition.take_collection_signal()
                });
                match progress {
                    Ok(TransitionProgress::Waiting { events, input }) => {
                        publish_control(events, input.as_deref(), env.context(step), &command_id);
                        *command = Some(ActiveCommand::Cancel {
                            command: queued,
                            transition,
                        });
                    }
                    Ok(TransitionProgress::Finished {
                        outcome: TransitionOutcome::Cancelled(outcome),
                        events,
                    }) => {
                        publish_control(events, None, env.context(step), &command_id);
                        if self.commands.storage_unavailable() {
                            let mut queued = queued;
                            self.commands
                                .lose_observation(&mut queued, trace, session_id, step);
                        } else {
                            self.commands.finish(
                                queued,
                                Ok(CommandOutcome::Cancelled(outcome)),
                                trace,
                                session_id,
                                step,
                            );
                        }
                    }
                    Ok(TransitionProgress::Finished {
                        outcome: TransitionOutcome::Reconfigured { .. },
                        ..
                    }) => {
                        unreachable!(
                            "explicit cancellation cannot finish as internal reconfiguration"
                        )
                    }
                    Err(TransitionFailure::Rejected(error)) => {
                        self.commands
                            .finish(queued, Err(error), trace, session_id, step);
                    }
                    Err(failure) => {
                        self.execution_stop_begin(services);
                        let retained = transition.into_retained();
                        let command_id = queued.record.request.id.clone();
                        if let TransitionFailure::Domain(error) = &failure {
                            trace.record(session_id, "exchange.command.commit_failed", serde_json::json!({
                                    "command_id":command_id.as_str(),"update_step":step,"reason":error.to_string(),
                                }));
                        }
                        self.stopped_command_finish(
                            env,
                            step,
                            queued,
                            OperationFailure::Transition(failure),
                        );
                        stop_collect_spawn(running_stops, command_id, retained, services);
                    }
                }
            }
        }
        WakeEnd::Finished
    }

    /// Finish a command whose work had to be stopped, under the outcome its failure selects.
    pub(in crate::exchange::driver) fn stopped_command_finish(
        &mut self,
        env: &DriverEnv,
        step: u64,
        queued: crate::exchange::queue::QueuedCommand,
        failure: OperationFailure,
    ) {
        let DriverEnv {
            trace, session_id, ..
        } = env;
        match failure.into_stopped_command() {
            crate::exchange::failure::StoppedCommand::Failed(failure) => {
                self.commands
                    .finish(queued, Err(failure), trace, session_id, step)
            }
            crate::exchange::failure::StoppedCommand::Uncommitted => self.commands.uncommitted(
                queued,
                CommandJournalError::DomainCommit,
                trace,
                session_id,
                step,
            ),
        }
    }
}
