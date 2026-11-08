//! Apply turn wakes for the exchange driver loop.

use super::super::*;

impl ExchangeDriver {
    /// Apply the outcome of the active turn's dispatch observation.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn dispatched_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        _foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: (
            uuid::Uuid,
            Result<
                forge_provider_api::submission::DispatchConfirmation,
                forge_provider_api::submission::DispatchObservationLost,
            >,
        ),
    ) -> WakeEnd {
        let DriverEnv {
            trace, session_id, ..
        } = env;
        let DriverState {
            foreground,
            command,
            ..
        } = state;
        let (id, outcome) = wake;
        if let Some(runtime) = foreground.as_mut()
            && runtime.id == id
        {
            runtime.observe_handoff(outcome, trace, session_id, step);
            if matches!(runtime.handoff, TurnDispatch::Dispatched(_))
                && let Some(input) = self.input_delivery.as_mut()
            {
                input.observe_dispatch(runtime.id);
            }
            if let Some(command) = command.as_mut() {
                match &runtime.handoff {
                    TurnDispatch::Dispatched(confirmation) => {
                        command.observe_dispatch(runtime.id, confirmation)
                    }
                    TurnDispatch::Lost => command.dispatch_lost(runtime.id),
                    TurnDispatch::NotStarted | TurnDispatch::InFlight { .. } => {}
                }
            }
        } else {
            trace.record(
                session_id,
                "exchange.dispatch.ignored",
                serde_json::json!({
                    "work_id":id,"update_step":step,"reason":"foreground_owner_changed",
                }),
            );
        }
        WakeEnd::Finished
    }

    /// Apply one signal or committed update from the active turn.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn foreground_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        _foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: ForegroundInput,
    ) -> WakeEnd {
        let DriverEnv {
            broker,
            trace,
            session_id,
            services,
            ..
        } = env;
        let DriverState {
            foreground,
            command,
            deferred_publications,
            tasks:
                RunningTasks {
                    failed_turns: failed_foregrounds,
                    ..
                },
            ..
        } = state;
        match wake {
            ForegroundInput::Update(update) => {
                let runtime = foreground
                    .as_ref()
                    .expect("publication belongs to the active foreground");
                trace.record(
                    session_id,
                    "exchange.event.read",
                    serde_json::json!({
                        "work_id":runtime.id,"source":"committed_publication","update_step":step,
                    }),
                );
                publish_updates(
                    deferred_publications,
                    &mut self.commands,
                    vec![*update],
                    runtime.messages.as_ref(),
                    env.context(step),
                )
                .await;
            }
            ForegroundInput::PublicationClosed => {}
            ForegroundInput::Signal(signal, publication) => {
                let runtime = foreground
                    .as_mut()
                    .expect("foreground input belongs to the active runtime");
                trace.record(session_id, "exchange.event.read", serde_json::json!({
                        "work_id":runtime.id,"update_step":step,
                        "source":if publication { "publication_result" } else { "foreground_input" },
                        "signal":signal.diagnostic_name(),
                        "running_jobs":(runtime.jobs.len() + runtime.publication_job.len()),"staged_effects":runtime.staged_effects.iter().map(|job| job.diagnostic_name()).collect::<Vec<_>>(),
                    }));
                runtime.read_handoff(trace, session_id, step);
                if matches!(runtime.handoff, TurnDispatch::Dispatched(_))
                    && let Some(input) = self.input_delivery.as_mut()
                {
                    input.observe_dispatch(runtime.id);
                }
                if let Some(command) = command.as_mut()
                    && let TurnDispatch::Dispatched(confirmation) = &runtime.handoff
                {
                    command.observe_dispatch(runtime.id, confirmation);
                }
                let progress = match self.input_receipts.observe_acceptance(services) {
                    Ok(()) => {
                        runtime
                            .update(
                                broker,
                                *signal,
                                publication,
                                if self.commands.storage_unavailable() {
                                    ForegroundCommit::Unavailable
                                } else {
                                    ForegroundCommit::Certified
                                },
                            )
                            .await
                    }
                    Err(error) => {
                        services.execution.close();
                        self.commands.retention_failed(trace, session_id, step);
                        match *signal {
                            ForegroundSignal::LiveHost(request) => {
                                request.reject(crate::provider::ProviderError::DeliveryUncertain);
                            }
                            signal => runtime.uncommitted_signals.push(signal),
                        }
                        Err(error.into())
                    }
                };
                match progress {
                    Ok(ForegroundProgress::Waiting) => {}
                    Ok(ForegroundProgress::Finished) => {
                        let runtime = foreground
                            .take()
                            .expect("active runtime produced settlement");
                        if let Some(command) = command.as_mut() {
                            command.dispatch_lost(runtime.id);
                        }
                        let ActiveTurn {
                            transaction,
                            completion,
                            id,
                            _execution: execution,
                            ..
                        } = runtime;
                        let (outcome, catalog) = {
                            let mut broker = broker.lock().await;
                            (
                                broker.finish_foreground(transaction),
                                broker.provider_catalog_request(),
                            )
                        };
                        let outcome = match catalog {
                            Ok(catalog) => {
                                *services.catalog.write().await = catalog;
                                outcome
                            }
                            Err(error) => {
                                services.execution.close();
                                Err(error)
                            }
                        };
                        drop(execution);
                        notify(completion, id, outcome, trace, session_id, step);
                        // Readiness answers Waiting while foreground work is active, so input
                        // requeued during this turn needs a wake once the turn releases execution.
                        services.input.changed.notify_one();
                    }
                    Err(error) => {
                        let runtime = foreground
                            .take()
                            .expect("active runtime produced this failure");
                        if let Some(command) = command.as_mut() {
                            command.dispatch_lost(runtime.id);
                        }
                        {
                            // Fence replacement immediately, but keep reading owner inputs while
                            // the retained worker collects native work and already-started effects.
                            self.execution_stop_begin(services);
                            let (provider, permissions) = {
                                let broker = broker.lock().await;
                                (broker.provider_handle(), broker.permission_coordinator())
                            };
                            let session = session_id.clone();
                            failed_foregrounds.spawn(collect_failed_foreground(
                                runtime,
                                error,
                                provider,
                                permissions,
                                session,
                                trace.clone(),
                            ));
                        }
                    }
                }
            }
        }
        WakeEnd::Finished
    }

    /// Apply the collected result of a failed turn.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn failed_foreground_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        _foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: Option<Result<FailedForeground, tokio::task::JoinError>>,
    ) -> WakeEnd {
        let DriverEnv {
            trace,
            session_id,
            services,
            ..
        } = env;
        let DriverState {
            command_messages,
            deferred_publications,
            ..
        } = state;
        match wake {
            Some(Ok(collected)) => {
                let FailedForeground {
                    mut runtime,
                    original,
                    cleanup,
                    catalog,
                    unpublished,
                } = collected;
                publish_updates(
                    deferred_publications,
                    &mut self.commands,
                    unpublished,
                    command_messages.as_ref(),
                    env.context(step),
                )
                .await;
                if let Some(catalog) = catalog {
                    *services.catalog.write().await = *catalog;
                }
                let error = match cleanup {
                    Ok(()) => original,
                    Err(error) => original.context(error),
                };
                let completion =
                    std::mem::replace(&mut runtime.completion, ForegroundCompletion::Notified);
                notify(completion, runtime.id, Err(error), trace, session_id, step);
                self.restart_after_failure(
                    env,
                    state,
                    step,
                    FailedWork::Turn(Box::new(runtime)),
                    "turn_failed",
                );
            }
            // The collection task ended without its result, so the provider state is unknown.
            Some(Err(_)) => {
                self.restart_after_failure(env, state, step, FailedWork::Unknown, "turn_lost");
            }
            None => {}
        }
        WakeEnd::Finished
    }

    /// Record the forwarded response of a turn the driver started internally.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn internal_completed_wake(
        &mut self,
        env: &DriverEnv,
        _state: &mut DriverState,
        step: u64,
        _foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: InternalCompletion,
    ) -> WakeEnd {
        let DriverEnv {
            trace, session_id, ..
        } = env;
        match wake {
            Some(Ok((id, result))) => {
                trace.record(
                    session_id,
                    "exchange.turn.completion_forwarded",
                    serde_json::json!({
                        "work_id":id,"update_step":step,"publication_succeeded":result.is_ok(),
                    }),
                );
            }
            Some(Err(_)) => {
                trace.record(
                    session_id,
                    "exchange.turn.internal_observation_lost",
                    serde_json::json!({"update_step":step}),
                );
            }
            None => {}
        }
        WakeEnd::Finished
    }

    /// Receive one owner event, or record that the owner inbox closed.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn event_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        _foreground_status: &TurnStatus,
        closed: &mut bool,
        wake: Option<OwnerEvent>,
    ) -> WakeEnd {
        let DriverEnv { broker, .. } = env;
        let DriverState {
            pending,
            foreground,
            command,
            command_messages,
            ..
        } = state;
        match wake {
            Some(event) => {
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
            None => *closed = true,
        }
        WakeEnd::Finished
    }
}
