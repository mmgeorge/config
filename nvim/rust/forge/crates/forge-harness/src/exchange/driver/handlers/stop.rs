//! Apply stop wakes for the exchange driver loop.

use super::super::*;

impl ExchangeDriver {
    /// Apply one finished phase of a command stop.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn stop_finished_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        _foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: Option<Result<CollectedCancellation, tokio::task::JoinError>>,
    ) -> WakeEnd {
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
            command_messages,
            deferred_publications,
            retained_cancellation,
            tasks:
                RunningTasks {
                    stops: running_stops,
                    ..
                },
            ..
        } = state;
        match wake {
            Some(Ok(collected)) => {
                let CollectedCancellation {
                    command_id,
                    mut retained,
                    phase,
                } = collected;
                let mut stop_failed = false;
                match phase {
                    CancellationCleanupPhase::Native(outcome) => {
                        let record = crate::exchange::core_log::CoreRecord::TransitionCommitted {
                            command: Some(command_id.clone()),
                            invocation: None,
                            kind: if outcome.is_ok() {
                                crate::exchange::core_log::CoreTransitionKind::CollectionCompleted
                            } else {
                                crate::exchange::core_log::CoreTransitionKind::CollectionFailed
                            },
                            outcome: if outcome.is_ok() {
                                crate::exchange::core_log::CoreOutcome::Succeeded
                            } else {
                                crate::exchange::core_log::CoreOutcome::ObservationLost
                            },
                        };
                        if record_owner_core(&mut self.commands, &record, trace, session_id, step)
                            .is_err()
                        {
                            self.execution_fail(services);
                            *retained_cancellation = Some(retained);
                            return WakeEnd::RestartStep;
                        }
                        let facts = retained.apply_compaction_facts(&mut *broker.lock().await);
                        match facts {
                            Ok(updates) => {
                                publish_updates(
                                    deferred_publications,
                                    &mut self.commands,
                                    updates,
                                    command_messages.as_ref(),
                                    env.context(step),
                                )
                                .await
                            }
                            Err(error) => {
                                trace.record(session_id, "exchange.stop.history_commit_failed", serde_json::json!({
                                    "command_id":command_id.as_str(),"update_step":step,"reason":error.to_string(),
                                }));
                            }
                        }
                        if outcome.is_ok()
                            && let Some(runtime) = foreground.as_mut()
                        {
                            runtime
                                .collection_signals
                                .push_back(retained.collection_completed_signal());
                        }
                        let outcome = match outcome {
                            Ok(()) => retained.apply_collected(&mut *broker.lock().await),
                            Err(_) => Err(TransitionFailure::Uncertain),
                        };
                        trace.record(session_id, "exchange.stop.finished", serde_json::json!({
                                "command_id":command_id.as_str(),"update_step":step,"succeeded":outcome.is_ok(),
                            }));
                        let collected = outcome.is_ok();
                        if let Ok(events) = outcome {
                            publish_control(events, None, env.context(step), &command_id);
                        }
                        running_stops.spawn(async move {
                            retained.drain_host().await;
                            CollectedCancellation {
                                command_id,
                                retained,
                                phase: CancellationCleanupPhase::HostDrained { collected },
                            }
                        });
                    }
                    CancellationCleanupPhase::HostDrained { collected } => {
                        if let Err(error) = retained.abandon_settlement(&mut *broker.lock().await) {
                            trace.record(session_id, "exchange.stop.settlement_release_failed", serde_json::json!({
                                "command_id":command_id.as_str(),"update_step":step,"reason":error.to_string(),
                            }));
                        }
                        retained.release_legacy_control();
                        *retained_cancellation = Some(retained);
                        if matches!(self.execution_state, ExecutionState::Stopping) {
                            if collected {
                                self.execution_reopen(services);
                            } else {
                                stop_failed = true;
                            }
                        }
                    }
                }
                queued_turns_reject(
                    pending,
                    "execution is unavailable after cancellation collection",
                    env.context(step),
                );
                // A stop that cannot prove the provider idle leaves its native state unknown.
                if stop_failed {
                    self.restart_after_failure(
                        env,
                        state,
                        step,
                        FailedWork::Unknown,
                        "stop_failed",
                    );
                }
            }
            // The stop task ended without its result, so the provider state is unknown.
            Some(Err(_)) => {
                self.restart_after_failure(env, state, step, FailedWork::Unknown, "stop_lost");
            }
            None => {}
        }
        WakeEnd::Finished
    }

    /// Apply one finished rollback recovery job.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn rollback_collected_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        _foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: Option<Result<(CommandId, RollbackRecovery), tokio::task::JoinError>>,
    ) -> WakeEnd {
        let DriverEnv {
            broker,
            trace,
            session_id,
            services,
            ..
        } = env;
        let DriverState {
            retained_rollbacks,
            tasks:
                RunningTasks {
                    stops: running_stops,
                    ..
                },
            ..
        } = state;
        match wake {
            Some(Ok((id, mut recovery))) => {
                let (result, catalog) = {
                    let mut broker = broker.lock().await;
                    let result = if self.commands.storage_unavailable() {
                        Err(crate::exchange::rollback::RollbackFailure::RecoveryRequired)
                    } else {
                        recovery.apply(&mut broker)
                    };
                    *services
                        .provider
                        .write()
                        .unwrap_or_else(std::sync::PoisonError::into_inner) =
                        broker.provider_handle();
                    (result, broker.provider_catalog_request())
                };
                if let Ok(catalog) = catalog {
                    services.catalog_publish(catalog).await;
                }
                match result {
                    Ok(events) => publish_control(events, None, env.context(step), &id),
                    Err(error) => {
                        trace.record(session_id, "exchange.rollback.recovery_failed", serde_json::json!({
                                "command_id":id.as_str(),"error_code":error.code(),"update_step":step,
                            }));
                        self.execution_fail(services);
                        retained_rollbacks.push(recovery);
                        return WakeEnd::RestartStep;
                    }
                }
                let retained = recovery.into_transition();
                stop_collect_spawn(running_stops, id, retained, services);
            }
            Some(Err(_)) => {
                self.execution_fail(services);
                trace.record(
                    session_id,
                    "exchange.rollback.recovery_observation_lost",
                    serde_json::json!({"update_step":step}),
                );
            }
            None => {}
        }
        WakeEnd::Finished
    }

    /// Apply one finished phase of an input delivery stop.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn input_collected_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        _foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: Option<Result<CollectedInput, tokio::task::JoinError>>,
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
            deferred_publications,
            retained_inputs,
            tasks:
                RunningTasks {
                    input_stops: input_cleanup,
                    ..
                },
            ..
        } = state;
        match wake {
            Some(Ok(mut collected)) => {
                let input = &collected.work.input;
                match collected.phase {
                    InputCleanupPhase::Native(outcome) => {
                        let facts = collected
                            .work
                            .retained
                            .apply_compaction_facts(&mut *broker.lock().await);
                        let facts_committed = match facts {
                            Ok(updates) => {
                                publish_updates(
                                    deferred_publications,
                                    &mut self.commands,
                                    updates,
                                    collected.work.messages.as_ref(),
                                    env.context(step),
                                )
                                .await;
                                !self.commands.storage_unavailable()
                            }
                            Err(error) => {
                                trace.record(session_id, "exchange.input.stop_fact_commit_failed", serde_json::json!({
                                    "input_id":input.id.as_str(),"update_step":step,"reason":error.to_string(),
                                }));
                                false
                            }
                        };
                        if outcome.is_ok()
                            && let Some(runtime) = foreground.as_mut()
                        {
                            runtime
                                .collection_signals
                                .push_back(collected.work.retained.collection_completed_signal());
                        }
                        let committed = if outcome.is_ok() && facts_committed {
                            collected
                                .work
                                .retained
                                .apply_collected(&mut *broker.lock().await)
                        } else {
                            Err(TransitionFailure::Uncertain)
                        };
                        if let Ok(events) = committed {
                            publish_input_events(
                                events,
                                collected.work.messages.as_ref(),
                                services,
                                trace,
                                session_id,
                                input,
                                step,
                            );
                        }
                        input_cleanup.spawn(async move {
                            collected.work.retained.drain_host().await;
                            CollectedInput {
                                work: collected.work,
                                phase: InputCleanupPhase::HostDrained,
                            }
                        });
                    }
                    InputCleanupPhase::HostDrained => {
                        collected.work.retained.release_legacy_control();
                        collected.work.execution.take();
                        retained_inputs.push(collected.work);
                        if matches!(self.execution_state, ExecutionState::Stopping) {
                            self.restart_after_failure(
                                env,
                                state,
                                step,
                                FailedWork::Unknown,
                                "input_stop_failed",
                            );
                        }
                    }
                }
            }
            // The input stop task ended without its result, so the provider state is unknown.
            Some(Err(_)) => {
                self.restart_after_failure(
                    env,
                    state,
                    step,
                    FailedWork::Unknown,
                    "input_stop_lost",
                );
            }
            None => {}
        }
        WakeEnd::Finished
    }
}
