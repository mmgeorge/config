//! Apply checks wakes for the exchange driver loop.

use super::super::*;

impl ExchangeDriver {
    /// Deliver retained results directly from the owner loop, independent of caller queue controls.
    pub(in crate::exchange::driver) async fn check_results_advance(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        closed: bool,
    ) {
        if let Err(error) = self.check_result_dispatch(env, state, step, closed).await {
            tracing::error!(target: "forge::harness", event_name = "check_result.dispatch_failed",
                session_id = env.session_id, error = %format!("{error:#}"), "Check result dispatch failed");
        }
    }

    /// Claim a result only after its original exchange and provider destination remain eligible.
    async fn check_result_dispatch(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        closed: bool,
    ) -> Result<()> {
        use crate::exchange::check_result::DeliveryDeferral;
        let services = &env.services;
        let Some(message) = services
            .check_inbox
            .snapshot()?
            .messages
            .into_iter()
            .find(|message| {
                !matches!(
                    message.delivery(),
                    crate::input::InputDelivery::Delivering | crate::input::InputDelivery::Accepted
                )
            })
        else {
            return Ok(());
        };
        let reason = if message.delivery() != crate::input::InputDelivery::Queued {
            Some(DeliveryDeferral::DeliveryUnresolved)
        } else if closed
            || self.commands.has_pending()
            || self.commands.storage_unavailable()
            || state.command.is_some()
            || !state.pending.is_empty()
            || self
                .input_delivery
                .as_ref()
                .is_some_and(InputDeliveryRun::blocks_input_admission)
            || !matches!(self.execution_state, ExecutionState::Ready)
            || services.tasks.configuration_is_uncertain()
        {
            Some(DeliveryDeferral::OwnerBusy)
        } else if services.observer.input_readiness()?.has_approval {
            Some(DeliveryDeferral::ApprovalPending)
        } else {
            None
        };
        if let Some(reason) = reason {
            self.check_results.defer(&env.session_id, &message, reason);
            return Ok(());
        }
        let Some(output) = state.command_messages.clone() else {
            return Ok(());
        };
        let Ok(control) = services.control.clone().try_acquire_owned() else {
            self.check_results
                .defer(&env.session_id, &message, DeliveryDeferral::ControlBusy);
            return Ok(());
        };
        let input = super::super::super::command::InputRevision {
            id: super::super::super::command::InputId::new(message.id.clone())?,
            revision: message.revision,
        };
        let (reply, _response) = oneshot::channel();
        let reply = InputOfferReply::Ready {
            id: InputReadinessId(uuid::Uuid::new_v4()),
            reply,
        };
        let event = match services.execution.clone().try_acquire_owned() {
            Ok(execution) => OwnerEvent::InputOffer {
                offer: InputDeliveryOffer {
                    input,
                    execution,
                    control,
                },
                messages: output,
                reply,
            },
            Err(_) => {
                let crate::input::InputPurpose::CheckResult { exchange_id, .. } = &message.purpose
                else {
                    anyhow::bail!("internal inbox contains non-check input");
                };
                let active = crate::storage::HistoryReader::current(&services.root)?
                    .latest_exchange(&env.session_id)?;
                if active.is_none_or(|active| active.id != *exchange_id) {
                    self.check_results.defer(
                        &env.session_id,
                        &message,
                        DeliveryDeferral::OtherExchange,
                    );
                    return Ok(());
                }
                if !services.observer.native_steer_supported()?
                    || self.steering.len() >= crate::provider::SteeringLane::CAPACITY
                {
                    self.check_results.defer(
                        &env.session_id,
                        &message,
                        DeliveryDeferral::ProviderBusy,
                    );
                    return Ok(());
                }
                let Ok(order) = self.steering_order.clone().try_acquire_owned() else {
                    self.check_results.defer(
                        &env.session_id,
                        &message,
                        DeliveryDeferral::ProviderBusy,
                    );
                    return Ok(());
                };
                OwnerEvent::SteeringOffer {
                    offer: SteeringDeliveryOffer {
                        input,
                        control,
                        order,
                    },
                    messages: output,
                    reply,
                }
            }
        };
        self.receive(
            event,
            &mut state.pending,
            OwnerIntake {
                active: state.command.as_mut(),
                foreground_target: state.foreground.as_ref().map(|turn| {
                    crate::exchange::queue::StopTarget {
                        foreground: turn.id,
                        invocation: turn.transaction.invocation_scope().ok().flatten(),
                    }
                }),
                steering: SteeringAdmission::from(state.foreground.as_ref()),
            },
            &mut state.command_messages,
            &env.broker,
            env.context(step),
        )
        .await;
        Ok(())
    }

    /// Apply one task check event.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn task_check_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        _update_step: u64,
        _foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: Option<Result<crate::broker::TaskCheckEvent>>,
    ) -> WakeEnd {
        let DriverEnv { broker, .. } = env;
        let DriverState {
            task_verification,
            task_check_completion,
            ..
        } = state;
        if let Some(result) = wake {
            let progress = match task_verification.as_mut() {
                Some(checks) => {
                    if self.commands.storage_unavailable() {
                        let _ = checks.run.cancel();
                    }
                    match result {
                        Ok(event) => checks.run.update(&mut *broker.lock().await, event),
                        Err(error) => Err(error),
                    }
                }
                None => Err(anyhow::anyhow!("task verification owner disappeared")),
            };
            match progress {
                Ok(crate::broker::TaskCheckProgress::Pending) => {}
                Ok(crate::broker::TaskCheckProgress::Finished { events, failure }) => {
                    *task_check_completion = Some((events, failure))
                }
                Err(error) => {
                    *task_check_completion = Some((
                        Vec::new(),
                        Some(crate::checks::owner::CheckOwnerFailure::Worker(error)),
                    ))
                }
            }
        }
        WakeEnd::Finished
    }

    /// Publish one task check update.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn task_check_update_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        _foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: std::io::Result<Option<crate::broker::publication::SessionUpdate>>,
    ) -> WakeEnd {
        let DriverEnv {
            trace, session_id, ..
        } = env;
        let DriverState {
            command_messages,
            deferred_publications,
            task_verification,
            task_check_updates,
            ..
        } = state;
        match wake {
            Ok(Some(update)) => {
                publish_updates(
                    deferred_publications,
                    &mut self.commands,
                    vec![update],
                    command_messages.as_ref(),
                    env.context(step),
                )
                .await;
            }
            Ok(None) => *task_check_updates = None,
            Err(error) => {
                *task_check_updates = None;
                if let Some(checks) = task_verification.as_mut() {
                    let _ = checks.run.cancel();
                }
                trace.record(
                    session_id,
                    "exchange.task_check.stream_failed",
                    serde_json::json!({"reason":error.to_string(),"update_step":step}),
                );
            }
        }
        WakeEnd::Finished
    }

    /// Publish one tool-started check update.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn tool_check_update_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        _foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: std::io::Result<Option<crate::broker::publication::SessionUpdate>>,
    ) -> WakeEnd {
        let DriverEnv {
            trace, session_id, ..
        } = env;
        let DriverState {
            command_messages,
            deferred_publications,
            ..
        } = state;
        match wake {
            Ok(Some(update)) => {
                self.check_results.request();
                publish_updates(
                    deferred_publications,
                    &mut self.commands,
                    vec![update],
                    command_messages.as_ref(),
                    env.context(step),
                )
                .await;
            }
            // The owner retains a sender, so the stream closes only when the owner exits.
            Ok(None) => {}
            Err(error) => {
                trace.record(
                    session_id,
                    "exchange.tool_check.stream_failed",
                    serde_json::json!({"reason":error.to_string(),"update_step":step}),
                );
            }
        }
        WakeEnd::Finished
    }

    /// Apply one automatic check result.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn automatic_check_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        _update_step: u64,
        _foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: Option<Result<crate::broker::OwnedCheckResult>>,
    ) -> WakeEnd {
        let DriverEnv { broker, .. } = env;
        let DriverState {
            automatic_checks, ..
        } = state;
        if let Some(result) = wake
            && let Some(checks) = automatic_checks.as_mut()
        {
            let result = if self.commands.storage_unavailable() {
                Err(anyhow::anyhow!(
                    "owner storage is unavailable for automatic check completion"
                ))
            } else {
                match result {
                    Ok(result) => checks.apply(&mut *broker.lock().await, result),
                    Err(error) => Err(error),
                }
            };
            if let Err(error) = result {
                checks.fail(error);
                let _ = checks.cancel();
            }
        }
        WakeEnd::Finished
    }

    /// Publish one automatic check update.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn automatic_update_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        _foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: std::io::Result<Option<crate::broker::publication::SessionUpdate>>,
    ) -> WakeEnd {
        let DriverState {
            command_messages,
            deferred_publications,
            automatic_checks,
            automatic_publication,
            ..
        } = state;
        match wake {
            Ok(Some(update)) => {
                publish_updates(
                    deferred_publications,
                    &mut self.commands,
                    vec![update],
                    command_messages.as_ref(),
                    env.context(step),
                )
                .await;
            }
            Ok(None) => *automatic_publication = None,
            Err(error) => {
                *automatic_publication = None;
                if let Some(checks) = automatic_checks.as_mut() {
                    checks.fail(error.into());
                    let _ = checks.cancel();
                }
            }
        }
        WakeEnd::Finished
    }

    /// Apply the response of an automatic repair turn.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn automatic_repair_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        _update_step: u64,
        _foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: Result<DispatchResult>,
    ) -> WakeEnd {
        let DriverEnv { broker, .. } = env;
        let DriverState {
            automatic_checks,
            automatic_repair,
            ..
        } = state;
        let result = wake;
        *automatic_repair = None;
        if let Some(checks) = automatic_checks.as_mut() {
            let result = result.and_then(|result| {
                if result.response.error().is_some() {
                    return Err(anyhow::anyhow!("automatic repair foreground failed"));
                }
                let value = result
                    .response
                    .result()
                    .cloned()
                    .ok_or_else(|| anyhow::anyhow!("automatic repair result is missing"))?;
                Ok((value, result.event))
            });
            let result = match result {
                Ok(result) if !self.commands.storage_unavailable() => {
                    checks.repaired(&mut *broker.lock().await, result)
                }
                Ok(_) => Err(anyhow::anyhow!(
                    "owner storage is unavailable for automatic repair"
                )),
                Err(error) => Err(error),
            };
            if let Err(error) = result {
                checks.fail(error);
                let _ = checks.cancel();
            }
        }
        WakeEnd::Finished
    }

    /// Admit one check owner request.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn check_event_wake(
        &mut self,
        _env: &DriverEnv,
        state: &mut DriverState,
        _update_step: u64,
        _foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: Option<crate::checks::owner::CheckOwnerEvent>,
    ) -> WakeEnd {
        let DriverState {
            check_runs,
            pending_check_tools,
            tool_check_sink,
            pending_automatic_checks,
            ..
        } = state;
        match wake {
            Some(crate::checks::owner::CheckOwnerEvent::StartTool(request)) => {
                if pending_check_tools.len() < OWNER_QUEUE_CAPACITY {
                    let mut request = *request;
                    request.sink = Some(tool_check_sink.clone());
                    pending_check_tools.push_back(crate::checks::owner::PendingCheckTool::Run(
                        Box::new(request),
                    ));
                } else {
                    let _ = request
                        .reply
                        .send(Err(crate::checks::owner::CheckOwnerFailure::Unavailable));
                }
            }
            Some(crate::checks::owner::CheckOwnerEvent::Discover(request)) => {
                let request = crate::checks::owner::PendingCheckTool::Discover(request);
                if pending_check_tools.len() < OWNER_QUEUE_CAPACITY {
                    pending_check_tools.push_back(request);
                } else {
                    request.reject(crate::checks::owner::CheckOwnerFailure::Unavailable);
                }
            }
            Some(crate::checks::owner::CheckOwnerEvent::CancelTool {
                scope,
                run_id,
                reply,
            }) => {
                let _ = reply.send(check_runs.cancel(&scope, &run_id));
            }
            Some(crate::checks::owner::CheckOwnerEvent::CompleteAutomatic(request)) => {
                if pending_automatic_checks.len() < OWNER_QUEUE_CAPACITY {
                    pending_automatic_checks.push_back(request);
                } else {
                    let _ = request
                        .reply
                        .send(Err(crate::checks::owner::CheckOwnerFailure::Unavailable));
                }
            }
            None => {}
        }
        WakeEnd::Finished
    }

    /// Apply one collected tool check run.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn check_collected_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        _foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: Option<Result<crate::checks::owner::CheckOwnerCompletion, tokio::task::JoinError>>,
    ) -> WakeEnd {
        let DriverEnv {
            trace,
            session_id,
            services,
            ..
        } = env;
        let DriverState {
            check_runs,
            check_scope,
            ..
        } = state;
        match wake {
            Some(Ok(completion)) => {
                trace.record(session_id, "exchange.check.collected", serde_json::json!({
                        "run_id":completion.run_id,"succeeded":completion.outcome.is_ok(),"update_step":step,
                    }));
                check_runs.settle(&completion);
                tracing::info!(target: "forge::harness", event_name = "check_result.collected",
                    session_id, run_id = completion.run_id, succeeded = completion.outcome.is_ok(),
                    "Collected check run for result delivery");
                self.check_results.request();
                if check_scope.as_ref() != Some(&completion.scope) {
                    let _ = check_runs.retire_scope(&completion.scope);
                }
                services.input.changed.notify_one();
            }
            Some(Err(error)) => {
                self.execution_fail(services);
                trace.record(
                    session_id,
                    "exchange.check.collection_lost",
                    serde_json::json!({"reason":error.to_string(),"update_step":step}),
                );
            }
            None => {}
        }
        WakeEnd::Finished
    }
}
