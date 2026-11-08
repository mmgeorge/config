//! Route each driver wake to the handler that owns its state changes.

use super::*;

/// Report whether the driver applies a wake now or keeps the one it waited for until the next pass.
pub(super) enum WakeSelection {
    /// Apply this wake under the turn status the selection observed.
    Apply {
        /// The wake to apply, boxed because some wakes carry a whole turn runtime.
        wake: Box<DriverWake>,
        /// The turn status computed before waiting.
        foreground_status: TurnStatus,
    },
    /// A waited-for wake is stored as the pending wake for the next pass.
    Deferred,
}

/// Report whether the driver finishes this step or restarts it without the step tail.
pub(super) enum WakeEnd {
    /// Run the step tail: notify input admission and yield.
    Finished,
    /// Start the next step immediately.
    RestartStep,
}

impl ExchangeDriver {
    /// Apply one wake through the handler that owns its state changes.
    pub(super) async fn wake_apply(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        foreground_status: &TurnStatus,
        closed: &mut bool,
        wake: DriverWake,
    ) -> WakeEnd {
        // A timer wake cannot invalidate handoff evidence already available when its command applies.
        if matches!(&wake, DriverWake::Command(_))
            && let Some(runtime) = state.foreground.as_mut()
        {
            runtime.read_handoff(&env.trace, &env.session_id, step);
            if let TurnDispatch::Dispatched(confirmation) = &runtime.handoff {
                if let Some(input) = self.input_delivery.as_mut() {
                    input.observe_dispatch(runtime.id);
                }
                if let Some(command) = state.command.as_mut() {
                    command.observe_dispatch(runtime.id, confirmation);
                }
            }
        }
        match wake {
            DriverWake::Command(wake) => {
                self.command_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::Dispatched(wake) => {
                self.dispatched_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::Foreground(wake) => {
                self.foreground_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::FailedForeground(wake) => {
                self.failed_foreground_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::InternalCompleted(wake) => {
                self.internal_completed_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::Event(wake) => {
                self.event_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::StopFinished(wake) => {
                self.stop_finished_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::RollbackCollected(wake) => {
                self.rollback_collected_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::InputCollected(wake) => {
                self.input_collected_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::InputDelivery(field_0, field_1) => {
                self.input_delivery_wake(
                    env,
                    state,
                    step,
                    foreground_status,
                    closed,
                    (field_0, field_1),
                )
                .await
            }
            DriverWake::InputReceipt(wake) => {
                self.input_receipt_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::TaskCheck(wake) => {
                self.task_check_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::TaskCheckUpdate(wake) => {
                self.task_check_update_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::ToolCheckUpdate(wake) => {
                self.tool_check_update_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::AutomaticCheck(wake) => {
                self.automatic_check_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::AutomaticUpdate(wake) => {
                self.automatic_update_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::AutomaticRepair(wake) => {
                self.automatic_repair_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::CheckEvent(wake) => {
                self.check_event_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::CheckCollected(wake) => {
                self.check_collected_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::ObservationRecovery(wake) => {
                self.observation_recovery_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::Shutdown(wake) => {
                self.shutdown_wake(env, state, step, foreground_status, closed, wake)
                    .await
            }
            DriverWake::ShutdownRetry => {
                self.shutdown_retry_wake(env, state, step, foreground_status, closed)
                    .await
            }
            DriverWake::BrokerAvailable => WakeEnd::Finished,
            DriverWake::Publication => WakeEnd::Finished,
            DriverWake::PublicationRetry => WakeEnd::Finished,
            DriverWake::PublicationStoreRetry => WakeEnd::Finished,
            DriverWake::QuestionInput => WakeEnd::Finished,
            DriverWake::QuestionTool => WakeEnd::Finished,
            DriverWake::QuestionWait => WakeEnd::Finished,
        }
    }

    /// Select the next wake: return a ready one, or wait for one and keep it for the next pass.
    #[allow(clippy::too_many_arguments)]
    pub(super) async fn wake_next(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        closed: bool,
        broker_deferred: bool,
        ready_wake: Option<DriverWake>,
    ) -> WakeSelection {
        let DriverEnv {
            broker,
            trace,
            session_id,
            services,
            ..
        } = env;
        let admission = self.admission(services, state, closed);
        let foreground_status = self.turn_status(state);
        let DriverState {
            pending,
            foreground,
            command,
            publication_delivery,
            publication_recovery_at,
            deferred_publications,
            steering_ready,
            shutdown,
            pending_wake,
            question_changes,
            check_events,
            check_runs,
            tool_check_updates,
            automatic_checks,
            automatic_publication,
            automatic_repair,
            task_verification,
            task_check_updates,
            task_check_completion,
            tasks:
                RunningTasks {
                    failed_turns: failed_foregrounds,
                    stops: running_stops,
                    rollbacks: rollback_cleanup,
                    internal_completions,
                    input_stops: input_cleanup,
                    ..
                },
            ..
        } = state;
        if let Some(runtime) = foreground.as_mut() {
            if let Some(command) = command.as_mut() {
                collection_signals_move(Some(&mut *runtime), || command.take_collection_signal());
            }
            if let Some(shutdown) = shutdown.as_mut() {
                collection_signals_move(Some(&mut *runtime), || shutdown.take_collection_signal());
            }
        }
        let task_delivery = admission.task_delivery;
        let handoff = foreground.as_mut().and_then(ActiveTurn::pending_handoff);
        if ready_wake.is_none()
            && let Some(runtime) = foreground.as_ref()
        {
            trace.record(session_id, "exchange.turn.waiting", serde_json::json!({
                "work_id":runtime.id,"update_step":step,"running_jobs":(runtime.jobs.len() + runtime.publication_job.len()),
                "staged_effects":runtime.staged_effects.iter().map(|job| (job.diagnostic_name(), job.is_collection())).collect::<Vec<_>>(),
                "queued_publications":runtime.publications.len(),"publication_active":!runtime.publication_job.is_empty(),
                "publication_drain_requested":runtime.publication_drain_requested,
                "deferred_publications":deferred_publications.updates.len(),
                "dispatch":runtime.transaction.diagnostic_name(),
            }));
        }
        let question_gate_open = admission.open_question_tools;
        let question_tool_ready = self.question_tools.iter().any(|request| {
            !question_gate_open || !request.waits_for_capacity(&self.question_waits)
        });
        if let Some(wake) = ready_wake {
            return WakeSelection::Apply {
                wake: Box::new(wake),
                foreground_status,
            };
        }
        {
            let wake = tokio::select! {
                _ = std::future::ready(()), if question_tool_ready => DriverWake::QuestionTool,
                _ = self.question_waits.next() => DriverWake::QuestionWait,
                _ = async {
                    match *publication_recovery_at {
                        Some(deadline) => tokio::time::sleep_until(deadline).await,
                        None => std::future::pending().await,
                    }
                } => DriverWake::PublicationStoreRetry,
                _ = async {
                    match deferred_publications.retry_at {
                        Some(deadline) => tokio::time::sleep_until(deadline).await,
                        None => std::future::pending().await,
                    }
                } => DriverWake::PublicationRetry,
                _ = async { drop(broker.lock().await); }, if broker_deferred => DriverWake::BrokerAvailable,
                _ = self.question_inputs.next(question_changes), if !closed && !self.commands.storage_unavailable() => DriverWake::QuestionInput,
                _ = async {
                    tokio::select! {
                        _ = publication_delivery.next() => {},
                        _ = services.publications.changed() => {},
                        _ = services.publications.reconciliation_expiry() => {},
                    }
                } => DriverWake::Publication,
                observed = async { match steering_ready.take() { Some(observed) => observed, None => next_steering(&mut self.steering, foreground_status.clone()).await } } => {
                    DriverWake::InputDelivery(InputDeliveryTarget::Steering(observed.0), observed.1)
                },
                event = self.observation_recovery.next(), if self.observation_recovery.started() => DriverWake::ObservationRecovery(event),
                event = async { match self.input_delivery.as_mut() {
                    Some(run) => run.next(foreground_status.clone()).await,
                    None => std::future::pending().await,
                }} => DriverWake::InputDelivery(InputDeliveryTarget::Foreground, event),
                collected = input_cleanup.join_next(), if !input_cleanup.is_empty() => DriverWake::InputCollected(collected),
                receipt = self.input_receipts.next() => DriverWake::InputReceipt(receipt),
                event = check_events.recv(), if !closed => DriverWake::CheckEvent(event),
                completed = check_runs.next(), if !check_runs.is_empty() => DriverWake::CheckCollected(completed),
                event = async {
                    match task_verification.as_mut() {
                        Some(checks) => checks.run.next().await,
                        None => std::future::pending().await,
                    }
                }, if task_check_completion.is_none() => DriverWake::TaskCheck(event),
                update = async {
                    match task_check_updates.as_mut() {
                        Some(updates) => updates.recv().await,
                        None => std::future::pending().await,
                    }
                } => DriverWake::TaskCheckUpdate(update),
                update = tool_check_updates.recv() => DriverWake::ToolCheckUpdate(update),
                event = async {
                    match automatic_checks.as_mut() {
                        Some(checks) if checks.has_worker() => checks.next().await,
                        Some(_) | None => std::future::pending().await,
                    }
                } => DriverWake::AutomaticCheck(event),
                update = async {
                    match automatic_publication.as_mut().map(AutomaticPublication::stream) {
                        Some(updates) => updates.recv().await,
                        None => std::future::pending().await,
                    }
                } => DriverWake::AutomaticUpdate(update),
                repaired = async {
                    match automatic_repair.as_mut() {
                        Some(response) => response.await.unwrap_or_else(|_| Err(anyhow::anyhow!("automatic repair observation was lost"))),
                        None => std::future::pending().await,
                    }
                } => DriverWake::AutomaticRepair(repaired),
                event = self.receiver.recv(), if !closed && pending.len() < OWNER_QUEUE_CAPACITY => DriverWake::Event(event),
                signal = async {
                    match foreground.as_mut() {
                        Some(runtime) => runtime.next().await,
                        None => std::future::pending().await,
                    }
                } => DriverWake::Foreground(signal),
                observed = async {
                    match handoff {
                        Some(mut pending) => {
                            let outcome = pending.observer.wait().await;
                            (pending.work_id, outcome)
                        }
                        None => std::future::pending().await,
                    }
                } => DriverWake::Dispatched(observed),
                _ = self.shutdown_retry.notified(), if closed => DriverWake::ShutdownRetry,
                event = async {
                    match shutdown.as_mut() {
                        Some(shutdown) => shutdown.next(foreground_status.clone()).await,
                        None => std::future::pending().await,
                    }
                } => DriverWake::Shutdown(event),
                prepared = async {
                    match command.as_mut() {
                        Some(command) => command.next(foreground_status.clone(), task_delivery).await,
                        None => std::future::pending().await,
                    }
                } => DriverWake::Command(prepared),
                collected = failed_foregrounds.join_next(), if !failed_foregrounds.is_empty() => DriverWake::FailedForeground(collected),
                collected = running_stops.join_next(), if !running_stops.is_empty() => DriverWake::StopFinished(collected),
                collected = rollback_cleanup.join_next(), if !rollback_cleanup.is_empty() => DriverWake::RollbackCollected(collected),
                completed = internal_completions.join_next(), if !internal_completions.is_empty() => DriverWake::InternalCompleted(completed),
            };
            *pending_wake = Some(wake);
        }
        WakeSelection::Deferred
    }

    /// Take the next wake that is already ready, in the driver's fixed priority order.
    ///
    /// Collection signals and a wake held over from the previous pass come first; the event
    /// budget bounds how many further ready events one step applies before its effect phase.
    pub(super) fn ready_wake_poll(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        closed: bool,
        event_budget: usize,
    ) -> (Option<DriverWake>, TurnStatus) {
        let task_delivery = self.admission(&env.services, state, closed).task_delivery;
        let event_status = self.turn_status(state);
        let DriverState {
            foreground,
            command,
            steering_ready,
            shutdown,
            pending_wake,
            check_events,
            check_runs,
            automatic_checks,
            automatic_publication,
            task_verification,
            task_check_updates,
            task_check_completion,
            tasks:
                RunningTasks {
                    failed_turns: failed_foregrounds,
                    stops: running_stops,
                    rollbacks: rollback_cleanup,
                    internal_completions,
                    input_stops: input_cleanup,
                    ..
                },
            ..
        } = state;
        // Apply a bounded ready-event prefix before admitting this update's command effects.
        if let Some(runtime) = foreground.as_mut() {
            if let Some(command) = command.as_mut() {
                collection_signals_move(Some(&mut *runtime), || command.take_collection_signal());
            }
            if let Some(shutdown) = shutdown.as_mut() {
                collection_signals_move(Some(&mut *runtime), || shutdown.take_collection_signal());
            }
        }
        // Apply the owner's collection barrier before a previously buffered job result.
        // Native cleanup can already have cancelled that job after the command committed.
        let mut ready_wake = foreground
            .as_mut()
            .and_then(|runtime| runtime.collection_signals.pop_front())
            .map(|signal| DriverWake::Foreground(ForegroundInput::Signal(Box::new(signal), false)))
            .or_else(|| pending_wake.take());
        if ready_wake.is_none() && event_budget > 0 {
            ready_wake = foreground
                .as_mut()
                .and_then(|runtime| runtime.next().now_or_never())
                .map(DriverWake::Foreground);
            if ready_wake.is_none() {
                ready_wake = command
                    .as_mut()
                    .and_then(|command| {
                        command
                            .next(event_status.clone(), task_delivery)
                            .now_or_never()
                    })
                    .map(DriverWake::Command);
            }
            if ready_wake.is_none() {
                ready_wake = steering_ready.take().map(|(id, event)| {
                    DriverWake::InputDelivery(InputDeliveryTarget::Steering(id), event)
                });
            }
            if ready_wake.is_none() {
                ready_wake = self
                    .input_delivery
                    .as_mut()
                    .and_then(|run| run.next(event_status.clone()).now_or_never())
                    .map(|event| DriverWake::InputDelivery(InputDeliveryTarget::Foreground, event));
            }
            if ready_wake.is_none() {
                ready_wake = self
                    .input_receipts
                    .next()
                    .now_or_never()
                    .map(DriverWake::InputReceipt);
            }
            if ready_wake.is_none() && !check_runs.is_empty() {
                ready_wake = check_runs
                    .next()
                    .now_or_never()
                    .map(DriverWake::CheckCollected);
            }
            if ready_wake.is_none() && task_check_completion.is_none() {
                ready_wake = task_verification
                    .as_mut()
                    .filter(|checks| checks.run.has_worker())
                    .and_then(|checks| checks.run.next().now_or_never())
                    .map(DriverWake::TaskCheck);
            }
            if ready_wake.is_none() {
                ready_wake = automatic_checks
                    .as_mut()
                    .filter(|checks| checks.has_worker())
                    .and_then(|checks| checks.next().now_or_never())
                    .map(DriverWake::AutomaticCheck);
            }
            if ready_wake.is_none() {
                ready_wake = shutdown
                    .as_mut()
                    .and_then(|shutdown| shutdown.next(event_status.clone()).now_or_never())
                    .map(DriverWake::Shutdown);
            }
            if ready_wake.is_none()
                && let Some(updates) = automatic_publication
                    .as_mut()
                    .map(AutomaticPublication::stream)
            {
                ready_wake = match updates.try_recv() {
                    Ok(update) => Some(DriverWake::AutomaticUpdate(Ok(Some(update)))),
                    Err(error) if error.kind() == std::io::ErrorKind::WouldBlock => None,
                    Err(error) if error.kind() == std::io::ErrorKind::UnexpectedEof => {
                        Some(DriverWake::AutomaticUpdate(Ok(None)))
                    }
                    Err(error) => Some(DriverWake::AutomaticUpdate(Err(error))),
                };
            }
            if ready_wake.is_none()
                && let Some(updates) = task_check_updates.as_mut()
            {
                ready_wake = match updates.try_recv() {
                    Ok(update) => Some(DriverWake::TaskCheckUpdate(Ok(Some(update)))),
                    Err(error) if error.kind() == std::io::ErrorKind::WouldBlock => None,
                    Err(error) if error.kind() == std::io::ErrorKind::UnexpectedEof => {
                        Some(DriverWake::TaskCheckUpdate(Ok(None)))
                    }
                    Err(error) => Some(DriverWake::TaskCheckUpdate(Err(error))),
                };
            }
            if ready_wake.is_none() && !failed_foregrounds.is_empty() {
                ready_wake = failed_foregrounds
                    .join_next()
                    .now_or_never()
                    .map(DriverWake::FailedForeground);
            }
            if ready_wake.is_none() && !running_stops.is_empty() {
                ready_wake = running_stops
                    .join_next()
                    .now_or_never()
                    .map(DriverWake::StopFinished);
            }
            if ready_wake.is_none() && !rollback_cleanup.is_empty() {
                ready_wake = rollback_cleanup
                    .join_next()
                    .now_or_never()
                    .map(DriverWake::RollbackCollected);
            }
            if ready_wake.is_none() && !input_cleanup.is_empty() {
                ready_wake = input_cleanup
                    .join_next()
                    .now_or_never()
                    .map(DriverWake::InputCollected);
            }
            if ready_wake.is_none() && !internal_completions.is_empty() {
                ready_wake = internal_completions
                    .join_next()
                    .now_or_never()
                    .map(DriverWake::InternalCompleted);
            }
            if ready_wake.is_none() {
                ready_wake = check_events
                    .try_recv()
                    .ok()
                    .map(|event| DriverWake::CheckEvent(Some(event)));
            }
        }
        (ready_wake, event_status)
    }
}
