//! Apply input wakes for the exchange driver loop.

use super::super::*;

impl ExchangeDriver {
    /// Apply one input delivery or steering fact.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn input_delivery_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: (InputDeliveryTarget, InputDeliveryEvent),
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
            command,
            deferred_publications,
            tasks:
                RunningTasks {
                    internal_completions,
                    input_stops: input_cleanup,
                    ..
                },
            ..
        } = state;
        let (target, event) = wake;
        let mut run = match &target {
            InputDeliveryTarget::Foreground => self.input_delivery.take(),
            InputDeliveryTarget::Steering(id) => self.steering.remove(id),
        }
        .expect("input event belongs to its retained delivery");
        let input = run.input.clone();
        let output = run.messages.clone();
        trace.record(session_id, "exchange.event.read", serde_json::json!({
                "source":"input_delivery","input_id":input.id.as_str(),"input_revision":input.revision,
                "event":event.name(),"update_step":step,
            }));
        let progress = run.update(
            event,
            foreground_status.clone(),
            &mut *broker.lock().await,
            services,
            &mut self.input_receipts,
            !self.commands.storage_unavailable(),
        );
        match progress {
            Ok(InputDeliveryProgress::Waiting {
                events,
                updates,
                input: changed,
            }) => {
                publish_input_events(
                    events,
                    output.as_ref(),
                    services,
                    trace,
                    session_id,
                    &input,
                    step,
                );
                publish_updates(
                    deferred_publications,
                    &mut self.commands,
                    updates,
                    output.as_ref(),
                    env.context(step),
                )
                .await;
                if let Some(changed) = changed {
                    publish_input_observation(
                        changed,
                        output.as_ref(),
                        services,
                        trace,
                        session_id,
                        step,
                    );
                }
                self.restore_delivery(&target, run);
            }
            Ok(InputDeliveryProgress::Foreground(start)) => {
                if let Some(ActiveCommand::Task(run)) = command.as_mut()
                    && let InputForegroundSource::Input(receipt) = &start.source
                    && run.input_foreground(&input, start.id, receipt.clone())
                {
                    trace.record(session_id, "exchange.command.effect_requested", serde_json::json!({
                            "command_id":run.command.record.request.id.as_str(),"effect":"task_input_foreground",
                            "input_id":input.id.as_str(),"input_revision":input.revision,
                            "work_id":start.id,"submission_id":receipt.as_str(),"update_step":step,
                        }));
                }
                publish_updates(
                    deferred_publications,
                    &mut self.commands,
                    start.committed,
                    output.as_ref(),
                    env.context(step),
                )
                .await;
                if let Some(changed) = start.input {
                    publish_input_observation(
                        changed,
                        output.as_ref(),
                        services,
                        trace,
                        session_id,
                        step,
                    );
                }
                pending.push_back(QueuedTurn {
                    id: start.id,
                    transaction: start.transaction,
                    messages: output,
                    initial: None,
                    completion: ForegroundCompletion::Caller(start.completion),
                    updates: Some(start.updates),
                    execution: Some(start.execution),
                });
                self.restore_delivery(&target, run);
            }
            Ok(InputDeliveryProgress::Finished {
                events,
                input: changed,
                completion,
            }) => {
                if let Some(ActiveCommand::Task(task)) = command.as_mut() {
                    task.input_ended(&input);
                }
                drop(run);
                if let Some(completion) = completion {
                    internal_completions
                        .spawn(completion.forward(services.publications.clone(), output.clone()));
                }
                publish_input_events(
                    events,
                    output.as_ref(),
                    services,
                    trace,
                    session_id,
                    &input,
                    step,
                );
                if let Some(changed) = changed {
                    publish_input_observation(
                        changed,
                        output.as_ref(),
                        services,
                        trace,
                        session_id,
                        step,
                    );
                }
                trace.record(session_id, "exchange.input.completed", serde_json::json!({
                        "input_id":input.id.as_str(),"input_revision":input.revision,"update_step":step,
                    }));
                services.input.changed.notify_one();
            }
            Err(error) => {
                if let Some(ActiveCommand::Task(task)) = command.as_mut() {
                    task.input_ended(&input);
                }
                let safe = matches!(
                    &error,
                    OperationFailure::Validation(_)
                        | OperationFailure::Transition(TransitionFailure::Rejected(_))
                );
                trace.record(session_id, "exchange.input.failed", serde_json::json!({
                        "input_id":input.id.as_str(),"input_revision":input.revision,"update_step":step,
                        "code":error.code(),"reason":error.diagnostic(),"collection_required":!safe,
                    }));
                publish_input_events(
                    vec![forge_protocol::message::SessionEvent {
                        session_id: session_id.clone(),
                        event: "provider_event".into(),
                        payload: serde_json::json!({"kind":"error","code":error.code(),"text":format!("Input delivery failed: {}", error.diagnostic())}),
                    }],
                    output.as_ref(),
                    services,
                    trace,
                    session_id,
                    &input,
                    step,
                );
                if !safe {
                    self.execution_stop_begin(services);
                }
                let changed = match self.input_receipts.apply(
                    InputReceiptFact::WorkerFailed {
                        input: input.clone(),
                    },
                    services,
                ) {
                    Ok(changed) => changed,
                    Err(failure) => {
                        self.execution_fail(services);
                        trace.record(session_id, "exchange.input.failure_commit_failed", serde_json::json!({
                                "input_id":input.id.as_str(),"update_step":step,"reason":failure.to_string(),
                            }));
                        None
                    }
                };
                if safe {
                    drop(run);
                    services.input.changed.notify_one();
                } else {
                    let mut work = run.retain();
                    publish_input_events(
                        std::mem::take(&mut work.terminal_events),
                        work.messages.as_ref(),
                        services,
                        trace,
                        session_id,
                        &work.input,
                        step,
                    );
                    if let Some(completion) = work.completion.take() {
                        internal_completions.spawn(
                            completion
                                .forward(services.publications.clone(), work.messages.clone()),
                        );
                    }
                    let recovery = services.clone();
                    input_cleanup.spawn(async move {
                        let outcome =
                            std::panic::AssertUnwindSafe(work.retained.collect(&recovery))
                                .catch_unwind()
                                .await
                                .unwrap_or(Err(crate::provider::ProviderError::DeliveryUncertain));
                        CollectedInput {
                            work,
                            phase: InputCleanupPhase::Native(outcome),
                        }
                    });
                }
                if let Some(changed) = changed {
                    publish_input_observation(
                        changed,
                        output.as_ref(),
                        services,
                        trace,
                        session_id,
                        step,
                    );
                }
            }
        }
        WakeEnd::Finished
    }

    /// Apply one native input receipt fact.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn input_receipt_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        _foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: InputReceiptWake,
    ) -> WakeEnd {
        let DriverEnv {
            trace,
            session_id,
            services,
            ..
        } = env;
        let DriverState {
            command_messages, ..
        } = state;
        let submission = match wake {
            InputReceiptWake::Evidence(id) => id,
            InputReceiptWake::Unavailable(id) => {
                services.execution.close();
                self.input_receipts.suspend_observation(&id);
                trace.record(
                    session_id,
                    "exchange.input_receipt.observation_lost",
                    serde_json::json!({
                        "submission_id":id.as_str(),"update_step":step,
                    }),
                );
                return WakeEnd::RestartStep;
            }
        };
        trace.record(session_id, "exchange.event.read", serde_json::json!({
                "source":"input_receipt_watch","submission_id":submission.as_str(),"update_step":step,
            }));
        let carries_task_input = self.input_receipts.carries_task_input(&submission);
        match self.input_receipts.apply(
            InputReceiptFact::Acceptance {
                submission: submission.clone(),
            },
            services,
        ) {
            Ok(update) => {
                trace.record(session_id, "exchange.event.applied", serde_json::json!({
                        "source":"input_receipt_watch","submission_id":submission.as_str(),"update_step":step,
                    }));
                if let Some(update) = update {
                    publish_input_observation(
                        update,
                        command_messages.as_ref(),
                        services,
                        trace,
                        session_id,
                        step,
                    );
                    // Acceptance of task input acknowledged the task entry or
                    // continuation; a held native turn may not settle for a long
                    // time, so clients must see the accepted task state now.
                    if carries_task_input {
                        publish_task_snapshot(
                            command_messages.as_ref(),
                            services,
                            trace,
                            session_id,
                            step,
                        );
                    }
                }
            }
            Err(error) => {
                self.input_receipts.suspend_observation(&submission);
                if error.fences_execution() {
                    services.execution.close();
                }
                trace.record(session_id, "exchange.input_receipt.apply_failed", serde_json::json!({
                        "submission_id":submission.as_str(),"update_step":step,"reason":error.to_string(),
                    }));
            }
        }
        WakeEnd::Finished
    }
}
