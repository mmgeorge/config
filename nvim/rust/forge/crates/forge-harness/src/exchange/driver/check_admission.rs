//! Admit tool, task, and automatic check work during the driver's effect phase.

use super::*;

impl ExchangeDriver {
    /// Start, advance, and apply automatic check work under the current resource gates.
    #[allow(clippy::too_many_arguments)]
    pub(super) async fn automatic_checks_advance(
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
        let admission = self.admission(services, state, closed);
        let DriverState {
            pending,
            command_messages,
            deferred_publications,
            pending_automatic_checks,
            automatic_checks,
            automatic_action,
            automatic_publication,
            automatic_repair,
            ..
        } = state;
        // Advance automatic work only after caller-command admission and under the same resource gates.
        // A Ready outcome is a finished run still publishing its updates, so closing tool
        // admission leaves it to complete below rather than reporting the run as interrupted.
        if admission.cancel_automatic_checks
            && !matches!(
                automatic_action,
                Some(crate::broker::OwnedCheckProgress::Ready(_))
            )
        {
            if let Some(checks) = automatic_checks.as_mut() {
                let _ = checks.cancel();
            }
            *automatic_action = None;
        }
        let automatic_available = admission.run_automatic_checks;
        if admission.start_approval_timeout
            && let Some(revision) = self.pending_approval_timeout.take()
        {
            let staged = match broker.try_lock() {
                Ok(mut broker) => match broker.approval_timeout_ready(revision) {
                    Ok(true) => {
                        let (sink, updates) = crate::broker::publication::channel();
                        Some(
                            broker
                                .with_publication_sink(&sink, |broker| {
                                    broker.begin_approval_timeout_dispatch(sink.clone())
                                })
                                .map(|(transaction, delivery)| (transaction, delivery, updates)),
                        )
                    }
                    Ok(false) => None,
                    Err(error) => Some(Err(error)),
                },
                Err(_) => {
                    self.pending_approval_timeout = Some(revision);
                    *broker_deferred = true;
                    None
                }
            };
            if let Some(staged) = staged {
                match staged {
                    Ok((transaction, delivery, updates)) => {
                        publish_updates(
                            deferred_publications,
                            &mut self.commands,
                            delivery.into_updates(),
                            command_messages.as_ref(),
                            env.context(step),
                        )
                        .await;
                        if let Some(transaction) = transaction {
                            pending.push_back(QueuedTurn {
                                id: uuid::Uuid::new_v4(),
                                transaction,
                                messages: command_messages.clone(),
                                initial: None,
                                completion: ForegroundCompletion::Retained {
                                    publications: services.publications.clone(),
                                    messages: command_messages.clone(),
                                },
                                updates: Some(updates),
                                execution: None,
                            });
                        }
                    }
                    Err(error) => {
                        trace.record(session_id, "exchange.approval_timeout.failed",
                        serde_json::json!({"schedule_revision":revision,"reason":error.to_string(),"update_step":step}));
                    }
                }
            }
        }
        if automatic_checks.is_none()
            && automatic_publication.is_none()
            && automatic_available
            && pending.is_empty()
            && let Some(request) = pending_automatic_checks.pop_front()
        {
            match broker.try_lock() {
                Ok(mut broker) => {
                    let (sink, updates) = crate::broker::publication::channel();
                    match crate::broker::OwnedCheckCompletion::start(
                        &mut broker,
                        request,
                        sink.clone(),
                    ) {
                        Ok(checks) => {
                            *automatic_checks = Some(checks);
                            *automatic_publication = Some(AutomaticPublication::Open {
                                sink,
                                stream: updates,
                            });
                        }
                        Err(error) => {
                            trace.record(
                                session_id,
                                "exchange.check.automatic_failed",
                                serde_json::json!({"reason":error.to_string(),"update_step":step}),
                            );
                        }
                    }
                }
                Err(_) => {
                    pending_automatic_checks.push_front(request);
                    *broker_deferred = true;
                }
            }
        }
        if automatic_action.is_none()
            && automatic_repair.is_none()
            && let Some(checks) = automatic_checks.as_mut()
        {
            match checks.advance() {
                Ok(crate::broker::OwnedCheckProgress::Pending) => {}
                Ok(crate::broker::OwnedCheckProgress::Finished) => {
                    *automatic_checks = None;
                    *automatic_publication = automatic_publication
                        .take()
                        .map(AutomaticPublication::close);
                }
                Ok(action) => *automatic_action = Some(action),
                Err(error) => {
                    checks.fail(error);
                    let _ = checks.cancel();
                }
            }
        }
        if matches!(
            automatic_action,
            Some(crate::broker::OwnedCheckProgress::Ready(_))
        ) {
            let Some(crate::broker::OwnedCheckProgress::Ready(mut outcome)) =
                automatic_action.take()
            else {
                unreachable!("ready automatic outcome remains retained until publication")
            };
            let mut drained = true;
            if let Some(updates) = automatic_publication
                .as_mut()
                .map(AutomaticPublication::stream)
            {
                let mut retained = Vec::new();
                for index in 0..OWNER_INTAKE_BUDGET {
                    match updates.try_recv() {
                        Ok(update) => {
                            retained.push(update);
                            if index + 1 == OWNER_INTAKE_BUDGET {
                                drained = false;
                            }
                        }
                        Err(_) => break,
                    }
                }
                publish_updates(
                    deferred_publications,
                    &mut self.commands,
                    retained,
                    command_messages.as_ref(),
                    env.context(step),
                )
                .await;
            }
            if drained {
                let events = std::mem::take(&mut outcome.events);
                let retained = services.publications.publish_batch(events);
                if let Some(checks) = automatic_checks.as_mut() {
                    match retained {
                        Ok(()) => checks.complete(outcome),
                        Err(error) => checks.fail(error.into()),
                    }
                }
                *automatic_checks = None;
                *automatic_publication = automatic_publication
                    .take()
                    .map(AutomaticPublication::close);
            } else {
                *automatic_action = Some(crate::broker::OwnedCheckProgress::Ready(outcome));
            }
        }
        if automatic_available && let Some(action) = automatic_action.take() {
            match action {
                crate::broker::OwnedCheckProgress::Revalidate => {
                    if let Some(checks) = automatic_checks.as_mut() {
                        match broker.try_lock() {
                            Ok(mut broker) => {
                                if let Err(error) = checks.revalidate(&mut broker) {
                                    checks.fail(error);
                                    let _ = checks.cancel();
                                }
                            }
                            Err(_) => {
                                *automatic_action =
                                    Some(crate::broker::OwnedCheckProgress::Revalidate);
                                *broker_deferred = true;
                            }
                        }
                    }
                }
                crate::broker::OwnedCheckProgress::Repair {
                    prompt,
                    change_owner,
                } => match services.execution.clone().try_acquire_owned() {
                    Ok(execution) => match broker.try_lock() {
                        Ok(mut broker) => {
                            let sink = automatic_publication
                                .as_ref()
                                .and_then(AutomaticPublication::sink)
                                .expect("repair retains its open publication stream")
                                .clone();
                            match broker.begin_check_repair(0, prompt, change_owner, sink) {
                                Ok(transaction) => {
                                    let (completion, response) = oneshot::channel();
                                    pending.push_back(QueuedTurn {
                                        id: uuid::Uuid::new_v4(),
                                        transaction,
                                        messages: command_messages.clone(),
                                        initial: None,
                                        completion: ForegroundCompletion::Caller(completion),
                                        updates: None,
                                        execution: Some(execution),
                                    });
                                    *automatic_repair = Some(response);
                                }
                                Err(error) => {
                                    if let Some(checks) = automatic_checks.as_mut() {
                                        checks.fail(error);
                                        let _ = checks.cancel();
                                    }
                                }
                            }
                        }
                        Err(_) => {
                            *automatic_action = Some(crate::broker::OwnedCheckProgress::Repair {
                                prompt,
                                change_owner,
                            });
                            *broker_deferred = true;
                        }
                    },
                    Err(_) => {
                        *automatic_action = Some(crate::broker::OwnedCheckProgress::Repair {
                            prompt,
                            change_owner,
                        })
                    }
                },
                action @ crate::broker::OwnedCheckProgress::Ready(_) => {
                    *automatic_action = Some(action)
                }
                crate::broker::OwnedCheckProgress::Pending
                | crate::broker::OwnedCheckProgress::Finished => {}
            }
        }
    }

    /// Interrupt checks for direct input, then admit task verification and tool checks.
    #[allow(clippy::too_many_arguments)]
    pub(super) async fn checks_admit(
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
        let admission = self.admission(services, state, closed);
        let DriverState {
            foreground,
            command_messages,
            deferred_publications,
            check_runs,
            check_scope,
            pending_check_tools,
            automatic_checks,
            task_verification,
            task_check_updates,
            task_check_completion,
            ..
        } = state;
        // Let the next admitted immediate input collect checks through the owner before delivery is retried.
        // An in-flight input delivery does not exclude this: a delivery whose turn waits on
        // `check_run` stays open while the run is active, and the run's collection is what
        // lets that turn and the direct prompt proceed.
        if admission.interrupt_checks_for_input {
            let interrupt = services
                .input
                .snapshot()
                .map_err(InputOfferError::Queue)
                .and_then(|snapshot| {
                    // A direct prompt interrupts checks even behind explicitly queued
                    // drafts, which wait for completion and must not hide it.
                    crate::exchange::input_readiness::select_pending_input(
                        snapshot,
                        &services.questions,
                        crate::exchange::input_readiness::InputQueuePolicy::AllowSteering,
                    )
                });
            if let Ok(crate::exchange::input_readiness::PendingInput::Ready(selection)) = interrupt
            {
                let (input, directive) = selection.into_parts();
                if directive.is_none()
                    && interrupt_checks_for_direct_input(&input, services, trace, session_id, step)
                    && let Some(checks) = automatic_checks.as_mut()
                {
                    let _ = checks.cancel();
                }
            }
        }
        if admission.cancel_task_checks
            && let Some(checks) = task_verification.as_mut()
        {
            let _ = checks.run.cancel();
        }
        // Admit task verification from durable task/input authority only after caller-command admission.
        if !self.task_check_requests.is_empty() {
            let mut readiness = Some(InputReadiness::Waiting);
            let relevant = services
                .tasks
                .current()
                .ok()
                .flatten()
                .is_some_and(|task| task.next_action() == crate::task::TaskNextAction::Checks);
            if task_verification.is_none() && !relevant {
                readiness = None;
            } else if admission.start_task_verification {
                let allowed = services
                    .input
                    .snapshot()
                    .map_err(InputOfferError::Queue)
                    .and_then(|input| {
                        if input.suspended
                            || input.messages.iter().any(|message| {
                                matches!(message.purpose, crate::input::InputPurpose::User)
                                    && !matches!(
                                        message.delivery(),
                                        crate::input::InputDelivery::Accepted
                                            | crate::input::InputDelivery::Delivering
                                    )
                            })
                        {
                            return Ok(false);
                        }
                        Ok(
                            !crate::exchange::input_readiness::questions_block_task_work(
                                &input,
                                &services.questions,
                            )?,
                        )
                    });
                match allowed {
                    Ok(true) => {
                        if let Ok(control) = services.control.clone().try_acquire_owned() {
                            match services.execution.clone().try_acquire_owned() {
                                Ok(execution) => match broker.try_lock() {
                                    Ok(mut broker) => {
                                        let (sink, updates) = crate::broker::publication::channel();
                                        match crate::broker::OwnedTaskCheckRun::prepare(
                                            &mut broker,
                                            sink,
                                        ) {
                                            Ok(Some(run)) => {
                                                *task_verification = Some(TaskVerification {
                                                    run,
                                                    _control: control,
                                                    _execution: execution,
                                                });
                                                *task_check_updates = Some(updates);
                                                readiness = Some(InputReadiness::Started);
                                            }
                                            Ok(None) => readiness = None,
                                            Err(error) => {
                                                trace.record(session_id, "exchange.task_check.admission_failed", serde_json::json!({"reason":error.to_string(),"update_step":step}));
                                            }
                                        }
                                    }
                                    Err(_) => *broker_deferred = true,
                                },
                                Err(_) => readiness = Some(InputReadiness::WaitExecution),
                            }
                        }
                    }
                    Ok(false) => readiness = None,
                    Err(error) => {
                        trace.record(
                            session_id,
                            "exchange.task_check.readiness_failed",
                            serde_json::json!({"reason":error.to_string(),"update_step":step}),
                        );
                    }
                }
            }
            for reply in self.task_check_requests.drain(..) {
                let _ = reply.send(Ok(readiness));
            }
        }
        if let Some((mut events, failure)) = task_check_completion.take() {
            let mut drained = true;
            if let Some(updates) = task_check_updates.as_mut() {
                let mut retained = Vec::new();
                for index in 0..OWNER_INTAKE_BUDGET {
                    match updates.try_recv() {
                        Ok(update) => {
                            retained.push(update);
                            if index + 1 == OWNER_INTAKE_BUDGET {
                                drained = false;
                            }
                        }
                        Err(_) => break,
                    }
                }
                publish_updates(
                    deferred_publications,
                    &mut self.commands,
                    retained,
                    command_messages.as_ref(),
                    env.context(step),
                )
                .await;
            }
            if drained {
                if let Some(error) = failure.as_ref() {
                    events.push(forge_protocol::message::SessionEvent { session_id: session_id.clone(), event: "provider_event".into(),
                    payload: serde_json::json!({"kind":"error","text":format!("Execution checks failed: {error}")}) });
                }
                if let Err(error) = services.publications.publish_batch(events.clone()) {
                    trace.record(
                        session_id,
                        "exchange.task_check.publication_failed",
                        serde_json::json!({"reason":error.to_string(),"update_step":step}),
                    );
                    self.execution_fail(services);
                }
                if let Some(error) = failure {
                    trace.record(
                        session_id,
                        "exchange.task_check.failed",
                        serde_json::json!({"reason":error.to_string(),"update_step":step}),
                    );
                }
                *task_verification = None;
                services.input.changed.notify_one();
            } else {
                *task_check_completion = Some((events, failure));
            }
        }
        // Revalidate tool candidates after this update's command has closed any collection gate.
        if let Some(request) = pending_check_tools.pop_front() {
            if !admission.open_check_tools {
                let _ = request
                    .reply
                    .send(Err(crate::checks::owner::CheckOwnerFailure::Unavailable));
            } else {
                match broker.try_lock() {
                    Ok(mut broker) => match broker.owner_control_view() {
                        Ok(view) => {
                            // The check request and the provider's turn observation travel on
                            // separate channels, so a fast tool call can precede the observed
                            // turn. The foreground transaction's dispatched scope remains
                            // authoritative until that observation is drained. A finalizing
                            // turn has already returned from the provider, so no live tool
                            // call can still await this check; its scope is stale.
                            let dispatched = foreground
                                .as_ref()
                                .filter(|runtime| {
                                    !matches!(
                                        runtime.transaction,
                                        TurnTransaction::Finalizing { .. }
                                    )
                                })
                                .and_then(|runtime| {
                                    runtime.transaction.invocation_scope().ok().flatten()
                                });
                            let current = dispatched.as_ref() == Some(&request.scope)
                                || view.steering_scope.as_ref() == Some(&request.scope);
                            let published = view.events.into_iter().try_for_each(|event| {
                                retain_publication(
                                    event,
                                    command_messages.as_ref(),
                                    services,
                                    trace,
                                    session_id,
                                    step,
                                )
                            });
                            if published.is_err() {
                                self.execution_fail(services);
                                let _ = request.reply.send(Err(
                                    crate::checks::owner::CheckOwnerFailure::Unavailable,
                                ));
                            } else if current {
                                if check_scope.as_ref() != Some(&request.scope)
                                    && let Some(prior) = check_scope.replace(request.scope.clone())
                                {
                                    let _ = check_runs.retire_scope(&prior);
                                }
                                if let Err(error) = check_runs.start(request) {
                                    trace.record(session_id, "exchange.check.admission_failed", serde_json::json!({"reason":error.to_string(),"update_step":step}));
                                }
                            } else {
                                let _ = request
                                    .reply
                                    .send(Err(crate::checks::owner::CheckOwnerFailure::StaleScope));
                            }
                        }
                        Err(error) => {
                            let _ = request
                                .reply
                                .send(Err(crate::checks::owner::CheckOwnerFailure::Worker(error)));
                        }
                    },
                    Err(_) => {
                        pending_check_tools.push_front(request);
                        *broker_deferred = true;
                    }
                }
            }
        }
    }
}
