//! Admit caller intent and internal facts into the driver at the start of each step.

use super::*;

impl ExchangeDriver {
    /// Admit caller intent and internal evidence without running nested lifecycle commands.
    pub(super) async fn receive(
        &mut self,
        event: OwnerEvent,
        pending: &mut VecDeque<QueuedTurn>,
        admission: OwnerIntake<'_>,
        messages: &mut Option<MessageSender>,
        broker: &Mutex<HarnessBroker>,
        context: OwnerUpdateContext<'_>,
    ) {
        let OwnerIntake {
            active,
            steering: steering_admission,
        } = admission;
        let OwnerUpdateContext {
            services,
            trace,
            session,
            step,
        } = context;
        // Resolve readiness under the owner before consuming any capability-bearing delivery offer.
        let event = match event {
            OwnerEvent::InputReady {
                id,
                messages: output,
                reply,
            } => {
                trace.record(session, "exchange.event.read", serde_json::json!({"source":"input_readiness","readiness_id":id.0.to_string(),"update_step":step}));
                *messages = Some(output.clone());
                match self
                    .prepare_candidate_offer(
                        &output,
                        active.is_some() || !pending.is_empty(),
                        steering_admission,
                        broker,
                        OwnerUpdateContext {
                            services,
                            trace,
                            session,
                            step,
                        },
                    )
                    .await
                {
                    Ok(CandidateAdmission::Foreground(offer)) => OwnerEvent::InputOffer {
                        offer,
                        messages: output,
                        reply: InputOfferReply::Ready { id, reply },
                    },
                    Ok(CandidateAdmission::Steering(offer)) => OwnerEvent::SteeringOffer {
                        offer,
                        messages: output,
                        reply: InputOfferReply::Ready { id, reply },
                    },
                    Ok(CandidateAdmission::Waiting) => {
                        notify_input_readiness(
                            id,
                            reply,
                            Ok(InputReadiness::Waiting),
                            trace,
                            session,
                            None,
                            step,
                        );
                        return;
                    }
                    Ok(CandidateAdmission::WaitExecution) => {
                        notify_input_readiness(
                            id,
                            reply,
                            Ok(InputReadiness::WaitExecution),
                            trace,
                            session,
                            None,
                            step,
                        );
                        return;
                    }
                    Err(error) => {
                        notify_input_readiness(id, reply, Err(error), trace, session, None, step);
                        return;
                    }
                }
            }
            event => event,
        };
        match event {
            OwnerEvent::PlanQuestionAnswer { answer, reply } => {
                if self.commands.storage_unavailable() {
                    let _ = reply.send(Err(crate::exchange::QuestionEventError::AdmissionClosed));
                    return;
                }
                let operation = answer.operation_id().to_owned();
                trace.record(session, "exchange.event.read", serde_json::json!({
                    "source":"plan_question_answer","operation_id":operation,
                    "question_id":answer.question_id(),"question_revision":answer.revision(),"update_step":step,
                }));
                let permit = QuestionUpdatePermit::for_update_step(session);
                let result = crate::exchange::plan_question::commit(&permit, &answer, services);
                if result.is_ok() {
                    self.question_inputs.request();
                    services.input.changed.notify_one();
                }
                let code = result
                    .as_ref()
                    .err()
                    .map(crate::exchange::QuestionEventError::code);
                let reason = result.as_ref().err().map(|error| format!("{error:?}"));
                let observed = reply.send(result).is_ok();
                trace.record(session, "exchange.question.response", serde_json::json!({
                    "operation_id":operation,"question_id":answer.question_id(),"observed":observed,
                    "error_code":code,"reason":reason,"update_step":step,
                }));
            }
            OwnerEvent::QuestionChange { change, reply } => {
                if self.commands.storage_unavailable() {
                    let _ = reply.send(Err(crate::exchange::QuestionEventError::AdmissionClosed));
                    return;
                }
                if change.tool_call().is_some() {
                    self.stage_question_tool(
                        crate::exchange::question_tool::QuestionTool::Change { change, reply },
                    );
                    return;
                }
                if let Err(error) = self.question_waits.update(services, trace, session, step) {
                    if error.error().requires_storage_fence() {
                        self.commands.retention_failed(trace, session, step);
                        services.execution.close();
                    }
                    trace.record(session, "exchange.question_wait.retirement_failed", serde_json::json!({"wait_id":error.wait_id().to_string(),"error_code":error.error().code(),"reason":error.to_string(),"update_step":step}));
                }
                let operation = change.operation_id().to_owned();
                let question = change.question().as_str().to_owned();
                let result = change.apply(&services.questions, &services.tasks);
                if result.is_ok() {
                    self.question_inputs.request();
                    services.input.changed.notify_one();
                }
                let code = result
                    .as_ref()
                    .err()
                    .map(crate::exchange::QuestionEventError::code);
                let observed = reply.send(result).is_ok();
                trace.record(session, "exchange.question.response", serde_json::json!({"operation_id":operation,"question_id":question,"observed":observed,"error_code":code,"update_step":step}));
            }
            OwnerEvent::QuestionCreate { creation, reply } => {
                self.stage_question_tool(crate::exchange::question_tool::QuestionTool::Create {
                    creation,
                    reply,
                });
            }
            OwnerEvent::QuestionWait { creation, reply } => {
                self.stage_question_tool(crate::exchange::question_tool::QuestionTool::Wait {
                    creation: creation.waiting(),
                    reply,
                });
            }
            #[cfg(test)]
            OwnerEvent::QuestionsReady {
                messages: output,
                reply,
            } => {
                if let Some(output) = output {
                    *messages = Some(output);
                }
                self.question_inputs.request();
                let result = self
                    .question_inputs
                    .update(services, messages.as_ref())
                    .and_then(|progress| {
                        progress.ok_or(crate::exchange::QuestionInputError::ObservationLost)
                    });
                let _ = reply.send(result);
            }

            OwnerEvent::TaskChecksReady {
                messages: output,
                reply,
            } => {
                *messages = Some(output);
                self.task_check_requests.push(reply);
            }
            OwnerEvent::TaskInputReady {
                id,
                messages: output,
                reply,
            } => {
                trace.record(session, "exchange.event.read", serde_json::json!({"source":"task_input_readiness","readiness_id":id.0.to_string(),"update_step":step}));
                if messages.is_none() {
                    *messages = Some(output.clone());
                }
                let production = (|| {
                    if self.commands.storage_unavailable() {
                        return Err(InputOfferError::StateUnavailable);
                    }
                    if self.receiver.is_closed()
                        || active.is_some()
                        || self.commands.has_pending()
                        || services.tasks.configuration_is_uncertain()
                        || !matches!(self.execution_state, ExecutionState::Ready)
                    {
                        return Ok(None);
                    }
                    let Ok(_control) = services.control.clone().try_acquire_owned() else {
                        return Ok(None);
                    };
                    crate::exchange::task_input::reconcile(services).map(Some)
                })();
                let disposition = production.as_ref().ok().map(|production| match production {
                    None => "blocked",
                    Some(TaskInputProduction::Idle) => "idle",
                    Some(TaskInputProduction::Updated(_)) => "updated",
                    Some(TaskInputProduction::ContinuationCancelled { .. }) => {
                        "continuation_cancelled"
                    }
                    Some(TaskInputProduction::Superseded) => "superseded",
                });
                let result = production.map(|production| match production {
                    Some(TaskInputProduction::Updated(updates)) => updates.into_iter().for_each(|update| publish_input_observation(update, Some(&output), services, trace, session, step)),
                    Some(TaskInputProduction::ContinuationCancelled { task, input }) => {
                        trace.record(session, "exchange.task_input.continuation_cancelled", serde_json::json!({"readiness_id":id.0.to_string(),"task_id":task.as_str(),"input_id":input.as_str(),"update_step":step}));
                        let publication = services.observer.snapshot().and_then(|snapshot| {
                            retain_publication(forge_protocol::message::SessionEvent {
                                session_id: session.into(), event: "session_changed".into(), payload: serde_json::json!({"snapshot":snapshot}),
                            }, Some(&output), services, trace, session, step)?;
                            Ok(())
                        });
                        if let Err(error) = publication {
                            trace.record(session, "exchange.task_input.publication_failed", serde_json::json!({"readiness_id":id.0.to_string(),"update_step":step,"reason":error.to_string()}));
                        }
                    }
                    None | Some(TaskInputProduction::Idle | TaskInputProduction::Superseded) => {}
                });
                let code = result.as_ref().err().map(InputOfferError::code);
                let observed = reply.send(result).is_ok();
                trace.record(session, "exchange.task_input.response", serde_json::json!({"readiness_id":id.0.to_string(),"update_step":step,"disposition":disposition,"error_code":code,"observed":observed}));
            }
            OwnerEvent::LocalInputSettled {
                input,
                outcome,
                messages: output,
                reply,
            } => {
                trace.record(session, "exchange.event.read", serde_json::json!({"source":"local_input_settlement","input_id":input.id.as_str(),"input_revision":input.revision,"update_step":step,"failure":outcome.as_ref().err().map(CommandFailure::code)}));
                let result = if self.commands.storage_unavailable() {
                    Err(InputOfferError::StateUnavailable)
                } else {
                    (|| {
                        let snapshot = services.input.snapshot().map_err(InputOfferError::Queue)?;
                        let Some(source) = snapshot.messages.iter().find(|message| {
                            message.id == input.id.as_str() && message.revision == input.revision
                        }) else {
                            return Ok(None);
                        };
                        if crate::task::command::SessionCommand::queued_input(source)
                            .map_err(InputOfferError::Queue)?
                            .is_none()
                        {
                            return Err(InputOfferError::InvalidLocalResult);
                        }
                        let (delivery, error) = match outcome {
                            Ok(()) => (None, None),
                            Err(failure) => (
                                Some(crate::input::InputDelivery::Failed),
                                Some(failure.to_string()),
                            ),
                        };
                        services
                            .input
                            .delivery_if_current(
                                input.id.as_str(),
                                crate::input::InputDeliveryGuard::Queued(input.revision),
                                delivery,
                                None,
                                error,
                            )
                            .map_err(InputOfferError::Queue)
                    })()
                };
                let result = result.map(|update| {
                    if let Some(update) = update {
                        publish_input_observation(
                            update,
                            Some(&output),
                            services,
                            trace,
                            session,
                            step,
                        );
                    }
                });
                let succeeded = result.is_ok();
                let observed = reply.send(result).is_ok();
                trace.record(session, "exchange.local_input.response", serde_json::json!({"input_id":input.id.as_str(),"input_revision":input.revision,"update_step":step,"succeeded":succeeded,"observed":observed}));
            }
            OwnerEvent::InputReady { .. } => {
                unreachable!("readiness is resolved before delivery admission")
            }
            OwnerEvent::SteeringOffer {
                offer,
                messages: output,
                reply,
            } => {
                let SteeringDeliveryOffer {
                    input,
                    control,
                    order,
                } = offer;
                *messages = Some(output.clone());
                trace.record(session, "exchange.event.read", serde_json::json!({"source":"input_steering_offer","input_id":input.id.as_str(),"input_revision":input.revision,"update_step":step}));
                let unavailable = active.is_some()
                    || matches!(steering_admission, SteeringAdmission::Closed)
                    || self.receiver.is_closed()
                    || self.commands.has_pending()
                    || services.tasks.configuration_is_uncertain()
                    || self.commands.storage_unavailable()
                    || !pending.is_empty()
                    || self
                        .input_delivery
                        .as_ref()
                        .is_some_and(InputDeliveryRun::blocks_input_admission)
                    || self.steering.len() >= crate::provider::SteeringLane::CAPACITY
                    || self.steering.contains_key(&input.id)
                    || !matches!(self.execution_state, ExecutionState::Ready);
                let outcome = async {
                    if unavailable {
                        return Ok(false);
                    }
                    let snapshot = services.input.snapshot().map_err(InputOfferError::Queue)?;
                    if snapshot.messages.iter().any(|message| message.delivery() == crate::input::InputDelivery::Unknown) { return Ok(false); }
                    let Some(message) = snapshot.messages.iter().find(|message| message.id == input.id.as_str() && message.revision == input.revision).cloned() else { return Ok(false); };
                    if !steering_input_eligible(&message, services)?
                        || snapshot.suspended && !matches!(message.purpose, crate::input::InputPurpose::Question { .. } | crate::input::InputPurpose::Clarification { .. })
                        || *services.cancellation.subscribe().borrow() {
                        return Ok(false);
                    }
                    if !crate::input::input_purpose_is_current(&services.tasks, &services.questions, &message.purpose, &services.root, services.observer.session_id()).map_err(InputOfferError::Authority)? { return Ok(false); }
                    let ready = services.observer.input_readiness().map_err(InputOfferError::Queue)?;
                    if ready.has_approval || ready.checks_running { return Ok(false); }
                    let mut broker = broker.lock().await;
                    let view = broker.owner_control_view().map_err(InputOfferError::Queue)?;
                    publish_input_events(view.events, Some(&output), services, trace, session, &input, step);
                    let registry = broker.execution_registry();
                    let target = if let Some(parent) = broker.parent_input.capture().filter(crate::provider::SteeringTarget::is_open) {
                        SteeringDispatchTarget::parent(parent, registry, crate::provider::ThreadOwner::root(session))
                    } else { return Ok(false); };
                    let Some(message) = services.input.claim(input.id.as_str(), input.revision, crate::input::InputRoute::Steering).map_err(InputOfferError::Queue)? else { return Ok(false); };
                    let claimed_revision = message.revision;
                    self.steering.insert(input.id.clone(), InputDeliveryRun::start_steering(message, control, order, target, output.clone(), services));
                    let snapshot = services.input.snapshot().map_err(InputOfferError::Queue)?;
                    publish_input_observation(crate::input::InputUpdate { revision: snapshot.revision, suspended: snapshot.suspended,
                        control_revision: snapshot.control_revision, snapshot: Some(snapshot), operation_id: None,
                        upsert: Vec::new(), removed: Vec::new(), error: None, error_code: None,
                        prepend: false, retracted: None }, Some(&output), services, trace, session, step);
                    trace.record(session, "exchange.event.applied", serde_json::json!({"source":"input_steering_offer","input_id":input.id.as_str(),"input_revision":input.revision,"claimed_revision":claimed_revision,"update_step":step}));
                    Ok(true)
                }.await;
                notify_input_offer(reply, outcome, trace, session, &input, step);
            }
            OwnerEvent::InputOffer {
                offer,
                messages: output,
                reply,
            } => {
                let InputDeliveryOffer {
                    input,
                    execution,
                    control,
                } = offer;
                *messages = Some(output.clone());
                trace.record(session, "exchange.event.read", serde_json::json!({
                    "source":"input_delivery_offer","input_id":input.id.as_str(),"input_revision":input.revision,"update_step":step,
                }));
                if active.is_some()
                    || self.receiver.is_closed()
                    || self.commands.has_pending()
                    || services.tasks.configuration_is_uncertain()
                    || self.commands.storage_unavailable()
                    || self.input_delivery.is_some()
                    || !self.steering.is_empty()
                    || !pending.is_empty()
                    || !matches!(self.execution_state, ExecutionState::Ready)
                {
                    drop(control);
                    drop(execution);
                    notify_input_offer(reply, Ok(false), trace, session, &input, step);
                    return;
                }
                let selected =
                    (|| -> Result<Option<crate::input::InputMessage>, InputOfferError> {
                        let snapshot = services.input.snapshot().map_err(InputOfferError::Queue)?;
                        let Some(message) = snapshot.messages.iter().find(|message| {
                            message.id == input.id.as_str() && message.revision == input.revision
                        }) else {
                            return Ok(None);
                        };
                        if !crate::input::input_purpose_is_current(
                            &services.tasks,
                            &services.questions,
                            &message.purpose,
                            &services.root,
                            services.observer.session_id(),
                        )
                        .map_err(InputOfferError::Authority)?
                        {
                            return Ok(None);
                        }
                        let ready = services
                            .observer
                            .input_readiness()
                            .map_err(InputOfferError::Queue)?;
                        if ready.checks_running
                            || ready.has_approval
                            || message.intent == crate::input::InputIntent::Queue
                                && ready.has_elicitation
                        {
                            return Ok(None);
                        }
                        Ok(Some(message.clone()))
                    })();
                match selected {
                    Ok(Some(message)) => {
                        if message.intent == crate::input::InputIntent::Queue {
                            let view = broker.lock().await.owner_control_view();
                            match view {
                                Ok(view) => {
                                    publish_input_events(
                                        view.events,
                                        Some(&output),
                                        services,
                                        trace,
                                        session,
                                        &input,
                                        step,
                                    );
                                }
                                Err(error) => {
                                    drop(control);
                                    drop(execution);
                                    notify_input_offer(
                                        reply,
                                        Err(InputOfferError::Queue(error)),
                                        trace,
                                        session,
                                        &input,
                                        step,
                                    );
                                    return;
                                }
                            }
                        }
                        let message = match services.input.claim(
                            input.id.as_str(),
                            input.revision,
                            crate::input::InputRoute::Turn,
                        ) {
                            Ok(Some(message)) => message,
                            Ok(None) => {
                                drop(control);
                                drop(execution);
                                notify_input_offer(reply, Ok(false), trace, session, &input, step);
                                return;
                            }
                            Err(error) => {
                                drop(control);
                                drop(execution);
                                notify_input_offer(
                                    reply,
                                    Err(InputOfferError::Queue(error)),
                                    trace,
                                    session,
                                    &input,
                                    step,
                                );
                                return;
                            }
                        };
                        if let Ok(snapshot) = services.input.snapshot() {
                            publish_input_observation(
                                crate::input::InputUpdate {
                                    revision: snapshot.revision,
                                    suspended: snapshot.suspended,
                                    control_revision: snapshot.control_revision,
                                    snapshot: Some(snapshot),
                                    operation_id: None,
                                    upsert: Vec::new(),
                                    removed: Vec::new(),
                                    error: None,
                                    error_code: None,
                                    prepend: false,
                                    retracted: None,
                                },
                                Some(&output),
                                services,
                                trace,
                                session,
                                step,
                            );
                        }
                        let claimed_revision = message.revision;
                        services.cancellation.arm();
                        self.input_delivery = Some(InputDeliveryRun::start(
                            message,
                            execution,
                            control,
                            Some(output.clone()),
                            services,
                        ));
                        trace.record(session, "exchange.event.applied", serde_json::json!({
                            "source":"input_delivery_offer","input_id":input.id.as_str(),"input_revision":input.revision,"claimed_revision":claimed_revision,"update_step":step,
                        }));
                        notify_input_offer(reply, Ok(true), trace, session, &input, step);
                    }
                    Ok(None) => {
                        drop(control);
                        drop(execution);
                        notify_input_offer(reply, Ok(false), trace, session, &input, step);
                    }
                    Err(error) => {
                        drop(control);
                        drop(execution);
                        notify_input_offer(reply, Err(error), trace, session, &input, step);
                    }
                }
            }
            #[cfg(test)]
            OwnerEvent::InputReceipt {
                fact,
                messages: output,
                reply,
            } => {
                if let Some(output) = output {
                    *messages = Some(output);
                }
                let source = fact.name();
                let submission = fact.submission().cloned();
                let input = fact.input().cloned();
                trace.record(
                    session,
                    "exchange.event.read",
                    serde_json::json!({
                    "source":source,"submission_id":submission.as_ref().map(|id| id.as_str()),
                    "input_id":input.as_ref().map(|input| input.id.as_str()),
                    "input_revision":input.as_ref().map(|input| input.revision),"update_step":step,
                    }),
                );
                let result = self.input_receipts.apply(fact, services);
                if let Ok(Some(update)) = &result {
                    publish_input_observation(
                        update.clone(),
                        messages.as_ref(),
                        services,
                        trace,
                        session,
                        step,
                    );
                }
                if result
                    .as_ref()
                    .err()
                    .is_some_and(InputReceiptError::fences_execution)
                {
                    services.execution.close();
                    if let Some(submission) = &submission {
                        self.input_receipts.suspend_observation(submission);
                    }
                }
                trace.record(
                    session,
                    if result.is_ok() {
                        "exchange.event.applied"
                    } else {
                        "exchange.event.rejected"
                    },
                    serde_json::json!({
                    "source":source,"submission_id":submission.as_ref().map(|id| id.as_str()),"update_step":step,
                        "reason":result.as_ref().err().map(ToString::to_string),
                    }),
                );
                let observed = reply.send(result).is_ok();
                trace.record(session, "exchange.input_receipt.response", serde_json::json!({
                    "source":source,"submission_id":submission.as_ref().map(|id| id.as_str()),"update_step":step,"observed":observed,
                }));
            }
            OwnerEvent::ReviewQuestion {
                scope,
                selection,
                catalog,
                sink,
                messages: output,
                request_id,
                reply,
            } => {
                let available = active.is_none()
                    && pending.is_empty()
                    && !self.commands.has_pending()
                    && !self.receiver.is_closed()
                    && !self.commands.storage_unavailable()
                    && matches!(self.execution_state, ExecutionState::Ready);
                let prepared = async {
                    anyhow::ensure!(available, "review answer owner is busy");
                    let control = services
                        .control
                        .clone()
                        .try_acquire_owned()
                        .context("review answer control is busy")?;
                    let execution = services
                        .execution
                        .clone()
                        .try_acquire_owned()
                        .context("review answer execution is busy")?;
                    let mut broker = broker.lock().await;
                    let catalog = match catalog {
                        Some(catalog) => broker.commit_configuration_catalog(catalog)?,
                        None => Vec::new(),
                    };
                    let prompt = broker.prepare_review_question(
                        &scope,
                        &selection,
                        &catalog,
                        Some((&output, request_id)),
                    )?;
                    let transaction = broker.begin_review_question_dispatch(
                        prompt,
                        scope.question.body.clone(),
                        sink,
                    )?;
                    drop(broker);
                    drop(control);
                    Ok::<_, anyhow::Error>((transaction, execution))
                }
                .await;
                match prepared {
                    Ok((transaction, execution)) => pending.push_back(QueuedTurn {
                        id: uuid::Uuid::new_v4(),
                        transaction,
                        messages: Some(output),
                        initial: None,
                        completion: ForegroundCompletion::Review {
                            reply,
                            scope,
                            root: services.root.clone(),
                        },
                        updates: None,
                        execution: Some(execution),
                    }),
                    Err(error) => {
                        let _ = reply.send(Err(error));
                    }
                }
            }
            OwnerEvent::Command {
                intent,
                task_admission,
                messages: output,
                reply,
            } => {
                let (received_id, received_command) = match intent.as_ref() {
                    #[cfg(test)]
                    CommandIntent::Fresh { id, command } => (id, command),
                    CommandIntent::Identified(request) => (&request.id, &request.command),
                };
                trace.record(session, "exchange.command.received", serde_json::json!({
                    "command_id":received_id.as_str(),"command":received_command.name(),"update_step":step,
                }));
                if let Some(output) = output {
                    *messages = Some(output);
                }
                #[cfg(not(test))]
                let CommandIntent::Identified(request) = *intent;
                #[cfg(test)]
                let request = match *intent {
                    CommandIntent::Identified(request) => request,
                    #[cfg(test)]
                    CommandIntent::Fresh { id, command } => CommandRequest {
                        id,
                        command,
                        input_control: InputControl::AtExecution,
                    },
                };
                self.commands.admit(
                    CommandAdmission {
                        request,
                        reply,
                        task_admission,
                    },
                    active.map(ActiveCommand::queued_mut),
                    trace,
                    session,
                    step,
                );
            }
            OwnerEvent::ApprovalTimeout { schedule_revision } => {
                self.pending_approval_timeout = Some(schedule_revision);
            }
            OwnerEvent::RecoverObservation { inbox, reply } => {
                self.observation_recovery.request(
                    inbox,
                    &services.observer.session_events(),
                    reply,
                );
            }
            OwnerEvent::ProviderInitialized {
                initialized,
                messages: output,
                reply,
            } => {
                if let Some(output) = output.as_ref() {
                    *messages = Some(output.clone());
                }
                let mut result = if self.commands.storage_unavailable() {
                    Err(crate::exchange::provider_lifecycle::ProviderLifecycleFailure::StateUnavailable)
                } else {
                    let broker = broker.lock().await;
                    initialized.and_then(|initialized| initialized.commit(&broker))
                };
                if result.is_ok() {
                    let catalog = { broker.lock().await.provider_catalog_request() };
                    match catalog {
                        Ok(catalog) => {
                            services.catalog_publish(catalog).await;
                        }
                        Err(_) => result = Err(
                            crate::exchange::provider_lifecycle::ProviderLifecycleFailure::StateUnavailable,
                        ),
                    }
                }
                if matches!(
                    &result,
                    Err(crate::exchange::provider_lifecycle::ProviderLifecycleFailure::StateUnavailable)
                ) {
                    self.execution_fail(services);
                }
                let _ = reply.send(result);
            }
            OwnerEvent::ProviderReady {
                messages: output,
                reply,
            } => {
                trace.record(
                    session,
                    "exchange.event.read",
                    serde_json::json!({
                        "source":"provider_session_inbox","update_step":step,
                    }),
                );
                if messages.is_none() {
                    *messages = output.clone();
                }
                let (observed, retirement) = {
                    let mut broker = broker.lock().await;
                    let observed = self
                        .input_receipts
                        .observe_acceptance(services)
                        .map_err(anyhow::Error::from)
                        .and_then(|()| broker.drain_session_events());
                    let retired_binding = match services.observer.session_events().end() {
                        Some(
                            crate::provider::session_execution::ObservationEnd::Collected {
                                retired_binding,
                                ..
                            }
                            | crate::provider::session_execution::ObservationEnd::Lost {
                                retired_binding,
                                ..
                            },
                        ) => retired_binding,
                        None => None,
                    };
                    let retirement = retired_binding
                        .as_ref()
                        .map(|binding| broker.observed_binding_retired(binding))
                        .transpose();
                    (observed, retirement)
                };
                // Consume the actual closed generation even when observation loss is reported after its prefix.
                let retirement = match retirement {
                    Ok(Some(delivery)) => delivery.deliver().await,
                    Ok(None) => Ok(Vec::new()),
                    Err(error) => Err(error),
                };
                let observed =
                    match (observed, retirement) {
                        (Ok(mut events), Ok(retirement)) => {
                            events.extend(retirement);
                            Ok(events)
                        }
                        (Err(error), Ok(_)) | (Ok(_), Err(error)) => Err(error),
                        (Err(error), Err(retirement)) => Err(error
                            .context(format!("closed binding publication failed: {retirement:#}"))),
                    };
                let observed = observed
                    .and_then(|events| {
                        let event_count = events.len();
                        for event in events {
                            retain_publication(
                                event,
                                output.as_ref(),
                                services,
                                trace,
                                session,
                                step,
                            )?;
                        }
                        trace.record(session, "exchange.event.applied", serde_json::json!({
                        "source":"provider_session_inbox","update_step":step,"events":event_count,
                    }));
                        Ok(())
                    })
                    .map_err(ProviderObservationError::Commit);
                let _ = reply.send(observed);
            }
            OwnerEvent::ReconnectSnapshot {
                cursor,
                limit,
                reply,
            } => {
                if self.reconnect_requests.len() >= OWNER_QUEUE_CAPACITY {
                    let _ = reply.send(Err(crate::exchange::ReconnectError::QueueFull));
                } else {
                    self.reconnect_requests.push_back(PendingReconnect {
                        cursor,
                        limit,
                        reply,
                    });
                }
            }
            OwnerEvent::ReconnectConfirmed {
                confirmation,
                messages: output,
                reply,
            } => {
                let result = confirmation.confirm(services);
                if result.is_ok() && output.is_some() {
                    *messages = output;
                }
                trace.record(
                    session,
                    "exchange.reconnect.confirmed",
                    serde_json::json!({
                        "update_step":step,"succeeded":result.is_ok(),
                    }),
                );
                let _ = reply.send(result);
            }
            OwnerEvent::Status { id, reply } => {
                let _ = reply.send(
                    self.commands
                        .committed(&id)
                        .map_err(|_| ExecuteCommandError::ObservationLost),
                );
            }
            OwnerEvent::Close => {
                for request in self.reconnect_requests.drain(..) {
                    let _ = request
                        .reply
                        .send(Err(crate::exchange::ReconnectError::OwnerClosed));
                }
                self.question_waits.close();
                self.receiver.close();
                self.commands.close(trace, session, step);
            }
        }
    }
}
