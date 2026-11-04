use super::*;

impl HarnessBroker {
    /// Stage clarification feedback and retain the original invocation's tool authority.
    pub(crate) fn prepare_owned_clarification(
        &mut self,
        interaction: &mut Exchange,
        previous: &ProviderRequest,
        admission: &mut foreground_dispatch::ForegroundAdmission,
    ) -> Result<Option<(ProviderRequest, Vec<SessionEvent>)>> {
        let Some(clarification) = &admission.clarification else {
            return Ok(None);
        };
        let now = self.clock.now_ms();
        let mut events = Vec::new();
        let (prompt, mode) = match clarification {
            foreground_dispatch::ClarificationContext::Interaction => {
                if self.active_plan_has_review_questions()? || self.active_plan_awaits_input()? {
                    return Ok(None);
                }
                let Some(mut candidate) = self.find_active_interaction_elicitation()? else {
                    return Ok(None);
                };
                anyhow::ensure!(
                    candidate.id == interaction.id,
                    "clarification exchange changed"
                );
                if candidate
                    .elicitation()
                    .is_none_or(|elicitation| elicitation.current_question().is_some())
                {
                    return Ok(None);
                }
                let elicitation = candidate
                    .take_elicitation()
                    .context("active interaction has no elicitation state")?;
                let feedback = elicitation.feedback();
                if candidate.check_setup.is_some()
                    && elicitation.answer.iter().any(|answer| {
                        answer.source == crate::session::ResolutionSource::User
                            && !matches!(answer.response, PlanQuestionResponse::Skipped)
                    })
                {
                    candidate.check_setup = Some(crate::checks::CheckSetupState::Reviewed);
                }
                candidate.set_awaiting_input(false);
                for node in &mut candidate.node_list {
                    if let ExchangeNode::QuestionPresented {
                        question, answer, ..
                    } = node
                        && question.id == elicitation.question_set.id
                    {
                        *answer = Some(feedback.clone());
                    }
                }
                candidate.append_question_input(
                    crate::exchange::InputIntent::Answer,
                    feedback.clone(),
                    crate::exchange::QuestionInput {
                        set_id: elicitation.question_set.id.clone(),
                        question_id: None,
                        answer: elicitation.answer.clone(),
                    },
                    now,
                )?;
                self.store.save_exchange(&candidate)?;
                events.push(self.event(
                    "question_answered",
                    json!({"exchange_id":candidate.id,
                    "question":elicitation.question_set,"answer":elicitation.answer}),
                )?);
                *interaction = candidate;
                (
                    format!(
                        "Harness resolved the pending questions and recorded their response sources. Do not call question_answer or question_withdraw for this set. Continue the original request using the responses and current task context.\n\n{feedback}"
                    ),
                    self.task_prompt_mode()?,
                )
            }
            foreground_dispatch::ClarificationContext::Plan { plan_id } => {
                let mut plan = self
                    .store
                    .load_plan(plan_id)?
                    .context("active plan record is missing after clarification")?;
                if plan.elicitation.is_none()
                    && matches!(plan.state, PlanState::Generating | PlanState::Revising)
                {
                    let withdrawal = self
                        .store
                        .list_plan_lifecycle(&self.session.id)?
                        .into_iter()
                        .rev()
                        .find(|item| {
                            item.plan_id == *plan_id
                                && item.kind == PlanLifecycleKind::QuestionWithdrawn
                        })
                        .context("withdrawn planning question has no lifecycle record")?;
                    let feedback = format!(
                        "Pending planning questions withdrawn: {}",
                        withdrawal
                            .answer
                            .as_deref()
                            .unwrap_or("no material decision remains")
                    );
                    (
                        PlanPrompt::feedback(&plan.request, &feedback),
                        PromptMode::Plan,
                    )
                } else {
                    let Some(elicitation) = plan.elicitation.as_ref() else {
                        return Ok(None);
                    };
                    if elicitation.current_question().is_some() {
                        return Ok(None);
                    }
                    let elicitation = plan
                        .elicitation
                        .take()
                        .context("active plan has no elicitation state")?;
                    let feedback = elicitation.feedback();
                    let question = elicitation.question_set.clone();
                    let mut candidate = interaction.clone();
                    candidate.append_question_input(
                        crate::exchange::InputIntent::Answer,
                        feedback.clone(),
                        crate::exchange::QuestionInput {
                            set_id: question.id.clone(),
                            question_id: None,
                            answer: elicitation.answer.clone(),
                        },
                        now,
                    )?;
                    for pending in &question.questions {
                        let response = Some(
                            elicitation
                                .answer
                                .iter()
                                .find(|item| item.question_id == pending.id)
                                .map(|item| item.response.clone())
                                .unwrap_or(PlanQuestionResponse::Skipped),
                        );
                        plan.question_ledger.resolve(pending, response, now);
                    }
                    PlanStateMachine::apply(&mut plan, PlanEvent::FeedbackConsumed, now)?;
                    plan.generation.reset_no_progress();
                    let task = self
                        .task_controller
                        .current()?
                        .context("planning task disappeared")?;
                    let permit = task.permit();
                    let document = self
                        .plan_file
                        .read_working_document(&self.session.id, plan_id)?;
                    let lifecycle = PlanLifecycleRecord {
                        title: plan.title.clone(),
                        anchor: Some(crate::plan::ExchangeAnchor::capture(&candidate)),
                        id: Uuid::new_v4().to_string(),
                        session_id: self.session.id.clone(),
                        plan_id: plan.id.clone(),
                        kind: PlanLifecycleKind::QuestionAnswered,
                        model_revision: plan.model_revision,
                        user_revision: plan.user_revision,
                        overall_comment: None,
                        annotation: Vec::new(),
                        question: Some(question),
                        answer: Some(feedback.clone()),
                        created_at_ms: now,
                    };
                    let event = self.event(
                        "plan_question_answered",
                        json!({"plan":plan,"lifecycle":lifecycle}),
                    )?;
                    self.task_controller.apply_with_plan_document(
                        &mut self.store,
                        &permit,
                        self.plan_file.guard_working_document(&document),
                        crate::task::TaskEvent::PlanArtifactUpdated(Box::new(plan.clone())),
                        crate::storage::TaskEffects {
                            lifecycle: vec![lifecycle],
                            exchange: vec![candidate.clone()],
                            ..Default::default()
                        },
                        now,
                    )?;
                    events.push(event);
                    *interaction = candidate;
                    (
                        PlanPrompt::feedback(&plan.request, &feedback),
                        PromptMode::Plan,
                    )
                }
            }
        };
        if matches!(mode, PromptMode::Plan | PromptMode::PlanDiscussion) {
            let task = self
                .task_controller
                .current()?
                .context("planning task disappeared")?;
            let crate::task::WorkflowSnapshot::Plan { plan } = &task.workflow else {
                anyhow::bail!("planning continuation lost its task")
            };
            let document = self
                .plan_file
                .read_working_document(&self.session.id, &plan.artifact.id)?;
            admission.planning = Some(foreground_dispatch::PlanningContext {
                plan_id: plan.artifact.id.clone(),
                permit: task.permit(),
                before_version: document.version,
                discussion: plan.artifact.state == PlanState::AwaitingReview,
            });
        }
        let request = self.refresh_owned_continuation_request(
            previous,
            ProviderInput::from_text(prompt),
            mode,
        )?;
        Ok(Some((request, events)))
    }

    /// Refresh control state while preserving the retained invocation's tool and completion scopes.
    pub(crate) fn refresh_owned_continuation_request(
        &self,
        previous: &ProviderRequest,
        input: ProviderInput,
        mode: PromptMode,
    ) -> Result<ProviderRequest> {
        let mut request = self.provider_request(input, mode)?;
        if let (Some(current), Some(old)) =
            (&mut request.control_context, &previous.control_context)
        {
            current.provider_completion = old.provider_completion.clone();
            current.checkpoint = old.checkpoint.clone();
            current.agent = old.agent.clone();
            current.checks = old.checks.clone();
            current.repository = old.repository.clone();
            current.harness = old.harness.clone();
            current.lsp = old.lsp.clone();
            current.execution = old.execution.clone();
        }
        request.provider_session_id = previous
            .provider_session_id
            .clone()
            .or(request.provider_session_id);
        request.submission = None;
        request.recovered_input.clear();
        Ok(request)
    }

    /// Finalize a synchronous session command and stage its ordered presentation.
    pub(crate) fn prepare_foreground_command(
        &mut self,
        admission: &mut foreground_dispatch::ForegroundAdmission,
    ) -> Result<ForegroundAction> {
        let mut result = admission
            .command_result
            .take()
            .context("foreground command result is missing")?;
        let admitted = result.is_ok();
        let submission_update = self.prepare_prompt_admission(admitted)?;
        let _submission = admission.take_submission()?;
        if let Err(error) = self.refresh_input_schedule() {
            result = Err(error.context("publish input deadline"));
        }
        self.active_wait_projection = None;
        let patch = self.reconcile_after_dispatch()?;
        let id = admission.request.id;
        let (response, events) = match result {
            Ok((value, events)) => (Response::success(id, value)?, events),
            Err(error) => {
                let code = if error.is::<RequestNotAdmitted>() {
                    "not_admitted"
                } else if error.is::<TurnCancelled>() {
                    "turn_cancelled"
                } else if error.is::<crate::provider::tool_timeout::ToolTimeout>() {
                    "tool_timeout"
                } else {
                    "request_failed"
                };
                (
                    Response::failure(id, code, crate::storage::error_message(&error)),
                    Vec::new(),
                )
            }
        };
        let delivery = self.prepare_provider_delivery(AppliedProviderEvent::Effects(vec![
            submission_update,
            AppliedProviderEvent::Timeline(patch),
        ]))?;
        admission.command_response = Some(DispatchResult {
            response,
            event: Vec::new(),
        });
        if events.is_empty() {
            Ok(ForegroundAction::Effect(ForegroundJob::CommandPublication(
                delivery,
            )))
        } else {
            admission.command_delivery = Some(delivery);
            Ok(ForegroundAction::Effect(ForegroundJob::CommandEvents(
                events,
            )))
        }
    }

    /// Continue the retained invocation until its child and user inputs are settled.
    pub(crate) fn complete_foreground_native(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        outcome: ForegroundOutcome,
    ) -> Result<ForegroundAction> {
        let ForegroundDispatch::Running {
            transaction,
            admission,
        } = dispatch
        else {
            anyhow::bail!("native completion has no running owner");
        };
        if let Some(timeout) = transaction.native.watchdog.timeout()? {
            return self.handle_foreground_timeout(transaction, timeout);
        }
        transaction.native.cancellation.native_cancel_settled();
        let (mut output, question_override) = match outcome {
            ForegroundOutcome::NativeReady(Ok(output)) => (output, None),
            ForegroundOutcome::NativeReady(Err(error)) if error.is_clean_cancellation() => {
                let submission = admission.take_submission()?;
                let events = std::mem::take(&mut admission.events);
                transaction.finalization =
                    Some(foreground_finalization::ForegroundFinalization::new(
                        submission,
                        transaction.mode,
                        events,
                        Err(ProviderError::Cancelled),
                        transaction.settlement.clone(),
                        None,
                        transaction.collection,
                    ));
                let old = std::mem::replace(dispatch, ForegroundDispatch::Transitioning);
                let ForegroundDispatch::Running {
                    transaction,
                    admission,
                } = old
                else {
                    unreachable!("running owner checked above")
                };
                *dispatch = ForegroundDispatch::Finalizing {
                    transaction,
                    admission,
                };
                return Ok(ForegroundAction::Wait);
            }
            ForegroundOutcome::NativeReady(Err(error)) => {
                return self.fail_foreground(transaction, error.into());
            }
            ForegroundOutcome::RequiredQuestion {
                ready: Ok(mut ready),
                completion,
            } => {
                ready.merge_native(completion.native.clone());
                (ready, Some(*completion))
            }
            ForegroundOutcome::RequiredQuestion {
                ready: Err(error),
                completion,
            } if error.is_clean_cancellation() => {
                let native = completion.native.clone();
                let mut retained = (*completion).clone();
                for batch in transaction.settlement.batches()? {
                    retained.merge_execution(batch.completion);
                }
                retained.merge_native(native);
                (retained, Some(*completion))
            }
            ForegroundOutcome::RequiredQuestion {
                ready: Err(error), ..
            } => return self.fail_foreground(transaction, error.into()),
        };
        output.dispatched_task_permit = transaction.dispatched_task_permit.clone();
        output.dispatched_execution_id = transaction.dispatched_execution_id.clone();
        output.dispatched_entry_id = transaction.dispatched_entry_id.clone();
        for task in &transaction.attached_results {
            self.acknowledge_agent_result(task.clone())?;
        }
        transaction.attached_results.clear();
        self.store.save_exchange(&transaction.state.interaction)?;
        let decision = question_override.is_some()
            || output.plan_question.is_some()
            || output.question_withdrawal.is_some()
            || output.plan_submit.is_some()
            || output.control_error.is_some()
            || output.evidence.structured_blocked
            || transaction.state.interaction.awaiting_input();
        transaction.request.provider_session_id = output
            .provider_session_id
            .clone()
            .or(transaction.request.provider_session_id.clone());
        if !decision && transaction.collection.is_none() {
            while let Some(input) = transaction.steering.active.try_receive() {
                transaction.steering.deferred.push_back(input);
            }
            if self.agent_coordinator.is_quiescing() {
                transaction.pending_native_ready = Some(Ok(output));
                return Ok(ForegroundAction::Wait);
            }
            let unresolved = self
                .agent_coordinator
                .has_unresolved_task(&transaction.state.interaction.id);
            let results = if transaction.steering.deferred.is_empty() {
                self.agent_coordinator
                    .pending_result(&transaction.state.interaction.id)
            } else {
                self.agent_coordinator.unreported_result()
            };
            let child_continuation = transaction.steering.deferred.is_empty()
                && unresolved
                && !self
                    .agent_coordinator
                    .has_active_task(&transaction.state.interaction.id)
                && !results.is_empty();
            if (!transaction.steering.deferred.is_empty() || child_continuation || !unresolved)
                && self
                    .active_wait_projection
                    .as_ref()
                    .is_some_and(|wait| wait.exchange_id == transaction.state.interaction.id)
            {
                transaction.pending_native_ready = Some(Ok(output));
                let interaction = &mut transaction.state.interaction;
                interaction.observe_blocker("agent_wait".into(), false, self.clock.now_ms());
                self.active_wait_projection = None;
                if let Some(runtime) = transaction.state.runtime.as_mut() {
                    runtime.active_wait = None;
                }
                self.store.save_exchange(interaction)?;
                let patch = self.reconcile_live_interaction(Some(interaction), None)?;
                let delivery =
                    self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))?;
                return Ok(ForegroundAction::Effect(ForegroundJob::Publication(
                    delivery,
                )));
            }
            if !transaction.steering.deferred.is_empty() || child_continuation {
                let messages = results
                    .iter()
                    .map(|task| self.agent_result_message(task))
                    .collect::<Result<Vec<_>>>()?
                    .join("\n\n");
                if child_continuation {
                    anyhow::ensure!(
                        transaction.continuation_count < self.goal_max_turns,
                        "child-result continuation reached the session turn limit"
                    );
                    transaction.continuation_count += 1;
                }
                let command = transaction.steering.deferred.pop_front();
                let (input, submission) = if let Some(command) = &command {
                    let mut text = command.text.clone();
                    if !messages.is_empty() {
                        text.push_str("\n\n");
                        text.push_str(&messages);
                    }
                    (
                        ProviderInput::from_text(text)
                            .with_attachments(command.attachments.clone()),
                        command.submission.clone(),
                    )
                } else {
                    (
                        ProviderInput::from_text(format!(
                            "Continue this same unfinished request using the child messages below. Synthesize the results and complete the original request when its required work is settled.\n\n{messages}"
                        )),
                        None,
                    )
                };
                transaction.request.input = input;
                transaction.request.submission = submission;
                transaction.request.recovered_input.clear();
                let provider = ProviderSubmission {
                    request: transaction.request.clone(),
                    engine: transaction.engine.clone(),
                    sink: transaction.provider_event_sink.clone(),
                    trace: self.trace.clone(),
                    store: self.submission_store.clone(),
                    continuation: command,
                    settlement: transaction.settlement.clone(),
                    retry: Default::default(),
                };
                transaction.native.restart(provider.run())?;
                transaction.native_open = true;
                transaction.question_enabled = true;
                transaction.attached_results = results;
                return Ok(ForegroundAction::Wait);
            }
            if unresolved {
                transaction.pending_native_ready = Some(Ok(output));
                let now_ms = self.clock.now_ms();
                let interaction = &mut transaction.state.interaction;
                let prior = self
                    .active_wait_projection
                    .as_ref()
                    .filter(|wait| wait.exchange_id == interaction.id);
                if prior.is_some() {
                    return Ok(ForegroundAction::Wait);
                }
                let started_at_ms = prior.map_or(now_ms, |wait| wait.started_at_ms);
                interaction.observe_blocker("agent_wait".into(), true, now_ms);
                self.active_wait_projection = Some(ActiveWait {
                    exchange_id: interaction.id.clone(),
                    started_at_ms,
                    agent_count: self.agent_coordinator.active_count(),
                });
                if let Some(runtime) = transaction.state.runtime.as_mut() {
                    runtime.active_wait = self.active_wait_projection.clone();
                }
                self.store.save_exchange(interaction)?;
                let patch = self.reconcile_live_interaction(Some(interaction), None)?;
                let delivery =
                    self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))?;
                return Ok(ForegroundAction::Effect(ForegroundJob::Publication(
                    delivery,
                )));
            }
        }
        // Close parent admission before checkpoint work can make this exchange terminal.
        // Retain queued, undispatched inputs for the final settlement to return safely.
        transaction.steering.active.retire();
        anyhow::ensure!(
            self.host_settlement()
                .as_ref()
                .is_some_and(|cell| Arc::ptr_eq(cell, &transaction.settlement)),
            "foreground settlement owner changed"
        );
        if let Some(finalization) = transaction.finalization.as_mut() {
            finalization.mode = transaction.mode;
            finalization.resume_native(output, question_override)?;
        } else {
            let submission = admission.take_submission()?;
            let events = std::mem::take(&mut admission.events);
            transaction.finalization = Some(foreground_finalization::ForegroundFinalization::new(
                submission,
                transaction.mode,
                events,
                Ok(output),
                transaction.settlement.clone(),
                question_override,
                transaction.collection,
            ));
        }
        let old = std::mem::replace(dispatch, ForegroundDispatch::Transitioning);
        let ForegroundDispatch::Running {
            transaction,
            admission,
        } = old
        else {
            unreachable!("running owner checked above")
        };
        *dispatch = ForegroundDispatch::Finalizing {
            transaction,
            admission,
        };
        Ok(ForegroundAction::Wait)
    }
}
