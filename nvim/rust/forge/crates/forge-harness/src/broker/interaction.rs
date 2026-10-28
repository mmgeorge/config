use super::*;

impl HarnessBroker {
    pub(super) async fn run_interaction_input_inner(
        &mut self,
        text: String,
        mode: PromptMode,
        admission: Option<ExchangeAdmission>,
        input: BackendInput,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let submission_store = self.submission_store.clone();
        let submission = match self.input_submission.take() {
            Some(submission) => submission,
            None => submission_store.prepare(
                &self.backend_request(input.clone(), mode),
                self.backend.descriptor().kind,
            )?,
        };
        let (mut interaction, new_runtime, mut event) = Box::pin(self.admit_interaction(
            &text,
            mode,
            admission.as_ref(),
            submission.record()?.input_message_id,
        ))
        .await?;

        let output = self
            .run_admitted_provider(
                mode,
                input,
                submission.clone(),
                new_runtime,
                &mut interaction,
                &mut event,
            )
            .await;
        let output = match output {
            Ok(output) => output,
            Err(error) => {
                self.trace.record(
                    &self.session.id,
                    "model.failed",
                    json!({ "error": format!("{error:#}") }),
                );
                let cancelled = error.downcast_ref::<TurnCancelled>().is_some();
                if cancelled && !self.turn_cancellation.restart.load(Ordering::Acquire) {
                    submission.cancel()?;
                }
                if self
                    .exchange_runtime
                    .as_ref()
                    .is_none_or(|runtime| runtime.exchange_id != interaction.id)
                {
                    interaction.finish(
                        if cancelled {
                            crate::exchange::ExchangeOutcome::Cancelled
                        } else {
                            crate::exchange::ExchangeOutcome::Failed
                        },
                        self.clock.now_ms(),
                    )?;
                    self.store.save_exchange(&interaction)?;
                    self.emit_live_interaction(
                        BackendEvent {
                            received_at_ms: None,
                            address: None,
                            turn_boundary: None,
                            kind: if cancelled {
                                "timeline_exchange_cancelled"
                            } else {
                                "timeline_exchange_failed"
                            }
                            .into(),
                            text: Some(format!("{error:#}")),
                            data: serde_json::to_value(&interaction)?,
                            activity: None,
                            summary: None,
                            task_update: None,
                        },
                        &interaction,
                        &mut event,
                    )
                    .await?;
                    return Err(error);
                }
                if cancelled
                    && let Some(backend_session_id) =
                        self.backend.active_session_id_for(&self.session.id).await
                {
                    self.session.backend_session_id = Some(backend_session_id);
                }
                if cancelled && self.turn_cancellation.restart.load(Ordering::Acquire) {
                    Box::pin(self.park_exchange_for_restart(&mut interaction)).await?;
                    return Err(error);
                }
                let outcome = if cancelled {
                    crate::exchange::ExchangeOutcome::Cancelled
                } else {
                    crate::exchange::ExchangeOutcome::Failed
                };
                if !cancelled {
                    Box::pin(self.record_execution_failure(&mut interaction, format!("{error:#}")))
                        .await?;
                }
                let retract_before_tools = cancelled
                    && self.turn_cancellation.restores_prompt()
                    && self.capability.native_turn_rollback
                    && self
                        .exchange_runtime
                        .as_ref()
                        .is_some_and(ExchangeRuntime::retractable_before_tools);
                if retract_before_tools
                    && Box::pin(self.retract_cancelled_interaction(&interaction, &mut event))
                        .await?
                {
                    return Err(anyhow::Error::new(TurnRetracted {
                        prompt: interaction.prompt,
                    }));
                }
                let final_checkpoint_id =
                    Box::pin(self.capture_final_checkpoint(&mut interaction, outcome))
                        .await
                        .context("finalize stopped interaction")?;
                let workspace_unchanged = if let (Some(before), Some(after)) =
                    (&interaction.checkpoint_before, &final_checkpoint_id)
                {
                    let before = self
                        .store
                        .load_checkpoint(before)?
                        .context("initial checkpoint disappeared")?;
                    let after = self
                        .store
                        .load_checkpoint(after)?
                        .context("final checkpoint disappeared")?;
                    let objects = self.store.objects.clone();
                    tokio::task::spawn_blocking(move || before.equivalent(&after, &objects))
                        .await
                        .context("checkpoint comparison worker failed")??
                } else {
                    false
                };
                let retraction_eligible = !retract_before_tools
                    && cancelled
                    && self.turn_cancellation.restores_prompt()
                    && self.capability.native_turn_rollback
                    && workspace_unchanged
                    && self
                        .exchange_runtime
                        .as_ref()
                        .is_some_and(|runtime| runtime.retraction_eligible);
                if retraction_eligible
                    && Box::pin(self.retract_cancelled_interaction(&interaction, &mut event))
                        .await?
                {
                    return Err(anyhow::Error::new(TurnRetracted {
                        prompt: interaction.prompt,
                    }));
                }
                if let Some(runtime) = self.exchange_runtime.take() {
                    finish_synthetic_turn(
                        &mut interaction,
                        runtime.synthetic_turn.as_ref(),
                        if cancelled {
                            crate::turn::TurnOutcome::Cancelled
                        } else {
                            crate::turn::TurnOutcome::Failed
                        },
                        self.clock.now_ms(),
                    )?;
                }
                let outcome = if cancelled {
                    crate::exchange::ExchangeOutcome::Cancelled
                } else {
                    crate::exchange::ExchangeOutcome::Failed
                };
                interaction.finish(outcome, self.clock.now_ms())?;
                self.store
                    .save_exchange(&interaction)
                    .context("persist stopped interaction")?;
                Box::pin(self.pause_goal_after_turn_failure()).await?;
                self.save_session().context("persist stopped session")?;
                if cancelled {
                    Box::pin(self.emit_live_interaction(
                        BackendEvent {
                            received_at_ms: None,
                            address: None,
                            turn_boundary: None,
                            kind: "timeline_exchange_cancelled".into(),
                            text: None,
                            data: serde_json::to_value(&interaction)?,
                            activity: None,
                            summary: None,
                            task_update: None,
                        },
                        &interaction,
                        &mut event,
                    ))
                    .await?;
                    return Err(error);
                }
                return Err(error).context("backend turn failed");
            }
        };
        Box::pin(self.complete_interaction(interaction, output, mode, event)).await
    }

    async fn admit_interaction(
        &mut self,
        text: &str,
        mode: PromptMode,
        admission: Option<&ExchangeAdmission>,
        input_message_id: Option<String>,
    ) -> Result<(Exchange, bool, Vec<SessionEvent>)> {
        let mut event = Vec::new();
        let now_ms = self.clock.now_ms();
        let admitted_prompt = admission.map(|value| value.prompt.as_str()).unwrap_or(text);
        let automatic_repair =
            admission.is_some_and(|admission| admission.automatic_owner.is_some());
        if automatic_repair {
            let mut previous = self
                .store
                .latest_exchange(&self.session.id)?
                .context("automatic repair has no originating exchange")?;
            anyhow::ensure!(
                previous.check_owner.as_ref()
                    == admission.and_then(|admission| admission.check_owner.as_ref()),
                "automatic repair check ownership changed"
            );
            previous.repair_checks(now_ms)?;
            self.store.save_exchange(&previous)?;
        }
        let (mut interaction, new_interaction, mut admission_event) =
            Box::pin(self.interaction_for_turn(
                admitted_prompt,
                admission.is_some() && !automatic_repair,
                now_ms,
            ))
            .await?;
        event.append(&mut admission_event);
        interaction.input_message_id = input_message_id;
        interaction.approval_mode = Some(self.session.approval_mode);
        if new_interaction && let Some(admission) = admission {
            if self.check_setup {
                interaction.check_setup = Some(crate::checks::CheckSetupState::Discovery);
            } else if mode == PromptMode::Chat {
                interaction.check_setup = self.continued_check_setup(admission)?;
            }
            interaction.lifecycle.clone_from(&admission.lifecycle);
            interaction.check_owner.clone_from(&admission.check_owner);
            interaction.kind = admission.kind;
            interaction.plan_id.clone_from(&admission.plan_id);
            interaction.execution_id.clone_from(&admission.execution_id);
            if let Some(execution_id) = &interaction.execution_id {
                let execution = self
                    .store
                    .load_plan_execution(execution_id)?
                    .context("phase exchange has no execution")?;
                interaction.execution_phase = Some(crate::exchange::ExecutionPhase {
                    phase: execution.phase,
                    outcome: None,
                    summary: None,
                    findings: Vec::new(),
                    verification: None,
                    declaration_findings: Vec::new(),
                    declaration_changes: Vec::new(),
                    verification_tools: Default::default(),
                });
            }
            interaction.goal_id.clone_from(&admission.goal_id);
            if let Some(run_id) = admission.agent_run_id.as_deref()
                && let Some(run) = self.agent_registry.get(run_id).cloned()
            {
                self.agent_registry.execution_mut(run_id).parent_exchange_id =
                    Some(interaction.id.clone());
                self.store.save_agent_run(&run)?;
                self.agent_registry.insert(run);
            }
            if interaction.task.is_none()
                && let Some(execution_id) = interaction.execution_id.as_deref()
            {
                interaction.task = self
                    .store
                    .latest_execution_exchange(&self.session.id, execution_id)?
                    .and_then(|previous| previous.task);
            }
        }
        if new_interaction {
            if let Some(event_id) =
                admission.and_then(|admission| admission.plan_event_id.as_deref())
            {
                let mut lifecycle = self
                    .store
                    .list_plan_lifecycle(&self.session.id)?
                    .into_iter()
                    .find(|record| record.id == event_id)
                    .context("admitted planning event is missing")?;
                lifecycle.anchor = Some(crate::plan::ExchangeAnchor::capture(&interaction));
                self.store.save_plan_lifecycle(&lifecycle)?;
            }
            if let Some(execution_id) = interaction.execution_id.as_deref()
                && let Some(mut execution) = self.store.load_plan_execution(execution_id)?
            {
                if execution.baseline_checkpoint.is_none() {
                    execution.baseline_checkpoint = interaction.checkpoint_before.clone();
                }
                self.store.save_plan_execution(&execution)?;
            }
        }
        interaction.resume(self.clock.now_ms())?;
        self.store.save_exchange(&interaction)?;
        Box::pin(self.settle_prompt_admission(true, &mut event)).await?;
        if let Some(owner) = admission.and_then(|admission| admission.automatic_owner.as_deref()) {
            let mut automatic =
                crate::checks::AutomaticChecks::open(&self.check_runner.root, owner)?;
            automatic.repair_exchange = Some(interaction.id.clone());
            automatic.save()?;
        }
        Box::pin(self.emit_live_interaction(
            BackendEvent {
                received_at_ms: None,
                address: None,
                turn_boundary: None,
                kind: "timeline_exchange_started".into(),
                text: None,
                data: serde_json::to_value(&interaction)?,
                activity: None,
                summary: None,
                task_update: None,
            },
            &interaction,
            &mut event,
        ))
        .await?;
        self.trace.record(
            &self.session.id,
            "turn.state",
            json!({
                "exchange_id":interaction.id,"message_id":interaction.input_message_id,
                "from_state":"queued","to_state":"working",
            }),
        );

        if mode == PromptMode::Plan
            || admission.is_some_and(|admission| admission.plan_event_id.is_some())
        {
            let patch = self.reconcile_timeline()?;
            Box::pin(self.emit_timeline_patch(patch, &mut event)).await?;
        }

        let goal = self
            .session
            .goal_id
            .as_deref()
            .map(|id| self.store.load_goal(id))
            .transpose()?
            .flatten();
        let goal_execution = self
            .store
            .list_plan_execution(&self.session.id)?
            .into_iter()
            .rev()
            .find(|execution| {
                goal.as_ref()
                    .is_some_and(|goal| goal.id == execution.goal_id)
            });
        Box::pin(self.emit_backend_event(BackendEvent {
            received_at_ms: None,
            address: None, turn_boundary: None, kind: "execution_state".into(), text: None,
            data: json!({"session":self.session,"task":self.store.list_task(&self.session.id)?,"goal":goal,"goal_execution":goal_execution}),
            activity: None, summary: None, task_update: None,
        }, &mut event)).await?;

        Ok((interaction, new_interaction, event))
    }

    async fn complete_interaction(
        &mut self,
        mut interaction: Exchange,
        mut output: crate::backend::BackendOutput,
        mode: PromptMode,
        mut event: Vec<SessionEvent>,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        self.trace.record(
            &self.session.id,
            "model.completed",
            serde_json::to_value(&output)?,
        );
        interaction.pause(self.clock.now_ms());
        self.capability = output.capability.clone();
        self.session.native_fork = output.capability.native_fork;
        self.session.native_compact = output.capability.native_compact;
        if let Some(context_usage) = output.metrics.context_usage.clone() {
            self.session.context_usage = Some(context_usage);
            self.context_usage_fresh = true;
        }
        self.session.backend_session_id = output.backend_session_id.clone();
        self.session.provider_checkpoint_id = output.provider_checkpoint_id.clone();
        if !output.runtime.provider.is_empty() {
            self.session.provider_label = output.runtime.provider.clone();
        }
        if output.runtime.model.is_some() {
            self.session.resolved_model = output.runtime.model.clone();
        }
        output.event.clear();

        if let Some(control_error) = output.control_error.take() {
            anyhow::bail!(control_error);
        }

        let elicitation_control_count = usize::from(output.plan_question.is_some())
            + usize::from(output.question_answer.is_some())
            + usize::from(output.question_withdrawal.is_some());
        let refresh_elicitation =
            interaction.elicitation().is_some() && elicitation_control_count > 0;
        anyhow::ensure!(
            elicitation_control_count <= 1,
            "backend turn emitted multiple Harness question control actions"
        );
        if mode == PromptMode::Plan {
            anyhow::ensure!(
                output.question_answer.is_none() && output.question_withdrawal.is_none(),
                "planning generation cannot answer or withdraw a pending question"
            );
        }

        if mode == PromptMode::Plan
            || (mode == PromptMode::PlanDiscussion
                && (!output.plan_edit.is_empty()
                    || !output.design_patch.is_empty()
                    || output.plan_submit.is_some()))
        {
            let mut plan = self
                .current_plan_id()?
                .as_deref()
                .map(|plan_id| self.store.load_plan(plan_id))
                .transpose()?
                .flatten()
                .filter(|plan| {
                    matches!(
                        plan.state,
                        PlanState::Generating
                            | PlanState::Revising
                            | PlanState::AwaitingInput
                            | PlanState::AwaitingReview
                    )
                })
                .context("planning turn has no active canonical plan")?;
            self.record_plan_trace(
                "plan_output_received",
                json!({
                    "plan": plan_trace_fields(&plan),
                    "edit_count": output.plan_edit.len(),
                    "declaration_patch_count": output.design_patch.len(),
                    "read": output.plan_read.is_some(),
                    "submitted": output.plan_submit.is_some(),
                    "questioned": output.plan_question.is_some(),
                }),
            );

            for edit_request in std::mem::take(&mut output.plan_edit) {
                anyhow::ensure!(
                    edit_request.plan_id == plan.id,
                    "edited plan id does not match active plan"
                );
                let result = self
                    .plan_file
                    .edit_working_document(&self.session.id, edit_request)?;
                if plan.state == PlanState::AwaitingReview {
                    PlanStateMachine::apply(
                        &mut plan,
                        PlanEvent::ChangesRequested,
                        self.clock.now_ms(),
                    )?;
                    plan.acceptance = None;
                    plan.user_revision += 1;
                    plan.generation.reset();
                    interaction.kind = ExchangeKind::PlanRevision;
                }
                plan.document_version = result.version;
                plan.title = result.document.title;
                plan.updated_at_ms = self.clock.now_ms();
            }
            for request in std::mem::take(&mut output.design_patch) {
                let document = self
                    .plan_file
                    .read_working_document(&self.session.id, &plan.id)?;
                let updated = document.patch_design(Path::new(&self.session.workspace), request)?;
                self.plan_file
                    .write_working_document(&self.session.id, &plan.id, &updated)?;
                if plan.state == PlanState::AwaitingReview {
                    PlanStateMachine::apply(
                        &mut plan,
                        PlanEvent::ChangesRequested,
                        self.clock.now_ms(),
                    )?;
                    plan.acceptance = None;
                    plan.user_revision += 1;
                    plan.generation.reset();
                    interaction.kind = ExchangeKind::PlanRevision;
                }
                plan.document_version = updated.version;
                plan.title = updated.title;
                plan.updated_at_ms = self.clock.now_ms();
            }
            if let Some(read_plan_id) = output.plan_read.take() {
                anyhow::ensure!(read_plan_id == plan.id, "requested plan is not active");
            }

            if let Some(submission) = output.plan_submit.take() {
                anyhow::ensure!(
                    submission.plan_id == plan.id,
                    "submitted plan id does not match active plan"
                );
                let mut validated_document = self
                    .plan_file
                    .read_working_document(&self.session.id, &plan.id)?;
                anyhow::ensure!(
                    validated_document.version == submission.expected_version,
                    "plan version changed before submission validation"
                );
                let validation_warning = if let Some(design) = &mut validated_document.design {
                    let validated = design.validated(Path::new(&self.session.workspace)).await?;
                    let warnings = validated.warnings();
                    *design = validated.into_design();
                    warnings
                } else {
                    Vec::new()
                };
                let previous_document = if plan.model_revision == 0 {
                    None
                } else {
                    Some(self.plan_file.read_submitted_document(
                        &self.session.id,
                        &plan.id,
                        plan.model_revision,
                    )?)
                };
                let lifecycle_kind = if plan.model_revision == 0 {
                    PlanLifecycleKind::Created
                } else {
                    PlanLifecycleKind::RevisionCreated
                };
                plan.model_revision += 1;
                let (document, rendered, digest) =
                    self.plan_file.submit_validated_document_revision(
                        &self.session.id,
                        &plan.id,
                        plan.model_revision,
                        submission.expected_version,
                        validated_document,
                    )?;
                plan.document_version = document.version;
                plan.submitted_version = Some(document.version);
                plan.acceptance = None;
                plan.title = document.title.clone();
                plan.review_digest = Some(digest);
                plan.validation_warning = validation_warning;
                PlanStateMachine::apply(&mut plan, PlanEvent::PlanSubmitted, self.clock.now_ms())?;
                self.store.save_plan(&plan)?;
                self.record_plan_trace("plan_submitted", plan_trace_fields(&plan));
                if lifecycle_kind == PlanLifecycleKind::RevisionCreated {
                    let lifecycle = self
                        .store
                        .list_plan_lifecycle(&self.session.id)?
                        .into_iter()
                        .rev()
                        .find(|lifecycle| {
                            lifecycle.plan_id == plan.id
                                && lifecycle.kind == PlanLifecycleKind::ChangesRequested
                                && lifecycle.user_revision == plan.user_revision
                        });
                    if let Some(lifecycle) =
                        lifecycle.filter(|lifecycle| !lifecycle.annotation.is_empty())
                    {
                        interaction
                            .node_list
                            .push(ExchangeNode::PlanCommentResolution {
                                resolution: crate::exchange::PlanCommentResolution {
                                    id: format!(
                                        "{}:resolved-comments:{}",
                                        interaction.id, plan.user_revision
                                    ),
                                    annotation: lifecycle.annotation,
                                    created_at_ms: self.clock.now_ms(),
                                },
                            });
                    }
                }
                let (diff_text, declaration) = if let Some(design) = &document.design {
                    let delta = crate::plan::revision::DeclarationDelta::between(
                        previous_document.as_ref(),
                        design,
                    )?;
                    (
                        delta.files,
                        Some(crate::exchange::DeclarationRevision {
                            plan_id: plan.id.clone(),
                            revision: plan.model_revision,
                            document_diff: delta.document,
                            baseline_paths: delta.baseline_paths,
                        }),
                    )
                } else {
                    let previous_markdown = previous_document
                        .as_ref()
                        .map(|document| {
                            crate::plan::render_plan_at(
                                document,
                                Path::new(&self.session.workspace),
                            )
                        })
                        .transpose()?
                        .map(|rendered| rendered.markdown)
                        .unwrap_or_default();
                    (
                        crate::plan::render_plan_delta(
                            &plan.working_path,
                            &previous_markdown,
                            &rendered.markdown,
                        ),
                        None,
                    )
                };
                interaction.node_list.push(ExchangeNode::ArtifactChange {
                    change: crate::exchange::ArtifactChange {
                        id: format!("{}:artifact:{}", interaction.id, plan.model_revision),
                        path: plan.working_path.clone(),
                        diff_text,
                        declaration,
                        created_at_ms: self.clock.now_ms(),
                    },
                });
                let lifecycle = PlanLifecycleRecord {
                    title: plan.title.clone(),
                    anchor: Some(crate::plan::ExchangeAnchor::capture(&interaction)),
                    id: Uuid::new_v4().to_string(),
                    session_id: self.session.id.clone(),
                    plan_id: plan.id.clone(),
                    kind: lifecycle_kind,
                    model_revision: plan.model_revision,
                    user_revision: plan.user_revision,
                    overall_comment: None,
                    annotation: Vec::new(),
                    question: None,
                    answer: None,
                    created_at_ms: self.clock.now_ms(),
                };
                self.store.save_plan_lifecycle(&lifecycle)?;
                event.push(self.event(
                    if lifecycle_kind == PlanLifecycleKind::Created {
                        "plan_created"
                    } else {
                        "plan_revision_created"
                    },
                    json!({
                        "plan": plan,
                        "lifecycle": lifecycle,
                        "document": document,
                        "content": rendered.markdown,
                        "navigation": rendered.navigation
                    }),
                )?);
            } else if let Some(question) = output.plan_question.take() {
                let question = question.normalize()?;
                if let Some(question) = plan.question_ledger.unresolved(question) {
                    match plan.elicitation.as_mut() {
                        Some(elicitation) => elicitation.replace_question_set(question.clone()),
                        None => {
                            let mut elicitation = PlanElicitation::new(question.clone());
                            elicitation.start_timeout(
                                self.permission_coordinator
                                    .input_timeouts(&self.session.id)?
                                    .1,
                                self.clock.now_ms(),
                            )?;
                            plan.elicitation = Some(elicitation);
                        }
                    }
                    PlanStateMachine::apply(
                        &mut plan,
                        PlanEvent::QuestionAsked,
                        self.clock.now_ms(),
                    )?;
                    self.store.save_plan(&plan)?;
                    self.record_plan_trace("plan_question_asked", plan_trace_fields(&plan));
                    interaction.set_awaiting_input(true);
                    let lifecycle = PlanLifecycleRecord {
                        title: plan.title.clone(),
                        anchor: Some(crate::plan::ExchangeAnchor::capture(&interaction)),
                        id: Uuid::new_v4().to_string(),
                        session_id: self.session.id.clone(),
                        plan_id: plan.id.clone(),
                        kind: PlanLifecycleKind::QuestionAsked,
                        model_revision: plan.model_revision,
                        user_revision: plan.user_revision,
                        overall_comment: None,
                        annotation: Vec::new(),
                        question: Some(question.clone()),
                        answer: None,
                        created_at_ms: self.clock.now_ms(),
                    };
                    self.store.save_plan_lifecycle(&lifecycle)?;
                    event.push(self.event(
                        "plan_question",
                        json!({ "plan": plan, "lifecycle": lifecycle, "question": question }),
                    )?);
                } else {
                    plan.updated_at_ms = self.clock.now_ms();
                    self.store.save_plan(&plan)?;
                    self.record_plan_trace(
                        "resolved_plan_question_suppressed",
                        plan_trace_fields(&plan),
                    );
                }
            } else {
                plan.updated_at_ms = self.clock.now_ms();
                self.store.save_plan(&plan)?;
                self.record_plan_trace(
                    "plan_turn_without_terminal_control",
                    plan_trace_fields(&plan),
                );
            }
        } else {
            if let Some(question) = output.plan_question.take() {
                let question = question.normalize()?;
                if !self.active_plan_awaits_input()? {
                    interaction.node_list.push(ExchangeNode::QuestionPresented {
                        id: format!("{}:question:{}", interaction.id, Uuid::new_v4()),
                        question: question.clone(),
                        answer: None,
                    });
                }
                if !self.replace_active_elicitation(question.clone(), &mut event)? {
                    interaction.set_awaiting_input(true);
                    let mut elicitation = PlanElicitation::new(question.clone());
                    elicitation.start_timeout(
                        self.permission_coordinator
                            .input_timeouts(&self.session.id)?
                            .1,
                        self.clock.now_ms(),
                    )?;
                    interaction.set_elicitation(Some(elicitation));
                    self.store.save_exchange(&interaction)?;
                    event.push(self.event(
                        "question",
                        json!({
                            "owner": "interaction",
                            "plan_id": interaction.plan_id,
                            "exchange_id": interaction.id,
                            "elicitation": interaction.elicitation(),
                        }),
                    )?);
                }
            } else if let Some(answer) = output.question_answer.take() {
                self.answer_active_elicitation_from_model(answer, &mut event)?;
            } else if let Some(withdrawal) = output.question_withdrawal.take() {
                self.withdraw_active_elicitation(withdrawal, &mut event)?;
            }
        }

        if refresh_elicitation {
            let owner = self
                .store
                .load_exchange_metadata(&interaction.id)?
                .context("question control lost its owning exchange")?;
            interaction.set_elicitation(owner.elicitation().cloned());
            interaction.set_awaiting_input(owner.awaiting_input());
        }
        let execution_paused = interaction.awaiting_input();
        if mode == PromptMode::ExecutePlan {
            output.evidence.structured_complete = false;
            output.evidence.tool_called = false;
            output.evidence.workspace_changed = self.observe_plan_progress(&interaction)?;
        }
        self.session.updated_at_ms = self.clock.now_ms();
        self.save_session()?;
        let continuing = if execution_paused {
            false
        } else {
            Box::pin(self.apply_goal_evidence(output.evidence, &mut event)).await?
        };
        let mut runtime = self
            .exchange_runtime
            .take()
            .context("active interaction timeline is missing")?;
        finish_synthetic_turn(
            &mut interaction,
            runtime.synthetic_turn.as_ref(),
            crate::turn::TurnOutcome::Completed,
            self.clock.now_ms(),
        )?;
        runtime.synthetic_turn = None;
        let planning_open = interaction
            .plan_id
            .as_deref()
            .map(|id| self.store.load_plan(id))
            .transpose()?
            .flatten()
            .is_some_and(|plan| {
                matches!(
                    plan.state,
                    PlanState::Generating | PlanState::Revising | PlanState::AwaitingInput
                )
            });
        let goal_state = interaction
            .goal_id
            .as_deref()
            .map(|goal_id| self.store.load_goal(goal_id))
            .transpose()?
            .flatten()
            .map(|goal| goal.state);
        let goal_suspended = matches!(
            goal_state,
            Some(GoalState::Paused | GoalState::UsageLimited | GoalState::BudgetLimited)
        );
        let phase_finished = interaction
            .execution_phase
            .as_ref()
            .is_some_and(|phase| phase.outcome.is_some());
        let remains_open = !phase_finished
            && (continuing || goal_suspended || interaction.awaiting_input() || planning_open);
        if remains_open {
            self.exchange_runtime = Some(runtime);
        } else {
            let outcome = if goal_state == Some(GoalState::Cleared) {
                crate::exchange::ExchangeOutcome::Cancelled
            } else {
                crate::exchange::ExchangeOutcome::Complete
            };
            Box::pin(self.capture_final_checkpoint(&mut interaction, outcome)).await?;
            interaction.finish(outcome, self.clock.now_ms())?;
        }
        self.store.save_exchange(&interaction)?;
        let can_finalize_incrementally = !continuing
            && mode == PromptMode::Chat
            && self.current_plan_id()?.is_none()
            && interaction.plan_id.is_none()
            && interaction.execution_id.is_none()
            && interaction.elicitation().is_none()
            && !interaction.awaiting_input()
            && !self.agent_registry.list().iter().any(|run| {
                self.agent_registry
                    .execution(&run.id)
                    .and_then(|execution| execution.parent_exchange_id.as_deref())
                    == Some(interaction.id.as_str())
            });
        if can_finalize_incrementally {
            self.active_wait_projection = None;
            let timeline_patch = self.reconcile_live_interaction(Some(&interaction), None)?;
            Box::pin(self.emit_timeline_patch(timeline_patch, &mut event)).await?;
            self.timeline_reconciled_after_dispatch = true;
        }
        event.push(self.event(
            if remains_open {
                "exchange_updated"
            } else {
                "exchange_complete"
            },
            json!({"id": interaction.id, "state": interaction.state()}),
        )?);
        Ok((
            json!({ "exchange": {"id": interaction.id, "state": interaction.state()}, "session": self.session, "capability": self.capability }),
            event,
        ))
    }
}
