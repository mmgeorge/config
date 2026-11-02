use super::*;

/// Own a foreground request across admission, native execution and host settlement.
pub(crate) enum ForegroundDispatch {
    Admitting(Box<ForegroundAdmission>),
    Running {
        transaction: Box<ForegroundTransaction>,
        admission: Box<ForegroundAdmission>,
    },
    Finalizing {
        transaction: Box<ForegroundTransaction>,
        admission: Box<ForegroundAdmission>,
    },
    Done(DispatchResult),
    /// An internal, synchronous ownership transfer between dispatch phases.
    Transitioning,
}

/// Retain the original request and rollback authority while admission is staged.
pub(crate) struct ForegroundAdmission {
    pub(crate) request: Request,
    pub(crate) sink: Option<publication::SessionUpdateSink>,
    pub(crate) submission: Option<PromptSubmission>,
    pub(crate) text: String,
    attachments: Vec<crate::attachment::InputAttachment>,
    pub(crate) purpose: crate::input::InputPurpose,
    pub(crate) command: Option<crate::task::command::SessionCommand>,
    pub(crate) command_result: Option<Result<(Value, Vec<SessionEvent>)>>,
    pub(crate) command_response: Option<DispatchResult>,
    pub(crate) command_delivery: Option<PendingProviderDelivery>,
    pub(crate) admission_published: bool,
    task_preparation: Option<(crate::task::TaskPermit, crate::task::ExecutionEntry)>,
    task_catalog_done: bool,
    task_compaction: Option<TaskCompaction>,
    task_compaction_job: Option<TaskCompactionJob>,
    task_compacted: bool,
    task_compaction_failure: Option<anyhow::Error>,
    pub(crate) workspace_prepared: bool,
    pub(crate) automatic_check_owner: Option<String>,
    pub(crate) idle_reopen: bool,
    pub(crate) prior_finalization: Option<prior_finalization::PriorFinalization>,
    prior_checked: bool,
    pub(crate) question_catalog: Option<Vec<crate::provider::ProviderModel>>,
    pub(crate) question_discussion: Option<QuestionDiscussion>,
    pub(crate) clarification: Option<ClarificationContext>,
    pub(crate) planning: Option<PlanningContext>,
    pub(crate) reset_check_setup: bool,
    pub(crate) events: Vec<SessionEvent>,
    pub(crate) failure: Option<foreground_failure::ForegroundFailure>,
    pub(crate) staged_interaction: Option<StagedInteraction>,
    local_interaction: Option<(PromptMode, Option<ExchangeAdmission>, ProviderInput)>,
    pub(crate) provider_prepared: bool,
    pub(crate) engine: Option<Arc<crate::provider::session_execution::SessionExecution>>,
    phase: AdmissionPhase,
}

enum AdmissionPhase {
    Begin,
    Waiting,
    Catalog,
    TaskCatalog,
    PermissionPublishing,
    AdmissionForwarding,
    CompactStart,
    CompactRunning,
    CompactFinish,
    Workspace,
    Prior,
    ProviderPrepare,
    Publishing,
    Prepared,
}

impl ForegroundDispatch {
    /// Capture the publication owner for one short broker state step.
    pub(crate) fn publication_sink(&self) -> Result<publication::SessionUpdateSink> {
        match self {
            Self::Admitting(admission)
            | Self::Running { admission, .. }
            | Self::Finalizing { admission, .. } => admission
                .sink
                .clone()
                .context("foreground publication sink is detached"),
            Self::Done(_) | Self::Transitioning => {
                anyhow::bail!("foreground publication owner is unavailable")
            }
        }
    }

    /// Attach resolved files to this request before admission begins.
    pub(crate) fn set_attachments(
        &mut self,
        attachments: Vec<crate::attachment::InputAttachment>,
    ) -> Result<()> {
        let Self::Admitting(admission) = self else {
            anyhow::bail!("attachments arrived after admission");
        };
        anyhow::ensure!(
            matches!(admission.phase, AdmissionPhase::Begin),
            "attachments arrived after admission began"
        );
        admission.attachments = attachments;
        Ok(())
    }

    /// Poll the owned phase without retaining the broker lock.
    pub(crate) async fn next(&mut self) -> Result<ForegroundSignal> {
        match self {
            Self::Admitting(admission) if admission.failure.is_some() => {
                std::future::pending().await
            }
            Self::Admitting(admission) => match admission.phase {
                AdmissionPhase::Begin => {
                    admission.phase = AdmissionPhase::Waiting;
                    Ok(ForegroundSignal::AdmissionRequested)
                }
                AdmissionPhase::Prepared => {
                    admission.phase = AdmissionPhase::Waiting;
                    Ok(ForegroundSignal::AdmissionPrepared)
                }
                AdmissionPhase::Waiting
                | AdmissionPhase::Catalog
                | AdmissionPhase::TaskCatalog
                | AdmissionPhase::PermissionPublishing
                | AdmissionPhase::AdmissionForwarding
                | AdmissionPhase::CompactStart
                | AdmissionPhase::CompactRunning
                | AdmissionPhase::CompactFinish
                | AdmissionPhase::Workspace
                | AdmissionPhase::Prior
                | AdmissionPhase::ProviderPrepare
                | AdmissionPhase::Publishing => std::future::pending().await,
            },
            Self::Running { transaction, .. } => transaction.next().await,
            Self::Finalizing { transaction, .. } => {
                if transaction
                    .finalization
                    .as_ref()
                    .is_some_and(|finalization| finalization.goal_waiting_questions)
                {
                    transaction.question_enabled = true;
                }
                if !transaction.failure_cleanup_active
                    && !transaction.observation_publication_in_flight
                    && transaction.native_goal.is_none()
                    && !transaction.pending_session_observations.is_empty()
                {
                    Ok(ForegroundSignal::SessionObservationsReady)
                } else if !transaction.failure_cleanup_active
                    && !transaction.observation_publication_in_flight
                    && transaction.native_goal.is_none()
                    && !transaction.idle_native_active
                    && transaction.pending_session_observations.is_empty()
                    && transaction
                        .finalization
                        .as_mut()
                        .is_some_and(|finalization| finalization.take_effect_request())
                {
                    Ok(ForegroundSignal::FinalizeEffectsRequested)
                } else if !transaction.failure_cleanup_active
                    && !transaction.observation_publication_in_flight
                    && transaction.native_goal.is_none()
                    && !transaction.idle_native_active
                    && transaction.pending_session_observations.is_empty()
                    && transaction
                        .finalization
                        .as_mut()
                        .is_some_and(|finalization| finalization.take_check_request())
                {
                    Ok(ForegroundSignal::FinalizeChecksRequested)
                } else if !transaction.failure_cleanup_active
                    && !transaction.observation_publication_in_flight
                    && transaction.native_goal.is_none()
                    && !transaction.idle_native_active
                    && transaction.pending_session_observations.is_empty()
                    && transaction
                        .finalization
                        .as_mut()
                        .is_some_and(|finalization| finalization.take_check_continuation_request())
                {
                    Ok(ForegroundSignal::FinalizeCheckContinuationRequested)
                } else {
                    transaction.next().await
                }
            }
            Self::Done(_) => anyhow::bail!("settled foreground dispatch cannot be polled"),
            Self::Transitioning => anyhow::bail!("foreground dispatch is transferring ownership"),
        }
    }
}

/// Own catalog I/O while the broker remains available to session observers.
pub(crate) struct QuestionModelCatalogJob {
    provider: Arc<Provider>,
    request: ProviderRequest,
}

/// Own the provider readiness call independently of broker state.
pub(crate) struct ProviderPrepareJob {
    provider: Arc<Provider>,
    session_id: String,
    cancellation: Arc<TurnCancellation>,
    registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
    owner: crate::provider::ExecutionOwner,
}

/// Retain the session model until question discussion has completed.
pub(crate) struct QuestionDiscussion {
    pub(crate) visible_text: String,
    previous_model: Option<String>,
    previous_usage: Option<crate::session::ContextUsage>,
}

/// Preserve the task and document evidence used by planning postprocessing.
pub(crate) struct PlanningContext {
    pub(crate) plan_id: String,
    pub(crate) permit: crate::task::TaskPermit,
    pub(crate) before_version: u64,
    pub(crate) discussion: bool,
}

/// Retain the clarification owner for post-turn continuation.
pub(crate) enum ClarificationContext {
    Interaction,
    Plan { plan_id: String },
}

/// Own the admitted exchange and its submission until provider setup is complete.
pub(crate) struct StagedInteraction {
    pub(crate) exchange: Exchange,
    pub(crate) new_runtime: bool,
    pub(crate) mode: PromptMode,
    pub(crate) input: ProviderInput,
    pub(crate) submission: Arc<crate::provider::submission::Submission>,
    pub(crate) events: Vec<SessionEvent>,
    pub(crate) effects: Vec<AppliedProviderEvent>,
}

impl ForegroundAdmission {
    /// Transfer rollback authority exactly once into the host finalization envelope.
    pub(crate) fn take_submission(&mut self) -> Result<PromptSubmission> {
        self.submission
            .take()
            .context("prompt submission authority already transferred")
    }

    /// Validate source-work admission and decide whether question selection needs I/O.
    pub(crate) fn requires_question_catalog(&self, broker: &HarnessBroker) -> Result<bool> {
        if self.command.is_some() {
            return Ok(false);
        }
        anyhow::ensure!(
            broker.check_runner.active()?.is_none(),
            "wait for or cancel the active check run before starting source work"
        );
        if !matches!(self.purpose, crate::input::InputPurpose::Question { .. })
            && !broker.questions.snapshot()?.required_pending()
        {
            return Ok(false);
        }
        let selection = crate::settings::SettingsStore::read_only(&broker.data_root)?
            .get(
                &broker.session.workspace,
                &broker.session.id,
                &broker.session.provider,
            )?
            .ask_model()?;
        Ok(selection.model.is_some())
    }
}

impl QuestionModelCatalogJob {
    pub(crate) async fn run(self) -> Result<Vec<crate::provider::ProviderModel>> {
        self.provider.model_list(self.request).await
    }
}

impl ProviderPrepareJob {
    pub(crate) async fn run(
        self,
    ) -> Result<Arc<crate::provider::session_execution::SessionExecution>> {
        let engine = self.registry.owner(self.owner).await?;
        tokio::select! {
            biased;
            () = self.cancellation.cancelled() => Err(TurnCancelled.into()),
            result = self.provider.prepare_turn(&self.session_id) => { result?; Ok(engine) },
        }
    }
}

impl HarnessBroker {
    /// Preserve a local caller's explicit mode, input and exchange admission.
    pub(super) fn begin_local_interaction_dispatch(
        &mut self,
        text: String,
        mode: PromptMode,
        display: Option<ExchangeAdmission>,
        input: ProviderInput,
        sink: publication::SessionUpdateSink,
    ) -> Result<ForegroundDispatch> {
        let request = Request {
            id: 0,
            method: "session.prompt.submit".into(),
            params: json!({"text":text}),
        };
        let mut dispatch = self.begin_foreground_dispatch(request, sink)?;
        let ForegroundDispatch::Admitting(admission) = &mut dispatch else {
            unreachable!()
        };
        admission.command = None;
        admission.automatic_check_owner = display
            .as_ref()
            .and_then(|display| display.automatic_owner.clone());
        admission.local_interaction = Some((mode, display, input));
        if matches!(mode, PromptMode::Plan | PromptMode::PlanDiscussion) {
            let task = self
                .task_controller
                .current()?
                .context("planning requires an active task")?;
            admission.planning = Some(self.capture_planning_context(&task)?);
        }
        admission.phase = AdmissionPhase::Prepared;
        Ok(dispatch)
    }

    /// Admit a review answer with its original display text and restricted prompt mode.
    pub(crate) fn begin_review_question_dispatch(
        &mut self,
        prompt: String,
        display: String,
        sink: publication::SessionUpdateSink,
    ) -> Result<ForegroundDispatch> {
        let request = Request {
            id: 0,
            method: "session.prompt.submit".into(),
            params: json!({"text":prompt}),
        };
        let mut dispatch = self.begin_foreground_dispatch(request, sink)?;
        let ForegroundDispatch::Admitting(admission) = &mut dispatch else {
            unreachable!()
        };
        admission.local_interaction = Some((
            PromptMode::PlanQuestion,
            Some(ExchangeAdmission::chat(display)),
            ProviderInput::from_text(admission.text.clone()),
        ));
        admission.phase = AdmissionPhase::Prepared;
        Ok(dispatch)
    }

    /// Transfer an already-authorized permission continuation into the owned prompt driver.
    pub(crate) fn begin_permission_resume_dispatch(
        &mut self,
        text: String,
        sink: publication::SessionUpdateSink,
    ) -> Result<Option<ForegroundDispatch>> {
        anyhow::ensure!(
            self.prompt_admission.is_none(),
            "another prompt admission is active"
        );
        let previous_purpose = std::mem::take(&mut self.input_purpose);
        let previous_submission = self.input_submission.take();
        let result = (|| {
            let request = Request {
                id: 0,
                method: "session.prompt.submit".into(),
                params: json!({"text":text.clone()}),
            };
            let mut dispatch = self.begin_foreground_dispatch(request, sink)?;
            let resume = self.prepare_permission_resume(text)?;
            let ForegroundDispatch::Admitting(admission) = &mut dispatch else {
                unreachable!()
            };
            match resume {
                configuration::PermissionResume::NoWork => Ok(None),
                configuration::PermissionResume::Staged {
                    interaction,
                    clarification,
                    planning,
                } => {
                    admission.staged_interaction = Some(*interaction);
                    admission.clarification = clarification;
                    admission.planning = planning;
                    admission.phase = AdmissionPhase::Prepared;
                    Ok(Some(dispatch))
                }
            }
        })();
        self.input_purpose = previous_purpose;
        self.input_submission = previous_submission;
        result
    }

    /// Start the owned exchange runtime after previous finalization has drained.
    pub(crate) fn activate_staged_runtime(
        &mut self,
        admission: &mut ForegroundAdmission,
    ) -> Result<()> {
        anyhow::ensure!(
            self.store
                .exchange_in_state(&self.session.id, ExchangeState::Finalizing)?
                .is_none(),
            "previous exchange finalization is still pending"
        );
        if self.turn_cancellation.requested.load(Ordering::Acquire) {
            return Err(TurnCancelled.into());
        }
        let staged = admission
            .staged_interaction
            .as_mut()
            .context("interaction admission is missing")?;
        if staged.new_runtime {
            self.begin_exchange_runtime(&mut staged.exchange, self.clock.now_ms())?;
            staged.new_runtime = false;
        } else if self
            .exchange_runtime
            .as_ref()
            .is_none_or(|runtime| runtime.exchange_id != staged.exchange.id)
        {
            self.resume_exchange_runtime(&staged.exchange)?;
        }
        Ok(())
    }

    /// Schedule provider preparation only after prior exchange finalization and runtime setup.
    pub(crate) fn start_foreground_provider_prepare(
        &mut self,
        admission: &mut ForegroundAdmission,
    ) -> Result<ForegroundAction> {
        anyhow::ensure!(
            matches!(admission.phase, AdmissionPhase::Waiting),
            "provider preparation is not ready"
        );
        let staged = admission
            .staged_interaction
            .as_ref()
            .context("interaction admission is missing")?;
        anyhow::ensure!(
            self.store
                .exchange_in_state(&self.session.id, ExchangeState::Finalizing)?
                .is_none(),
            "previous exchange finalization is still pending"
        );
        let runtime = self
            .exchange_runtime
            .as_ref()
            .context("interaction runtime is missing")?;
        anyhow::ensure!(
            runtime.exchange_id == staged.exchange.id,
            "provider preparation crosses exchange ownership"
        );
        if let Some(permit) = &runtime.task_permit {
            permit.validate(self.task_controller.current()?.as_ref())?;
        }
        let job = self.prepare_provider_turn_job();
        admission.phase = AdmissionPhase::ProviderPrepare;
        Ok(ForegroundAction::Effect(ForegroundJob::ProviderPrepare(
            job,
        )))
    }

    /// Capture provider readiness after earlier exchange finalization is complete.
    pub(crate) fn prepare_provider_turn_job(&mut self) -> ProviderPrepareJob {
        let registry = self.execution_registry();
        ProviderPrepareJob {
            provider: self.provider.clone(),
            session_id: self.session.id.clone(),
            cancellation: self.turn_cancellation.clone(),
            registry,
            owner: crate::provider::ExecutionOwner::root(&self.session.id),
        }
    }

    /// Allocate the native submission and apply the exchange's durable admission.
    pub(super) fn stage_foreground_interaction(
        &mut self,
        text: &str,
        mode: PromptMode,
        admission: Option<&ExchangeAdmission>,
        input: ProviderInput,
    ) -> Result<StagedInteraction> {
        let submission = match self.input_submission.take() {
            Some(submission) => submission,
            None => self.submission_store.prepare(
                &self.provider_request(input.clone(), mode)?,
                self.provider.descriptor().kind,
            )?,
        };
        let input_message_id = submission.record()?.input_message_id;
        let (exchange, new_runtime, events, effects) =
            self.stage_admit_interaction(text, mode, admission, input_message_id)?;
        Ok(StagedInteraction {
            exchange,
            new_runtime,
            mode,
            input,
            submission,
            events,
            effects,
        })
    }

    /// Select question discussion against the current question and model settings.
    pub(crate) fn prepare_question_discussion(
        &mut self,
        admission: &ForegroundAdmission,
    ) -> Result<Option<QuestionDiscussion>> {
        let snapshot = self.questions.snapshot()?;
        if !matches!(
            admission.purpose,
            crate::input::InputPurpose::Question { .. }
        ) && !snapshot.required_pending()
        {
            return Ok(None);
        }
        let selection = crate::settings::SettingsStore::read_only(&self.data_root)?
            .get(
                &self.session.workspace,
                &self.session.id,
                &self.session.provider,
            )?
            .ask_model()?;
        let catalog = match (&selection.model, &admission.question_catalog) {
            (Some(_), Some(catalog)) => catalog.as_slice(),
            (Some(_), None) => {
                anyhow::bail!("question model selection changed before catalog admission")
            }
            (None, _) => &[],
        };
        let selected = selection.resolve(&self.session, catalog)?;
        let visible_text = if let crate::input::InputPurpose::Question {
            question_id,
            message_kind,
            ..
        } = &admission.purpose
        {
            let question = snapshot
                .questions
                .iter()
                .find(|question| &question.id == question_id)
                .context("question discussion source is missing")?;
            let label = match message_kind {
                crate::question::QuestionMessageKind::Clarification => "Clarification",
                crate::question::QuestionMessageKind::Answer => "Answer",
                crate::question::QuestionMessageKind::Correction => "Correction",
                crate::question::QuestionMessageKind::Reply => "Reply",
            };
            format!("{label}: {}", question.content.header)
        } else {
            admission.text.clone()
        };
        let discussion = QuestionDiscussion {
            visible_text,
            previous_model: self.session.resolved_model.clone(),
            previous_usage: self.session.context_usage.clone(),
        };
        self.question_model = Some(selected);
        self.question_discussion = true;
        Ok(Some(discussion))
    }

    /// Restore session model state after the question interaction settles.
    pub(crate) fn finish_question_discussion(
        &mut self,
        discussion: QuestionDiscussion,
    ) -> Result<()> {
        self.question_discussion = false;
        self.question_model = None;
        self.session.resolved_model = discussion.previous_model;
        self.session.context_usage = discussion.previous_usage;
        self.save_session()
    }

    /// Apply one admission result under the broker lock and schedule owned I/O.
    pub(crate) fn step_foreground_admission(
        &mut self,
        admission: &mut ForegroundAdmission,
        signal: ForegroundSignal,
    ) -> Result<ForegroundAction> {
        match signal {
            ForegroundSignal::AdmissionRequested => {
                anyhow::ensure!(
                    matches!(admission.phase, AdmissionPhase::Waiting),
                    "unexpected admission request"
                );
                if admission.staged_interaction.is_some() {
                    admission.phase = AdmissionPhase::Prepared;
                    return Ok(ForegroundAction::Wait);
                }
                if let Some(command) = admission.command.take() {
                    admission.command_result = Some(self.commit_session_command(command));
                    admission.phase = AdmissionPhase::Prepared;
                    return Ok(ForegroundAction::Wait);
                }
                if matches!(&admission.purpose, crate::input::InputPurpose::User)
                    && admission.text.trim() == "/plan"
                {
                    admission.command_result = Some(self.observer.list_replanning_choices());
                    admission.phase = AdmissionPhase::Prepared;
                    return Ok(ForegroundAction::Wait);
                }
                if matches!(&admission.purpose, crate::input::InputPurpose::User)
                    && self
                        .current_plan_id()?
                        .as_deref()
                        .map(|id| self.store.load_plan(id))
                        .transpose()?
                        .flatten()
                        .is_some_and(|plan| plan.state == PlanState::ConfiguringChecks)
                    && self.find_active_interaction_elicitation()?.is_none()
                    && !self.questions.snapshot()?.required_pending()
                {
                    let snapshot = serde_json::to_value(self.snapshot()?)?;
                    let result = self.advance_configured_plan_now((snapshot, Vec::new()))?;
                    let still_configuring = self
                        .current_plan_id()?
                        .as_deref()
                        .map(|id| self.store.load_plan(id))
                        .transpose()?
                        .flatten()
                        .is_some_and(|plan| plan.state == PlanState::ConfiguringChecks);
                    if !still_configuring {
                        admission.command_result = Some(Ok(result));
                        admission.phase = AdmissionPhase::Prepared;
                        return Ok(ForegroundAction::Wait);
                    }
                }
                if admission.requires_question_catalog(self)? {
                    let job = self.prepare_question_model_catalog()?;
                    admission.phase = AdmissionPhase::Catalog;
                    return Ok(ForegroundAction::Effect(ForegroundJob::QuestionCatalog(
                        Box::new(job),
                    )));
                }
                admission.phase = AdmissionPhase::Prepared;
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::QuestionCatalog(result)
                if matches!(admission.phase, AdmissionPhase::TaskCatalog) =>
            {
                let catalog = result?;
                let (permit, preparation) = admission
                    .task_preparation
                    .as_ref()
                    .context("task preparation is missing")?;
                permit.validate(self.task_controller.current()?.as_ref())?;
                let model = preparation
                    .executor
                    .model
                    .as_deref()
                    .context("plan executor model is not configured")?;
                let effort = preparation
                    .executor
                    .effort
                    .as_deref()
                    .context("plan executor thinking is not configured")?;
                let selected = catalog
                    .iter()
                    .find(|candidate| candidate.id == model)
                    .with_context(|| {
                        format!("plan executor model {model} is unavailable for this provider")
                    })?;
                anyhow::ensure!(
                    selected.reasoning.is_empty()
                        || selected
                            .reasoning
                            .iter()
                            .any(|candidate| candidate == effort),
                    "plan executor thinking {effort} is unavailable for model {model}"
                );
                admission.task_catalog_done = true;
                admission.phase = AdmissionPhase::Prepared;
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::QuestionCatalog(result) => {
                anyhow::ensure!(
                    matches!(admission.phase, AdmissionPhase::Catalog),
                    "unexpected question catalog"
                );
                let catalog = result?;
                if admission.requires_question_catalog(self)? {
                    admission.question_catalog = Some(catalog);
                }
                admission.phase = AdmissionPhase::Prepared;
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::AdmissionEventsForwarded(result) => {
                anyhow::ensure!(
                    matches!(admission.phase, AdmissionPhase::AdmissionForwarding),
                    "unexpected admission event acknowledgment"
                );
                result?;
                admission.phase = AdmissionPhase::Prepared;
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::PermissionPublished(result) => {
                anyhow::ensure!(
                    matches!(admission.phase, AdmissionPhase::PermissionPublishing),
                    "unexpected permission publication"
                );
                admission.events.extend(result?);
                admission.phase = AdmissionPhase::Prepared;
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::PublicationDelivered(events)
                if matches!(
                    admission.phase,
                    AdmissionPhase::CompactStart | AdmissionPhase::CompactFinish
                ) =>
            {
                admission.events.extend(events);
                if admission.failure.is_some() {
                    return Ok(ForegroundAction::Wait);
                }
                if matches!(admission.phase, AdmissionPhase::CompactFinish) {
                    if let Some(error) = admission.task_compaction_failure.take() {
                        return Err(error);
                    }
                    admission.task_compacted = true;
                }
                admission.phase = AdmissionPhase::Prepared;
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::PriorObservationReady(result) => {
                anyhow::ensure!(
                    matches!(admission.phase, AdmissionPhase::Prior),
                    "unexpected prior observation"
                );
                if let Err(error) = result {
                    return Err(self.abort_admission_prior(admission, error));
                }
                admission.phase = AdmissionPhase::Prepared;
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::PriorInitialCompleted(result) => {
                anyhow::ensure!(
                    matches!(admission.phase, AdmissionPhase::Prior),
                    "unexpected prior checkpoint"
                );
                let applied = self.apply_prior_initial(
                    admission
                        .prior_finalization
                        .as_mut()
                        .context("prior finalization missing")?,
                    result,
                );
                if let Err(error) = applied {
                    return Err(self.abort_admission_prior(admission, error));
                }
                admission.phase = AdmissionPhase::Prepared;
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::PriorFinalCompleted(result) => {
                anyhow::ensure!(
                    matches!(admission.phase, AdmissionPhase::Prior),
                    "unexpected prior final capture"
                );
                let outcome = self.apply_prior_final(
                    admission
                        .prior_finalization
                        .as_mut()
                        .context("prior finalization missing")?,
                    result,
                    &mut admission.events,
                );
                match outcome {
                    Ok(action) => self.schedule_prior_action(admission, action),
                    Err(error) => Err(self.abort_admission_prior(admission, error)),
                }
            }
            ForegroundSignal::AdmissionWorkspaceCompleted(result) => {
                anyhow::ensure!(
                    matches!(admission.phase, AdmissionPhase::Workspace),
                    "unexpected workspace preparation result"
                );
                self.complete_foreground_workspace(admission, *result)?;
                admission.phase = AdmissionPhase::Prepared;
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::TaskCompacted(result) => {
                anyhow::ensure!(
                    matches!(admission.phase, AdmissionPhase::CompactRunning),
                    "unexpected task compaction result"
                );
                let state = admission
                    .task_compaction
                    .take()
                    .context("task compaction state is missing")?;
                let (delivery, failure) =
                    self.apply_task_compaction(state, *result, admission.failure.is_none())?;
                admission.task_compaction_failure = failure;
                admission.phase = AdmissionPhase::CompactFinish;
                Ok(ForegroundAction::Effect(ForegroundJob::Publication(
                    delivery,
                )))
            }
            ForegroundSignal::CommandEventsForwarded(result) => {
                anyhow::ensure!(
                    matches!(admission.phase, AdmissionPhase::Publishing),
                    "unexpected command event acknowledgment"
                );
                result?;
                let delivery = admission
                    .command_delivery
                    .take()
                    .context("command acceptance delivery is missing")?;
                Ok(ForegroundAction::Effect(ForegroundJob::CommandPublication(
                    delivery,
                )))
            }
            ForegroundSignal::CommandPublished(result) => {
                anyhow::ensure!(
                    matches!(admission.phase, AdmissionPhase::Publishing)
                        && admission.command_response.is_some(),
                    "unexpected command publication"
                );
                admission.events.extend(result?);
                admission.admission_published = true;
                admission.phase = AdmissionPhase::Prepared;
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::PublicationDelivered(events) => {
                anyhow::ensure!(
                    matches!(admission.phase, AdmissionPhase::Publishing),
                    "unexpected admission publication"
                );
                admission.events.extend(events);
                admission.admission_published = true;
                admission.phase = AdmissionPhase::Prepared;
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::ProviderPrepared(result) => {
                anyhow::ensure!(
                    matches!(admission.phase, AdmissionPhase::ProviderPrepare),
                    "unexpected provider preparation"
                );
                let engine = result?;
                if admission.failure.is_some() {
                    admission.engine = Some(engine);
                    return Ok(ForegroundAction::Wait);
                }
                let staged = admission
                    .staged_interaction
                    .as_ref()
                    .context("interaction admission is missing")?;
                let runtime = self
                    .exchange_runtime
                    .as_ref()
                    .context("interaction runtime is missing")?;
                anyhow::ensure!(
                    runtime.exchange_id == staged.exchange.id,
                    "provider preparation owner changed"
                );
                if let Some(permit) = &runtime.task_permit {
                    permit.validate(self.task_controller.current()?.as_ref())?;
                }
                admission.provider_prepared = true;
                admission.engine = Some(engine);
                admission.phase = AdmissionPhase::Prepared;
                Ok(ForegroundAction::Wait)
            }
            _ => anyhow::bail!("unexpected signal during foreground admission"),
        }
    }

    /// Capture the catalog request required by question-model selection.
    pub(crate) fn prepare_question_model_catalog(&self) -> Result<QuestionModelCatalogJob> {
        Ok(QuestionModelCatalogJob {
            provider: self.provider.clone(),
            request: self.provider_request(ProviderInput::from_text(""), PromptMode::Chat)?,
        })
    }

    /// Match legacy command routing, which does not resolve unused file attachments.
    pub(crate) fn foreground_requires_attachments(&self, request: &Request) -> Result<bool> {
        if !matches!(self.input_purpose, crate::input::InputPurpose::User) {
            return Ok(true);
        }
        let text = required_text(&request.params, "text")?;
        let operation = request
            .params
            .get("operation_id")
            .and_then(Value::as_str)
            .or(self.active_operation_id.as_deref());
        Ok(crate::task::command::SessionCommand::parse_text(&text, operation)?.is_none())
    }

    /// Capture prompt admission without retaining a broker borrow across external work.
    pub(crate) fn begin_foreground_dispatch(
        &mut self,
        request: Request,
        sink: publication::SessionUpdateSink,
    ) -> Result<ForegroundDispatch> {
        let scoped = sink.clone();
        self.with_publication_sink(&scoped, |broker| {
            broker.begin_foreground_dispatch_inner(request, sink)
        })
    }

    fn begin_foreground_dispatch_inner(
        &mut self,
        request: Request,
        sink: publication::SessionUpdateSink,
    ) -> Result<ForegroundDispatch> {
        anyhow::ensure!(
            RuntimeMethod::decode(&request.method)? == RuntimeMethod::SessionPromptSubmit,
            "foreground dispatch requires a prompt submission"
        );
        self.require_restore_recovered()?;
        let text = required_text(&request.params, "text")?;
        let command = if matches!(self.input_purpose, crate::input::InputPurpose::User) {
            let operation_id = request
                .params
                .get("operation_id")
                .and_then(Value::as_str)
                .or(self.active_operation_id.as_deref());
            crate::task::command::SessionCommand::parse_text(&text, operation_id)?
        } else {
            None
        };
        let submission = self.begin_prompt_submission(&request.params)?;
        let purpose = std::mem::take(&mut self.input_purpose);
        Ok(ForegroundDispatch::Admitting(Box::new(
            ForegroundAdmission {
                request,
                sink: Some(sink),
                submission: Some(submission),
                text,
                attachments: Vec::new(),
                purpose,
                command,
                command_result: None,
                command_response: None,
                command_delivery: None,
                admission_published: false,
                task_preparation: None,
                task_catalog_done: false,
                task_compaction: None,
                task_compaction_job: None,
                task_compacted: false,
                task_compaction_failure: None,
                workspace_prepared: false,
                prior_finalization: None,
                prior_checked: false,
                question_catalog: None,
                question_discussion: None,
                clarification: None,
                planning: None,
                reset_check_setup: false,
                automatic_check_owner: None,
                idle_reopen: false,
                events: Vec::new(),
                staged_interaction: None,
                local_interaction: None,
                provider_prepared: false,
                engine: None,
                failure: None,
                phase: AdmissionPhase::Begin,
            },
        )))
    }

    /// Return only a fully settled foreground response.
    pub(crate) fn finish_foreground(
        &mut self,
        dispatch: ForegroundDispatch,
    ) -> Result<DispatchResult> {
        match dispatch {
            ForegroundDispatch::Done(result) => Ok(result),
            _ => anyhow::bail!("foreground dispatch has not settled"),
        }
    }
}

/// Own tool scopes and task provenance captured for an admitted provider turn.
pub(crate) struct ForegroundProviderContext {
    pub(crate) request: ProviderRequest,
    pub(crate) repository_lease: crate::tools::repository::RepositoryToolLease,
    pub(crate) inspection_lease: crate::tools::session::InspectionLease,
    pub(crate) lsp_lease: crate::tools::lsp::LspLease,
    pub(crate) task_permit: Option<crate::task::TaskPermit>,
    pub(crate) execution_id: Option<String>,
    pub(crate) entry_id: Option<crate::task::TaskInputId>,
}

impl HarnessBroker {
    /// Capture provider-facing tools and their task authority before releasing the broker.
    pub(crate) fn prepare_foreground_provider_context(
        &mut self,
        staged: &StagedInteraction,
    ) -> Result<ForegroundProviderContext> {
        let mut input = staged.input.clone();
        if staged.mode == PromptMode::PlanDiscussion {
            input = match input {
                ProviderInput::Text { text, attachments } => {
                    ProviderInput::from_text(PlanPrompt::discussion(&text))
                        .with_attachments(attachments)
                }
                ProviderInput::Skill {
                    name,
                    arguments,
                    attachments,
                } => ProviderInput::Skill {
                    name,
                    arguments: PlanPrompt::discussion(&arguments),
                    attachments,
                },
            };
        }
        if matches!(staged.mode, PromptMode::Plan | PromptMode::PlanDiscussion)
            && !input.text().contains("Active declaration design:")
        {
            let plan_id = self
                .current_plan_id()?
                .context("planning turn has no active plan")?;
            let document = self
                .plan_file
                .read_working_document(&self.session.id, &plan_id)?;
            let prefix = PlanPrompt::with_active_document(String::new(), &document.model_json()?);
            input = match input {
                ProviderInput::Text { text, attachments } => {
                    ProviderInput::from_text(format!("{prefix}{text}"))
                        .with_attachments(attachments)
                }
                ProviderInput::Skill {
                    name,
                    arguments,
                    attachments,
                } => ProviderInput::Skill {
                    name,
                    arguments: format!("{prefix}{arguments}"),
                    attachments,
                },
            };
        }
        if self.session.provider_session_id.is_none() {
            input = self.rollback_context(&staged.exchange.id, input)?;
        }
        let mut request = self.provider_request(input, staged.mode)?;
        request.submission = Some(staged.submission.clone());
        let owner = crate::provider::ExecutionOwner::root(&self.session.id);
        if let Some(feedback) = self.permission_coordinator.take_timeout_feedback(&owner)? {
            match &mut request.input {
                ProviderInput::Text { text, .. } => *text = format!("{feedback}\n\n{text}"),
                ProviderInput::Skill { arguments, .. } => {
                    *arguments = format!("{feedback}\n\n{arguments}")
                }
            }
        }
        let policy = request
            .control_context
            .as_ref()
            .and_then(|context| context.permission_snapshot.as_ref())
            .context("foreground permission authority is missing")?
            .compiled();
        let (repository, repository_lease) = crate::tools::repository::RepositoryToolScope::open(
            self.session.id.clone(),
            staged.exchange.id.clone(),
            PathBuf::from(&self.session.workspace),
            policy.clone(),
            Arc::clone(&self.turn_cancellation.requested),
            Arc::clone(&self.repositories),
            Arc::clone(&self.diff),
        )?;
        let (harness, inspection_lease) = crate::tools::session::HarnessInspectionScope::open(
            self.session.clone(),
            self.data_root.clone(),
            self.permission_store
                .read()
                .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?
                .open()
                .0,
            policy.clone(),
            Arc::clone(&self.turn_cancellation.requested),
            self.trace.clone(),
        )?;
        let (lsp, lsp_lease) = self.lsp.scope(
            self.session.id.clone(),
            staged.exchange.id.clone(),
            PathBuf::from(&self.session.workspace),
            policy,
            self.turn_cancellation.requested.clone(),
        );
        if let Some(context) = &mut request.control_context {
            context.lsp = Some(lsp);
            context.repository = Some(repository);
            context.harness = Some(harness);
            context.has_active_elicitation |= staged.exchange.elicitation().is_some();
        }
        let input_id = staged.submission.record()?.input_message_id;
        let task = self.task_controller.current()?.filter(|task| {
            (task.kind() != crate::task::TaskKind::Execute
                || staged.mode == PromptMode::ExecutePlan)
                && (task.entry_accepted()
                    || task
                        .entry
                        .as_ref()
                        .is_some_and(|entry| Some(entry.id.as_str()) == input_id.as_deref()))
        });
        let task_permit = task.as_ref().map(|task| task.permit());
        let execution_id = task.as_ref().and_then(|task| match &task.workflow {
            crate::task::WorkflowSnapshot::Execute { execution } => {
                Some(execution.record.id.clone())
            }
            _ => None,
        });
        let entry_id = task
            .as_ref()
            .and_then(|task| task.entry.as_ref())
            .map(|entry| entry.id.clone());
        self.trace.record(
            &self.session.id,
            "model.requested",
            serde_json::to_value(&request)?,
        );
        Ok(ForegroundProviderContext {
            request,
            repository_lease,
            inspection_lease,
            lsp_lease,
            task_permit,
            execution_id,
            entry_id,
        })
    }
}

impl HarnessBroker {
    /// Normalize a prompt into the same exchange mode and origin as legacy admission.
    fn normalize_foreground_interaction(
        &mut self,
        admission: &mut ForegroundAdmission,
    ) -> Result<StagedInteraction> {
        let text = admission.text.clone();
        if let Some(discussion) = self.prepare_question_discussion(admission)? {
            let visible = discussion.visible_text.clone();
            admission.question_discussion = Some(discussion);
            return self.stage_foreground_interaction(
                &visible,
                PromptMode::Chat,
                Some(&ExchangeAdmission::chat(visible.clone())),
                ProviderInput::from_text(text),
            );
        }
        if matches!(
            &admission.purpose,
            crate::input::InputPurpose::QuestionContinuation { .. }
        ) {
            anyhow::ensure!(
                self.task_controller.current()?.is_none(),
                "question continuation cannot replace an active task"
            );
            return self.stage_foreground_interaction(
                &text,
                PromptMode::Chat,
                Some(&ExchangeAdmission::chat(text.clone())),
                ProviderInput::from_text(text.clone()),
            );
        }
        let task_input = match &admission.purpose {
            crate::input::InputPurpose::TaskEntry {
                revision, start, ..
            } => Some((
                revision,
                crate::exchange::ExchangeOrigin::TaskStarted {
                    start: start.clone(),
                },
            )),
            crate::input::InputPurpose::TaskContinuation {
                revision, reason, ..
            } => Some((
                revision,
                crate::exchange::ExchangeOrigin::task_continuation(reason.clone()),
            )),
            _ => None,
        };
        if let Some((revision, origin)) = task_input {
            let task = self
                .task_controller
                .current()?
                .context("task input was cleared before dispatch")?;
            anyhow::ensure!(
                &task.revision == revision,
                "task input was replaced before dispatch"
            );
            let review = if matches!(&origin, crate::exchange::ExchangeOrigin::TaskStarted { .. }) {
                task.entry
                    .as_ref()
                    .and_then(|entry| entry.review_exchange.as_ref())
            } else {
                None
            };
            if let Some(expected) = review {
                let mut exchange = self
                    .store
                    .latest_exchange(&self.session.id)?
                    .context("review exchange is missing")?;
                anyhow::ensure!(
                    &exchange.id == expected,
                    "review exchange changed before revision dispatch"
                );
                exchange.revise_plan(self.clock.now_ms())?;
                self.store.save_exchange(&exchange)?;
            }
            let mut exchange_admission = ExchangeAdmission::chat(text.clone());
            exchange_admission.origin = origin;
            let mode = match &task.workflow {
                crate::task::WorkflowSnapshot::Plan { plan }
                    if plan.artifact.state == PlanState::ConfiguringChecks =>
                {
                    self.observer.set_check_editing(true)?;
                    self.check_setup = true;
                    admission.reset_check_setup = true;
                    PromptMode::Chat
                }
                crate::task::WorkflowSnapshot::Plan { plan }
                    if plan.artifact.state == PlanState::AwaitingReview =>
                {
                    PromptMode::PlanDiscussion
                }
                crate::task::WorkflowSnapshot::Plan { .. } => PromptMode::Plan,
                _ if task.kind() == crate::task::TaskKind::Execute => PromptMode::ExecutePlan,
                _ => PromptMode::GoalContinuation,
            };
            if matches!(mode, PromptMode::Plan | PromptMode::PlanDiscussion) {
                admission.planning = Some(self.capture_planning_context(&task)?);
            }
            return self.stage_foreground_interaction(
                &text,
                mode,
                review.is_none().then_some(&exchange_admission),
                ProviderInput::from_text(text.clone()),
            );
        }
        anyhow::ensure!(
            matches!(&admission.purpose, crate::input::InputPurpose::User),
            "unsupported prompt purpose"
        );
        anyhow::ensure!(
            !matches!(
                text.split_whitespace().next(),
                Some(
                    "/read"
                        | "/write"
                        | "/mode"
                        | "/ask"
                        | "/deny"
                        | "/allow"
                        | "/yolo"
                        | "/clear"
                        | "/plan"
                )
            ),
            "Use /read full, /write, /write full, /ask, /deny, /allow, or /yolo"
        );
        if self.task_controller.current()?.is_some_and(|task| {
            task.status() == crate::task::TaskStatus::Paused || !task.entry_accepted()
        }) {
            return self.stage_foreground_interaction(
                &text,
                PromptMode::Chat,
                Some(&ExchangeAdmission::chat(text.clone())),
                ProviderInput::parse(&text),
            );
        }
        let active_plan = self
            .current_plan_id()?
            .as_deref()
            .map(|id| self.store.load_plan(id))
            .transpose()?
            .flatten();
        anyhow::ensure!(
            !active_plan
                .as_ref()
                .is_some_and(|plan| plan.state == PlanState::Failed),
            "plan generation stopped; run /plan retry or /plan cancel"
        );
        if active_plan
            .as_ref()
            .is_some_and(|plan| plan.state == PlanState::AwaitingInput)
        {
            let mut plan = self.active_elicitation_plan()?;
            let elicitation = plan
                .elicitation
                .as_mut()
                .context("active plan has no elicitation state")?;
            let question_id = elicitation
                .current_question()
                .map(|question| question.id.clone())
                .unwrap_or_else(|| elicitation.question_set.id.clone());
            elicitation.begin_clarification(&question_id)?;
            let elicitation_json = serde_json::to_string_pretty(&elicitation)?;
            let target = crate::exchange::QuestionInput {
                set_id: elicitation.question_set.id.clone(),
                question_id: Some(
                    elicitation
                        .question(&question_id)
                        .or_else(|| elicitation.current_question())
                        .context("clarification has no pending question")?
                        .id
                        .clone(),
                ),
                answer: Vec::new(),
            };
            plan.updated_at_ms = self.clock.now_ms();
            self.append_plan_feedback(
                &plan.id,
                text.clone(),
                crate::exchange::InputIntent::Clarification,
                Some(target),
            )?;
            self.publish_plan(&plan)?;
            let prompt = PlanPrompt::clarification(&plan.request, &elicitation_json, &text);
            admission.clarification = Some(ClarificationContext::Plan {
                plan_id: plan.id.clone(),
            });
            return self.stage_foreground_interaction(
                &prompt,
                PromptMode::Chat,
                None,
                ProviderInput::from_text(prompt.clone()),
            );
        }
        if let Some(mut exchange) = self.find_active_interaction_elicitation()? {
            anyhow::ensure!(
                !self.active_plan_has_acceptance()?,
                "plan acceptance questions do not support clarification turns"
            );
            let elicitation = exchange
                .elicitation_mut()
                .context("active interaction has no elicitation state")?;
            let question_id = elicitation
                .current_question()
                .map(|question| question.id.clone())
                .unwrap_or_else(|| elicitation.question_set.id.clone());
            elicitation.begin_clarification(&question_id)?;
            let elicitation_json = serde_json::to_string_pretty(elicitation)?;
            let target = crate::exchange::QuestionInput {
                set_id: elicitation.question_set.id.clone(),
                question_id: Some(
                    elicitation
                        .question(&question_id)
                        .or_else(|| elicitation.current_question())
                        .context("clarification has no pending question")?
                        .id
                        .clone(),
                ),
                answer: Vec::new(),
            };
            exchange.append_question_input(
                crate::exchange::InputIntent::Clarification,
                text.clone(),
                target,
                self.clock.now_ms(),
            )?;
            self.store.save_exchange(&exchange)?;
            let prompt = PlanPrompt::question_follow_up(&elicitation_json, &text);
            admission.clarification = Some(ClarificationContext::Interaction);
            return self.stage_foreground_interaction(
                &prompt,
                self.task_prompt_mode()?,
                None,
                ProviderInput::from_text(prompt.clone()),
            );
        }
        if let Some(plan) = active_plan
            .as_ref()
            .filter(|plan| plan.state == PlanState::ConfiguringChecks)
        {
            let exchange_admission = ExchangeAdmission::chat(text.clone());
            self.observer.set_check_editing(true)?;
            self.check_setup = true;
            admission.reset_check_setup = true;
            let prompt = PlanPrompt::checks_setup(&plan.request);
            return self.stage_foreground_interaction(
                &prompt,
                PromptMode::Chat,
                Some(&exchange_admission),
                ProviderInput::from_text(prompt.clone()),
            );
        }
        let mode = if self.current_task_is_plan()?
            && active_plan.as_ref().is_some_and(|plan| {
                matches!(plan.state, PlanState::Generating | PlanState::Revising)
            }) {
            PromptMode::Plan
        } else if self.current_task_is_plan()?
            && active_plan
                .as_ref()
                .is_some_and(|plan| plan.state == PlanState::AwaitingReview)
        {
            PromptMode::PlanDiscussion
        } else {
            PromptMode::Chat
        };
        if mode == PromptMode::PlanDiscussion {
            let task = self
                .task_controller
                .current()?
                .context("planning requires an active task")?;
            admission.planning = Some(self.capture_planning_context(&task)?);
        }
        let prompt = match active_plan.as_ref() {
            Some(plan) if !plan.working_path.is_empty() => {
                let document = self
                    .plan_file
                    .read_working_document(&self.session.id, &plan.id)?;
                PlanPrompt::with_active_document(text.clone(), &document.model_json()?)
            }
            _ => text.clone(),
        };
        let input = match ProviderInput::parse(&text) {
            ProviderInput::Skill {
                name,
                mut arguments,
                attachments,
            } => {
                if let Some(prefix) = prompt
                    .strip_suffix(&text)
                    .filter(|prefix| !prefix.is_empty())
                {
                    arguments = format!("{prefix}{arguments}");
                }
                ProviderInput::Skill {
                    name,
                    arguments,
                    attachments,
                }
            }
            ProviderInput::Text { .. } => ProviderInput::from_text(prompt),
        };
        self.stage_foreground_interaction(
            &text,
            mode,
            Some(&ExchangeAdmission::chat(text.clone())),
            input,
        )
    }
}

impl HarnessBroker {
    /// Advance admission only after each owned presentation and preparation has completed.
    pub(crate) fn admit_prepared_foreground(
        &mut self,
        dispatch: &mut ForegroundDispatch,
    ) -> Result<ForegroundAction> {
        let ForegroundDispatch::Admitting(admission) = dispatch else {
            anyhow::bail!("foreground admission has no owner");
        };
        if admission.command_result.is_some() {
            let action = self.prepare_foreground_command(admission)?;
            admission.phase = AdmissionPhase::Publishing;
            return Ok(action);
        }
        if admission.command_response.is_some() && admission.admission_published {
            let mut response = admission
                .command_response
                .take()
                .expect("response checked above");
            response.event.append(&mut admission.events);
            *dispatch = ForegroundDispatch::Done(response);
            return Ok(ForegroundAction::Finish);
        }
        if admission.staged_interaction.is_none()
            && matches!(&admission.purpose, crate::input::InputPurpose::User)
            && !self.questions.snapshot()?.required_pending()
        {
            if admission.text == "/clear" {
                admission.command_result = Some(self.new_session_now(Value::Null));
                admission.phase = AdmissionPhase::Prepared;
                return Ok(ForegroundAction::Wait);
            }
            let change = match admission.text.as_str() {
                "/read full" => Some(crate::task::PermissionChange::ToggleReadScope),
                "/write" => Some(crate::task::PermissionChange::ToggleWriteEnabled),
                "/write full" => Some(crate::task::PermissionChange::ToggleWriteScope),
                "/ask" => Some(crate::task::PermissionChange::SetApproval {
                    mode: ApprovalMode::Ask,
                }),
                "/deny" => Some(crate::task::PermissionChange::SetApproval {
                    mode: ApprovalMode::Deny,
                }),
                "/allow" => Some(crate::task::PermissionChange::SetApproval {
                    mode: ApprovalMode::Allow,
                }),
                "/yolo" => Some(crate::task::PermissionChange::ToggleYolo),
                _ => None,
            };
            if let Some(change) = change {
                let captured = self.prepare_permission_configuration(&json!({"change": change}))?;
                let result = self.commit_permission_configuration(captured)?;
                anyhow::ensure!(
                    result.resume.is_none(),
                    "prompt permission unexpectedly requested a restart"
                );
                admission.command_result = Some(Ok((result.value, Vec::new())));
                admission.phase = AdmissionPhase::PermissionPublishing;
                return Ok(ForegroundAction::Effect(
                    ForegroundJob::PermissionPublication(result.delivery),
                ));
            }
        }
        if admission.staged_interaction.is_none() {
            if let Some(task) = self.task_controller.current()? {
                let input_id = self
                    .input_submission
                    .as_ref()
                    .map(|submission| submission.record())
                    .transpose()?
                    .and_then(|record| record.input_message_id);
                if let Some(preparation) = task
                    .entry
                    .as_ref()
                    .filter(|entry| input_id.as_deref() == Some(entry.id.as_str()))
                    .and_then(|entry| entry.execution.clone())
                {
                    let permit = task.permit();
                    if admission.task_preparation.is_none() {
                        admission.task_preparation = Some((permit.clone(), preparation.clone()));
                    }
                    let (captured, saved) = admission
                        .task_preparation
                        .as_ref()
                        .expect("preparation captured");
                    captured.validate(Some(&task))?;
                    anyhow::ensure!(saved == &preparation, "task execution preparation changed");
                    if preparation.executor.enabled && !admission.task_catalog_done {
                        anyhow::ensure!(
                            self.capability.model_selection,
                            "current provider does not support model selection"
                        );
                        let job = self.prepare_question_model_catalog()?;
                        admission.phase = AdmissionPhase::TaskCatalog;
                        return Ok(ForegroundAction::Effect(ForegroundJob::QuestionCatalog(
                            Box::new(job),
                        )));
                    }
                    if preparation.compact && !admission.task_compacted {
                        if let Some(job) = admission.task_compaction_job.take() {
                            admission.phase = AdmissionPhase::CompactRunning;
                            return Ok(ForegroundAction::Effect(ForegroundJob::TaskCompaction(
                                Box::new(job),
                            )));
                        }
                        let (state, job, delivery) =
                            self.prepare_task_compaction(permit.clone())?;
                        admission.task_compaction = Some(state);
                        admission.task_compaction_job = Some(job);
                        admission.phase = AdmissionPhase::CompactStart;
                        return Ok(ForegroundAction::Effect(ForegroundJob::Publication(
                            delivery,
                        )));
                    }
                    if preparation.executor.enabled {
                        self.session.model = preparation
                            .executor
                            .model
                            .clone()
                            .context("executor model is missing")?;
                        self.session.resolved_model = Some(self.session.model.clone());
                        self.session.effort = preparation
                            .executor
                            .effort
                            .clone()
                            .context("executor thinking is missing")?;
                        self.session.context_window = None;
                    }
                    self.session.approval_mode = preparation.approval_mode;
                    self.save_session()?;
                    self.task_controller.apply(
                        &mut self.store,
                        &permit,
                        crate::task::TaskEvent::EntryPreparationFinished,
                        self.clock.now_ms(),
                    )?;
                    admission.task_preparation = None;
                }
            }
            let previous = std::mem::replace(
                &mut self.submitted_attachments,
                admission.attachments.clone(),
            );
            let staged = if let Some((mode, display, input)) = admission.local_interaction.take() {
                self.stage_foreground_interaction(&admission.text, mode, display.as_ref(), input)
            } else {
                self.normalize_foreground_interaction(admission)
            };
            self.submitted_attachments = previous;
            let mut staged = staged?;
            if !admission.attachments.is_empty() {
                staged.input = staged.input.with_attachments(admission.attachments.clone());
            }
            admission.staged_interaction = Some(staged);
        }
        if !admission.admission_published {
            let staged = admission.staged_interaction.as_mut().expect("staged above");
            admission.events.append(&mut staged.events);
            let effects = std::mem::take(&mut staged.effects);
            let delivery =
                self.prepare_provider_delivery(AppliedProviderEvent::Effects(effects))?;
            admission.phase = AdmissionPhase::Publishing;
            return Ok(ForegroundAction::Effect(ForegroundJob::Publication(
                delivery,
            )));
        }
        if !admission.provider_prepared {
            if !admission.prior_checked {
                if admission.prior_finalization.is_none() {
                    admission.prior_finalization = self.begin_prior_finalization()?;
                }
                if let Some(state) = admission.prior_finalization.as_mut() {
                    let action = self.advance_prior_finalization(state, &mut admission.events);
                    return match action {
                        Ok(action) => self.schedule_prior_action(admission, action),
                        Err(error) => Err(self.abort_admission_prior(admission, error)),
                    };
                }
                admission.prior_checked = true;
            }
            if !admission.events.is_empty() {
                let events = std::mem::take(&mut admission.events);
                admission.phase = AdmissionPhase::AdmissionForwarding;
                return Ok(ForegroundAction::Effect(ForegroundJob::AdmissionEvents(
                    events,
                )));
            }
            self.activate_staged_runtime(admission)?;
            if !admission.workspace_prepared {
                if let Some(action) = self.prepare_foreground_workspace(admission)? {
                    admission.phase = AdmissionPhase::Workspace;
                    return Ok(action);
                }
                admission.workspace_prepared = true;
            }
            return self.start_foreground_provider_prepare(admission);
        }
        let engine = admission
            .engine
            .as_ref()
            .context("prepared provider engine is missing")?
            .clone();
        let staged = admission
            .staged_interaction
            .as_ref()
            .context("staged interaction is missing")?;
        let transaction = self.construct_foreground_transaction(staged, engine)?;
        admission.staged_interaction.take();
        admission.engine.take();
        let ForegroundDispatch::Admitting(admission) =
            std::mem::replace(dispatch, ForegroundDispatch::Transitioning)
        else {
            unreachable!();
        };
        *dispatch = ForegroundDispatch::Running {
            transaction: Box::new(transaction),
            admission,
        };
        Ok(ForegroundAction::Wait)
    }
}

/// Retain the durable compaction record and task authority while native work runs.
pub(crate) struct TaskCompaction {
    permit: crate::task::TaskPermit,
    compaction: crate::provider::Compaction,
    record: SessionEventRecord,
}

pub(crate) struct TaskCompactionJob {
    provider: Arc<Provider>,
    request: crate::provider::ProviderCompactionRequest,
}

impl TaskCompactionJob {
    pub(crate) async fn run(self) -> Result<crate::provider::ProviderOutput> {
        self.provider.compact(self.request).await
    }
}

impl HarnessBroker {
    /// Persist the start of task-entry compaction and return its unlocked native job.
    pub(crate) fn prepare_task_compaction(
        &mut self,
        permit: crate::task::TaskPermit,
    ) -> Result<(TaskCompaction, TaskCompactionJob, PendingProviderDelivery)> {
        permit.validate(self.task_controller.current()?.as_ref())?;
        anyhow::ensure!(
            self.capability.native_compact,
            "current provider does not support context compaction"
        );
        let thread_id = self
            .session
            .provider_session_id
            .clone()
            .context("current session has no provider conversation to compact")?;
        let now = self.clock.now_ms();
        let compaction = crate::provider::Compaction {
            id: Uuid::new_v4().to_string(),
            thread_id,
            state: crate::provider::CompactionState::Running,
            manual: true,
            started_at_ms: Some(now),
            completed_at_ms: None,
            result: crate::provider::CompactionResult {
                context_tokens_before: self.session.context_usage.as_ref().map(|usage| usage.used),
                ..Default::default()
            },
        };
        let record = SessionEventRecord {
            id: format!("{}:compaction:{}", self.session.id, compaction.id),
            session_id: self.session.id.clone(),
            owner: crate::timeline::TimelineOwner::session(&self.session.id),
            created_at_ms: now,
            detail: SessionEventKind::Compaction {
                compaction: compaction.clone().into(),
            },
        };
        let request = crate::provider::ProviderCompactionRequest {
            id: compaction.id.clone(),
            request: self.provider_request(ProviderInput::from_text(""), PromptMode::Chat)?,
        };
        self.store.save_session_event(&record)?;
        let patch = self.reconcile_timeline()?;
        let delivery = self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))?;
        Ok((
            TaskCompaction {
                permit,
                compaction,
                record,
            },
            TaskCompactionJob {
                provider: self.provider.clone(),
                request,
            },
            delivery,
        ))
    }

    /// Persist compaction result and fence task/session authority before applying native metadata.
    pub(crate) fn apply_task_compaction(
        &mut self,
        mut state: TaskCompaction,
        result: Result<crate::provider::ProviderOutput>,
        allow_metadata: bool,
    ) -> Result<(PendingProviderDelivery, Option<anyhow::Error>)> {
        state.compaction.completed_at_ms = Some(self.clock.now_ms());
        let output = match result {
            Ok(output) => output,
            Err(error) => {
                state.compaction.state = crate::provider::CompactionState::Failed;
                state.compaction.result.error = Some(format!("{error:#}"));
                state.record.detail = SessionEventKind::Compaction {
                    compaction: state.compaction.into(),
                };
                self.store.save_session_event(&state.record)?;
                let patch = self.reconcile_timeline()?;
                let delivery =
                    self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))?;
                return Ok((delivery, Some(error)));
            }
        };
        let authority = state
            .permit
            .validate(self.task_controller.current()?.as_ref())
            .and_then(|()| {
                anyhow::ensure!(
                    self.session.provider_session_id.as_deref()
                        == Some(state.compaction.thread_id.as_str()),
                    "provider conversation changed during task compaction"
                );
                Ok(())
            });
        let observed = output.event.iter().rev().find_map(|event| match event {
            ProviderEvent::Compaction { update, .. } => Some(update.clone()),
            _ => None,
        });
        let completion = output.completion;
        let before = state.compaction.result.context_tokens_before;
        if let Some(observed) = observed {
            state.compaction.state = observed.state;
            state.compaction.result = observed.result;
        } else {
            state.compaction.state = crate::provider::CompactionState::Completed;
        }
        state.compaction.result.context_tokens_before = before;
        if state.compaction.state == crate::provider::CompactionState::Completed {
            state.compaction.result.context_tokens_after = completion
                .native
                .metrics
                .context_usage
                .as_ref()
                .map(|usage| usage.used);
        }
        let failed = state.compaction.state == crate::provider::CompactionState::Failed;
        let failure = state
            .compaction
            .result
            .error
            .clone()
            .unwrap_or_else(|| "Provider compaction failed".into());
        state.record.detail = SessionEventKind::Compaction {
            compaction: state.compaction.into(),
        };
        self.store.save_session_event(&state.record)?;
        if let Err(error) = authority {
            let patch = self.reconcile_timeline()?;
            let delivery = self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))?;
            return Ok((delivery, Some(error)));
        }
        if !allow_metadata {
            let patch = self.reconcile_timeline()?;
            let delivery = self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))?;
            return Ok((delivery, None));
        }
        self.capability = completion.native.capability.clone();
        self.session.native_fork = completion.native.capability.native_fork;
        self.session.native_compact = completion.native.capability.native_compact;
        if let Some(id) = completion.native.provider_session_id {
            self.session.provider_session_id = Some(id.into());
        }
        if let Some(usage) = completion.native.metrics.context_usage {
            let size = self
                .session
                .context_usage
                .as_ref()
                .map_or(usage.size, |current| current.size);
            self.session.context_usage = ContextUsage::reported(usage.used, size);
        }
        if !completion.native.runtime.provider.is_empty() {
            self.session.provider_label = completion.native.runtime.provider;
        }
        if completion.native.runtime.model.is_some() {
            self.session.resolved_model = completion.native.runtime.model;
        }
        self.save_session()?;
        let snapshot = self.snapshot()?;
        let patch = self.reconcile_timeline()?;
        let mut delivery = self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))?;
        if failed {
            return Ok((delivery, Some(anyhow::anyhow!("{failure}"))));
        }
        delivery
            .events
            .push(self.event("context_compacted", serde_json::to_value(&snapshot)?)?);
        Ok((delivery, None))
    }
}

impl HarnessBroker {
    fn abort_admission_prior(
        &mut self,
        admission: &mut ForegroundAdmission,
        error: anyhow::Error,
    ) -> anyhow::Error {
        if let Some(mut prior) = admission.prior_finalization.take()
            && let Err(cleanup) = self.abort_prior_finalization(&mut prior, &error)
        {
            return error.context(format!("prior finalization cleanup failed: {cleanup:#}"));
        }
        error
    }

    fn schedule_prior_action(
        &mut self,
        admission: &mut ForegroundAdmission,
        action: prior_finalization::PriorFinalizationAction,
    ) -> Result<ForegroundAction> {
        use prior_finalization::PriorFinalizationAction;
        let job = match action {
            PriorFinalizationAction::Ready => {
                admission.prior_finalization = None;
                admission.prior_checked = true;
                admission.phase = AdmissionPhase::Prepared;
                return Ok(ForegroundAction::Wait);
            }
            PriorFinalizationAction::Wait(job) => ForegroundJob::PriorObservation(job),
            PriorFinalizationAction::Initial(job) => ForegroundJob::PriorInitial(job),
            PriorFinalizationAction::Final(job) => ForegroundJob::PriorFinal(job),
        };
        admission.phase = AdmissionPhase::Prior;
        Ok(ForegroundAction::Effect(job))
    }
}

impl HarnessBroker {
    /// Apply a task command that has no provider or filesystem wait, returning owned output.
    pub(crate) fn dispatch_immediate_task_command(
        &mut self,
        request: &Request,
    ) -> Result<DispatchResult> {
        anyhow::ensure!(
            RuntimeMethod::decode(&request.method)? == RuntimeMethod::SessionCommandApply,
            "immediate task dispatch requires a task command"
        );
        self.timeline_reconciled_after_dispatch = false;
        self.active_operation_id = request
            .params
            .get("operation_id")
            .and_then(Value::as_str)
            .map(str::to_owned);
        self.trace.record(
            &self.session.id,
            "lua.rpc.received",
            json!({
                "id": request.id, "method": request.method, "params": request.params,
            }),
        );
        let command = crate::task::command::SessionCommand::parse_request(request)?
            .context("task command requires an asynchronous transition")?;
        let result = self.commit_session_command(command);
        self.refresh_input_schedule()
            .context("publish input deadline")?;
        self.active_wait_projection = None;
        let patch = self.reconcile_after_dispatch()?;
        let (response, mut event) = match result {
            Ok((value, event)) => (Response::success(request.id, value)?, event),
            Err(error) => (
                Response::failure(
                    request.id,
                    "request_failed",
                    crate::storage::error_message(&error),
                ),
                Vec::new(),
            ),
        };
        if !patch.is_empty() {
            event.push(self.event("document_changed", patch.notification())?);
        }
        Ok(DispatchResult { response, event })
    }
}

impl HarnessBroker {
    pub(super) fn capture_planning_context(
        &self,
        task: &crate::task::Task,
    ) -> Result<PlanningContext> {
        let crate::task::WorkflowSnapshot::Plan { plan } = &task.workflow else {
            anyhow::bail!("planning requires a Plan task");
        };
        let plan_id = plan.artifact.id.clone();
        let before_version = self
            .plan_file
            .read_working_document(&self.session.id, &plan_id)?
            .version;
        Ok(PlanningContext {
            plan_id,
            permit: task.permit(),
            before_version,
            discussion: plan.artifact.state == PlanState::AwaitingReview,
        })
    }
}

impl HarnessBroker {
    /// Capture publications for one serialized state step and restore the previous owner.
    pub(crate) fn with_publication_sink<T>(
        &mut self,
        sink: &publication::SessionUpdateSink,
        apply: impl FnOnce(&mut Self) -> Result<T>,
    ) -> Result<T> {
        let previous = self.event_sink.replace(sink.clone());
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| apply(self)));
        self.event_sink = previous;
        match result {
            Ok(result) => result,
            Err(panic) => std::panic::resume_unwind(panic),
        }
    }
}

impl HarnessBroker {
    /// Record the external prompt boundary before attachment or admission work begins.
    pub(crate) fn trace_foreground_request(&mut self, request: &Request) {
        self.timeline_reconciled_after_dispatch = false;
        self.active_operation_id = None;
        self.trace.record(
            &self.session.id,
            "lua.rpc.received",
            json!({
                "id": request.id, "method": request.method, "params": request.params.clone(),
            }),
        );
        self.record_plan_trace(
            "request_received",
            json!({ "request_id": request.id, "method": request.method }),
        );
    }

    /// Record the final owned prompt response after its ordered events have been delivered.
    pub(crate) fn trace_foreground_response(&self, id: u64, result: &DispatchResult) {
        if let Some(failure) = result.response.error() {
            if failure.code == "turn_retracted" {
                return;
            }
            self.record_plan_trace(
                "request_failed",
                json!({ "request_id": id, "code": failure.code }),
            );
            self.trace.record(
                &self.session.id,
                "lua.rpc.failed",
                json!({
                    "id": id, "code": failure.code, "error": failure.message, "event": result.event,
                }),
            );
        } else {
            self.trace.record(
                &self.session.id,
                "lua.rpc.completed",
                json!({
                    "id": id, "response": result.response, "event": result.event,
                }),
            );
            self.record_plan_trace(
                "request_succeeded",
                json!({ "request_id": id, "event_count": result.event.len() }),
            );
        }
    }

    pub(crate) fn trace_foreground_error(&self, id: u64, error: &anyhow::Error) {
        if error.is::<TurnRetracted>() {
            return;
        }
        let code = if error.is::<RequestNotAdmitted>() {
            "not_admitted"
        } else if error.is::<TurnCancelled>() {
            "turn_cancelled"
        } else if error.is::<crate::provider::tool_timeout::ToolTimeout>() {
            "tool_timeout"
        } else {
            "request_failed"
        };
        self.record_plan_trace("request_failed", json!({ "request_id": id, "code": code }));
        self.trace.record(
            &self.session.id,
            "lua.rpc.failed",
            json!({
                "id": id, "code": code, "error": format!("{error:#}"), "event": [],
            }),
        );
    }
}

impl HarnessBroker {
    /// Stage approval-timeout feedback without borrowing broker state across provider work.
    pub(crate) fn begin_approval_timeout_dispatch(
        &mut self,
        sink: publication::SessionUpdateSink,
    ) -> Result<(Option<ForegroundDispatch>, PendingProviderDelivery)> {
        let owner = crate::provider::ExecutionOwner::root(&self.session.id);
        let feedback = self.permission_coordinator.take_timeout_feedback(&owner)?;
        let mut dispatch = None;
        if let Some(feedback) = feedback {
            if let Some(task) = self.task_controller.current()? {
                if matches!(
                    task.status(),
                    crate::task::TaskStatus::Running | crate::task::TaskStatus::Waiting
                ) {
                    let exchange = self
                        .store
                        .latest_exchange(&self.session.id)?
                        .context("task has no completed turn")?;
                    self.task_controller.reserve_continuation(
                        &mut self.store,
                        &task.permit(),
                        crate::task::TaskTurnId {
                            exchange_ordinal: exchange.ordinal,
                            turn_ordinal: exchange.turn.len(),
                        },
                        feedback,
                        "An approval timed out".into(),
                        self.clock.now_ms(),
                    )?;
                }
            } else {
                anyhow::ensure!(
                    self.prompt_admission.is_none(),
                    "another prompt admission is active"
                );
                let previous_purpose = std::mem::take(&mut self.input_purpose);
                let previous_submission = self.input_submission.take();
                let staged = (|| {
                    let request = Request {
                        id: 0,
                        method: "session.prompt.submit".into(),
                        params: json!({"text":feedback.clone()}),
                    };
                    let mut owned = self.begin_foreground_dispatch(request, sink.clone())?;
                    let mut admission = ExchangeAdmission::chat(feedback.clone());
                    admission.origin =
                        crate::exchange::ExchangeOrigin::task_continuation("An approval timed out");
                    let interaction = self.stage_foreground_interaction(
                        &feedback,
                        PromptMode::Chat,
                        Some(&admission),
                        ProviderInput::from_text(feedback.clone()),
                    )?;
                    let ForegroundDispatch::Admitting(state) = &mut owned else {
                        unreachable!()
                    };
                    state.staged_interaction = Some(interaction);
                    state.phase = AdmissionPhase::Prepared;
                    Ok::<_, anyhow::Error>(owned)
                })();
                self.input_purpose = previous_purpose;
                self.input_submission = previous_submission;
                dispatch = Some(staged?);
            }
        }
        self.refresh_input_schedule()?;
        let patch = self.reconcile_after_dispatch()?;
        let delivery = self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))?;
        Ok((dispatch, delivery))
    }
}

impl HarnessBroker {
    /// Apply the ordinary request completion boundary after unlocked owned work.
    pub(crate) fn finish_owned_external_request(
        &mut self,
        id: u64,
        method: RuntimeMethod,
        mut result: DispatchResult,
    ) -> DispatchResult {
        if let Err(error) = self.refresh_input_schedule() {
            result.response = Response::failure(
                id,
                "request_failed",
                format!("publish input deadline: {error:#}"),
            );
            result.event.clear();
        }
        if method.settles_task() {
            self.active_wait_projection = None;
        }
        match self.reconcile_timeline() {
            Ok(patch) if !patch.is_empty() => {
                match self.event("document_changed", patch.notification()) {
                    Ok(event) => result.event.push(event),
                    Err(error) if result.response.error().is_none() => {
                        result.response =
                            Response::failure(id, "timeline_patch_failed", error.to_string());
                        result.event.clear();
                    }
                    Err(_) => {}
                }
            }
            Err(error) if result.response.error().is_none() => {
                result.response =
                    Response::failure(id, "timeline_projection_failed", error.to_string());
                result.event.clear();
            }
            _ => {}
        }
        self.trace_foreground_response(id, &result);
        result
    }
}
