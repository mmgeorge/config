use super::*;

/// Describe how far a turn's provider send has progressed, for owners that must not stop it mid-send.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum DispatchStage {
    /// The journal still records the input as `Prepared`, so stopping the turn prevents the send.
    Unsent,
    /// The submission is running and the provider has not confirmed it. Stopping the turn now
    /// interrupts a send whose delivery is then unknown.
    InFlight,
    /// The send ended without confirmation, so the turn fails and its provider generation with it.
    Lost,
    /// The provider confirmed the send, or the turn has no send left.
    Settled,
}

/// Own a foreground request across admission, native execution and host settlement.
pub(crate) enum TurnTransaction {
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
    /// Retain this admission's native receipt instead of borrowing the broker's next-input slot.
    pub(super) input_submission: Option<Arc<crate::provider::submission::Submission>>,
    pub(crate) text: String,
    attachments: Vec<crate::attachment::InputAttachment>,
    pub(crate) purpose: crate::input::InputPurpose,
    pub(crate) command: Option<crate::task::command::SessionCommand>,
    pub(crate) command_result: Option<Result<(Value, Vec<SessionEvent>)>>,
    pub(crate) command_response: Option<DispatchResult>,
    pub(crate) command_delivery: Option<PendingProviderDelivery>,
    pub(crate) admission_published: bool,
    task_preparation: Option<(crate::task::TaskPermit, crate::task::ExecutionEntry)>,
    task_model_selection: Option<crate::settings::ValidatedModelSelection>,
    task_configuration: Option<TaskInvocationConfiguration>,
    task_compaction: Option<TaskCompaction>,
    task_compaction_job: Option<TaskCompactionJob>,
    task_compacted: bool,
    task_compaction_failure: Option<anyhow::Error>,
    pub(crate) workspace_prepared: bool,
    pub(crate) automatic_check_owner: Option<String>,
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
    collection: Option<AdmissionCollection>,
}

#[derive(Clone, Copy)]
enum AdmissionCollection {
    Collecting(ForegroundCollection),
    Collected(ForegroundCollection),
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

impl TurnTransaction {
    /// Capture the latest admitted invocation in this foreground's explicit execution chain.
    pub(crate) fn invocation_scope(
        &self,
    ) -> Result<Option<crate::provider::InvocationScope>, ProviderError> {
        let transaction = match self {
            Self::Running { transaction, .. } | Self::Finalizing { transaction, .. } => transaction,
            Self::Admitting(admission) => {
                return Ok(admission
                    .staged_interaction
                    .as_ref()
                    .and_then(|staged| staged.reservation.as_ref())
                    .map(|reservation| reservation.scope.clone()));
            }
            Self::Done(_) | Self::Transitioning => return Ok(None),
        };
        let scope = transaction
            .engine
            .active_invocation_scope()?
            .unwrap_or_else(|| transaction.state.scope.clone());
        if scope.owner() != transaction.state.scope.owner()
            || scope.exchange_id != transaction.state.scope.exchange_id
        {
            return Err(ProviderError::Failure(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnInterrupt,
                    forge_provider_api::ProviderFailureKind::WrongInvocation,
                ),
            ));
        }
        Ok(Some(scope))
    }

    /// Static diagnostic label; never includes command or provider payloads.
    pub(crate) fn diagnostic_name(&self) -> &'static str {
        match self {
            Self::Admitting(..) => "Admitting",
            Self::Running { .. } => "Running",
            Self::Finalizing { .. } => "Finalizing",
            Self::Done(..) => "Done",
            Self::Transitioning => "Transitioning",
        }
    }

    /// Admit steering only while this foreground can still consume native input.
    pub(crate) fn accepts_steering(&self) -> bool {
        matches!(self, Self::Running { transaction, .. } if transaction.accepts_steering())
    }

    /// Authorize native work only while the owner admits effects for this exact foreground.
    pub(crate) fn authorize_submission(&mut self) {
        match self {
            Self::Running { transaction, .. } | Self::Finalizing { transaction, .. } => {
                if transaction.collection.is_none() && !transaction.failure_cleanup_active {
                    transaction.native.authorize_submission();
                }
            }
            Self::Admitting(_) | Self::Done(_) | Self::Transitioning => {}
        }
    }

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

    /// Get how far this turn's provider send has progressed.
    pub(crate) fn dispatch_stage(&self) -> DispatchStage {
        match self {
            Self::Admitting(_) => DispatchStage::Unsent,
            Self::Running { transaction, .. } | Self::Finalizing { transaction, .. } => {
                match transaction.request.dispatch.observer().current() {
                    // Only the journal proves the adapter has not crossed the send boundary; a
                    // turn without a readable `Prepared` record may already be sending.
                    Ok(None) => match transaction
                        .request
                        .submission
                        .as_ref()
                        .map(|submission| submission.record())
                    {
                        Some(Ok(record))
                            if matches!(
                                record.delivery,
                                crate::provider::submission::SubmissionState::Prepared
                            ) =>
                        {
                            DispatchStage::Unsent
                        }
                        Some(Ok(_) | Err(_)) | None => DispatchStage::InFlight,
                    },
                    Ok(Some(_)) => DispatchStage::Settled,
                    Err(_) => DispatchStage::Lost,
                }
            }
            Self::Done(_) | Self::Transitioning => DispatchStage::Settled,
        }
    }

    /// Get the scoped handoff observer once native request preparation has an invocation identity.
    /// Pending host admission and immediate local commands have no native handoff to observe.
    pub(crate) fn dispatch_observer(
        &self,
    ) -> Option<(
        crate::provider::InvocationScope,
        crate::provider::DispatchObserver,
    )> {
        match self {
            Self::Running { transaction, .. } | Self::Finalizing { transaction, .. } => Some((
                transaction.state.scope.clone(),
                transaction.request.dispatch.observer(),
            )),
            Self::Admitting(_) | Self::Done(_) | Self::Transitioning => None,
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
            Self::Admitting(admission)
                if admission.failure.is_some() || admission.collection.is_some() =>
            {
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
                    && !transaction.pending_session_observations.is_empty()
                {
                    Ok(ForegroundSignal::SessionObservationsReady)
                } else if !transaction.failure_cleanup_active
                    && !transaction.observation_publication_in_flight
                    && transaction.pending_session_observations.is_empty()
                    && transaction
                        .finalization
                        .as_mut()
                        .is_some_and(|finalization| finalization.take_effect_request())
                {
                    Ok(ForegroundSignal::FinalizeEffectsRequested)
                } else if !transaction.failure_cleanup_active
                    && !transaction.observation_publication_in_flight
                    && transaction.pending_session_observations.is_empty()
                    && transaction
                        .finalization
                        .as_mut()
                        .is_some_and(|finalization| finalization.take_check_request())
                {
                    Ok(ForegroundSignal::FinalizeChecksRequested)
                } else if !transaction.failure_cleanup_active
                    && !transaction.observation_publication_in_flight
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
    request: ProviderCatalogRequest,
}

/// Acquire the exact session execution owner without retaining broker state.
pub(crate) struct ProviderPrepareJob {
    cancellation: Arc<TurnCancellationSignal>,
    registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
    owner: crate::provider::ThreadOwner,
}

/// Retain presentation context while a temporary question-model selection is active.
pub(crate) struct QuestionDiscussion {
    pub(crate) visible_text: String,
    previous_usage: Option<crate::session::ContextUsage>,
}

/// Preserve the task and document evidence used by planning postprocessing.
pub(crate) struct PlanningContext {
    pub(crate) plan_id: String,
    pub(crate) permit: crate::task::TaskPermit,
    pub(crate) before_version: u64,
    pub(crate) discussion: bool,
    /// True when the turn configures project checks before planning can start.
    pub(crate) configures_checks: bool,
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
    pub(super) reservation: Option<ForegroundReservation>,
    task_configuration: Option<TaskInvocationConfiguration>,
    pub(crate) events: Vec<SessionEvent>,
    pub(crate) effects: Vec<AppliedProviderEvent>,
}

/// Keep the executor model on its selected invocation instead of persisted session defaults.
///
/// Approval is not part of this override: the accepted review answer becomes the session's
/// approval mode, so every later task invocation and the winbar observe the same policy.
struct TaskInvocationConfiguration {
    model: Option<crate::settings::ValidatedModelSelection>,
}

impl TaskInvocationConfiguration {
    /// Apply frozen task choices to one request while preserving saved session defaults.
    fn apply(&self, session: &HarnessSession, request: &mut ProviderRequest) {
        if let Some(selection) = self.model.as_ref() {
            let mut selected = session.clone();
            selection.apply(&mut selected);
            request.model = selected.model;
            request.effort = selected.effort;
            request.context_window = selected.context_window;
            request.service_tier = selected.service_tier;
        }
    }
}

/// Retain owner-issued cancellation authority before any native execution future is polled.
pub(super) struct ForegroundReservation {
    pub(super) scope: crate::provider::InvocationScope,
    pub(super) engine: Arc<crate::provider::session_execution::SessionExecution>,
}

impl ForegroundAdmission {
    /// Retain the owner's collection start without starting independent native cleanup.
    pub(super) fn begin_owner_collection(
        &mut self,
        disposition: ForegroundCollection,
    ) -> Result<()> {
        self.record_owner_collection(AdmissionCollection::Collecting(disposition))
    }

    /// Record the owner's positive native collection evidence before settling this admission.
    pub(super) fn complete_owner_collection(
        &mut self,
        disposition: ForegroundCollection,
    ) -> Result<()> {
        self.record_owner_collection(AdmissionCollection::Collected(disposition))
    }

    fn record_owner_collection(&mut self, next: AdmissionCollection) -> Result<()> {
        let disposition = match &next {
            AdmissionCollection::Collecting(disposition)
            | AdmissionCollection::Collected(disposition) => disposition,
        };
        if let Some(
            AdmissionCollection::Collecting(prior) | AdmissionCollection::Collected(prior),
        ) = &self.collection
        {
            anyhow::ensure!(prior == disposition, "admission collection owner changed");
        }
        self.collection = Some(next);
        Ok(())
    }

    /// Report whether the owner still retains native collection work.
    pub(super) fn owner_collection_pending(&self) -> bool {
        matches!(self.collection, Some(AdmissionCollection::Collecting(_)))
    }

    /// Identify internal reconfiguration without presenting it as user cancellation.
    pub(super) fn owner_collection_restarts(&self) -> bool {
        matches!(
            self.collection,
            Some(
                AdmissionCollection::Collecting(ForegroundCollection::Restart)
                    | AdmissionCollection::Collected(ForegroundCollection::Restart)
            )
        )
    }

    /// Report positive collection evidence before skipping a duplicate native fence.
    pub(super) fn owner_collection_complete(&self) -> bool {
        matches!(self.collection, Some(AdmissionCollection::Collected(_)))
    }

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
        if !matches!(
            self.purpose,
            crate::input::InputPurpose::Question { .. }
                | crate::input::InputPurpose::Clarification { .. }
        ) && !broker.questions.snapshot()?.required_pending()
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
        let context = self.provider.query_context(&self.request).await?;
        self.provider.model_list(context).await.map_err(Into::into)
    }
}

impl ProviderPrepareJob {
    pub(crate) async fn run(
        self,
    ) -> Result<Arc<crate::provider::session_execution::SessionExecution>> {
        tokio::select! {
            biased;
            () = self.cancellation.cancelled() => Err(TurnCancelled.into()),
            engine = self.registry.owner(self.owner) => Ok(engine?),
        }
    }
}

impl HarnessBroker {
    /// Capture an owner's validated queued input without leaving mutable next-input state behind.
    pub(crate) fn begin_queued_input_dispatch(
        &mut self,
        message: crate::input::InputMessage,
        submission: Arc<crate::provider::submission::Submission>,
        attachments: Vec<crate::attachment::InputAttachment>,
        sink: publication::SessionUpdateSink,
    ) -> Result<TurnTransaction> {
        let previous_purpose = std::mem::replace(&mut self.input_purpose, message.purpose);
        let previous_submission = self.input_submission.take();
        let result = self.begin_foreground_dispatch(
            Request {
                id: 0,
                method: "session.prompt.submit".into(),
                params: json!({"text":message.text}),
            },
            sink,
        );
        self.input_purpose = previous_purpose;
        self.input_submission = previous_submission;
        let mut dispatch = result?;
        dispatch.set_attachments(attachments)?;
        let TurnTransaction::Admitting(admission) = &mut dispatch else {
            unreachable!("queued input begins at foreground admission")
        };
        admission.input_submission = Some(submission);
        Ok(dispatch)
    }

    /// Preserve a local caller's explicit mode, input and exchange admission.
    pub(super) fn begin_local_interaction_dispatch(
        &mut self,
        text: String,
        mode: PromptMode,
        display: Option<ExchangeAdmission>,
        input: ProviderInput,
        sink: publication::SessionUpdateSink,
    ) -> Result<TurnTransaction> {
        let request = Request {
            id: 0,
            method: "session.prompt.submit".into(),
            params: json!({"text":text}),
        };
        let mut request = request;
        if let Some((document, token)) = &self.prompt_admission {
            request.params["_prompt_admission"] = json!({"document":document,"token":token});
        }
        let mut dispatch = self.begin_foreground_dispatch(request, sink)?;
        let TurnTransaction::Admitting(admission) = &mut dispatch else {
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
            admission.planning = self.planning_context(&task, mode)?;
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
    ) -> Result<TurnTransaction> {
        let request = Request {
            id: 0,
            method: "session.prompt.submit".into(),
            params: json!({"text":prompt}),
        };
        let mut dispatch = self.begin_foreground_dispatch(request, sink)?;
        let TurnTransaction::Admitting(admission) = &mut dispatch else {
            unreachable!()
        };
        // The question's stance applies to this one forked turn, so it travels with its input
        // rather than as a tracked context section.
        admission.local_interaction = Some((
            PromptMode::PlanQuestion,
            Some(ExchangeAdmission::chat(display)),
            ProviderInput::from_text(format!(
                "Answer the plan review question below in this forked session with a concise final response, which Harness saves on the original question. Do not edit, submit, implement, or ask follow-up questions.\n\n{}",
                admission.text
            )),
        ));
        admission.phase = AdmissionPhase::Prepared;
        Ok(dispatch)
    }

    /// Stage an owner-authorized selected continuation without deciding whether it should resume.
    pub(crate) fn begin_selected_continuation_dispatch(
        &mut self,
        selected: &crate::provider::ExchangeId,
        text: String,
        sink: publication::SessionUpdateSink,
    ) -> Result<Option<TurnTransaction>> {
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
            let resume = self.prepare_selected_continuation(selected, text)?;
            let TurnTransaction::Admitting(admission) = &mut dispatch else {
                unreachable!()
            };
            match resume {
                configuration::SelectedContinuation::NoWork => Ok(None),
                configuration::SelectedContinuation::Staged {
                    mut interaction,
                    clarification,
                    planning,
                } => {
                    let owner = crate::provider::ThreadOwner::root(&self.session.id);
                    let engine = self
                        .execution_registry()
                        .existing_owner(&owner)?
                        .context("selected continuation has no retained execution owner")?;
                    let scope = crate::provider::InvocationScope::new(
                        owner,
                        interaction.exchange.id.clone(),
                    );
                    engine.reserve_invocation(scope.clone(), interaction.submission.clone())?;
                    interaction.reservation = Some(ForegroundReservation { scope, engine });
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
        if self
            .turn_cancellation
            .submission_flag()
            .load(Ordering::Acquire)
        {
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

    /// Capture execution-owner acquisition after earlier exchange finalization is complete.
    pub(crate) fn prepare_provider_turn_job(&mut self) -> ProviderPrepareJob {
        let registry = self.execution_registry();
        ProviderPrepareJob {
            cancellation: self.turn_cancellation.capture(),
            registry,
            owner: crate::provider::ThreadOwner::root(&self.session.id),
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
        self.stage_selected_foreground_interaction(text, mode, admission, input, None)
    }

    /// Preserve an owner-captured continuation target through durable turn admission.
    pub(super) fn stage_selected_foreground_interaction(
        &mut self,
        text: &str,
        mode: PromptMode,
        admission: Option<&ExchangeAdmission>,
        input: ProviderInput,
        selected: Option<&crate::provider::ExchangeId>,
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
            self.stage_admit_interaction(text, mode, admission, input_message_id, selected)?;
        Ok(StagedInteraction {
            exchange,
            new_runtime,
            mode,
            input,
            submission,
            reservation: None,
            task_configuration: None,
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
                | crate::input::InputPurpose::Clarification { .. }
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
            previous_usage: self.session.context_usage.clone(),
        };
        self.question_model = Some(selected);
        self.question_discussion = true;
        Ok(Some(discussion))
    }

    /// Restore requested-model inheritance while leaving actual execution model unconfirmed.
    pub(crate) fn finish_question_discussion(
        &mut self,
        discussion: QuestionDiscussion,
    ) -> Result<()> {
        self.question_discussion = false;
        self.question_model = None;
        self.session.resolved_model = None;
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
                admission.task_model_selection =
                    Some(crate::settings::ValidatedModelSelection::executor(
                        &preparation.executor,
                        self.session.service_tier,
                        &self.capability,
                        &catalog,
                    )?);
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
            signal => anyhow::bail!(
                "unexpected {} signal during foreground admission",
                signal.diagnostic_name()
            ),
        }
    }

    /// Capture the catalog request required by question-model selection.
    pub(crate) fn prepare_question_model_catalog(&self) -> Result<QuestionModelCatalogJob> {
        Ok(QuestionModelCatalogJob {
            provider: self.provider.clone(),
            request: self.provider_catalog_request()?,
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
    ) -> Result<TurnTransaction> {
        let scoped = sink.clone();
        self.with_publication_sink(&scoped, |broker| {
            broker.begin_foreground_dispatch_inner(request, sink)
        })
    }

    fn begin_foreground_dispatch_inner(
        &mut self,
        request: Request,
        sink: publication::SessionUpdateSink,
    ) -> Result<TurnTransaction> {
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
        Ok(TurnTransaction::Admitting(Box::new(ForegroundAdmission {
            request,
            sink: Some(sink),
            submission: Some(submission),
            input_submission: self.input_submission.take(),
            text,
            attachments: Vec::new(),
            purpose,
            command,
            command_result: None,
            command_response: None,
            command_delivery: None,
            admission_published: false,
            task_preparation: None,
            task_model_selection: None,
            task_configuration: None,
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
            events: Vec::new(),
            staged_interaction: None,
            local_interaction: None,
            provider_prepared: false,
            engine: None,
            failure: None,
            phase: AdmissionPhase::Begin,
            collection: None,
        })))
    }

    /// Return only a fully settled foreground response.
    pub(crate) fn finish_foreground(
        &mut self,
        dispatch: TurnTransaction,
    ) -> Result<DispatchResult> {
        match dispatch {
            TurnTransaction::Done(result) => Ok(result),
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
        // Harness workflow text travels in the input's note, which providers render as a
        // `<note>` beside the user's text, so the user's words reach the model unmodified.
        let mut input = staged.input.clone();
        if staged.mode == PromptMode::PlanDiscussion {
            input.prepend_note(PlanPrompt::discussion());
        }
        if matches!(staged.mode, PromptMode::Plan | PromptMode::PlanDiscussion)
            && !PlanPrompt::has_active_plan(&input.text())
            && !PlanPrompt::has_active_plan(input.additional_instructions())
        {
            let plan_id = self
                .current_plan_id()?
                .context("planning turn has no active plan")?;
            let document = self
                .plan_file
                .read_working_document(&self.session.id, &plan_id)?;
            input.prepend_note(&PlanPrompt::active_plan(&document));
        }
        let mut request = self.provider_request(input, staged.mode)?;
        if let Some(configuration) = staged.task_configuration.as_ref() {
            configuration.apply(&self.session, &mut request);
        }
        request.submission = Some(staged.submission.clone());
        let owner = crate::provider::ThreadOwner::root(&self.session.id);
        if let Some(feedback) = self.permission_coordinator.take_timeout_feedback(&owner)? {
            request.input.prepend_note(&feedback);
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
            self.turn_cancellation.requested_flag(),
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
            self.turn_cancellation.requested_flag(),
            self.trace.clone(),
        )?;
        let (lsp, lsp_lease) = self.lsp.scope(
            self.session.id.clone(),
            staged.exchange.id.clone(),
            PathBuf::from(&self.session.workspace),
            policy,
            self.turn_cancellation.subscribe(),
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
    /// Continue a detached required tool answer under its original exchange and task authority.
    fn prepare_required_question_continuation(
        &mut self,
        admission: &mut ForegroundAdmission,
    ) -> Result<Option<StagedInteraction>> {
        let crate::input::InputPurpose::Question {
            question_id,
            message_kind: crate::question::QuestionMessageKind::Answer,
            ..
        } = &admission.purpose
        else {
            return Ok(None);
        };
        let snapshot = self.questions.snapshot()?;
        let question = snapshot
            .questions
            .iter()
            .find(|question| &question.id == question_id)
            .context("required answer source is missing")?;
        if question.requirement != crate::question::QuestionRequirement::Required
            || question.delivery != crate::question::QuestionDelivery::ToolResult
        {
            return Ok(None);
        }
        // Validate the original owner before changing exchange or model state.
        let mut exchange = self
            .store
            .latest_exchange(&self.session.id)?
            .context("required answer lost its originating exchange")?;
        anyhow::ensure!(
            exchange.id == question.exchange_id,
            "required answer cannot replace a newer exchange"
        );
        let task = self.task_controller.current()?;
        anyhow::ensure!(
            task.as_ref().map(|task| task.id.as_str()) == question.task_id.as_deref(),
            "required answer task authority changed"
        );
        anyhow::ensure!(
            task.as_ref().is_none_or(|task| task.entry_accepted()
                && matches!(
                    task.status(),
                    crate::task::TaskStatus::Running | crate::task::TaskStatus::Waiting
                )),
            "required answer task is no longer active"
        );
        exchange.continue_required_question(question, self.clock.now_ms())?;
        let mode = match task.as_ref().map(|task| &task.workflow) {
            Some(crate::task::WorkflowSnapshot::Plan { plan })
                if plan.artifact.state == PlanState::ConfiguringChecks =>
            {
                self.observer.set_check_editing(true)?;
                self.check_setup = true;
                admission.reset_check_setup = true;
                PromptMode::Chat
            }
            Some(crate::task::WorkflowSnapshot::Plan { plan })
                if plan.artifact.state == PlanState::AwaitingReview =>
            {
                PromptMode::PlanDiscussion
            }
            Some(crate::task::WorkflowSnapshot::Plan { .. }) => PromptMode::Plan,
            Some(crate::task::WorkflowSnapshot::Execute { .. }) => PromptMode::ExecutePlan,
            Some(crate::task::WorkflowSnapshot::Goal { .. }) => PromptMode::GoalContinuation,
            None => PromptMode::Chat,
        };
        if let Some(task) = task.as_ref() {
            admission.planning = self.planning_context(task, mode)?;
        }

        // The explicit recorded answer authorizes continuation; a late native event does not.
        self.store.save_exchange(&exchange)?;
        let text = admission.text.clone();
        let input = match &admission.purpose {
            crate::input::InputPurpose::Question { reply, .. } => ProviderInput::QuestionReply {
                reply: reply.clone(),
                context: String::new(),
                attachments: Vec::new(),
            },
            _ => ProviderInput::from_text(text.clone()),
        };
        self.stage_foreground_interaction(&text, mode, None, input)
            .map(Some)
    }

    /// Normalize a prompt into the same exchange mode and origin as legacy admission.
    fn normalize_foreground_interaction(
        &mut self,
        admission: &mut ForegroundAdmission,
    ) -> Result<StagedInteraction> {
        let text = admission.text.clone();
        if let Some(staged) = self.prepare_selected_clarification(admission)? {
            return Ok(staged);
        }
        if let Some(staged) = self.prepare_required_question_continuation(admission)? {
            return Ok(staged);
        }
        if let Some(discussion) = self.prepare_question_discussion(admission)? {
            let visible = discussion.visible_text.clone();
            admission.question_discussion = Some(discussion);
            let input = match &admission.purpose {
                crate::input::InputPurpose::Question { reply, .. } => {
                    ProviderInput::QuestionReply {
                        reply: reply.clone(),
                        context: String::new(),
                        attachments: Vec::new(),
                    }
                }
                _ => ProviderInput::from_text(text),
            };
            return self.stage_foreground_interaction(
                &visible,
                PromptMode::Chat,
                Some(&ExchangeAdmission::chat(visible.clone())),
                input,
            );
        }
        if matches!(
            &admission.purpose,
            crate::input::InputPurpose::QuestionContinuation { .. }
                | crate::input::InputPurpose::WorkflowContinuation { .. }
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
                match start {
                    Some(start) => crate::exchange::ExchangeOrigin::TaskStarted {
                        start: start.clone(),
                    },
                    None => {
                        crate::exchange::ExchangeOrigin::task_continuation("The task was resumed")
                    }
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
            let review = if matches!(
                &admission.purpose,
                crate::input::InputPurpose::TaskEntry { .. }
            ) {
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
            admission.planning = self.planning_context(&task, mode)?;
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
        anyhow::ensure!(
            !active_plan
                .as_ref()
                .is_some_and(|plan| plan.state == PlanState::AwaitingInput)
                && self.find_active_interaction_elicitation()?.is_none(),
            "question clarification requires an explicit source and question revision"
        );
        if let Some(plan) = active_plan
            .as_ref()
            .filter(|plan| plan.state == PlanState::ConfiguringChecks)
        {
            let exchange_admission = ExchangeAdmission::chat(text.clone());
            self.observer.set_check_editing(true)?;
            self.check_setup = true;
            admission.reset_check_setup = true;
            let prompt = PlanPrompt::checks_setup(&plan.request);
            if let Some(task) = self.task_controller.current()? {
                admission.planning = self.planning_context(&task, PromptMode::Chat)?;
            }
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
            admission.planning = self.planning_context(&task, mode)?;
        }
        let mut input = ProviderInput::parse(&text);
        if let Some(plan) = active_plan
            .as_ref()
            .filter(|plan| !plan.working_path.is_empty())
            && !matches!(input, ProviderInput::QuestionReply { .. })
        {
            let document = self
                .plan_file
                .read_working_document(&self.session.id, &plan.id)?;
            input.prepend_note(&PlanPrompt::active_plan(&document));
        }
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
        dispatch: &mut TurnTransaction,
    ) -> Result<ForegroundAction> {
        let TurnTransaction::Admitting(admission) = dispatch else {
            anyhow::bail!("foreground admission has no owner");
        };
        if admission.collection.is_some() {
            return Ok(ForegroundAction::Wait);
        }
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
            *dispatch = TurnTransaction::Done(response);
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
                    // The admitted input names its task entry by purpose; the native receipt
                    // carries the same identity once the input queue has recorded it.
                    .filter(|entry| {
                        input_id.as_deref() == Some(entry.id.as_str())
                            || matches!(&admission.purpose,
                                crate::input::InputPurpose::TaskEntry { id, .. } if id == &entry.id)
                    })
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
                    if preparation.executor.enabled && admission.task_model_selection.is_none() {
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
                    // The review answer governs every invocation of the task, including
                    // continuations, Verify, and resumed phases, so it becomes the session's
                    // approval mode through the permission authority that `/allow` uses. A
                    // per-invocation override would revert after the entry turn and leave the
                    // winbar showing a mode that no longer applied.
                    if self.session.approval_mode != preparation.approval_mode {
                        let captured = self.prepare_permission_configuration(&json!({
                            "change": crate::task::PermissionChange::SetApproval {
                                mode: preparation.approval_mode,
                            },
                        }))?;
                        let result = self.commit_permission_configuration(captured)?;
                        anyhow::ensure!(
                            result.resume.is_none(),
                            "execution approval unexpectedly requested a restart"
                        );
                        admission.phase = AdmissionPhase::PermissionPublishing;
                        return Ok(ForegroundAction::Effect(
                            ForegroundJob::PermissionPublication(result.delivery),
                        ));
                    }
                    admission.task_configuration = Some(TaskInvocationConfiguration {
                        model: if preparation.executor.enabled {
                            Some(
                                admission
                                    .task_model_selection
                                    .take()
                                    .context("executor model selection is not prepared")?,
                            )
                        } else {
                            None
                        },
                    });
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
            staged.task_configuration = admission.task_configuration.take();
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
        let TurnTransaction::Admitting(admission) =
            std::mem::replace(dispatch, TurnTransaction::Transitioning)
        else {
            unreachable!();
        };
        *dispatch = TurnTransaction::Running {
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
    pub(crate) async fn run(
        self,
    ) -> Result<crate::provider::Compaction, crate::provider::ProviderError> {
        self.provider.thread_compact(self.request).await
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
        result: Result<crate::provider::Compaction, crate::provider::ProviderError>,
        allow_metadata: bool,
    ) -> Result<(PendingProviderDelivery, Option<anyhow::Error>)> {
        state.compaction.completed_at_ms = Some(self.clock.now_ms());
        let evidence = match result {
            Ok(evidence) => evidence,
            Err(error) => {
                state.compaction.state = crate::provider::CompactionState::Failed;
                state.compaction.result.error = Some(crate::provider::lifecycle_error::diagnostic(
                    forge_provider_api::ProviderOperation::ThreadCompact,
                    &error,
                ));
                state.record.detail = SessionEventKind::Compaction {
                    compaction: state.compaction.into(),
                };
                self.store.save_session_event(&state.record)?;
                let patch = self.reconcile_timeline()?;
                return Ok((
                    self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))?,
                    Some(error.into()),
                ));
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
        let settlement =
            super::compaction::apply_compaction_evidence(&mut state.compaction, evidence);
        state.record.detail = SessionEventKind::Compaction {
            compaction: state.compaction.clone().into(),
        };
        self.store.save_session_event(&state.record)?;
        let failure = authority
            .err()
            .or_else(|| settlement.err().map(anyhow::Error::from));
        if failure.is_some() || !allow_metadata {
            let patch = self.reconcile_timeline()?;
            return Ok((
                self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))?,
                failure,
            ));
        }
        self.apply_compaction_context(&state.compaction);
        self.save_session()?;
        let snapshot = self.snapshot()?;
        let patch = self.reconcile_timeline()?;
        let delivery = self.prepare_provider_delivery(AppliedProviderEvent::Effects(vec![
            AppliedProviderEvent::Timeline(patch),
            AppliedProviderEvent::Update(SessionUpdate::ContextCompacted {
                snapshot: Box::new(snapshot),
            }),
        ]))?;
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
                // Several exchanges can await finalization, such as an interrupted
                // exchange and an admission whose baseline capture then failed. Leave
                // the check open so the next pass settles any remaining one before
                // `activate_staged_runtime` requires that none is pending.
                admission.prior_finalization = None;
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
    /// Capture planning postprocessing evidence for a planning, plan-discussion, or check-setup
    /// turn of a Plan task. Check setup runs in Chat mode, yet its completion must still advance
    /// the retained plan, so setup turns carry their starting configuration digest.
    pub(super) fn planning_context(
        &self,
        task: &crate::task::Task,
        mode: PromptMode,
    ) -> Result<Option<PlanningContext>> {
        let crate::task::WorkflowSnapshot::Plan { plan } = &task.workflow else {
            return Ok(None);
        };
        let configures_checks = plan.artifact.state == PlanState::ConfiguringChecks;
        if !configures_checks && !matches!(mode, PromptMode::Plan | PromptMode::PlanDiscussion) {
            return Ok(None);
        }
        let plan_id = plan.artifact.id.clone();
        let before_version = self
            .plan_file
            .read_working_document(&self.session.id, &plan_id)?
            .version;
        Ok(Some(PlanningContext {
            plan_id,
            permit: task.permit(),
            before_version,
            discussion: plan.artifact.state == PlanState::AwaitingReview,
            configures_checks,
        }))
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
                "request_id": request.id, "method": request.method, "params": request.params.clone(),
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
                    "request_id": id, "code": failure.code, "error": failure.message, "event": result.event,
                }),
            );
        } else {
            self.trace.record(
                &self.session.id,
                "lua.rpc.completed",
                json!({
                    "request_id": id, "response": result.response, "event": result.event,
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
        let code = super::request_failure_code(error);
        self.record_plan_trace("request_failed", json!({ "request_id": id, "code": code }));
        self.trace.record(
            &self.session.id,
            "lua.rpc.failed",
            json!({
                "request_id": id, "code": code, "error": format!("{error:#}"), "event": [],
            }),
        );
    }
}

impl HarnessBroker {
    /// Stage approval-timeout feedback without borrowing broker state across provider work.
    pub(crate) fn begin_approval_timeout_dispatch(
        &mut self,
        sink: publication::SessionUpdateSink,
    ) -> Result<(Option<TurnTransaction>, PendingProviderDelivery)> {
        let owner = crate::provider::ThreadOwner::root(&self.session.id);
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
                    let TurnTransaction::Admitting(state) = &mut owned else {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::session::SessionStore;

    /// The executor model affects one invocation and cannot replace ordinary or reloaded defaults.
    #[test]
    fn task_invocation_configuration_preserves_session_defaults_after_reload() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        let before = broker.session.clone();
        let catalog = vec![crate::provider::ProviderModel {
            id: "task-executor".into(),
            label: None,
            reasoning: vec!["high".into()],
            service_tiers: vec![crate::provider::ServiceTier::Standard],
            default_reasoning: None,
            selected_reasoning: None,
            context_window: Vec::new(),
            default_context_window: None,
            selected_context_window: None,
            vision: false,
            description: None,
            is_default: true,
        }];
        let capability = crate::provider::ProviderCapability {
            model_selection: true,
            effort_selection: true,
            ..Default::default()
        };
        let model = crate::settings::ValidatedModelSelection::executor(
            &crate::session::PlanExecutor {
                enabled: true,
                model: Some("task-executor".into()),
                effort: Some("high".into()),
            },
            crate::provider::ServiceTier::Standard,
            &capability,
            &catalog,
        )?;
        let configuration = TaskInvocationConfiguration { model: Some(model) };
        let mut task =
            broker.provider_request(ProviderInput::from_text("task input"), PromptMode::Chat)?;
        configuration.apply(&broker.session, &mut task);
        assert_eq!(task.model, "task-executor");
        assert_eq!(task.effort, "high");
        assert_eq!(task.approval_mode, before.approval_mode);
        let ordinary = broker
            .provider_request(ProviderInput::from_text("ordinary input"), PromptMode::Chat)?;
        assert_eq!(ordinary.model, before.model);
        assert_eq!(ordinary.effort, before.effort);
        assert_eq!(ordinary.approval_mode, before.approval_mode);
        broker.save_session()?;
        let reloaded = broker
            .store
            .load_session(&before.id)?
            .context("saved session missing")?;
        assert_eq!(reloaded.model, before.model);
        assert_eq!(reloaded.resolved_model, before.resolved_model);
        assert_eq!(reloaded.effort, before.effort);
        assert_eq!(reloaded.approval_mode, before.approval_mode);
        assert_eq!(reloaded.service_tier, before.service_tier);
        assert_eq!(reloaded.context_window, before.context_window);
        Ok(())
    }
}
