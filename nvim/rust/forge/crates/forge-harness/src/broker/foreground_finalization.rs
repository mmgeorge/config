use super::*;

/// Retain the host request and its settlement authority until acknowledgment.
pub(crate) struct ForegroundFinalization {
    pub(crate) submission: Option<PromptSubmission>,
    pub(crate) mode: PromptMode,
    pub(crate) events: Vec<SessionEvent>,
    pub(crate) outcome: Result<ProviderCompletion, ProviderError>,
    pub(crate) settlement: Arc<crate::provider::session_execution::SettlementCell>,
    question_override: Option<ProviderCompletion>,
    pub(crate) staged: Option<Box<interaction::StagedHostCompletion>>,
    pub(crate) plan_mutations: Option<foreground_plan_effect::PlanMutationQueue>,
    plan_current: Option<PlanRecord>,
    plan_submission_id: Option<crate::provider::session_execution::HostEffectId>,
    pub(crate) plan_submission_publication_pending: bool,
    nonplan_question_terminal: bool,
    goal_terminal_pending: bool,
    goal_continuing: bool,
    pub(crate) goal_waiting_questions: bool,
    checks: Option<check_admission::AutomaticCheckTransaction>,
    check_result: Option<(Value, Vec<SessionEvent>)>,
    auto_compaction_job: Option<compaction::CompactionJob>,
    pub(crate) prepared_response: Option<Response>,
    pub(crate) pending_check_action: Option<check_admission::AutomaticCheckAction>,
    check_continuation_requested: bool,
    pub(crate) clarification_request: Option<ProviderRequest>,
    repair_request: Option<ProviderRequest>,
    repair_active: bool,
    exchange_event_published: bool,
    exchange_event_staged: bool,
    admission_postprocessed: bool,
    planning_observed_turn: Option<crate::task::TaskTurnId>,
    needs_host_revalidation: bool,
    pending_stale_continuation: Option<(
        ProviderRequest,
        Option<crate::provider::SteerCommand>,
        crate::provider::retry::RetryState,
    )>,
    phase: FinalizationPhase,
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum FinalizationPhase {
    Effects,
    EffectsRequested,
    AwaitingPublication,
    InitialCheckpoint,
    Checkpoint,
    Checks,
    ChecksRequested,
    ClarificationPublication,
    CheckRepairPublication,
    CheckDiagnostic,
    AutoCompactionStart,
    AutoCompactionRunning,
    AutoCompactionEnd,
    FinalPublication,
    FinalDrain,
    Commit,
    RefreshSettlement,
    ResumeStale,
    WaitingStaleQuestion,
    Complete,
}

impl ForegroundFinalization {
    /// Start another host settlement for native work on an already committed scope.
    pub(crate) fn begin_idle_refresh(
        &mut self,
    ) -> Result<crate::provider::session_execution::SettlementCheckout> {
        anyhow::ensure!(
            self.phase == FinalizationPhase::Complete,
            "idle settlement is not committed"
        );
        anyhow::ensure!(
            self.submission.is_none(),
            "committed prompt authority was not released"
        );
        let checkout = self.settlement.checkout()?;
        anyhow::ensure!(
            checkout.ticket().is_none(),
            "committed settlement still has a ticket"
        );
        self.prepared_response = None;
        self.phase = FinalizationPhase::RefreshSettlement;
        Ok(checkout)
    }
    /// Reuse this host envelope for an admitted same-invocation follow-up.
    pub(crate) fn resume_native(
        &mut self,
        outcome: ProviderCompletion,
        question_override: Option<ProviderCompletion>,
    ) -> Result<()> {
        anyhow::ensure!(
            matches!(
                self.phase,
                FinalizationPhase::ClarificationPublication
                    | FinalizationPhase::CheckRepairPublication
                    | FinalizationPhase::ResumeStale
            ),
            "host continuation is not active"
        );
        self.outcome = Ok(outcome);
        self.question_override = question_override;
        self.staged = None;
        self.plan_mutations = None;
        self.plan_current = None;
        self.plan_submission_id = None;
        self.plan_submission_publication_pending = false;
        self.nonplan_question_terminal = false;
        self.phase = FinalizationPhase::Effects;
        Ok(())
    }
    pub(crate) fn take_effect_request(&mut self) -> bool {
        if self.phase != FinalizationPhase::Effects {
            return false;
        }
        self.phase = FinalizationPhase::EffectsRequested;
        true
    }
    pub(crate) fn take_check_request(&mut self) -> bool {
        if self.phase != FinalizationPhase::Checks {
            return false;
        }
        self.phase = FinalizationPhase::ChecksRequested;
        true
    }
    pub(crate) fn take_check_continuation_request(&mut self) -> bool {
        if self.phase == FinalizationPhase::ResumeStale && !self.check_continuation_requested {
            self.check_continuation_requested = true;
            return true;
        }
        if self.pending_check_action.is_none() || self.check_continuation_requested {
            return false;
        }
        self.check_continuation_requested = true;
        true
    }

    /// Wait until the service has drained every publication and outstanding effect job.
    pub(crate) fn effects_applied(&mut self) -> Result<ForegroundAction> {
        anyhow::ensure!(
            self.phase == FinalizationPhase::EffectsRequested,
            "host effects are not pending"
        );
        self.phase = FinalizationPhase::AwaitingPublication;
        Ok(ForegroundAction::WaitForPublicationDrain)
    }
    /// Create a heap-owned envelope retained across host settlement phases.
    pub(crate) fn new(
        submission: PromptSubmission,
        mode: PromptMode,
        events: Vec<SessionEvent>,
        outcome: Result<ProviderCompletion, ProviderError>,
        settlement: Arc<crate::provider::session_execution::SettlementCell>,
        question_override: Option<ProviderCompletion>,
    ) -> Box<Self> {
        Box::new(Self {
            submission: Some(submission),
            mode,
            events,
            outcome,
            settlement,
            question_override,
            staged: None,
            plan_mutations: None,
            plan_current: None,
            plan_submission_id: None,
            plan_submission_publication_pending: false,
            nonplan_question_terminal: false,
            goal_terminal_pending: false,
            goal_continuing: false,
            goal_waiting_questions: false,
            checks: None,
            check_result: None,
            auto_compaction_job: None,
            prepared_response: None,
            pending_check_action: None,
            check_continuation_requested: false,
            clarification_request: None,
            repair_request: None,
            repair_active: false,
            exchange_event_published: false,
            exchange_event_staged: false,
            admission_postprocessed: false,
            planning_observed_turn: None,
            needs_host_revalidation: false,
            pending_stale_continuation: None,
            phase: FinalizationPhase::Effects,
        })
    }

    /// Advance only after the effects have been applied and their presentation delivered.
    pub(crate) fn effects_delivered(
        &mut self,
        broker: &mut HarnessBroker,
        runtime: &mut ExchangeRuntime,
        exchange: &mut Exchange,
        outcome: crate::exchange::ExchangeOutcome,
    ) -> Result<ForegroundAction> {
        anyhow::ensure!(
            self.phase == FinalizationPhase::AwaitingPublication,
            "host publication is not pending"
        );
        self.settlement.validate_effects_settled()?;
        if outcome == crate::exchange::ExchangeOutcome::Cancelled {
            broker.questions.cancel_chat(Some(&exchange.id))?;
            exchange.set_awaiting_input(false);
        } else if outcome == crate::exchange::ExchangeOutcome::Complete
            && broker
                .questions
                .snapshot()?
                .required_pending_in_exchange(&exchange.id)
        {
            // Steering can end a native question tool without resolving its durable decision.
            exchange.set_awaiting_input(true);
            exchange.pause(broker.clock.now_ms());
            broker.store.save_exchange(exchange)?;
        }
        if exchange.awaiting_input() {
            self.phase = FinalizationPhase::Checks;
            return Ok(ForegroundAction::Wait);
        }
        anyhow::ensure!(
            runtime.exchange_id == exchange.id,
            "checkpoint runtime crosses exchange ownership"
        );
        exchange.begin_finalization(outcome, broker.clock.now_ms())?;
        broker.store.save_exchange(exchange)?;
        if outcome == crate::exchange::ExchangeOutcome::Cancelled && runtime.tools_blocked() {
            runtime
                .tool_gate
                .as_ref()
                .context("blocked checkpoint gate is missing")?
                .complete(Err(
                    "turn cancelled before checkpoint admission completed".into()
                ))?;
            runtime.checkpoint.take();
            exchange.finalization_error = None;
            exchange.finish(outcome, broker.clock.now_ms())?;
            broker.store.save_exchange(exchange)?;
            self.phase = FinalizationPhase::Checks;
            return Ok(ForegroundAction::Wait);
        }
        if runtime.checkpoint.is_some() {
            self.phase = FinalizationPhase::InitialCheckpoint;
            return Ok(ForegroundAction::Wait);
        }
        self.prepare_checkpoint(broker, exchange)
    }

    /// Commit the completed initial barrier before starting final capture.
    pub(crate) fn initial_checkpoint_completed(
        &mut self,
        broker: &mut HarnessBroker,
        runtime: &mut ExchangeRuntime,
        exchange: &mut Exchange,
        result: Result<crate::checkpoint::CheckpointRecord>,
    ) -> Result<ForegroundAction> {
        anyhow::ensure!(
            self.phase == FinalizationPhase::InitialCheckpoint,
            "unexpected initial checkpoint"
        );
        broker.finish_initial_checkpoint(runtime, exchange, result)?;
        self.prepare_checkpoint(broker, exchange)
    }

    fn prepare_checkpoint(
        &mut self,
        broker: &mut HarnessBroker,
        exchange: &mut Exchange,
    ) -> Result<ForegroundAction> {
        let outcome = exchange
            .finalization_outcome()
            .context("exchange has no finalization outcome")?;
        if broker.checkpoint.is_none() {
            let descendants: Vec<_> = referenced_child_exchange_list(exchange)
                .into_iter()
                .map(|(_, id)| id)
                .collect();
            broker
                .store
                .require_settled_exchange_tree(&broker.session.id, &descendants)?;
            let mut changes = crate::exchange::ProviderDiffBuilder::default();
            broker.visit_provider_exchanges(exchange, |item| changes.record(item))?;
            exchange.attributed_diff_text = changes.finish();
            exchange.attributed_matches_checkpoint = false;
            exchange.finish(outcome, broker.clock.now_ms())?;
            broker.store.save_exchange(exchange)?;
            self.phase = FinalizationPhase::Checks;
            return Ok(ForegroundAction::Wait);
        }
        let job = broker.prepare_final_checkpoint(exchange, outcome)?;
        self.phase = FinalizationPhase::Checkpoint;
        Ok(ForegroundAction::Effect(ForegroundJob::FinalCheckpoint(
            Box::new(job),
        )))
    }

    pub(crate) fn checkpoint_completed(
        &mut self,
        broker: &mut HarnessBroker,
        exchange: &mut Exchange,
        result: Result<checkpoint_settlement::FinalCheckpointResult>,
    ) -> Result<ForegroundAction> {
        anyhow::ensure!(
            self.phase == FinalizationPhase::Checkpoint,
            "unexpected final checkpoint"
        );
        match broker.commit_final_checkpoint(exchange, result?)? {
            checkpoint_settlement::FinalCheckpointCommit::Stale => {
                let outcome = exchange
                    .finalization_outcome()
                    .context("exchange has no finalization outcome")?;
                Ok(ForegroundAction::Effect(ForegroundJob::FinalCheckpoint(
                    Box::new(broker.prepare_final_checkpoint(exchange, outcome)?),
                )))
            }
            checkpoint_settlement::FinalCheckpointCommit::Committed(_) => {
                self.phase = FinalizationPhase::Checks;
                Ok(ForegroundAction::Wait)
            }
        }
    }

    /// Advance after the automatic-check transaction and its publication finish.
    pub(crate) fn checks_delivered(&mut self) -> Result<ForegroundAction> {
        anyhow::ensure!(
            self.phase == FinalizationPhase::FinalDrain,
            "final publication has not drained"
        );
        self.commit()
    }
    fn commit(&mut self) -> Result<ForegroundAction> {
        self.settlement.validate_effects_settled()?;
        anyhow::ensure!(
            self.prepared_response.is_some(),
            "foreground response was not prepared before settlement"
        );
        self.phase = FinalizationPhase::Commit;
        Ok(ForegroundAction::Effect(ForegroundJob::SettlementCommit {
            cell: self.settlement.clone(),
            undispatched: Vec::new(),
        }))
    }

    pub(crate) fn settlement_committed(
        &mut self,
        engine: Arc<crate::provider::session_execution::SessionExecution>,
        scope: crate::provider::InvocationScope,
        result: Result<(), crate::provider::session_execution::SettlementError>,
    ) -> Result<ForegroundAction> {
        use crate::provider::session_execution::SettlementError;
        anyhow::ensure!(
            self.phase == FinalizationPhase::Commit,
            "unexpected settlement acknowledgment"
        );
        match result {
            Ok(()) => {
                self.phase = FinalizationPhase::Complete;
                Ok(ForegroundAction::Wait)
            }
            Err(SettlementError::Stale) => {
                let checkout = self.settlement.checkout()?;
                self.phase = FinalizationPhase::RefreshSettlement;
                Ok(ForegroundAction::Effect(ForegroundJob::SettlementRefresh {
                    engine,
                    scope,
                    checkout,
                }))
            }
            Err(error) => Err(error.into()),
        }
    }

    pub(crate) fn is_complete(&self) -> bool {
        self.phase == FinalizationPhase::Complete
    }

    /// Restore the newer ticket before rerunning host effects and prerequisites.
    pub(crate) fn settlement_refreshed(
        &mut self,
        mut checkout: crate::provider::session_execution::SettlementCheckout,
        ready: Result<crate::provider::session_execution::NativeReady, ProviderError>,
    ) -> Result<()> {
        anyhow::ensure!(
            self.phase == FinalizationPhase::RefreshSettlement,
            "unexpected settlement readiness"
        );
        let ready = ready?;
        checkout.replace(ready.ticket);
        self.settlement.set_batches(ready.batches)?;
        let provenance = self
            .staged
            .as_ref()
            .map(|staged| &staged.output)
            .or_else(|| self.outcome.as_ref().ok());
        let mut outcome = match (ready.outcome, &self.question_override) {
            (Ok(mut output), Some(override_output)) => {
                output.merge_native(override_output.native.clone());
                Ok(output)
            }
            (Err(ProviderError::Cancelled), Some(override_output)) => {
                let mut output = override_output.clone();
                let native = output.native.clone();
                for batch in self.settlement.batches()? {
                    output.merge_execution(batch.completion);
                }
                output.merge_native(native);
                Ok(output)
            }
            (outcome, _) => outcome,
        };
        if let (Some(previous), Ok(current)) = (provenance, &mut outcome) {
            current.dispatched_task_permit = previous.dispatched_task_permit.clone();
            current.dispatched_execution_id = previous.dispatched_execution_id.clone();
            current.dispatched_entry_id = previous.dispatched_entry_id.clone();
        }
        self.outcome = outcome;
        self.staged = None;
        self.plan_mutations = None;
        self.plan_current = None;
        self.plan_submission_id = None;
        self.plan_submission_publication_pending = false;
        self.nonplan_question_terminal = false;
        self.goal_terminal_pending = false;
        self.goal_continuing = false;
        self.goal_waiting_questions = false;
        self.checks = None;
        self.check_result = None;
        self.auto_compaction_job = None;
        self.prepared_response = None;
        self.pending_check_action = None;
        self.check_continuation_requested = false;
        self.needs_host_revalidation = true;
        self.phase = FinalizationPhase::Effects;
        Ok(())
    }
}

impl HarnessBroker {
    pub(crate) fn stale_continuation_waits_for_question(
        &self,
        dispatch: &ForegroundDispatch,
    ) -> bool {
        matches!(dispatch, ForegroundDispatch::Finalizing { transaction, .. }
            if transaction.finalization.as_ref().is_some_and(|finalization| finalization.phase == FinalizationPhase::WaitingStaleQuestion))
    }

    pub(crate) fn resume_stale_after_question(
        &mut self,
        dispatch: &mut ForegroundDispatch,
    ) -> Result<ForegroundAction> {
        let ForegroundDispatch::Finalizing { transaction, .. } = dispatch else {
            anyhow::bail!("stale continuation question has no finalizing owner");
        };
        if self
            .questions
            .snapshot()?
            .required_in_exchange(&transaction.state.interaction.id)
        {
            return Ok(ForegroundAction::Wait);
        }
        transaction.state.interaction = self
            .store
            .load_exchange(&transaction.state.interaction.id)?
            .context("stale continuation exchange disappeared")?;
        let finalization = transaction
            .finalization
            .as_mut()
            .context("finalization is missing")?;
        finalization.needs_host_revalidation = true;
        finalization.phase = FinalizationPhase::Effects;
        Ok(ForegroundAction::Wait)
    }

    /// Refresh host prerequisites before re-admitting a proven unsent continuation.
    pub(crate) fn refresh_stale_continuation(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        request: ProviderRequest,
        command: Option<crate::provider::SteerCommand>,
        retry: crate::provider::retry::RetryState,
    ) -> Result<ForegroundAction> {
        let ForegroundDispatch::Running {
            transaction,
            admission,
        } = dispatch
        else {
            anyhow::bail!("stale continuation has no running owner");
        };
        anyhow::ensure!(
            transaction
                .finalization
                .as_ref()
                .is_none_or(|finalization| finalization.pending_stale_continuation.is_none()),
            "another stale continuation is retained"
        );
        let checkout = transaction.settlement.checkout()?;
        let ticket = checkout
            .ticket()
            .context("stale continuation has no settlement ticket")?;
        if transaction.finalization.is_none() {
            let submission = admission.take_submission()?;
            let events = std::mem::take(&mut admission.events);
            let provenance = ProviderCompletion {
                dispatched_task_permit: transaction.dispatched_task_permit.clone(),
                dispatched_execution_id: transaction.dispatched_execution_id.clone(),
                dispatched_entry_id: transaction.dispatched_entry_id.clone(),
                ..Default::default()
            };
            transaction.finalization = Some(ForegroundFinalization::new(
                submission,
                transaction.mode,
                events,
                Ok(provenance),
                transaction.settlement.clone(),
                None,
            ));
        }
        let finalization = transaction
            .finalization
            .as_mut()
            .context("finalization was not initialized")?;
        finalization.pending_stale_continuation = Some((request, command, retry));
        finalization.phase = FinalizationPhase::RefreshSettlement;
        ticket.retire_for_refresh();
        let engine = transaction.engine.clone();
        let scope = transaction.state.scope.clone();
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
        Ok(ForegroundAction::Effect(ForegroundJob::SettlementRefresh {
            engine,
            scope,
            checkout,
        }))
    }

    /// Apply one owned checkpoint or settlement result to the retained finalization.
    pub(crate) fn step_foreground_finalization(
        &mut self,
        dispatch: &mut super::foreground_dispatch::ForegroundDispatch,
        signal: ForegroundSignal,
    ) -> Result<ForegroundAction> {
        let super::foreground_dispatch::ForegroundDispatch::Finalizing {
            transaction,
            admission,
        } = dispatch
        else {
            anyhow::bail!("foreground dispatch is not finalizing");
        };
        let finalization = transaction
            .finalization
            .as_mut()
            .context("finalization envelope is missing")?;
        match signal {
            ForegroundSignal::FinalizeEffectsRequested => {
                self.prepare_foreground_host_effects(transaction, admission)
            }
            ForegroundSignal::PlanMutationCompleted(result) => {
                let queue = finalization
                    .plan_mutations
                    .as_mut()
                    .context("plan mutation queue is missing")?;
                queue.complete(self, result)?;
                if let Some(staged) = finalization.staged.as_mut() {
                    staged.output.dispatched_task_permit = Some(queue.permit().clone());
                }
                if let Some(job) = queue.next_job(self)? {
                    Ok(ForegroundAction::Effect(ForegroundJob::PlanMutation(
                        Box::new(job),
                    )))
                } else {
                    finalization.plan_current = Some(queue.plan().clone());
                    if let Some(action) = self.schedule_plan_submission(finalization)? {
                        return Ok(action);
                    }
                    if let Some(action) = self.schedule_plan_question(finalization)? {
                        return Ok(action);
                    }
                    if let Some(action) = self
                        .apply_nonplan_question(finalization, &mut transaction.state.interaction)?
                    {
                        return Ok(action);
                    }
                    self.schedule_remaining_host_effects(
                        finalization,
                        &transaction.state.interaction,
                    )
                }
            }
            ForegroundSignal::PlanQuestionCompleted(result) => {
                let id = finalization
                    .plan_submission_id
                    .context("plan question receipt is missing")?;
                let staged = finalization
                    .staged
                    .as_mut()
                    .context("host completion is missing")?;
                let permit = staged
                    .output
                    .dispatched_task_permit
                    .as_ref()
                    .context("plan question task authority is missing")?;
                let plan = finalization
                    .plan_current
                    .as_mut()
                    .context("plan question owner is missing")?;
                let events = self.commit_plan_question(
                    &mut transaction.state.interaction,
                    plan,
                    permit,
                    staged.output.dispatched_entry_id.as_ref(),
                    result?,
                )?;
                let current = self
                    .task_controller
                    .current()?
                    .context("planning task disappeared")?;
                permit.validate_observation(Some(&current))?;
                staged.output.dispatched_task_permit = Some(current.permit());
                if finalization
                    .settlement
                    .pending_effect_publications()?
                    .contains(&id)
                {
                    let mut delivery =
                        self.prepare_provider_delivery(AppliedProviderEvent::None)?;
                    delivery.events = events;
                    finalization.plan_submission_publication_pending = true;
                    return Ok(ForegroundAction::Effect(
                        ForegroundJob::PlanTerminalPublication {
                            delivery,
                            cell: finalization.settlement.clone(),
                            id,
                        },
                    ));
                }
                self.finish_plan_terminal_effects(finalization)
            }
            ForegroundSignal::PlanSubmissionCompleted(result) => {
                let id = finalization
                    .plan_submission_id
                    .context("plan submission receipt is missing")?;
                let staged = finalization
                    .staged
                    .as_mut()
                    .context("host completion is missing")?;
                let permit = staged
                    .output
                    .dispatched_task_permit
                    .as_ref()
                    .context("plan submission task authority is missing")?;
                let plan = finalization
                    .plan_current
                    .as_mut()
                    .context("plan submission owner is missing")?;
                let events = self.commit_plan_submission(
                    &mut transaction.state.interaction,
                    plan,
                    permit,
                    staged.output.dispatched_entry_id.as_ref(),
                    result?,
                )?;
                let current = self
                    .task_controller
                    .current()?
                    .context("planning task disappeared")?;
                permit.validate_observation(Some(&current))?;
                staged.output.dispatched_task_permit = Some(current.permit());
                if finalization
                    .settlement
                    .pending_effect_publications()?
                    .contains(&id)
                {
                    let mut delivery =
                        self.prepare_provider_delivery(AppliedProviderEvent::None)?;
                    delivery.events = events;
                    finalization.plan_submission_publication_pending = true;
                    return Ok(ForegroundAction::Effect(
                        ForegroundJob::PlanTerminalPublication {
                            delivery,
                            cell: finalization.settlement.clone(),
                            id,
                        },
                    ));
                }
                self.finish_plan_terminal_effects(finalization)
            }
            ForegroundSignal::PlanTerminalPublished(result) => {
                anyhow::ensure!(
                    finalization.plan_submission_publication_pending,
                    "plan submission publication was not pending"
                );
                admission.events.extend(result?);
                finalization.plan_submission_publication_pending = false;
                if finalization.goal_terminal_pending {
                    finalization.goal_terminal_pending = false;
                    self.finish_goal_effects(finalization)
                } else if finalization.nonplan_question_terminal {
                    self.finish_nonplan_question_effects(
                        finalization,
                        &transaction.state.interaction,
                    )
                } else {
                    self.finish_plan_terminal_effects(finalization)
                }
            }
            ForegroundSignal::GoalProgressCompleted(result) => self.commit_owned_goal_progress(
                finalization,
                &transaction.state.interaction,
                (*result)?,
            ),
            ForegroundSignal::ExecutionProgressCompleted(result) => {
                let foreground_goal_effect::ExecutionProgressResult {
                    progress,
                    permit,
                    guard,
                } = (*result)?;
                self.commit_owned_execution_evidence(
                    finalization,
                    &transaction.state.interaction,
                    permit,
                    guard,
                    Some(progress),
                )
            }
            ForegroundSignal::PublicationDrained => {
                self.complete_foreground_publication(transaction, admission)
            }
            ForegroundSignal::ClarificationPublished(result) => {
                anyhow::ensure!(
                    finalization.phase == FinalizationPhase::ClarificationPublication,
                    "clarification publication was not pending"
                );
                admission.events.extend(result?);
                let request = finalization
                    .clarification_request
                    .take()
                    .context("clarification request is missing")?;
                admission.clarification = None;
                let task = self
                    .task_controller
                    .current()?
                    .filter(|task| task.entry_accepted());
                transaction.dispatched_task_permit = task.as_ref().map(|task| task.permit());
                transaction.dispatched_execution_id =
                    task.as_ref().and_then(|task| match &task.workflow {
                        crate::task::WorkflowSnapshot::Execute { execution } => {
                            Some(execution.record.id.clone())
                        }
                        _ => None,
                    });
                transaction.dispatched_entry_id = task
                    .as_ref()
                    .and_then(|task| task.entry.as_ref())
                    .map(|entry| entry.id.clone());
                transaction.mode = request.mode;
                transaction.request = request.clone();
                let submission = ProviderSubmission {
                    request,
                    provider: self.provider.clone(),
                    engine: transaction.engine.clone(),
                    sink: transaction.provider_event_sink.clone(),
                    trace: self.trace.clone(),
                    store: self.submission_store.clone(),
                    continuation: None,
                    settlement: transaction.settlement.clone(),
                    retry: Default::default(),
                };
                transaction.native.restart(submission.run())?;
                transaction.native_open = true;
                transaction.question_enabled = true;
                let old = std::mem::replace(
                    dispatch,
                    super::foreground_dispatch::ForegroundDispatch::Transitioning,
                );
                let super::foreground_dispatch::ForegroundDispatch::Finalizing {
                    transaction,
                    admission,
                } = old
                else {
                    unreachable!("finalization owner checked above")
                };
                *dispatch = super::foreground_dispatch::ForegroundDispatch::Running {
                    transaction,
                    admission,
                };
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::FinalizeChecksRequested => {
                self.prepare_foreground_final_checks(transaction, admission)
            }
            ForegroundSignal::FinalizeCheckContinuationRequested => {
                if finalization.phase == FinalizationPhase::ResumeStale {
                    if self
                        .questions
                        .snapshot()?
                        .required_in_exchange(&transaction.state.interaction.id)
                    {
                        finalization.phase = FinalizationPhase::WaitingStaleQuestion;
                        transaction.question_enabled = true;
                        return Ok(ForegroundAction::Wait);
                    }
                    let (request, command, retry) = finalization
                        .pending_stale_continuation
                        .take()
                        .context("stale continuation input is missing")?;
                    let interaction = &mut transaction.state.interaction;
                    if interaction.state() == ExchangeState::Complete {
                        interaction.revalidate_host_settlement(&transaction.state.scope)?;
                        self.store.save_exchange(interaction)?;
                    }
                    transaction.request = request.clone();
                    let submission = ProviderSubmission {
                        request,
                        provider: self.provider.clone(),
                        engine: transaction.engine.clone(),
                        sink: transaction.provider_event_sink.clone(),
                        trace: self.trace.clone(),
                        store: self.submission_store.clone(),
                        continuation: command,
                        settlement: transaction.settlement.clone(),
                        retry,
                    };
                    transaction.native.restart(submission.run())?;
                    transaction.native_open = true;
                    transaction.question_enabled = true;
                    let old = std::mem::replace(dispatch, ForegroundDispatch::Transitioning);
                    let ForegroundDispatch::Finalizing {
                        transaction,
                        admission,
                    } = old
                    else {
                        unreachable!("finalization owner checked above")
                    };
                    *dispatch = ForegroundDispatch::Running {
                        transaction,
                        admission,
                    };
                    return Ok(ForegroundAction::Wait);
                }
                match finalization
                    .pending_check_action
                    .take()
                    .context("automatic check continuation is missing")?
                {
                    check_admission::AutomaticCheckAction::Revalidate => {
                        if let Some(checks) = finalization.checks.as_mut() {
                            let (_, mut events) = self.take_automatic_check_result(checks);
                            finalization.events.append(&mut events);
                        }
                        let checkout = finalization.settlement.checkout()?;
                        checkout
                            .ticket()
                            .context("host settlement ticket is missing")?
                            .retire_for_refresh();
                        finalization.phase = FinalizationPhase::RefreshSettlement;
                        Ok(ForegroundAction::Effect(ForegroundJob::SettlementRefresh {
                            engine: transaction.engine.clone(),
                            scope: transaction.state.scope.clone(),
                            checkout,
                        }))
                    }
                    check_admission::AutomaticCheckAction::Repair {
                        prompt,
                        change_owner,
                    } => {
                        let interaction = &mut transaction.state.interaction;
                        let latest = self
                            .store
                            .latest_exchange(&self.session.id)?
                            .context("automatic repair exchange disappeared")?;
                        anyhow::ensure!(
                            latest.id == interaction.id
                                && latest.state() == ExchangeState::Complete
                                && interaction.check_owner.as_deref()
                                    == Some(change_owner.as_str()),
                            "automatic repair owner changed"
                        );
                        let checks = finalization
                            .checks
                            .as_mut()
                            .context("automatic repair transaction is missing")?;
                        self.record_automatic_repair_exchange(checks, interaction, &change_owner)?;
                        interaction.revalidate_host_settlement(&transaction.state.scope)?;
                        self.store.save_exchange(interaction)?;
                        let request = self.refresh_owned_continuation_request(
                            &transaction.request,
                            ProviderInput::from_text(prompt),
                            PromptMode::Chat,
                        )?;
                        let applied = self.prepare_live_interaction(
                            publication::LiveUpdate::StateChanged,
                            interaction,
                        )?;
                        let delivery = self.prepare_provider_delivery(applied)?;
                        finalization.repair_request = Some(request);
                        finalization.phase = FinalizationPhase::CheckRepairPublication;
                        Ok(ForegroundAction::Effect(
                            ForegroundJob::CheckRepairPublication(delivery),
                        ))
                    }
                    _ => anyhow::bail!("unexpected automatic check continuation"),
                }
            }
            ForegroundSignal::CheckRepairPublished(result) => {
                anyhow::ensure!(
                    finalization.phase == FinalizationPhase::CheckRepairPublication,
                    "automatic repair publication was not pending"
                );
                admission.events.extend(result?);
                let request = finalization
                    .repair_request
                    .take()
                    .context("automatic repair request is missing")?;
                transaction.mode = request.mode;
                transaction.request = request.clone();
                let submission = ProviderSubmission {
                    request,
                    provider: self.provider.clone(),
                    engine: transaction.engine.clone(),
                    sink: transaction.provider_event_sink.clone(),
                    trace: self.trace.clone(),
                    store: self.submission_store.clone(),
                    continuation: None,
                    settlement: transaction.settlement.clone(),
                    retry: Default::default(),
                };
                transaction.native.restart(submission.run())?;
                transaction.native_open = true;
                transaction.question_enabled = true;
                finalization.repair_active = true;
                let old = std::mem::replace(
                    dispatch,
                    super::foreground_dispatch::ForegroundDispatch::Transitioning,
                );
                let super::foreground_dispatch::ForegroundDispatch::Finalizing {
                    transaction,
                    admission,
                } = old
                else {
                    unreachable!("finalization owner checked above")
                };
                *dispatch = super::foreground_dispatch::ForegroundDispatch::Running {
                    transaction,
                    admission,
                };
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::AutomaticCheckRefreshed(result) => {
                let checks = finalization
                    .checks
                    .as_mut()
                    .context("automatic check transaction is missing")?;
                let action = self.apply_automatic_check_refresh(checks, (*result)?)?;
                self.apply_final_check_action(finalization, admission.request.id, action)
            }
            ForegroundSignal::AutomaticCheckCompleted(result) => {
                let checks = finalization
                    .checks
                    .as_mut()
                    .context("automatic check transaction is missing")?;
                let action = self.apply_automatic_check_result(checks, (*result)?)?;
                self.apply_final_check_action(finalization, admission.request.id, action)
            }
            ForegroundSignal::FinalResponsePublished(result) => {
                admission.events.extend(result?);
                match finalization.phase {
                    FinalizationPhase::CheckDiagnostic => {
                        let result = finalization
                            .check_result
                            .take()
                            .context("check diagnostic result is missing")?;
                        self.apply_final_check_action(
                            finalization,
                            admission.request.id,
                            check_admission::AutomaticCheckAction::Finish(result),
                        )
                    }
                    FinalizationPhase::FinalPublication => {
                        finalization.exchange_event_published = true;
                        finalization.phase = FinalizationPhase::FinalDrain;
                        Ok(ForegroundAction::WaitForPublicationDrain)
                    }
                    _ => anyhow::bail!("unexpected final response publication"),
                }
            }
            ForegroundSignal::AutoCompactionPublished(result) => {
                admission.events.extend(result?);
                match finalization.phase {
                    FinalizationPhase::AutoCompactionStart => {
                        let job = finalization
                            .auto_compaction_job
                            .take()
                            .context("automatic compaction job is missing")?;
                        finalization.phase = FinalizationPhase::AutoCompactionRunning;
                        Ok(ForegroundAction::Effect(ForegroundJob::Compaction(
                            Box::new(job),
                        )))
                    }
                    FinalizationPhase::AutoCompactionEnd => {
                        let result = finalization
                            .check_result
                            .take()
                            .context("automatic compaction result is missing")?;
                        self.prepare_final_response(finalization, admission.request.id, result)
                    }
                    _ => anyhow::bail!("unexpected automatic compaction publication"),
                }
            }
            ForegroundSignal::CompactionCompleted(result) => {
                anyhow::ensure!(
                    finalization.phase == FinalizationPhase::AutoCompactionRunning,
                    "automatic compaction is not running"
                );
                let (delivery, error, provenance) = self.commit_owned_compaction(*result)?;
                anyhow::ensure!(
                    matches!(
                        provenance,
                        compaction::CompactionProvenance::Automatic { .. }
                    ),
                    "compaction provenance changed"
                );
                if let Some(error) = error {
                    let (_, events) = finalization
                        .check_result
                        .as_mut()
                        .context("automatic compaction result is missing")?;
                    events.push(self.event(
                        "background_error",
                        json!({"error":format!("Automatic compaction failed: {error:#}")}),
                    )?);
                }
                finalization.phase = FinalizationPhase::AutoCompactionEnd;
                Ok(ForegroundAction::Effect(
                    ForegroundJob::AutoCompactionPublication(delivery),
                ))
            }
            ForegroundSignal::Checkpoint(result) => {
                let runtime = transaction
                    .state
                    .runtime
                    .as_mut()
                    .context("finalization runtime is missing")?;
                finalization.initial_checkpoint_completed(
                    self,
                    runtime,
                    &mut transaction.state.interaction,
                    result,
                )
            }
            ForegroundSignal::FinalCheckpoint(result) => {
                finalization.checkpoint_completed(self, &mut transaction.state.interaction, result)
            }
            ForegroundSignal::SettlementCommitted(result) => {
                let action = finalization.settlement_committed(
                    transaction.engine.clone(),
                    transaction.state.scope.clone(),
                    result,
                )?;
                if !finalization.is_complete() {
                    return Ok(action);
                }
                anyhow::ensure!(
                    finalization.prepared_response.is_some(),
                    "settled response is missing"
                );
                anyhow::ensure!(
                    self.exchange_runtime.is_none(),
                    "another foreground runtime is already installed"
                );
                if admission.idle_reopen {
                    self.reopened_execution
                        .end_settlement(&transaction.state.scope);
                } else {
                    transaction.session_events.monitor_background(
                        transaction.state.scope.clone(),
                        transaction.native.watchdog.clone(),
                        self.provider.clone(),
                    )?;
                }
                self.clear_host_settlement(&transaction.settlement)?;
                let response = finalization
                    .prepared_response
                    .take()
                    .context("settled response is missing")?;
                if transaction.state.interaction.awaiting_input() {
                    self.exchange_runtime = transaction.state.runtime.take();
                } else {
                    transaction.state.runtime.take();
                }
                let events = std::mem::take(&mut admission.events);
                finalization.submission.take();
                let id = transaction.state.scope.invocation_id.clone();
                transaction.steering.active.retire();
                let old = std::mem::replace(dispatch, ForegroundDispatch::Transitioning);
                let ForegroundDispatch::Finalizing {
                    transaction,
                    mut admission,
                } = old
                else {
                    unreachable!("finalization owner checked above")
                };
                let mut transaction = transaction;
                transaction._check_lease.bind_lifecycle(None)?;
                transaction.delivery_sink = None;
                admission.sink = None;
                self.retained_foregrounds.insert(
                    id,
                    reopening::RetainedForeground {
                        transaction,
                        admission,
                    },
                );
                *dispatch = super::foreground_dispatch::ForegroundDispatch::Done(DispatchResult {
                    response,
                    event: events,
                });
                Ok(ForegroundAction::Finish)
            }
            ForegroundSignal::SettlementReady { checkout, ready } => {
                finalization.settlement_refreshed(checkout, *ready)?;
                self.collect_foreground_observations(transaction)?;
                Ok(ForegroundAction::Wait)
            }
            _ => anyhow::bail!("unexpected finalization signal"),
        }
    }

    /// Stage completion effects under the retained host settlement authority.
    fn prepare_foreground_host_effects(
        &mut self,
        transaction: &mut ForegroundTransaction,
        admission: &mut foreground_dispatch::ForegroundAdmission,
    ) -> Result<ForegroundAction> {
        let finalization = transaction
            .finalization
            .as_mut()
            .context("finalization envelope is missing")?;
        // Revalidate the retained exchange before applying completion effects.
        if finalization.needs_host_revalidation {
            let latest = if admission.idle_reopen {
                self.store
                    .load_exchange(&transaction.state.interaction.id)?
            } else {
                self.store.latest_exchange(&self.session.id)?
            }
            .context("settlement exchange disappeared")?;
            anyhow::ensure!(
                latest.id == transaction.state.interaction.id,
                "settlement exchange was superseded"
            );
            if transaction.state.interaction.state() == ExchangeState::Complete {
                anyhow::ensure!(
                    latest.state() == ExchangeState::Complete
                        && latest.turn.len() == transaction.state.interaction.turn.len()
                        && latest.turn.last().map(|turn| turn.provider())
                            == transaction
                                .state
                                .interaction
                                .turn
                                .last()
                                .map(|turn| turn.provider()),
                    "settlement exchange changed during revalidation"
                );
                transaction
                    .state
                    .interaction
                    .revalidate_host_settlement(&transaction.state.scope)?;
                self.store.save_exchange(&transaction.state.interaction)?;
            }
            finalization.needs_host_revalidation = false;
        }
        // Stage the terminal metadata and effects without consuming settlement authority.
        let mut output = match &finalization.outcome {
            Ok(output) => output.clone(),
            Err(ProviderError::Cancelled) => {
                let mut output = ProviderCompletion::default();
                output.native.provider_session_id = transaction.request.provider_session_id.clone();
                output.native.capability = self.capability.clone();
                for batch in finalization.settlement.batches()? {
                    output.merge_execution(batch.completion);
                }
                output
            }
            Err(error) => {
                return Err(anyhow::anyhow!(
                    "native completion failed before host application: {error}"
                ));
            }
        };
        output.dispatched_task_permit = transaction.dispatched_task_permit.clone();
        output.dispatched_execution_id = transaction.dispatched_execution_id.clone();
        output.dispatched_entry_id = transaction.dispatched_entry_id.clone();
        let mut staged = self.stage_host_completion(
            &mut transaction.state.interaction,
            output,
            finalization.mode,
        )?;
        if staged.output.goal_control.is_some()
            && staged.output.plan_question.is_none()
            && self.question_discussion
            && !self.questions.snapshot()?.required_pending()
            && let Some(discussion) = admission.question_discussion.take()
        {
            self.finish_question_discussion(discussion)?;
        }
        if !staged.output.plan_edit.is_empty() || !staged.output.design_patch.is_empty() {
            let plan_id = self
                .current_plan_id()?
                .context("planning turn has no active canonical plan")?;
            let plan = self
                .store
                .load_plan(&plan_id)?
                .context("planning turn has no active canonical plan")?;
            let permit = staged
                .output
                .dispatched_task_permit
                .clone()
                .context("planning effects have no task authority")?;
            let mut queue =
                foreground_plan_effect::PlanMutationQueue::new(&mut staged, plan, permit)?;
            let job = queue
                .next_job(self)?
                .context("planning effect queue is empty")?;
            finalization.staged = Some(staged);
            finalization.plan_mutations = Some(queue);
            return Ok(ForegroundAction::Effect(ForegroundJob::PlanMutation(
                Box::new(job),
            )));
        }
        let ordinary_chat = staged.mode == PromptMode::Chat
            && staged.expected_effects.is_empty()
            && staged.output.plan_submit.is_none()
            && staged.output.plan_question.is_none()
            && staged.output.plan_read.is_none()
            && staged.output.question_answer.is_none()
            && staged.output.question_withdrawal.is_none()
            && staged.output.goal_control.is_none()
            && staged.output.dispatched_task_permit.is_none()
            && transaction.state.interaction.elicitation().is_none()
            && !transaction.state.interaction.awaiting_input()
            && self.task_controller.current()?.is_none();
        if ordinary_chat {
            self.session.updated_at_ms = self.clock.now_ms();
            self.save_session()?;
            finalization
                .settlement
                .seal_effects(&staged.expected_effects)?;
            finalization.staged = Some(staged);
            return finalization.effects_applied();
        }
        let planning_without_controls =
            matches!(staged.mode, PromptMode::Plan | PromptMode::PlanDiscussion)
                && staged.expected_effects.is_empty()
                && staged.output.plan_submit.is_none()
                && staged.output.plan_question.is_none()
                && staged.output.question_answer.is_none()
                && staged.output.question_withdrawal.is_none()
                && staged.output.goal_control.is_none();
        if planning_without_controls {
            if let Some(requested) = staged.output.plan_read.take() {
                anyhow::ensure!(
                    self.current_plan_id()?.as_deref() == Some(requested.as_str()),
                    "requested plan is not active"
                );
            }
            self.session.updated_at_ms = self.clock.now_ms();
            self.save_session()?;
            finalization
                .settlement
                .seal_effects(&staged.expected_effects)?;
            finalization.staged = Some(staged);
            return finalization.effects_applied();
        }
        finalization.staged = Some(staged);
        if let Some(action) = self.schedule_plan_submission(finalization)? {
            return Ok(action);
        }
        if let Some(action) = self.schedule_plan_question(finalization)? {
            return Ok(action);
        }
        if let Some(action) =
            self.apply_nonplan_question(finalization, &mut transaction.state.interaction)?
        {
            return Ok(action);
        }
        self.schedule_remaining_host_effects(finalization, &transaction.state.interaction)
    }

    /// Stage automatic checks after completion effects have been published.
    fn prepare_foreground_final_checks(
        &mut self,
        transaction: &mut ForegroundTransaction,
        admission: &mut foreground_dispatch::ForegroundAdmission,
    ) -> Result<ForegroundAction> {
        let finalization = transaction
            .finalization
            .as_mut()
            .context("finalization envelope is missing")?;
        // Finish admission bookkeeping before constructing automatic check ownership.
        anyhow::ensure!(
            finalization.phase == FinalizationPhase::ChecksRequested,
            "automatic checks were not requested"
        );
        if !finalization.admission_postprocessed {
            if admission.reset_check_setup {
                self.check_setup = false;
                admission.reset_check_setup = false;
            }
            if let Some(discussion) = admission.question_discussion.take() {
                self.finish_question_discussion(discussion)?;
            }
            if let crate::input::InputPurpose::QuestionContinuation { exchange_id } =
                &admission.purpose
            {
                self.questions.chat_continuation_completed(exchange_id)?;
            }
            finalization.admission_postprocessed = true;
        }
        let interaction = &transaction.state.interaction;
        if let Some(planning) = admission.planning.as_ref() {
            let turn = crate::task::TaskTurnId {
                exchange_ordinal: interaction.ordinal,
                turn_ordinal: interaction.turn.len(),
            };
            let questions = self.questions.snapshot()?;
            if !questions.required_pending()
                && !questions.required_in_exchange(&interaction.id)
                && finalization.planning_observed_turn.as_ref() != Some(&turn)
            {
                finalization
                    .events
                    .extend(self.finish_owned_planning_observation(
                        planning,
                        interaction,
                        turn.clone(),
                    )?);
                finalization.planning_observed_turn = Some(turn);
            }
        }
        let value = json!({ "exchange": {"id": interaction.id, "state": interaction.state()},
            "session": self.session_view()?, "capability": self.capability });
        let mut events = std::mem::take(&mut finalization.events);
        if !finalization.exchange_event_published && !finalization.exchange_event_staged {
            events.push(self.event(
                if interaction.awaiting_input() {
                    "exchange_updated"
                } else {
                    "exchange_complete"
                },
                json!({"id": interaction.id, "state": interaction.state()}),
            )?);
            finalization.exchange_event_staged = true;
        }
        if finalization.repair_active {
            let checks = finalization
                .checks
                .as_mut()
                .context("automatic repair transaction is missing")?;
            let action = self.apply_automatic_check_repair(checks, (value, events))?;
            finalization.repair_active = false;
            return self.apply_final_check_action(finalization, admission.request.id, action);
        }
        let (checks, action) = if admission.automatic_check_owner.is_some()
            || matches!(finalization.outcome, Err(ProviderError::Cancelled))
        {
            (
                None,
                check_admission::AutomaticCheckAction::Finish((value, events)),
            )
        } else {
            self.begin_automatic_checks((value, events), finalization.mode)?
        };
        finalization.checks = checks;
        self.apply_final_check_action(finalization, admission.request.id, action)
    }

    /// Advance checkpoint or settlement ownership after publication drains.
    fn complete_foreground_publication(
        &mut self,
        transaction: &mut ForegroundTransaction,
        admission: &mut foreground_dispatch::ForegroundAdmission,
    ) -> Result<ForegroundAction> {
        let finalization = transaction
            .finalization
            .as_mut()
            .context("finalization envelope is missing")?;
        // Commit the final publication barrier or admit an owned continuation.
        if finalization.phase == FinalizationPhase::FinalDrain {
            let action = finalization.checks_delivered()?;
            if let ForegroundAction::Effect(ForegroundJob::SettlementCommit {
                cell,
                mut undispatched,
            }) = action
            {
                // Retention preserves event ownership, never a pending input reply.
                // End admission before draining so no command can enter the retired lane.
                transaction.steering.active.retire();
                undispatched.extend(transaction.steering.deferred.drain(..));
                while let Some(input) = transaction.steering.active.try_receive() {
                    undispatched.push(input);
                }
                return Ok(ForegroundAction::Effect(ForegroundJob::SettlementCommit {
                    cell,
                    undispatched,
                }));
            }
            return Ok(action);
        }
        if let Some((request, events)) = self.prepare_owned_clarification(
            &mut transaction.state.interaction,
            &transaction.request,
            admission,
        )? {
            let mut delivery = self.prepare_provider_delivery(AppliedProviderEvent::None)?;
            delivery.events = events;
            finalization.clarification_request = Some(request);
            finalization.phase = FinalizationPhase::ClarificationPublication;
            return Ok(ForegroundAction::Effect(
                ForegroundJob::ClarificationPublication(delivery),
            ));
        }
        let outcome = match &finalization.outcome {
            Ok(_) => crate::exchange::ExchangeOutcome::Complete,
            Err(ProviderError::Cancelled) => crate::exchange::ExchangeOutcome::Cancelled,
            Err(_) => crate::exchange::ExchangeOutcome::Failed,
        };
        let runtime = transaction
            .state
            .runtime
            .as_mut()
            .context("finalization runtime is missing")?;
        finalization.effects_delivered(self, runtime, &mut transaction.state.interaction, outcome)
    }

    /// Advance automatic checks without retaining the broker during discovery or execution.
    fn apply_final_check_action(
        &mut self,
        finalization: &mut ForegroundFinalization,
        request_id: u64,
        action: check_admission::AutomaticCheckAction,
    ) -> Result<ForegroundAction> {
        use check_admission::AutomaticCheckAction;
        match action {
            AutomaticCheckAction::Refresh(job) => Ok(ForegroundAction::Effect(
                ForegroundJob::AutomaticCheckRefresh(Box::new(job)),
            )),
            AutomaticCheckAction::Run(job) => Ok(ForegroundAction::Effect(
                ForegroundJob::AutomaticCheckRun(job),
            )),
            AutomaticCheckAction::Publish { update, result } => {
                finalization.check_result = Some(result);
                finalization.phase = FinalizationPhase::CheckDiagnostic;
                let delivery =
                    self.prepare_provider_delivery(AppliedProviderEvent::Update(update))?;
                Ok(ForegroundAction::Effect(
                    ForegroundJob::FinalResponsePublication(delivery),
                ))
            }
            AutomaticCheckAction::Finish(result) => {
                if finalization.pending_stale_continuation.is_some() {
                    finalization.events.extend(result.1);
                    finalization.check_continuation_requested = false;
                    finalization.phase = FinalizationPhase::ResumeStale;
                    Ok(ForegroundAction::Wait)
                } else {
                    self.prepare_final_compaction(finalization, request_id, result)
                }
            }
            action @ (AutomaticCheckAction::Repair { .. } | AutomaticCheckAction::Revalidate) => {
                finalization.pending_check_action = Some(action);
                finalization.check_continuation_requested = false;
                Ok(ForegroundAction::Wait)
            }
        }
    }

    fn prepare_final_compaction(
        &mut self,
        finalization: &mut ForegroundFinalization,
        request_id: u64,
        mut result: (Value, Vec<SessionEvent>),
    ) -> Result<ForegroundAction> {
        if finalization.mode != PromptMode::Background
            && !matches!(finalization.outcome, Err(ProviderError::Cancelled))
        {
            match self.prepare_automatic_compaction() {
                Ok(Some((job, delivery))) => {
                    finalization.auto_compaction_job = Some(job);
                    finalization.check_result = Some(result);
                    finalization.phase = FinalizationPhase::AutoCompactionStart;
                    return Ok(ForegroundAction::Effect(
                        ForegroundJob::AutoCompactionPublication(delivery),
                    ));
                }
                Ok(None) => {}
                Err(error) => result.1.push(self.event(
                    "background_error",
                    json!({"error":format!("Automatic compaction failed: {error:#}")}),
                )?),
            }
        }
        self.prepare_final_response(finalization, request_id, result)
    }

    fn prepare_final_response(
        &mut self,
        finalization: &mut ForegroundFinalization,
        request_id: u64,
        (value, mut events): (Value, Vec<SessionEvent>),
    ) -> Result<ForegroundAction> {
        self.refresh_input_schedule()?;
        self.active_wait_projection = None;
        let response = if matches!(finalization.outcome, Err(ProviderError::Cancelled)) {
            Response::failure(request_id, "turn_cancelled", "Turn cancelled by user")
        } else {
            Response::success(request_id, value)?
        };
        let patch = self.reconcile_after_dispatch()?;
        let mut delivery = self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))?;
        events.append(&mut delivery.events);
        delivery.events = events;
        finalization.prepared_response = Some(response);
        finalization.phase = FinalizationPhase::FinalPublication;
        Ok(ForegroundAction::Effect(
            ForegroundJob::FinalResponsePublication(delivery),
        ))
    }

    /// Record one planning turn once, retaining any resulting presentation for delivery.
    fn finish_owned_planning_observation(
        &mut self,
        context: &foreground_dispatch::PlanningContext,
        interaction: &Exchange,
        turn: crate::task::TaskTurnId,
    ) -> Result<Vec<SessionEvent>> {
        let Some(current) = self.task_controller.current()?.filter(|task| matches!(&task.workflow,
            crate::task::WorkflowSnapshot::Plan { plan } if plan.artifact.id == context.plan_id && !plan.paused)) else { return Ok(Vec::new()) };
        if context.permit.validate_observation(Some(&current)).is_err() {
            return Ok(Vec::new());
        }
        let crate::task::WorkflowSnapshot::Plan { plan } = &current.workflow else {
            unreachable!()
        };
        if context.discussion && plan.artifact.state == PlanState::AwaitingReview {
            return Ok(Vec::new());
        }
        let snapshot = self.questions.snapshot()?;
        if snapshot.required_pending() || snapshot.required_in_exchange(&interaction.id) {
            return Ok(Vec::new());
        }
        let document = self
            .plan_file
            .read_working_document(&self.session.id, &context.plan_id)?;
        let observed = match self.task_controller.observe_plan(
            &mut self.store,
            &current.permit(),
            turn,
            document.version != context.before_version,
            self.clock.now_ms(),
        ) {
            Ok(observed) => observed,
            Err(error) if error.is::<crate::task::StaleTask>() => return Ok(Vec::new()),
            Err(error) => return Err(error),
        };
        let Some((plan, _)) = observed else {
            return Ok(Vec::new());
        };
        let mut events = Vec::new();
        if plan.state == PlanState::Failed {
            events.push(self.event(
                "plan_generation_failed",
                json!({"plan":plan,
                "reason":"planning stopped without submitting the canonical plan"}),
            )?);
        }
        if let Some(task) = self.task_controller.current()?
            && let Some(command) = crate::task::command::SessionCommand::approved_revision(&task)?
        {
            events.extend(self.commit_session_command(command)?.1);
        }
        Ok(events)
    }

    /// Schedule the terminal plan revision only after its ordered edits have committed.
    fn schedule_plan_submission(
        &self,
        finalization: &mut ForegroundFinalization,
    ) -> Result<Option<ForegroundAction>> {
        let staged = finalization
            .staged
            .as_mut()
            .context("host completion is missing")?;
        let Some(request) = staged.output.plan_submit.as_ref().cloned() else {
            return Ok(None);
        };
        let id = staged
            .batches
            .as_ref()
            .and_then(|batches| {
                batches
                    .iter()
                    .rev()
                    .find(|batch| batch.completion.plan_submit.is_some())
            })
            .map(|batch| crate::provider::session_execution::HostEffectId {
                batch_revision: batch.revision,
                kind: crate::provider::session_execution::HostEffectKind::PlanSubmit,
                ordinal: 0,
            })
            .context("plan submission has no batch provenance")?;
        let permit = staged
            .output
            .dispatched_task_permit
            .clone()
            .context("plan submission task authority is missing")?;
        permit.validate(self.task_controller.current()?.as_ref())?;
        let plan = match &finalization.plan_current {
            Some(plan) => plan.clone(),
            None => {
                let id = self
                    .current_plan_id()?
                    .context("planning turn has no active canonical plan")?;
                self.store
                    .load_plan(&id)?
                    .context("planning turn has no active canonical plan")?
            }
        };
        let job = foreground_plan_effect::PlanSubmissionJob {
            store: self.plan_file.clone(),
            session_id: self.session.id.clone(),
            workspace: PathBuf::from(&self.session.workspace),
            plan: plan.clone(),
            request,
            id,
            cell: finalization.settlement.clone(),
            task_controller: self.task_controller.clone(),
            permit,
        };
        staged.output.plan_submit.take();
        finalization.plan_current = Some(plan);
        finalization.plan_submission_id = Some(id);
        Ok(Some(ForegroundAction::Effect(
            ForegroundJob::PlanSubmission(Box::new(job)),
        )))
    }

    fn schedule_plan_question(
        &self,
        finalization: &mut ForegroundFinalization,
    ) -> Result<Option<ForegroundAction>> {
        let staged = finalization
            .staged
            .as_mut()
            .context("host completion is missing")?;
        if staged.mode != PromptMode::Plan
            && !(staged.mode == PromptMode::PlanDiscussion && finalization.plan_mutations.is_some())
        {
            return Ok(None);
        }
        let Some(question) = staged.output.plan_question.as_ref().cloned() else {
            return Ok(None);
        };
        let id = staged
            .batches
            .as_ref()
            .and_then(|batches| {
                batches
                    .iter()
                    .rev()
                    .find(|batch| batch.completion.plan_question.is_some())
            })
            .map(|batch| crate::provider::session_execution::HostEffectId {
                batch_revision: batch.revision,
                kind: crate::provider::session_execution::HostEffectKind::PlanQuestion,
                ordinal: 0,
            })
            .context("plan question has no batch provenance")?;
        let permit = staged
            .output
            .dispatched_task_permit
            .as_ref()
            .context("plan question task authority is missing")?;
        permit.validate(self.task_controller.current()?.as_ref())?;
        let plan = match &finalization.plan_current {
            Some(plan) => plan.clone(),
            None => {
                let id = self
                    .current_plan_id()?
                    .context("planning turn has no active canonical plan")?;
                self.store
                    .load_plan(&id)?
                    .context("planning turn has no active canonical plan")?
            }
        };
        let job = foreground_plan_effect::PlanQuestionJob {
            store: self.plan_file.clone(),
            session_id: self.session.id.clone(),
            plan: plan.clone(),
            question,
            id,
            cell: finalization.settlement.clone(),
        };
        staged.output.plan_question.take();
        finalization.plan_current = Some(plan);
        finalization.plan_submission_id = Some(id);
        Ok(Some(ForegroundAction::Effect(ForegroundJob::PlanQuestion(
            Box::new(job),
        ))))
    }

    fn apply_nonplan_question(
        &mut self,
        finalization: &mut ForegroundFinalization,
        interaction: &mut Exchange,
    ) -> Result<Option<ForegroundAction>> {
        let staged = finalization
            .staged
            .as_mut()
            .context("host completion is missing")?;
        if staged.mode == PromptMode::Plan
            || (staged.mode == PromptMode::PlanDiscussion && finalization.plan_mutations.is_some())
        {
            return Ok(None);
        }
        let Some(effect) = self.apply_owned_question_effect(interaction, staged)? else {
            return Ok(None);
        };
        finalization.nonplan_question_terminal = true;
        finalization.plan_submission_id = Some(effect.id);
        if effect.needs_publication {
            let mut delivery = self.prepare_provider_delivery(AppliedProviderEvent::None)?;
            delivery.events = effect.events;
            finalization.plan_submission_publication_pending = true;
            return Ok(Some(ForegroundAction::Effect(
                ForegroundJob::PlanTerminalPublication {
                    delivery,
                    cell: finalization.settlement.clone(),
                    id: effect.id,
                },
            )));
        }
        Ok(Some(self.finish_nonplan_question_effects(
            finalization,
            interaction,
        )?))
    }

    fn finish_nonplan_question_effects(
        &mut self,
        finalization: &mut ForegroundFinalization,
        interaction: &Exchange,
    ) -> Result<ForegroundAction> {
        self.schedule_remaining_host_effects(finalization, interaction)
    }

    /// Capture goal evidence outside the broker and retain its per-turn receipt.
    fn schedule_remaining_host_effects(
        &mut self,
        finalization: &mut ForegroundFinalization,
        interaction: &Exchange,
    ) -> Result<ForegroundAction> {
        use crate::provider::session_execution::ResultEffectAdmission;
        let staged = finalization
            .staged
            .as_mut()
            .context("host completion is missing")?;
        if let Some(read_plan_id) = staged.output.plan_read.take() {
            anyhow::ensure!(
                self.current_plan_id()?.as_deref() == Some(read_plan_id.as_str()),
                "requested plan is not active"
            );
        }
        let snapshot = self.questions.snapshot()?;
        let required_stop =
            snapshot.required_in_exchange(&interaction.id) && !self.question_discussion;
        let paused = interaction.awaiting_input()
            || snapshot.required_pending()
            || required_stop
            || self.question_discussion;
        if required_stop
            && let Some(task) = self.task_controller.current()?
            && task.status() == crate::task::TaskStatus::Running
            && task.entry_accepted()
        {
            let turn = crate::task::TaskTurnId {
                exchange_ordinal: interaction.ordinal,
                turn_ordinal: interaction.turn.len(),
            };
            if task.kind() == crate::task::TaskKind::Execute {
                self.reserve_execution_continuation(turn)?;
            } else {
                self.task_controller.reserve_continuation(&mut self.store, &task.permit(), turn,
                    "Continue the current task at its preserved phase using the recorded user answers. Do not repeat resolved questions.".into(),
                    "Continue after required questions are resolved".into(), self.clock.now_ms())?;
            }
        }
        if paused {
            if staged.output.goal_control.is_some() {
                finalization.goal_waiting_questions = true;
                return Ok(ForegroundAction::Wait);
            }
            return self.finish_goal_effects(finalization);
        }
        let (_, permit) = staged.output.goal_evidence();
        let Some(permit) = permit else {
            return self.finish_goal_effects(finalization);
        };
        let Some(task) = self.task_controller.current()? else {
            return self.finish_goal_effects(finalization);
        };
        if permit.validate_observation(Some(&task)).is_err() {
            return self.finish_goal_effects(finalization);
        }
        if task.kind() == crate::task::TaskKind::Plan {
            return self.finish_goal_effects(finalization);
        }
        let id = interaction::goal_evidence_id(staged)?;
        if !staged.expected_effects.contains(&id) {
            staged.expected_effects.push(id);
        }
        match finalization.settlement.begin_result_effect(id)? {
            ResultEffectAdmission::Applied(output) => {
                finalization.goal_continuing = output
                    .continuing
                    .context("goal receipt has no continuation outcome")?;
                if !output.publication_drained {
                    let mut delivery =
                        self.prepare_provider_delivery(AppliedProviderEvent::None)?;
                    delivery.events = output.events;
                    finalization.plan_submission_publication_pending = true;
                    finalization.goal_terminal_pending = true;
                    return Ok(ForegroundAction::Effect(
                        ForegroundJob::PlanTerminalPublication {
                            delivery,
                            cell: finalization.settlement.clone(),
                            id,
                        },
                    ));
                }
                self.finish_goal_effects(finalization)
            }
            ResultEffectAdmission::Apply(guard) => match &task.workflow {
                crate::task::WorkflowSnapshot::Goal { goal } => {
                    let previous_id = goal.observed_checkpoint.clone();
                    let previous = previous_id
                        .as_ref()
                        .map(|id| self.store.load_checkpoint(id))
                        .transpose()?
                        .flatten();
                    let job = foreground_goal_effect::GoalProgressJob {
                        capture: self.prepare_workspace_capture(),
                        previous,
                        previous_id,
                        permit,
                        guard,
                    };
                    Ok(ForegroundAction::Effect(ForegroundJob::GoalProgress(
                        Box::new(job),
                    )))
                }
                crate::task::WorkflowSnapshot::Execute { .. } => {
                    if let Some(progress) = self.prepare_plan_progress(interaction)? {
                        let job = foreground_goal_effect::ExecutionProgressJob {
                            progress,
                            permit,
                            guard,
                        };
                        Ok(ForegroundAction::Effect(ForegroundJob::ExecutionProgress(
                            Box::new(job),
                        )))
                    } else {
                        self.commit_owned_execution_evidence(
                            finalization,
                            interaction,
                            permit,
                            guard,
                            None,
                        )
                    }
                }
                crate::task::WorkflowSnapshot::Plan { .. } => {
                    guard.cancel_definite()?;
                    self.finish_goal_effects(finalization)
                }
            },
        }
    }

    /// Commit Execute-plan progress and its goal evidence under the retained effect receipt.
    fn commit_owned_execution_evidence(
        &mut self,
        finalization: &mut ForegroundFinalization,
        interaction: &Exchange,
        permit: crate::task::TaskPermit,
        mut guard: crate::provider::session_execution::HostEffectGuard,
        progress: Option<execution_progress::PlanProgressResult>,
    ) -> Result<ForegroundAction> {
        use crate::provider::session_execution::HostEffectOutput;
        permit.validate_observation(self.task_controller.current()?.as_ref())?;
        let has_progress = progress.is_some();
        guard.begin_mutation();
        let changed = match progress {
            Some(progress) => match self.commit_plan_progress(progress) {
                Ok(changed) => changed,
                Err(error) => {
                    if error.is::<crate::task::StaleTask>()
                        || error.is::<crate::plan::WorkingDocumentChanged>()
                    {
                        guard.cancel_definite()?;
                    }
                    return Err(error);
                }
            },
            None => false,
        };
        let staged = finalization
            .staged
            .as_mut()
            .context("host completion is missing")?;
        staged.output.evidence.structured_complete = false;
        staged.output.evidence.tool_called = false;
        staged.output.evidence.workspace_changed = changed;
        let (evidence, evidence_permit) = staged.output.goal_evidence();
        let turn = crate::task::TaskTurnId {
            exchange_ordinal: interaction.ordinal,
            turn_ordinal: interaction.turn.len(),
        };
        let mut events = Vec::new();
        let continuing =
            match self.apply_goal_evidence_strict(evidence, evidence_permit, turn, &mut events) {
                Ok(continuing) => continuing,
                Err(error) => {
                    if !has_progress && error.is::<crate::task::StaleTask>() {
                        guard.cancel_definite()?;
                    }
                    return Err(error);
                }
            };
        let id = interaction::goal_evidence_id(staged)?;
        guard.commit_output(HostEffectOutput {
            nodes: Vec::new(),
            events: events.clone(),
            elicitation: None,
            awaiting_input: false,
            publication_drained: false,
            plan_document: None,
            continuing: Some(continuing),
            plan_record: None,
        })?;
        finalization.goal_continuing = continuing;
        let mut delivery = self.prepare_provider_delivery(AppliedProviderEvent::None)?;
        delivery.events = events;
        finalization.plan_submission_publication_pending = true;
        finalization.goal_terminal_pending = true;
        Ok(ForegroundAction::Effect(
            ForegroundJob::PlanTerminalPublication {
                delivery,
                cell: finalization.settlement.clone(),
                id,
            },
        ))
    }

    /// Resume deferred goal evidence only after required questions have resolved.
    pub(crate) fn resume_foreground_goal_after_question(
        &mut self,
        dispatch: &mut super::foreground_dispatch::ForegroundDispatch,
    ) -> Result<ForegroundAction> {
        let super::foreground_dispatch::ForegroundDispatch::Finalizing {
            transaction,
            admission,
        } = dispatch
        else {
            anyhow::bail!("goal evidence has no finalizing owner");
        };
        let finalization = transaction
            .finalization
            .as_mut()
            .context("finalization envelope is missing")?;
        anyhow::ensure!(
            finalization.goal_waiting_questions,
            "goal evidence is not waiting for questions"
        );
        if let Some(discussion) = admission.question_discussion.take() {
            self.finish_question_discussion(discussion)?;
        }
        if let Some(owner) = self
            .store
            .load_exchange_metadata(&transaction.state.interaction.id)?
        {
            transaction
                .state
                .interaction
                .set_elicitation(owner.elicitation().cloned());
            transaction
                .state
                .interaction
                .set_awaiting_input(owner.awaiting_input());
        }
        let snapshot = self.questions.snapshot()?;
        if snapshot.required_pending()
            || snapshot.required_in_exchange(&transaction.state.interaction.id)
            || transaction.state.interaction.awaiting_input()
            || self.question_discussion
        {
            transaction.question_enabled = true;
            return Ok(ForegroundAction::Wait);
        }
        finalization.goal_waiting_questions = false;
        self.schedule_remaining_host_effects(finalization, &transaction.state.interaction)
    }

    /// Apply captured workspace and goal evidence only to the still-current task owner.
    fn commit_owned_goal_progress(
        &mut self,
        finalization: &mut ForegroundFinalization,
        interaction: &Exchange,
        result: foreground_goal_effect::GoalProgressResult,
    ) -> Result<ForegroundAction> {
        use crate::provider::session_execution::HostEffectOutput;
        let (changed, guard) = result.commit_workspace(self)?;
        let staged = finalization
            .staged
            .as_mut()
            .context("host completion is missing")?;
        staged.output.evidence.workspace_changed = changed;
        let (evidence, evidence_permit) = staged.output.goal_evidence();
        let turn = crate::task::TaskTurnId {
            exchange_ordinal: interaction.ordinal,
            turn_ordinal: interaction.turn.len(),
        };
        let mut events = Vec::new();
        let continuing =
            self.apply_goal_evidence_strict(evidence, evidence_permit, turn, &mut events)?;
        let id = interaction::goal_evidence_id(staged)?;
        guard.commit_output(HostEffectOutput {
            nodes: Vec::new(),
            events: events.clone(),
            elicitation: None,
            awaiting_input: false,
            publication_drained: false,
            plan_document: None,
            continuing: Some(continuing),
            plan_record: None,
        })?;
        finalization.goal_continuing = continuing;
        let mut delivery = self.prepare_provider_delivery(AppliedProviderEvent::None)?;
        delivery.events = events;
        finalization.plan_submission_publication_pending = true;
        finalization.goal_terminal_pending = true;
        Ok(ForegroundAction::Effect(
            ForegroundJob::PlanTerminalPublication {
                delivery,
                cell: finalization.settlement.clone(),
                id,
            },
        ))
    }

    fn finish_goal_effects(
        &mut self,
        finalization: &mut ForegroundFinalization,
    ) -> Result<ForegroundAction> {
        let staged = finalization
            .staged
            .as_ref()
            .context("host completion is missing")?;
        self.session.updated_at_ms = self.clock.now_ms();
        self.save_session()?;
        finalization
            .settlement
            .seal_effects(&staged.expected_effects)?;
        finalization.effects_applied()
    }

    /// Complete the plan submission branch after every mutation and its publication has settled.
    fn finish_plan_terminal_effects(
        &mut self,
        finalization: &mut ForegroundFinalization,
    ) -> Result<ForegroundAction> {
        let staged = finalization
            .staged
            .as_mut()
            .context("host completion is missing")?;
        let plan = finalization
            .plan_current
            .as_ref()
            .context("plan submission owner is missing")?;
        if let Some(read_plan_id) = staged.output.plan_read.take() {
            anyhow::ensure!(read_plan_id == plan.id, "requested plan is not active");
        }
        if staged.output.plan_question.is_some()
            || staged.output.question_answer.is_some()
            || staged.output.question_withdrawal.is_some()
            || staged.output.goal_control.is_some()
        {
            return Ok(ForegroundAction::Wait);
        }
        let task = self
            .task_controller
            .current()?
            .context("planning task disappeared")?;
        anyhow::ensure!(
            task.kind() == crate::task::TaskKind::Plan,
            "plan submission task changed"
        );
        self.session.updated_at_ms = self.clock.now_ms();
        self.save_session()?;
        finalization
            .settlement
            .seal_effects(&staged.expected_effects)?;
        finalization.effects_applied()
    }
}
