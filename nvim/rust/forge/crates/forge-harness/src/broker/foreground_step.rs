//! Apply one owned foreground signal under the broker's short state lock.

use super::*;

impl HarnessBroker {
    /// Route one admitted signal to its owner-checked state transition.
    pub(crate) fn step_foreground(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        signal: ForegroundSignal,
    ) -> Result<ForegroundAction> {
        let receipt = match dispatch {
            ForegroundDispatch::Admitting(admission)
            | ForegroundDispatch::Running { admission, .. }
            | ForegroundDispatch::Finalizing { admission, .. } => admission.input_submission.take(),
            ForegroundDispatch::Done(_) | ForegroundDispatch::Transitioning => None,
        };
        let previous = std::mem::replace(&mut self.input_submission, receipt);
        let result = self.step_foreground_inner(dispatch, signal);
        let receipt = std::mem::replace(&mut self.input_submission, previous);
        match dispatch {
            ForegroundDispatch::Admitting(admission)
            | ForegroundDispatch::Running { admission, .. }
            | ForegroundDispatch::Finalizing { admission, .. } => {
                admission.input_submission = receipt
            }
            ForegroundDispatch::Done(_) | ForegroundDispatch::Transitioning => {}
        }
        result
    }

    /// Apply one signal with the current admission's receipt installed for legacy reducers.
    fn step_foreground_inner(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        signal: ForegroundSignal,
    ) -> Result<ForegroundAction> {
        let signal = match signal {
            ForegroundSignal::LiveHost(request) => {
                return self.begin_live_host_effect(dispatch, *request);
            }
            ForegroundSignal::PlanMutationCompleted(result) if matches!(dispatch, ForegroundDispatch::Running { transaction, .. } | ForegroundDispatch::Finalizing { transaction, .. } if transaction.live_host.is_some()) =>
            {
                return self.complete_live_plan_mutation(dispatch, result);
            }
            ForegroundSignal::PlanSubmissionCompleted(result) if matches!(dispatch, ForegroundDispatch::Running { transaction, .. } | ForegroundDispatch::Finalizing { transaction, .. } if transaction.live_host.is_some()) =>
            {
                return self.complete_live_plan_submission(dispatch, result);
            }
            ForegroundSignal::PlanTerminalPublished(result) if matches!(dispatch, ForegroundDispatch::Running { transaction, .. } | ForegroundDispatch::Finalizing { transaction, .. } if transaction.live_host.is_some()) =>
            {
                return self.complete_live_host_publication(dispatch, result);
            }
            ForegroundSignal::AdmissionCleanupCompleted(result) => {
                return self.complete_staged_admission_cleanup(dispatch, result);
            }
            ForegroundSignal::AdmissionFailureCheckpointCompleted(result) => {
                return self.complete_staged_admission_checkpoint(dispatch, result);
            }
            ForegroundSignal::FailureAdmissionEventsForwarded(result) => {
                return self.foreground_failure_admission_events_forwarded(dispatch, result);
            }
            ForegroundSignal::FailurePublicationCompleted(result) => {
                return self.foreground_failure_published(dispatch, result);
            }
            ForegroundSignal::FailureCheckpointCompleted(result) => {
                return self.complete_foreground_failure_checkpoint(dispatch, result);
            }
            ForegroundSignal::FailureBaselineCompleted(result) => {
                return self.complete_failure_baseline(dispatch, result);
            }
            signal => signal,
        };
        let failure_waiting = match &*dispatch {
            ForegroundDispatch::Admitting(admission)
            | ForegroundDispatch::Running { admission, .. }
            | ForegroundDispatch::Finalizing { admission, .. } => admission.failure.is_some(),
            ForegroundDispatch::Done(_) | ForegroundDispatch::Transitioning => false,
        };
        if failure_waiting && matches!(&signal, ForegroundSignal::PublicationDrained) {
            return self.finish_foreground_failure(dispatch);
        }
        let signal = match signal {
            ForegroundSignal::SettlementCommitted(result) if failure_waiting => {
                return self.complete_failure_settlement_commit(dispatch, result);
            }
            signal => signal,
        };
        let failure_cleanup = match &*dispatch {
            ForegroundDispatch::Running { transaction, .. }
            | ForegroundDispatch::Finalizing { transaction, .. } => {
                transaction.failure_cleanup_active
            }
            _ => false,
        };
        let signal = match signal {
            ForegroundSignal::Checkpoint(result) if failure_cleanup => {
                let transaction = match dispatch {
                    ForegroundDispatch::Running { transaction, .. }
                    | ForegroundDispatch::Finalizing { transaction, .. } => transaction,
                    _ => unreachable!(),
                };
                self.apply_foreground_failure_checkpoint(transaction, result)?;
                return self.advance_foreground_failure(dispatch);
            }
            signal => signal,
        };
        if matches!(
            &signal,
            ForegroundSignal::Native(NativeForegroundSignal::NativeReady(_))
        ) && let ForegroundDispatch::Running { transaction, .. }
        | ForegroundDispatch::Finalizing { transaction, .. } = dispatch
        {
            transaction.acknowledge_native_result();
        }
        if let ForegroundDispatch::Admitting(admission) = dispatch {
            match &signal {
                ForegroundSignal::CollectionStarted(disposition) => {
                    admission.begin_owner_collection(*disposition)?;
                    return Ok(ForegroundAction::Wait);
                }
                ForegroundSignal::CollectionCompleted(disposition) => {
                    admission.complete_owner_collection(*disposition)?;
                    self.fail_foreground_dispatch(dispatch, TurnCancelled.into())?;
                    return Ok(ForegroundAction::WaitForPublicationDrain);
                }
                _ => {}
            }
        }
        let signal = match signal {
            ForegroundSignal::EffectFailed(error)
                if matches!(dispatch, ForegroundDispatch::Admitting(_)) =>
            {
                return self.fail_foreground_dispatch(dispatch, error);
            }
            signal => signal,
        };
        if matches!(&signal, ForegroundSignal::AdmissionPrepared) {
            return self.admit_prepared_foreground(dispatch);
        }
        if matches!(&*dispatch, ForegroundDispatch::Admitting(_)) {
            let ForegroundDispatch::Admitting(admission) = dispatch else {
                unreachable!()
            };
            return self.step_foreground_admission(admission, signal);
        }
        if matches!(&signal, ForegroundSignal::QuestionChanged)
            && self.stale_continuation_waits_for_question(dispatch)
        {
            return self.resume_stale_after_question(dispatch);
        }
        if matches!(&signal, ForegroundSignal::QuestionChanged)
            && matches!(&*dispatch, ForegroundDispatch::Finalizing { transaction, .. }
                if transaction.finalization.as_ref().is_some_and(|finalization| finalization.goal_waiting_questions))
        {
            return self.resume_foreground_goal_after_question(dispatch);
        }
        if matches!(&*dispatch, ForegroundDispatch::Finalizing { .. })
            && matches!(
                &signal,
                ForegroundSignal::FinalizeEffectsRequested
                    | ForegroundSignal::FinalizeChecksRequested
                    | ForegroundSignal::FinalizeCheckContinuationRequested
                    | ForegroundSignal::PublicationDrained
                    | ForegroundSignal::Checkpoint(_)
                    | ForegroundSignal::FinalCheckpoint(_)
                    | ForegroundSignal::AutomaticCheckRefreshed(_)
                    | ForegroundSignal::AutomaticCheckCompleted(_)
                    | ForegroundSignal::SettlementReady { .. }
                    | ForegroundSignal::SettlementCommitted(_)
                    | ForegroundSignal::PlanMutationCompleted(_)
                    | ForegroundSignal::PlanSubmissionCompleted(_)
                    | ForegroundSignal::PlanQuestionCompleted(_)
                    | ForegroundSignal::GoalProgressCompleted(_)
                    | ForegroundSignal::ExecutionProgressCompleted(_)
                    | ForegroundSignal::PlanTerminalPublished(_)
                    | ForegroundSignal::ClarificationPublished(_)
                    | ForegroundSignal::CheckRepairPublished(_)
                    | ForegroundSignal::CompactionCompleted(_)
                    | ForegroundSignal::AutoCompactionPublished(_)
                    | ForegroundSignal::FinalResponsePublished(_)
            )
        {
            return self.step_foreground_finalization(dispatch, signal);
        }
        let signal = match signal {
            ForegroundSignal::Native(NativeForegroundSignal::NativeReady(
                ProviderAttempt::Stale {
                    request,
                    command,
                    retry,
                },
            )) => {
                return self.refresh_stale_continuation(
                    dispatch,
                    *request,
                    command.map(|command| *command),
                    retry,
                );
            }
            ForegroundSignal::Native(NativeForegroundSignal::NativeReady(
                ProviderAttempt::Ready(ready),
            )) => {
                let ForegroundDispatch::Running { transaction, .. } = dispatch else {
                    anyhow::bail!("native completion has no running foreground owner");
                };
                if let Err(error) = self.collect_foreground_observations(transaction) {
                    return self.fail_foreground(transaction, error);
                }
                if transaction.question_stop_pending
                    || !transaction.steering.pending_deliveries.is_empty()
                    || transaction.observation_publication_in_flight
                    || !transaction.pending_session_observations.is_empty()
                {
                    transaction.pending_native_ready = Some(*ready);
                    return Ok(ForegroundAction::Wait);
                }
                if transaction.steering.refresh_readiness {
                    // Delivery acceptance may precede a steering turn's start and terminal events.
                    // Refresh the covered-submission barrier before checkpointing the exchange.
                    let mut checkout = transaction.settlement.checkout()?;
                    drop(checkout.take());
                    let engine = transaction.engine.clone();
                    let scope = transaction.state.scope.clone();
                    let settlement = transaction.settlement.clone();
                    transaction.native.restart_observation(async move {
                        let refreshed = engine.wait_ready(scope).await?;
                        settlement.set_batches(refreshed.batches)?;
                        checkout.replace(refreshed.ticket);
                        Ok(ProviderAttempt::Ready(Box::new(refreshed.outcome)))
                    })?;
                    transaction.native_open = true;
                    transaction.steering.refresh_readiness = false;
                    return Ok(ForegroundAction::Wait);
                }
                let ready = *ready;
                let outcome = match transaction.question_override.take() {
                    Some(completion) => ForegroundOutcome::RequiredQuestion {
                        ready,
                        completion: Box::new(completion),
                    },
                    None => ForegroundOutcome::NativeReady(ready),
                };
                return self.complete_foreground_native(dispatch, outcome);
            }
            ForegroundSignal::QuestionStopped(result) => {
                let ForegroundDispatch::Running { transaction, .. } = dispatch else {
                    anyhow::bail!("question cleanup has no running foreground owner");
                };
                transaction.question_stop_pending = false;
                let completion = match *result {
                    Ok(completion) => completion,
                    Err(error) => return self.fail_foreground(transaction, error),
                };
                transaction.question_override = Some(completion);
                return Ok(ForegroundAction::Wait);
            }
            ForegroundSignal::CleanupCompleted { original, cleanup } => {
                return self.complete_foreground_cleanup(dispatch, original, cleanup);
            }
            signal => signal,
        };
        let already_finalizing = matches!(dispatch, ForegroundDispatch::Finalizing { .. });
        let (transaction, admission) = match dispatch {
            ForegroundDispatch::Running {
                transaction,
                admission,
            }
            | ForegroundDispatch::Finalizing {
                transaction,
                admission,
            } => (transaction, admission),
            _ => anyhow::bail!("foreground signal arrived without a running owner"),
        };
        let action = match signal {
            ForegroundSignal::Native(NativeForegroundSignal::ProviderEvent(observation)) => {
                // Apply through the merged queue so queued session events with earlier native
                // sequences are not overtaken by this sink event.
                transaction
                    .pending_session_observations
                    .push_back(observation);
                self.apply_foreground_session_observations(transaction)
            }
            ForegroundSignal::Native(NativeForegroundSignal::Cancelled)
                if transaction.collection.is_some() =>
            {
                // The owner already holds collection authority; a second cleanup would retire its reader early.
                transaction.native_open = transaction.native.has_dispatched_task();
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::Native(NativeForegroundSignal::Cancelled) => {
                self.fail_foreground(transaction, TurnCancelled.into())
            }
            ForegroundSignal::Native(NativeForegroundSignal::ToolExpired(expired)) => {
                self.handle_foreground_timeout(transaction, expired)
            }
            ForegroundSignal::QuestionChanged => {
                self.handle_foreground_question_change(transaction)
            }
            ForegroundSignal::Checkpoint(result) => {
                self.apply_foreground_checkpoint(transaction, result)
            }
            ForegroundSignal::UserInput(input) => {
                Ok(self.prepare_foreground_input(transaction, input))
            }
            ForegroundSignal::UserDelivery((input, result)) => Ok(ForegroundAction::Effect(
                ForegroundJob::SettleUserDelivery { input, result },
            )),
            ForegroundSignal::UserDeliverySettled(input, _flight) => {
                if let Some(input) = input {
                    transaction.steering.deferred.push_back(input);
                } else {
                    // Refresh only after successful delivery can extend native coverage.
                    // A staged effect cancelled before delivery leaves no later readiness to collect.
                    transaction.steering.refresh_readiness = true;
                }
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::AgentChanged => self.poll_foreground_agents(transaction),
            ForegroundSignal::AgentDelivery(delivery) => {
                let (task, result) = *delivery;
                self.apply_foreground_agent_result(transaction, task, result)
            }
            ForegroundSignal::AgentResultDelivered {
                flight,
                task,
                result,
            } => {
                // Release the deferred native result only after acknowledgement is recorded.
                let applied = self.apply_foreground_agent_result(transaction, *task, result);
                drop(flight);
                applied
            }
            ForegroundSignal::AgentApprovalsCancelled { completion, result } => {
                self.apply_foreground_agent_completion(transaction, *completion, result)
            }
            ForegroundSignal::AgentInterrupted {
                operation,
                completed,
                result,
            } => self.apply_foreground_agent_interrupt(transaction, operation, completed, result),
            ForegroundSignal::AgentSpawnSelected { request, selection } => {
                self.apply_foreground_agent_selection(transaction, request, *selection)
            }
            ForegroundSignal::AgentMessageSteered {
                request,
                registry,
                owner,
                result,
            } => self.commit_foreground_agent_message(
                transaction,
                request,
                Some((owner, registry)),
                result,
            ),
            ForegroundSignal::ExecutionControl(control) => {
                Ok(self.prepare_foreground_control(transaction, control))
            }
            ForegroundSignal::ExecutionControlEvidence {
                response,
                submission,
                result,
            } => self.apply_foreground_control_evidence(transaction, response, submission, *result),
            ForegroundSignal::ExecutionControlReport {
                response,
                submission,
                result,
            } => self.apply_foreground_control_report(transaction, response, submission, *result),
            ForegroundSignal::RevisionReview(request) => {
                self.apply_foreground_revision(transaction, request)
            }
            #[cfg(test)]
            ForegroundSignal::RevisionChecksUpdated { response, result } => {
                Ok(self.apply_foreground_revision_checks(transaction, response, *result))
            }
            // A scan is ordinary work that collection's closed effect gate keeps staged. Finalization
            // would then wait on a result that only arrives once the collecting command times out.
            ForegroundSignal::PlanScan if transaction.collection.is_some() => {
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::PlanScan => Ok(ForegroundAction::Effect(
                self.prepare_foreground_scan(transaction)?,
            )),
            ForegroundSignal::PlanScanned(result) => {
                let applied = self.apply_foreground_scan(transaction, (*result)?)?;
                Ok(ForegroundAction::Effect(ForegroundJob::Publication(
                    self.prepare_provider_delivery(applied)?,
                )))
            }
            ForegroundSignal::CollectionStarted(disposition) => {
                admission.begin_owner_collection(disposition)?;
                transaction.collection = Some(disposition);
                if let Some(finalization) = transaction.finalization.as_mut() {
                    finalization.set_collection(disposition);
                }
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::CollectionCompleted(disposition) => {
                admission.complete_owner_collection(disposition)?;
                transaction.collection = Some(disposition);
                if let Some(finalization) = transaction.finalization.as_mut() {
                    finalization.set_collection(disposition);
                }
                transaction.native_open = transaction.native.has_dispatched_task();
                if !already_finalizing
                    && !transaction.native_open
                    && transaction.pending_native_ready.is_none()
                {
                    transaction.pending_native_ready = Some(Err(ProviderError::Cancelled));
                }
                self.collect_foreground_observations(transaction)?;
                // A collected cancellation may already be settling an output-free prompt,
                // which waits for this positive collection before deleting its exchange.
                if transaction.failure.is_some() {
                    return self.advance_foreground_failure(dispatch);
                }
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::SessionObservationsReady
                if transaction.observation_publication_in_flight =>
            {
                self.collect_foreground_observations(transaction)?;
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::SessionObservationsReady => {
                self.apply_foreground_session_observations(transaction)
            }
            ForegroundSignal::PublicationFailed(error) => {
                self.fail_foreground(transaction, error.into())
            }
            ForegroundSignal::EffectFailed(error)
                if transaction.collection.is_some()
                    && (error.is::<TurnCancelled>()
                        || error
                            .downcast_ref::<ProviderError>()
                            .is_some_and(ProviderError::is_clean_cancellation)) =>
            {
                transaction.native_open = transaction.native.has_dispatched_task();
                if !transaction.native_open && !already_finalizing {
                    transaction
                        .pending_native_ready
                        .get_or_insert(Err(ProviderError::Cancelled));
                }
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::EffectFailed(error) => self.fail_foreground(transaction, error),
            ForegroundSignal::ObservationPublicationDelivered(result) => {
                transaction.observation_publication_in_flight = false;
                admission.events.extend(result?);
                if !transaction.pending_session_observations.is_empty() {
                    self.apply_foreground_session_observations(transaction)
                } else {
                    Ok(ForegroundAction::Wait)
                }
            }
            ForegroundSignal::PublicationDelivered(events) => {
                admission.events.extend(events);
                if !transaction.observation_publication_in_flight
                    && !transaction.pending_session_observations.is_empty()
                {
                    self.apply_foreground_session_observations(transaction)
                } else if transaction.failure.is_some() {
                    Ok(ForegroundAction::Wait)
                } else if transaction.pending_agent_interrupt.is_some() {
                    self.continue_foreground_agent_interrupt(transaction)
                } else {
                    self.prepare_foreground_agent_launch(transaction)
                }
            }
            ForegroundSignal::CommandEventsNeedForward { .. } => {
                anyhow::bail!("command events must be forwarded before acknowledgment")
            }
            ForegroundSignal::CommandNeedsForward { .. } => {
                anyhow::bail!("command events must be forwarded before acknowledgment")
            }
            ForegroundSignal::PermissionNeedsForward { .. } => {
                anyhow::bail!("permission events must be forwarded before acknowledgment")
            }
            ForegroundSignal::AdmissionEventsNeedForward { .. } => {
                anyhow::bail!("admission events must be forwarded before acknowledgment")
            }
            ForegroundSignal::FailureAdmissionEventsNeedForward { .. } => {
                anyhow::bail!("admission events must be forwarded before acknowledgment")
            }
            ForegroundSignal::AutoCompactionNeedsForward { .. } => {
                anyhow::bail!("compaction events must be forwarded before acknowledgment")
            }
            ForegroundSignal::CheckRepairNeedsForward { .. } => {
                anyhow::bail!("check repair events must be forwarded before acknowledgment")
            }
            ForegroundSignal::ClarificationNeedsForward { .. } => {
                anyhow::bail!("clarification events must be forwarded before acknowledgment")
            }
            ForegroundSignal::FailurePublicationNeedsForward { .. } => {
                anyhow::bail!("failure events must be forwarded before acknowledgment")
            }
            ForegroundSignal::PlanTerminalNeedsForward { .. } => {
                anyhow::bail!("plan submission events must be forwarded before acknowledgment")
            }
            ForegroundSignal::ObservationPublicationNeedsForward { .. } => {
                anyhow::bail!("observation events must be forwarded before acknowledgment")
            }
            ForegroundSignal::FinalResponseNeedsForward { .. } => {
                anyhow::bail!("final events must be forwarded before acknowledgment")
            }
            ForegroundSignal::AdmissionRequested
            | ForegroundSignal::AdmissionPrepared
            | ForegroundSignal::QuestionCatalog(_)
            | ForegroundSignal::CommandEventsForwarded(_)
            | ForegroundSignal::CommandPublished(_)
            | ForegroundSignal::PermissionPublished(_)
            | ForegroundSignal::AdmissionEventsForwarded(_)
            | ForegroundSignal::PriorObservationReady(_)
            | ForegroundSignal::PriorInitialCompleted(_)
            | ForegroundSignal::PriorFinalCompleted(_)
            | ForegroundSignal::AdmissionWorkspaceCompleted(_)
            | ForegroundSignal::TaskCompacted(_)
            | ForegroundSignal::ProviderPrepared(_)
            | ForegroundSignal::QuestionStopped(_)
            | ForegroundSignal::Native(NativeForegroundSignal::NativeReady(_))
            | ForegroundSignal::CleanupCompleted { .. }
            | ForegroundSignal::FailureAdmissionEventsForwarded(_)
            | ForegroundSignal::FailurePublicationCompleted(_)
            | ForegroundSignal::FailureCheckpointCompleted(_)
            | ForegroundSignal::FinalCheckpoint(_)
            | ForegroundSignal::FailureBaselineCompleted(_)
            | ForegroundSignal::AdmissionCleanupCompleted(_)
            | ForegroundSignal::AdmissionFailureCheckpointCompleted(_)
            | ForegroundSignal::PlanMutationCompleted(_)
            | ForegroundSignal::LiveHost(_)
            | ForegroundSignal::PlanSubmissionCompleted(_)
            | ForegroundSignal::PlanQuestionCompleted(_)
            | ForegroundSignal::GoalProgressCompleted(_)
            | ForegroundSignal::ExecutionProgressCompleted(_)
            | ForegroundSignal::PlanTerminalPublished(_)
            | ForegroundSignal::ClarificationPublished(_)
            | ForegroundSignal::CheckRepairPublished(_)
            | ForegroundSignal::CompactionCompleted(_)
            | ForegroundSignal::AutoCompactionPublished(_)
            | ForegroundSignal::FinalResponsePublished(_)
            | ForegroundSignal::FinalizeChecksRequested
            | ForegroundSignal::FinalizeCheckContinuationRequested
            | ForegroundSignal::FinalizeEffectsRequested
            | ForegroundSignal::PublicationDrained
            | ForegroundSignal::AutomaticCheckRefreshed(_)
            | ForegroundSignal::AutomaticCheckCompleted(_)
            | ForegroundSignal::SettlementReady { .. }
            | ForegroundSignal::SettlementCommitted(_) => {
                anyhow::bail!("foreground signal arrived in the wrong phase")
            }
        }?;
        let failure_drained = transaction.failure.is_some()
            && transaction.pending_session_observations.is_empty()
            && matches!(&action, ForegroundAction::Wait);
        if failure_drained {
            self.advance_foreground_failure(dispatch)
        } else {
            Ok(action)
        }
    }

    /// Stop native work for a newly required question without holding broker state.
    fn handle_foreground_question_change(
        &self,
        transaction: &mut ForegroundTransaction,
    ) -> Result<ForegroundAction> {
        if !self
            .questions
            .snapshot()?
            .required_in_exchange(&transaction.state.interaction.id)
        {
            return Ok(ForegroundAction::Wait);
        }
        transaction.question_enabled = false;
        transaction.question_stop_pending = true;
        Ok(ForegroundAction::Effect(ForegroundJob::QuestionStop {
            provider: self.provider.clone(),
            engine: transaction.engine.clone(),
            scope: transaction
                .engine
                .active_invocation_scope()?
                .unwrap_or_else(|| transaction.state.scope.clone()),
            fallback: self
                .session
                .provider_session_id
                .clone()
                .map(TryInto::try_into)
                .transpose()?,
            checkpoint: self.session.provider_checkpoint_id.clone(),
            capability: self.capability.clone(),
        }))
    }

    /// Apply retained session events in order, publishing each before the next is applied.
    pub(super) fn apply_foreground_session_observations(
        &mut self,
        transaction: &mut ForegroundTransaction,
    ) -> Result<ForegroundAction> {
        self.collect_foreground_observations(transaction)?;
        while let Some(observation) = transaction.pending_session_observations.pop_front() {
            if observation.scope().invocation().is_some_and(|scope| {
                scope.exchange_id
                    == forge_provider_api::ExchangeId::from(
                        transaction.state.interaction.id.as_str(),
                    )
            }) {
                let delivery = self.apply_foreground_observation(transaction, observation)?;
                transaction.observation_publication_in_flight = true;
                return Ok(ForegroundAction::Effect(
                    ForegroundJob::ObservationPublication(delivery),
                ));
            }
            let mut events = Vec::new();
            let applied = match &observation {
                crate::provider::session_execution::OwnerObservation::HostFileChange {
                    binding_id,
                    scope,
                    change,
                } => Some(self.apply_observed_file_change(binding_id, scope, change)?),
                crate::provider::session_execution::OwnerObservation::Event(
                    event @ crate::provider::HarnessEvent::Native {
                        event: native_event,
                        ..
                    },
                ) if matches!(
                    native_event.as_ref(),
                    forge_provider_api::ProviderEvent::Item { .. }
                ) =>
                {
                    self.apply_settled_parent_event(&transaction.state.interaction, event)?
                }
                crate::provider::session_execution::OwnerObservation::Event(_)
                | crate::provider::session_execution::OwnerObservation::InputAcceptance {
                    ..
                } => {
                    events.extend(self.observe_session_event(observation)?);
                    None
                }
            };
            if applied.is_some() || !events.is_empty() {
                let mut delivery = self.prepare_provider_delivery(
                    applied.unwrap_or_else(|| AppliedProviderEvent::Effects(Vec::new())),
                )?;
                events.append(&mut delivery.events);
                delivery.events = events;
                transaction.observation_publication_in_flight = true;
                return Ok(ForegroundAction::Effect(
                    ForegroundJob::ObservationPublication(delivery),
                ));
            }
        }
        Ok(ForegroundAction::Wait)
    }

    /// Retain invocation events even if the subsequent session inbox reports failure.
    pub(super) fn collect_foreground_observations(
        &self,
        transaction: &mut ForegroundTransaction,
    ) -> Result<()> {
        let mut failure = None;
        while transaction.native.stream_open
            && transaction.pending_session_observations.len() < forge_protocol::MAX_PENDING_FRAMES
        {
            match transaction.native.events.try_next() {
                Ok(Some(event)) => transaction.pending_session_observations.push_back(event),
                Ok(None) => break,
                Err(error) => {
                    transaction.native.stream_open = false;
                    failure = Some(anyhow::Error::new(error));
                    break;
                }
            }
        }
        let remaining = forge_protocol::MAX_PENDING_FRAMES
            .saturating_sub(transaction.pending_session_observations.len());
        if remaining != 0 && (!transaction.failure_cleanup_active || transaction.failure_inbox_open)
        {
            match transaction.session_events.drain_up_to(remaining) {
                Ok(events) => {
                    if events.is_empty() && transaction.session_events.is_closed() {
                        transaction.failure_inbox_open = false;
                    }
                    transaction.pending_session_observations.extend(events);
                }
                Err(error) => {
                    transaction.failure_inbox_open = false;
                    failure = Some(match failure {
                        Some(native) => {
                            native.context(format!("session observation drain failed: {error}"))
                        }
                        None => anyhow::Error::new(error),
                    });
                }
            }
        }
        failure.map_or(Ok(()), Err)
    }
}

#[cfg(test)]
mod receipt_scope_tests {
    use super::*;

    /// Preserve the broker's unrelated input receipt while applying or rejecting admission signals.
    #[tokio::test]
    async fn foreground_receipt_scope_restores_unrelated_input_on_error() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut broker = HarnessBroker::initialize(serde_json::from_value(json!({
            "workspace":root.path(),"data_root":root.path().join("data"),
            "client_id":"foreground-receipt-scope","provider":{"kind":"mock","command":[]},
        }))?)?;
        let catalog = broker.provider_catalog_request()?;
        let provider = broker.provider.descriptor().kind;
        let selected = broker.submission_store.prepare_input(
            &catalog,
            ProviderInput::from_text("Selected input"),
            provider.clone(),
            Some("selected".into()),
        )?;
        let unrelated = broker.submission_store.prepare_input(
            &catalog,
            ProviderInput::from_text("Unrelated input"),
            provider,
            Some("unrelated".into()),
        )?;
        broker.input_submission = Some(unrelated.clone());
        let message = serde_json::from_value(json!({
            "id":"selected","revision":2,"text":"Selected input","delivery":"delivering","route":"turn",
        }))?;
        let (publication, _updates) = publication::channel();
        let mut dispatch = broker.begin_queued_input_dispatch(
            message,
            selected.clone(),
            Vec::new(),
            publication,
        )?;
        let signal = dispatch.next().await?;
        assert!(matches!(
            broker.step_foreground(&mut dispatch, signal)?,
            ForegroundAction::Wait
        ));
        assert_eq!(
            broker.input_submission.as_ref().unwrap().id(),
            unrelated.id()
        );
        assert!(
            broker
                .step_foreground(&mut dispatch, ForegroundSignal::AdmissionRequested)
                .is_err()
        );
        assert_eq!(
            broker.input_submission.as_ref().unwrap().id(),
            unrelated.id()
        );
        let ForegroundDispatch::Admitting(admission) = dispatch else {
            anyhow::bail!("unexpected foreground phase after rejected admission signal");
        };
        assert_eq!(
            admission.input_submission.as_ref().unwrap().id(),
            selected.id()
        );
        assert!(matches!(
            selected.record()?.delivery,
            crate::provider::submission::SubmissionState::Prepared
        ));
        assert!(broker.store.latest_exchange(&broker.session.id)?.is_none());
        Ok(())
    }
}
