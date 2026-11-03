//! Apply one owned foreground signal under the broker's short state lock.

use super::*;

impl HarnessBroker {
    /// Route one admitted signal to its owner-checked state transition.
    pub(crate) fn step_foreground(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        signal: ForegroundSignal,
    ) -> Result<ForegroundAction> {
        let signal = match signal {
            ForegroundSignal::AdmissionCleanupCompleted(result) => {
                return self.complete_staged_admission_cleanup(dispatch, result);
            }
            ForegroundSignal::AdmissionFailureCheckpointCompleted(result) => {
                return self.complete_staged_admission_checkpoint(dispatch, result);
            }
            ForegroundSignal::RetractionGoalRestored(result) => {
                return self.complete_foreground_retraction_goal(dispatch, result);
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
                    || transaction.native_goal.is_some()
                    || transaction.observation_publication_in_flight
                    || !transaction.pending_session_observations.is_empty()
                {
                    transaction.pending_native_ready = Some(*ready);
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
                if transaction.native_goal.is_some() {
                    transaction
                        .pending_session_observations
                        .push_back(observation);
                    return Ok(ForegroundAction::Wait);
                }
                match self.apply_foreground_observation(transaction, observation)? {
                    ForegroundEventApplication::Publication(job) => {
                        Ok(ForegroundAction::Effect(*job))
                    }
                    ForegroundEventApplication::NativeGoalContinuation(observation) => {
                        self.begin_native_goal_transition(transaction, *observation)
                    }
                }
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
            ForegroundSignal::UserDeliverySettled(input) => {
                if let Some(input) = input {
                    transaction.steering.deferred.push_back(input);
                }
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::AgentChanged => self.poll_foreground_agents(transaction),
            ForegroundSignal::AgentDelivery(delivery) => {
                let (task, result) = *delivery;
                self.apply_foreground_agent_result(transaction, task, result)
            }
            ForegroundSignal::AgentResultDelivered { task, result } => {
                self.apply_foreground_agent_result(transaction, *task, result)
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
            ForegroundSignal::RevisionChecksUpdated { response, result } => {
                Ok(self.apply_foreground_revision_checks(transaction, response, *result))
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
            ForegroundSignal::FinalCheckpoint(result) => match result {
                Ok(result) => self.complete_native_goal_checkpoint(transaction, result),
                Err(error) => self.fail_native_goal_checkpoint(transaction, error),
            },
            ForegroundSignal::Cleanup(ExecutionCleanup::Idle | ExecutionCleanup::Pending) => {
                Ok(ForegroundAction::Wait)
            }
            ForegroundSignal::Cleanup(ExecutionCleanup::Failed(message)) => self.fail_foreground(
                transaction,
                anyhow::anyhow!("execution cleanup failed: {message}"),
            ),
            ForegroundSignal::SessionObservationsReady if transaction.native_goal.is_some() => {
                self.collect_foreground_observations(transaction)?;
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
                if transaction.native_goal.is_none()
                    && !transaction.observation_publication_in_flight
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
            | ForegroundSignal::FailureBaselineCompleted(_)
            | ForegroundSignal::AdmissionCleanupCompleted(_)
            | ForegroundSignal::AdmissionFailureCheckpointCompleted(_)
            | ForegroundSignal::RetractionGoalRestored(_)
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
            session_id: self.session.id.clone(),
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
        if transaction.pending_session_observations.is_empty() {
            self.collect_foreground_observations(transaction)?;
        }
        while let Some(observation) = transaction.pending_session_observations.pop_front() {
            if observation
                .scope()
                .invocation()
                .is_some_and(|scope| scope.exchange_id == transaction.state.interaction.id)
            {
                return match self.apply_foreground_observation(transaction, observation)? {
                    ForegroundEventApplication::Publication(job) => match *job {
                        ForegroundJob::Publication(delivery) => {
                            transaction.observation_publication_in_flight = true;
                            Ok(ForegroundAction::Effect(
                                ForegroundJob::ObservationPublication(delivery),
                            ))
                        }
                        _ => anyhow::bail!("unexpected observation publication job"),
                    },
                    ForegroundEventApplication::NativeGoalContinuation(observation) => {
                        self.begin_native_goal_transition(transaction, *observation)
                    }
                };
            }
            let mut events = Vec::new();
            let applied = if matches!(observation, ProviderEvent::Tool { .. }) {
                anyhow::ensure!(
                    observation.scope().owner()
                        == &crate::provider::ExecutionOwner::root(&self.session.id),
                    "session tool event crosses execution ownership"
                );
                self.apply_settled_parent_event(&transaction.state.interaction, &observation)?
            } else {
                events.extend(self.observe_session_event(observation)?);
                None
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
