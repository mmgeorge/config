//! Apply positively correlated late execution to its original durable exchange.

use std::collections::{HashMap, HashSet};

use anyhow::{Context, Result, ensure};
use forge_provider_api::lifecycle::{ExecutionObservation, ExecutionResult, ExecutionUpdate};

use super::{HarnessBroker, SessionEvent, SessionUpdate, publication};
use crate::exchange::{ExchangeOutcome, ExchangeState};
use crate::provider::{
    ExecutionOwner, InvocationId, InvocationScope, ProviderDiagnostic, ProviderEvent, TurnAddress,
};

fn publication_diagnostic(scope: &InvocationScope) -> ProviderEvent {
    ProviderEvent::Diagnostic {
        scope: scope.clone().into(),
        diagnostic: ProviderDiagnostic {
            kind: forge_provider_api::ProviderFailureKind::Persistence,
            message: forge_provider_api::SanitizedMessage::new(
                "Required host effects could not be published",
            )
            .expect("static diagnostic is nonempty"),
            fatal: true,
        },
    }
}

/// Preserve the original host workflow and its lease owners after foreground commit.
pub(super) struct RetainedForeground {
    pub(super) transaction: Box<super::ForegroundTransaction>,
    pub(super) admission: Box<super::foreground_dispatch::ForegroundAdmission>,
}

/// Fence host settlement when an original invocation acquires another live native execution.
#[derive(Default)]
pub(super) struct ReopenedExecutions {
    revisions: HashMap<InvocationId, ExecutionRevision>,
    settling: HashSet<InvocationId>,
    settled: std::sync::Arc<tokio::sync::Notify>,
}

struct ExecutionRevision {
    revision: u64,
    active: Option<TurnAddress>,
}

impl ReopenedExecutions {
    /// Keep completion under checkpoint ownership while a continuation is observed.
    pub(super) fn begin_settlement(&mut self, scope: &InvocationScope) {
        self.settling.insert(scope.invocation_id.clone());
    }

    /// Release checkpoint ownership after commit or failure.
    pub(super) fn end_settlement(&mut self, scope: &InvocationScope) {
        self.settling.remove(&scope.invocation_id);
        self.settled.notify_one();
    }

    pub(super) fn settlement_notification(&self) -> std::sync::Arc<tokio::sync::Notify> {
        self.settled.clone()
    }

    /// Report whether an owned host prerequisite currently holds the invocation's settlement fence.
    pub(super) fn is_settling(&self, scope: &InvocationScope) -> bool {
        self.settling.contains(&scope.invocation_id)
    }

    /// Get the current host settlement revision without creating an execution.
    pub(super) fn revision(&self, scope: &InvocationScope) -> u64 {
        self.revisions
            .get(&scope.invocation_id)
            .map_or(0, |state| state.revision)
    }

    fn next(&self, scope: &InvocationScope) -> Result<u64> {
        self.revision(scope)
            .checked_add(1)
            .context("host settlement revision exhausted")
    }

    fn begin(&mut self, address: TurnAddress, revision: u64) {
        self.revisions.insert(
            address.scope.invocation_id.clone(),
            ExecutionRevision {
                revision,
                active: Some(address),
            },
        );
    }

    fn matches(&self, address: &TurnAddress) -> bool {
        self.revisions
            .get(&address.scope.invocation_id)
            .and_then(|state| state.active.as_ref())
            == Some(address)
    }

    pub(super) fn finish(&mut self, scope: &InvocationScope) {
        if let Some(state) = self.revisions.get_mut(&scope.invocation_id) {
            state.active = None;
        }
    }

    /// Apply confirmed owner-wide collection even when no further terminal event is emitted.
    pub(super) fn finish_owner(&mut self, owner: &ExecutionOwner) {
        for state in self.revisions.values_mut() {
            if state
                .active
                .as_ref()
                .is_some_and(|address| &address.scope.owner == owner)
            {
                state.active = None;
            }
        }
    }

    pub(super) fn active(&self, scope: &InvocationScope) -> bool {
        self.revisions
            .get(&scope.invocation_id)
            .is_some_and(|state| state.active.is_some())
    }
}

impl HarnessBroker {
    #[cfg(test)]
    pub(crate) fn retained_idle_test_owner(
        &self,
    ) -> Result<(
        InvocationScope,
        std::sync::Arc<crate::provider::session_execution::SessionExecution>,
    )> {
        let retained = self
            .retained_foregrounds
            .values()
            .next()
            .context("retained foreground is missing")?;
        Ok((
            retained.transaction.state.scope.clone(),
            retained.transaction.engine.clone(),
        ))
    }

    #[cfg(test)]
    pub(crate) fn retained_idle_test_question(
        &self,
        scope: &InvocationScope,
        question_id: &str,
    ) -> Result<bool> {
        let Some(retained) = self.retained_foregrounds.get(&scope.invocation_id) else {
            return Ok(false);
        };
        anyhow::ensure!(
            retained.transaction.state.scope == *scope,
            "retained question has another owner"
        );
        let Some(exchange) = self.store.load_exchange(scope.exchange_id.as_str())? else {
            return Ok(false);
        };
        let count = exchange.node_list.iter().filter(|node| matches!(node,
            crate::exchange::ExchangeNode::QuestionPresented { question, .. } if question.id == question_id)).count();
        anyhow::ensure!(
            count <= 1,
            "idle question effect was persisted more than once"
        );
        Ok(count == 1
            && !self.parent_input.is_active()
            && exchange.awaiting_input()
            && exchange
                .elicitation()
                .is_some_and(|elicitation| elicitation.question_set.id == question_id))
    }

    #[cfg(test)]
    pub(crate) fn retained_idle_test_next_admission(&self) -> Result<()> {
        let active = self.parent_input.activate(None)?;
        drop(active);
        Ok(())
    }
    /// Restore the retained host owner and request a native-ready idle revision.
    pub(crate) fn begin_idle_foreground(
        &mut self,
        sink: publication::SessionUpdateSink,
    ) -> Result<Option<(super::ForegroundDispatch, super::ForegroundJob)>> {
        let exchange = self
            .store
            .exchange_in_state(&self.session.id, ExchangeState::Finalizing)?
            .or(self
                .store
                .exchange_in_state(&self.session.id, ExchangeState::Running)?);
        let Some(exchange) = exchange else {
            return Ok(None);
        };
        let Some(scope) = exchange
            .turn
            .last()
            .map(|turn| turn.provider().scope.clone())
        else {
            return Ok(None);
        };
        if self.reopened_execution.revision(&scope) == 0
            || self.reopened_execution.is_settling(&scope)
        {
            return Ok(None);
        }
        anyhow::ensure!(
            self.pending_host_settlement.is_none(),
            "another host settlement is active"
        );
        // The committed transaction retired its old receiver. Reacquire the session's
        // original lane for this invocation; a competing owner must remain exclusive.
        let retained = self
            .retained_foregrounds
            .get_mut(&scope.invocation_id)
            .context("idle execution has no retained host authority")?;
        let steering = self
            .parent_input
            .activate(Some(retained.transaction.provider_event_sink.clone()))?;
        anyhow::ensure!(
            retained.transaction.state.scope == scope,
            "idle settlement scope changed"
        );
        if let Some(permit) = &retained.transaction.dispatched_task_permit {
            permit.validate_observation(self.task_controller.current()?.as_ref())?;
        }
        retained
            .transaction
            ._check_lease
            .bind_lifecycle(Some(sink.clone()))?;
        let checkout = retained
            .transaction
            .finalization
            .as_mut()
            .context("retained idle finalization is missing")?
            .begin_idle_refresh();
        let checkout = match checkout {
            Ok(checkout) => checkout,
            Err(error) => {
                retained.transaction._check_lease.bind_lifecycle(None)?;
                return Err(error);
            }
        };
        let retained = self
            .retained_foregrounds
            .remove(&scope.invocation_id)
            .context("idle host authority disappeared")?;
        let mut transaction = retained.transaction;
        let mut admission = retained.admission;
        transaction.state.interaction = exchange;
        transaction.steering.active = steering;
        transaction.state.runtime = Some(super::ExchangeRuntime {
            task_permit: transaction.dispatched_task_permit.clone(),
            checkpoint: None,
            tool_gate: None,
            exchange_id: scope.exchange_id.as_str().to_owned(),
            checklist: transaction
                .state
                .interaction
                .checklist
                .clone()
                .map(super::ChecklistTracker::from_snapshot)
                .unwrap_or_default(),
            retraction_eligible: false,
            active_wait: None,
        });
        transaction.native_open = false;
        transaction.idle_native_active = self.reopened_execution.active(&scope);
        transaction.delivery_sink = Some(sink.clone());
        admission.sink = Some(sink);
        admission.idle_reopen = true;
        self.pending_host_settlement = Some(transaction.settlement.clone());
        self.reopened_execution.begin_settlement(&scope);
        let job = super::ForegroundJob::SettlementRefresh {
            engine: transaction.engine.clone(),
            scope,
            checkout,
        };
        Ok(Some((
            super::ForegroundDispatch::Finalizing {
                transaction,
                admission,
            },
            job,
        )))
    }

    /// Complete checkpoint settlement after idle continuation ends on its original exchange.
    pub(crate) async fn finalize_observed_execution(&mut self) -> Result<Vec<SessionEvent>> {
        let (sink, updates) = publication::channel();
        let Some((dispatch, job)) = self.begin_idle_foreground(sink)? else {
            return Ok(Vec::new());
        };
        let result = self
            .drive_local_foreground_initial(dispatch, updates, Some(job))
            .await?;
        Ok(result.event)
    }

    /// Drain continuation evidence before publishing a checkpoint for an older terminal boundary.
    pub(super) async fn refresh_settlement(
        &mut self,
        interaction: &mut crate::exchange::Exchange,
        scope: &InvocationScope,
    ) -> Result<bool> {
        let revision = self.reopened_execution.revision(scope);
        let was_active = self.reopened_execution.active(scope);
        loop {
            let observations = self.observer.session_events().drain()?;
            for observation in observations {
                let events = self.observe_session_event(observation)?;
                self.settlement_events.extend(events);
            }
            if !self.reopened_execution.active(scope) {
                break;
            }
            let inbox = self.observer.session_events();
            let cancellation = self.turn_cancellation.capture();
            tokio::select! {
                result = inbox.notified() => result?,
                _ = cancellation.cancelled() => return Err(forge_provider_api::error::TurnCancelled.into()),
            }
        }
        let changed = was_active || revision != self.reopened_execution.revision(scope);
        if changed {
            *interaction = self
                .store
                .load_exchange(&interaction.id)?
                .context("continued exchange disappeared during settlement")?;
        }
        Ok(changed)
    }

    /// Reopen and settle only lifecycle observations admitted by the owner's session engine.
    pub(super) fn observe_execution_lifecycle(
        &mut self,
        observation: ExecutionObservation,
    ) -> Result<Vec<SessionEvent>> {
        let (scope, update) = observation.into_parts();
        ensure!(
            scope.owner == ExecutionOwner::root(&self.session.id),
            "late execution crosses root ownership"
        );
        let mut exchange = self
            .store
            .load_exchange(scope.exchange_id.as_str())?
            .context("late execution has no original exchange")?;
        ensure!(
            exchange.session_id == self.session.id
                && exchange.agent_id == scope.owner.agent_id.as_str(),
            "late execution resolves to another owner"
        );
        let mut diagnostic = None;
        let publication_failure = match &update {
            ExecutionUpdate::PublicationFailed { failure, .. }
            | ExecutionUpdate::ResolutionPublicationFailed { failure, .. } => Some(*failure),
            _ => None,
        };
        match update {
            ExecutionUpdate::ResolvedWithoutExecution { .. } => return Ok(Vec::new()),
            ExecutionUpdate::ResolutionPublicationFailed { .. } => {
                diagnostic = Some(publication_diagnostic(&scope));
            }
            ExecutionUpdate::Started { address } => {
                let current = self.store.current_exchange_state(&self.session.id)?;
                ensure!(
                    !current
                        .latest
                        .as_ref()
                        .is_some_and(|latest| latest.id != exchange.id
                            && matches!(
                                latest.state,
                                ExchangeState::Running | ExchangeState::Finalizing
                            )),
                    "late execution conflicts with a newer active exchange"
                );
                ensure!(
                    self.exchange_runtime
                        .as_ref()
                        .is_none_or(|runtime| runtime.exchange_id == exchange.id),
                    "late execution conflicts with current host work"
                );
                let revision = self.reopened_execution.next(&scope)?;
                if !exchange.reopen_provider_execution(address.clone(), self.clock.now_ms())? {
                    return Ok(Vec::new());
                }
                self.store.save_exchange(&exchange)?;
                self.reopened_execution.begin(address, revision);
            }
            ExecutionUpdate::Finished {
                address,
                completed_at_ms,
                result,
                ..
            }
            | ExecutionUpdate::PublicationFailed {
                address,
                completed_at_ms,
                result,
                ..
            } => {
                if !self.reopened_execution.matches(&address) {
                    return Ok(Vec::new());
                }
                let (outcome, turn_outcome) = match result {
                    ExecutionResult::Completed { .. } => (
                        ExchangeOutcome::Complete,
                        crate::turn::TurnOutcome::Completed,
                    ),
                    ExecutionResult::Cancelled { .. } => (
                        ExchangeOutcome::Cancelled,
                        crate::turn::TurnOutcome::Cancelled,
                    ),
                    ExecutionResult::Failed { .. } => {
                        (ExchangeOutcome::Failed, crate::turn::TurnOutcome::Failed)
                    }
                };
                for turn in &mut exchange.turn {
                    if turn.provider().scope == scope
                        && turn.state() == crate::turn::TurnState::Running
                    {
                        turn.finish(turn_outcome, completed_at_ms)?;
                    }
                }
                exchange.begin_finalization(
                    if publication_failure.is_some() {
                        ExchangeOutcome::Failed
                    } else {
                        outcome
                    },
                    completed_at_ms,
                )?;
                self.store.save_exchange(&exchange)?;
                self.reopened_execution.finish(&scope);
                if publication_failure.is_some() {
                    diagnostic = Some(publication_diagnostic(&scope));
                }
            }
            ExecutionUpdate::ObservationLost { failure, .. } => {
                if !self.reopened_execution.active(&scope) {
                    return Ok(Vec::new());
                }
                exchange.finish(ExchangeOutcome::Failed, self.clock.now_ms())?;
                self.store.save_exchange(&exchange)?;
                self.reopened_execution.finish(&scope);
                diagnostic = Some(ProviderEvent::Diagnostic {
                    scope: scope.into(),
                    diagnostic: ProviderDiagnostic {
                        kind: forge_provider_api::ProviderFailureKind::Protocol,
                        message: forge_provider_api::SanitizedMessage::new(failure.to_string())
                            .expect("normalized observation failures are nonempty"),
                        fatal: true,
                    },
                });
            }
        }
        let patch = self.reconcile_timeline()?;
        let mut events = Vec::new();
        if !patch.is_empty() {
            events.push(self.event("document_changed", patch.notification())?);
        }
        if let Some(diagnostic) =
            diagnostic.and_then(|event| SessionUpdate::from_observation(&event))
        {
            events.push(self.event(
                "provider_event",
                publication::encode_session_update(&diagnostic)?,
            )?);
        }
        Ok(events)
    }
}
