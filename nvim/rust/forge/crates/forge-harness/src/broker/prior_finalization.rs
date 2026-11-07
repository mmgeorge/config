//! Retain and finalize an older exchange without borrowing the broker across I/O.
use super::*;

pub(crate) struct PriorFinalization {
    exchange: Exchange,
    scope: Option<crate::provider::InvocationScope>,
    owns_settlement: bool,
}

pub(crate) enum PriorFinalizationAction {
    Ready,
    Wait(PriorObservationJob),
    Initial(PriorInitialJob),
    Final(Box<PriorFinalJob>),
}

pub(crate) struct PriorObservationJob {
    inbox: Arc<crate::provider::session_execution::SessionObservationInbox>,
    cancellation: Arc<TurnCancellationSignal>,
    settlement: Arc<tokio::sync::Notify>,
}

impl PriorObservationJob {
    pub(crate) async fn run(self) -> Result<()> {
        tokio::select! {
            biased;
            () = self.cancellation.cancelled() => Err(TurnCancelled.into()),
            () = self.settlement.notified() => Ok(()),
            result = self.inbox.notified() => result.map_err(anyhow::Error::new),
        }
    }
}

pub(crate) struct PriorInitialJob {
    task: crate::checkpoint::CheckpointTask,
}

pub(crate) struct PriorFinalJob {
    capture: checkpoint_settlement::FinalCheckpointJob,
    cancellation: Arc<TurnCancellationSignal>,
}

impl PriorFinalJob {
    pub(crate) async fn run(self) -> Result<checkpoint_settlement::FinalCheckpointResult> {
        tokio::select! {
            biased;
            () = self.cancellation.cancelled() => Err(TurnCancelled.into()),
            result = self.capture.run() => result,
        }
    }
}

impl PriorInitialJob {
    pub(crate) async fn run(mut self) -> Result<crate::checkpoint::CheckpointRecord> {
        (&mut self.task.0)
            .await
            .context("prior checkpoint worker failed")?
            .map_err(anyhow::Error::new)
    }
}

impl HarnessBroker {
    /// Capture the older exchange and retain its settlement fence.
    pub(crate) fn begin_prior_finalization(&mut self) -> Result<Option<PriorFinalization>> {
        let Some(exchange) = self
            .store
            .exchange_in_state(&self.session.id, ExchangeState::Finalizing)?
        else {
            return Ok(None);
        };
        let scope = exchange
            .turn
            .last()
            .map(|turn| turn.provider().scope.clone());
        let owns_settlement = scope
            .as_ref()
            .is_none_or(|scope| !self.host_settlement.is_settling(scope));
        if owns_settlement && let Some(scope) = &scope {
            self.host_settlement.begin_settlement(scope);
        }
        Ok(Some(PriorFinalization {
            exchange,
            scope,
            owns_settlement,
        }))
    }

    /// Recheck observations and capture exactly one owned prerequisite.
    pub(crate) fn advance_prior_finalization(
        &mut self,
        state: &mut PriorFinalization,
        events: &mut Vec<SessionEvent>,
    ) -> Result<PriorFinalizationAction> {
        anyhow::ensure!(
            !self.turn_cancellation.is_requested(),
            "prior finalization cancelled"
        );
        for observation in self.observer.session_events().drain()? {
            events.extend(self.observe_session_event(observation)?);
        }
        state.exchange = self
            .store
            .load_exchange(&state.exchange.id)?
            .context("prior exchange disappeared")?;
        if state.exchange.is_finished() {
            if !state.owns_settlement {
                return Ok(PriorFinalizationAction::Ready);
            }
            return self.finish_prior_finalization(state, events);
        }
        if let Some(scope) = &state.scope
            && !state.owns_settlement
        {
            if !self.host_settlement.is_settling(scope) {
                self.host_settlement.begin_settlement(scope);
                state.owns_settlement = true;
            } else {
                return Ok(PriorFinalizationAction::Wait(PriorObservationJob {
                    inbox: self.observer.session_events(),
                    cancellation: self.turn_cancellation.capture(),
                    settlement: self.host_settlement.settlement_notification(),
                }));
            }
        }
        let outcome = state
            .exchange
            .finalization_outcome()
            .context("prior finalization outcome is missing")?;
        let descendants: Vec<_> = referenced_child_exchange_list(&state.exchange)
            .into_iter()
            .map(|(_, id)| id)
            .collect();
        if let Err(error) = self
            .store
            .require_settled_exchange_tree(&self.session.id, &descendants)
        {
            state.exchange.finalization_error = Some(format!("{error:#}"));
            self.store.save_exchange(&state.exchange)?;
            return Err(error);
        }
        if let Some(runtime) = self
            .exchange_runtime
            .as_mut()
            .filter(|runtime| runtime.exchange_id == state.exchange.id)
        {
            if outcome == crate::exchange::ExchangeOutcome::Cancelled && runtime.tools_blocked() {
                runtime
                    .tool_gate
                    .as_ref()
                    .expect("blocked tool gate")
                    .complete(Err(crate::checkpoint::CheckpointGateError::OwnerEnded));
                runtime.checkpoint.take();
                state.exchange.finalization_error = None;
                state.exchange.finish(outcome, self.clock.now_ms())?;
                return self.finish_prior_finalization(state, events);
            }
            if let Some(task) = runtime.checkpoint.take() {
                return Ok(PriorFinalizationAction::Initial(PriorInitialJob { task }));
            }
        }
        if self.checkpoint.is_none() {
            let mut changes = crate::exchange::ProviderDiffBuilder::default();
            self.visit_provider_exchanges(&state.exchange, |exchange| changes.record(exchange))?;
            state.exchange.attributed_diff_text = changes.finish();
            state.exchange.attributed_matches_checkpoint = false;
            state.exchange.finish(outcome, self.clock.now_ms())?;
            return self.finish_prior_finalization(state, events);
        }
        Ok(PriorFinalizationAction::Final(Box::new(PriorFinalJob {
            capture: self.prepare_final_checkpoint(&state.exchange, outcome)?,
            cancellation: self.turn_cancellation.capture(),
        })))
    }

    /// Persist a completed baseline only to the runtime that admitted it.
    pub(crate) fn apply_prior_initial(
        &mut self,
        state: &mut PriorFinalization,
        result: Result<crate::checkpoint::CheckpointRecord>,
    ) -> Result<()> {
        let mut runtime = self
            .exchange_runtime
            .take()
            .context("prior runtime disappeared")?;
        let applied = if runtime.exchange_id == state.exchange.id {
            self.finish_initial_checkpoint(&mut runtime, &mut state.exchange, result)
        } else {
            Err(anyhow::anyhow!(
                "prior checkpoint crossed exchange ownership"
            ))
        };
        self.exchange_runtime = Some(runtime);
        applied
    }

    /// Commit only an unchanged final capture; stale evidence is recaptured by the caller.
    pub(crate) fn apply_prior_final(
        &mut self,
        state: &mut PriorFinalization,
        result: Result<checkpoint_settlement::FinalCheckpointResult>,
        events: &mut Vec<SessionEvent>,
    ) -> Result<PriorFinalizationAction> {
        match self.commit_final_checkpoint(&mut state.exchange, result?)? {
            checkpoint_settlement::FinalCheckpointCommit::Committed(_) => {
                self.finish_prior_finalization(state, events)
            }
            checkpoint_settlement::FinalCheckpointCommit::Stale => {
                self.advance_prior_finalization(state, events)
            }
        }
    }

    fn finish_prior_finalization(
        &mut self,
        state: &mut PriorFinalization,
        events: &mut Vec<SessionEvent>,
    ) -> Result<PriorFinalizationAction> {
        self.store.save_exchange(&state.exchange)?;
        if self
            .exchange_runtime
            .as_ref()
            .is_some_and(|runtime| runtime.exchange_id == state.exchange.id)
        {
            self.exchange_runtime = None;
        }
        if state.owns_settlement
            && let Some(scope) = &state.scope
        {
            self.host_settlement.end_settlement(scope);
        }
        self.save_session()?;
        events.push(self.event(
            "exchange_complete",
            json!({"id":state.exchange.id,"state":state.exchange.state()}),
        )?);
        Ok(PriorFinalizationAction::Ready)
    }

    /// Persist an uncompleted prior exchange and release its settlement fence on failure.
    pub(crate) fn abort_prior_finalization(
        &mut self,
        state: &mut PriorFinalization,
        error: &anyhow::Error,
    ) -> Result<()> {
        state.exchange.finalization_error = Some(format!("{error:#}"));
        self.store.save_exchange(&state.exchange)?;
        if state.owns_settlement
            && let Some(scope) = &state.scope
        {
            self.host_settlement.end_settlement(scope);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A retained prior exchange is finalized before the next provider is admitted.
    #[tokio::test]
    async fn prior_exchange_without_git_is_settled_before_admission() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        broker.checkpoint = None;
        let mut exchange =
            super::super::test::completed_interaction("prior", &broker.session.id, Vec::new());
        exchange.phase =
            crate::exchange::ExchangePhase::Finalizing(crate::exchange::ExchangeOutcome::Cancelled);
        broker.store.save_exchange(&exchange)?;
        let mut state = broker
            .begin_prior_finalization()?
            .context("missing prior finalization")?;
        let mut events = Vec::new();
        assert!(matches!(
            broker.advance_prior_finalization(&mut state, &mut events)?,
            PriorFinalizationAction::Ready
        ));
        assert_eq!(
            broker
                .store
                .load_exchange(&exchange.id)?
                .context("missing prior exchange")?
                .state(),
            ExchangeState::Cancelled
        );
        assert!(
            events
                .iter()
                .any(|event| event.event == "exchange_complete")
        );
        Ok(())
    }
}
