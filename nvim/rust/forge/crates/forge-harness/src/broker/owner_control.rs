//! Prepare lifecycle decisions applied only by the session exchange owner.

use super::*;
use crate::exchange::command::CancellationOutcome;

/// Capture the root exchange selected when an owner control command begins.
pub(crate) struct ExchangeControlView {
    /// Current nonterminal exchange, if one exists.
    pub(crate) selection: Option<ExchangeControlSelection>,
    /// Latest normalized invocation selected from the same accepted observation prefix.
    pub(crate) steering_scope: Option<crate::provider::InvocationScope>,
    /// Committed observation updates awaiting outbound publication.
    pub(crate) events: Vec<SessionEvent>,
}

/// Capture exchange identity and continuation authority before collection parks active work.
#[derive(Clone)]
pub(crate) struct ExchangeControlSelection {
    id: crate::provider::ExchangeId,
    continuation: ContinuationAuthority,
}

impl ExchangeControlSelection {
    /// Get the exact exchange whose resources collection must settle.
    pub(crate) fn id(&self) -> &crate::provider::ExchangeId {
        &self.id
    }

    /// Consume authorization captured before collection; an already-paused exchange stays paused.
    pub(crate) fn into_continuation(self) -> Option<crate::provider::ExchangeId> {
        match self.continuation {
            ContinuationAuthority::Resume => Some(self.id),
            ContinuationAuthority::Preserve => None,
        }
    }
}

#[derive(Clone)]
enum ContinuationAuthority {
    Resume,
    Preserve,
}

impl HarnessBroker {
    /// Capture queued input and committed configuration without borrowing an earlier foreground's context.
    /// Native admission seals any later task or question overrides into the same receipt.
    pub(crate) fn input_submission_request(
        &self,
        input: ProviderInput,
        mode: PromptMode,
    ) -> Result<ProviderRequest> {
        Ok(ProviderRequest {
            dispatch: forge_provider_api::submission::DispatchTracker::default(),
            execution_owner: None,
            harness_session_id: self.session.id.clone(),
            workspace: self.session.workspace.clone(),
            input,
            mode: if self.session.background.is_some() {
                PromptMode::Background
            } else {
                mode
            },
            model: self.session.model.clone(),
            effort: self.session.effort.clone(),
            context_window: self.session.context_window.clone(),
            service_tier: self.session.service_tier,
            access: self.session.access.clone(),
            approval_mode: self.session.approval_mode,
            provider_session_id: self
                .session
                .provider_session_id
                .clone()
                .map(TryInto::try_into)
                .transpose()?,
            submission: None,
            acknowledged_child_reports: Vec::new(),
            recovered_input: Vec::new(),
            control_context: None,
        })
    }

    /// Record the exact command/revision before provider settings application begins.
    pub(crate) fn owner_begin_configuration_application(
        &self,
        command: &crate::exchange::command::CommandId,
        revision: u64,
    ) -> Result<()> {
        crate::settings::SettingsStore::open(&self.data_root)?.begin_application(
            &self.session.id,
            command.as_str(),
            revision,
        )
    }

    /// Publish committed configuration and derive continuation only from the collected exchange.
    pub(crate) fn owner_finish_configuration(
        &mut self,
        command: Option<&crate::exchange::command::CommandId>,
        target: Option<&crate::provider::ExchangeId>,
        policy_baseline: Option<&super::configuration::PolicyBaseline>,
    ) -> Result<super::configuration::PermissionConfigurationResult> {
        let resume = match target {
            Some(target) if self.owner_resume_selected(target)? => {
                let selected = self
                    .store
                    .load_exchange(target.as_str())?
                    .context("selected configuration exchange disappeared")?;
                // A permission change names the policy transition; other settings resume generically.
                let notice = policy_baseline.map_or_else(
                    || "Continue the interrupted request using the committed session configuration and completed tool results. Do not repeat completed actions.".to_owned(),
                    |baseline| baseline.resume_notice(self.session.access.abbreviation(), self.session.approval_mode),
                );
                Some(if selected.elicitation().is_some() {
                    notice
                } else {
                    format!("{notice}\n\nOriginal request: {}", selected.prompt)
                })
            }
            Some(_) | None => None,
        };
        let thought_patch = self
            .presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
            .set_status_thoughts(
                self.observer
                    .status_thoughts
                    .load(std::sync::atomic::Ordering::Relaxed),
            )?;
        let delivery =
            self.prepare_provider_delivery(super::AppliedProviderEvent::Effects(vec![
                super::AppliedProviderEvent::Timeline(thought_patch),
                super::AppliedProviderEvent::Update(
                    super::publication::SessionUpdate::Configuration {
                        snapshot: Box::new(self.snapshot()?),
                        operation_id: command.map(|command| command.as_str().to_owned()),
                        catalog: Box::new(self.provider_catalog_request()?),
                    },
                ),
            ]))?;
        Ok(super::configuration::PermissionConfigurationResult {
            delivery,
            resume,
            value: serde_json::to_value(self.snapshot()?)?,
        })
    }

    /// Capture current control scope after applying the accepted observation prefix.
    pub(crate) fn owner_control_view(&mut self) -> Result<ExchangeControlView> {
        let events = self.drain_session_events()?;
        let current = self.owner_current_exchange()?;
        Ok(ExchangeControlView {
            steering_scope: current
                .as_ref()
                .and_then(|exchange| exchange.turn.last())
                .map(|turn| turn.provider().scope.clone()),
            selection: current.map(|exchange| ExchangeControlSelection {
                continuation: match exchange.activity() {
                    crate::exchange::ExchangeActivity::Paused
                    | crate::exchange::ExchangeActivity::Settled => ContinuationAuthority::Preserve,
                    crate::exchange::ExchangeActivity::Active
                    | crate::exchange::ExchangeActivity::AwaitingApproval
                    | crate::exchange::ExchangeActivity::AwaitingInput
                    | crate::exchange::ExchangeActivity::WaitingForAgent
                    | crate::exchange::ExchangeActivity::Compacting { .. } => {
                        ContinuationAuthority::Resume
                    }
                },
                id: exchange.id.into(),
            }),
            events,
        })
    }

    /// Apply positive collection evidence without inventing an additional native terminal event.
    pub(crate) fn owner_collection_completed(
        &mut self,
        target: Option<&crate::provider::ExchangeId>,
    ) -> Result<Vec<SessionEvent>> {
        let events = self.drain_session_events()?;
        if let Some(target) = target
            && !self.exchange_retracted(target)
        {
            let selected = self
                .store
                .load_exchange(target.as_str())?
                .context("collected exchange disappeared")?;
            anyhow::ensure!(
                selected.session_id == self.session.id,
                "collection target belongs to another session"
            );
        }
        Ok(events)
    }

    /// Cancel local/waiting work after foreground and native collection have settled.
    /// Preserve terminal history if natural completion won before this transition.
    pub(crate) fn owner_cancel_local(
        &mut self,
        target: Option<&crate::provider::ExchangeId>,
    ) -> Result<(Option<PriorFinalization>, Vec<SessionEvent>)> {
        let mut events = self.drain_session_events()?;
        if let Some(task) = self.task_controller.current()?
            && !task.status().terminal()
        {
            self.task_controller.change(
                &mut self.store,
                Some(&task.revision),
                crate::task::TaskChange::Pause,
                self.clock.now_ms(),
            )?;
            events.push(self.event("session_changed", json!({"snapshot":self.snapshot()?}))?);
        }
        let Some(target) = target else {
            return Ok((None, events));
        };
        let Some(mut exchange) = self.store.load_exchange(target.as_str())? else {
            anyhow::ensure!(
                self.exchange_retracted(target),
                "selected cancellation exchange disappeared"
            );
            return Ok((None, events));
        };
        if exchange.is_finished() {
            return Ok((None, events));
        }
        // Completed child-admission records can follow an active root exchange.
        // They do not transfer its continuation authority; a newer pending or
        // active exchange still prevents resuming this exact selection.
        anyhow::ensure!(
            self.owner_current_exchange()?
                .is_some_and(|current| current.id == exchange.id),
            "selected cancellation exchange changed"
        );
        self.questions.cancel_chat(Some(&exchange.id))?;
        exchange.set_awaiting_input(false);
        match exchange.state() {
            ExchangeState::Queued => {
                exchange.finish(
                    crate::exchange::ExchangeOutcome::Cancelled,
                    self.clock.now_ms(),
                )?;
                self.store.save_exchange(&exchange)?;
                self.save_session()?;
                events.push(self.event(
                    "exchange_complete",
                    json!({"id":exchange.id,"state":exchange.state()}),
                )?);
                Ok((None, events))
            }
            ExchangeState::Running => {
                exchange.begin_finalization(
                    crate::exchange::ExchangeOutcome::Cancelled,
                    self.clock.now_ms(),
                )?;
                self.store.save_exchange(&exchange)?;
                Ok((self.begin_prior_finalization()?, events))
            }
            ExchangeState::Finalizing => Ok((self.begin_prior_finalization()?, events)),
            ExchangeState::Complete
            | ExchangeState::Failed
            | ExchangeState::Cancelled
            | ExchangeState::Interrupted => {
                unreachable!("terminal cancellation targets were preserved before transition")
            }
        }
    }

    /// Read the selected exchange's committed cancellation result without retargeting.
    pub(crate) fn owner_cancellation_outcome(
        &self,
        target: Option<&crate::provider::ExchangeId>,
    ) -> Result<CancellationOutcome> {
        let Some(target) = target else {
            return Ok(CancellationOutcome::NoWork);
        };
        if self.exchange_retracted(target) {
            return Ok(CancellationOutcome::Retracted {
                exchange: target.clone(),
            });
        }
        let exchange = self
            .store
            .load_exchange(target.as_str())?
            .context("cancelled exchange disappeared")?;
        if exchange.state() == ExchangeState::Cancelled && exchange.is_finished() {
            return Ok(CancellationOutcome::Cancelled {
                exchange: target.clone(),
            });
        }
        anyhow::ensure!(
            exchange.is_finished(),
            "cancellation has not settled its selected exchange"
        );
        Ok(CancellationOutcome::NoWork)
    }

    /// Get whether confirmed output-free retraction, rather than lost history, removed the exchange.
    fn exchange_retracted(&self, exchange: &crate::provider::ExchangeId) -> bool {
        self.retracted_exchange_id.as_deref() == Some(exchange.as_str())
    }

    /// Get whether a collected exchange remains finalizing with no task left to resume it.
    pub(crate) fn owner_has_orphaned_finalization(&self) -> Result<bool> {
        Ok(self.task_controller.current()?.is_none()
            && self
                .store
                .exchange_in_state(&self.session.id, ExchangeState::Finalizing)?
                .is_some())
    }

    /// Close the exchange that task-control collection parked once no task remains to resume it.
    ///
    /// Task controls collect through reconfiguration, which parks the selected exchange and
    /// relies on the next admission to close it. A committed control that leaves no current
    /// task schedules no such admission, so the parked exchange would otherwise stay running.
    pub(crate) fn owner_close_orphaned_parked_exchange(&mut self) -> Result<Vec<SessionEvent>> {
        if self.task_controller.current()?.is_some() {
            return Ok(Vec::new());
        }
        let Some(mut exchange) = self
            .store
            .exchange_in_state(&self.session.id, ExchangeState::Running)?
            .filter(Exchange::is_parked)
        else {
            return Ok(Vec::new());
        };
        self.questions.cancel_chat(Some(&exchange.id))?;
        exchange.set_awaiting_input(false);
        self.close_superseded_exchange(&mut exchange, self.clock.now_ms())?;
        self.save_session()?;
        let mut events = vec![self.event(
            if exchange.is_finished() {
                "exchange_complete"
            } else {
                "exchange_updated"
            },
            json!({"id":exchange.id,"state":exchange.state()}),
        )?];
        let patch = self.reconcile_timeline()?;
        if !patch.is_empty() {
            events.push(self.event("document_changed", patch.notification())?);
        }
        Ok(events)
    }

    /// Revalidate a selected parked exchange without silently continuing a different request.
    pub(crate) fn owner_resume_selected(
        &self,
        target: &crate::provider::ExchangeId,
    ) -> Result<bool> {
        let selected = self
            .store
            .load_exchange(target.as_str())?
            .context("selected parked exchange disappeared")?;
        anyhow::ensure!(
            selected.session_id == self.session.id,
            "parked exchange belongs to another session"
        );
        if selected.is_finished() {
            return Ok(false);
        }
        anyhow::ensure!(
            self.owner_current_exchange()?
                .is_some_and(|current| current.id == selected.id)
                && self
                    .store
                    .latest_exchange(&self.session.id)?
                    .is_some_and(|latest| latest.id == selected.id || latest.is_finished()),
            "parked exchange changed before reconfiguration dispatch"
        );
        Ok(true)
    }

    /// Select active exchange work ahead of a newer queued input awaiting delivery.
    fn owner_current_exchange(&self) -> Result<Option<Exchange>> {
        if let Some(exchange) = self
            .store
            .exchange_in_state(&self.session.id, ExchangeState::Running)?
        {
            return Ok(Some(exchange));
        }
        if let Some(exchange) = self
            .store
            .exchange_in_state(&self.session.id, ExchangeState::Finalizing)?
        {
            return Ok(Some(exchange));
        }
        Ok(self
            .store
            .latest_exchange(&self.session.id)?
            .filter(|exchange| {
                !exchange.is_finished()
                    && exchange.disposition() == crate::exchange::HistoryDisposition::Current
            }))
    }
}
