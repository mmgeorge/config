//! Capture and revalidate automatic check work independently of the broker lock.

use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use anyhow::{Result, ensure};

use super::{
    Exchange, ExchangeState, HarnessBroker, SessionEvent, TurnCancelled,
    publication::SessionUpdateSink,
};
use crate::checks::{
    AutomaticChecks, CheckRun, CheckState, CheckSupervisor, PreparedCheckRun, RunRequest,
};
use crate::provider::InvocationScope;
use serde_json::{Value, json};

/// Own the inputs needed to refresh check discovery without borrowing broker state.
pub(crate) struct AutomaticCheckRefresh {
    supervisor: Arc<CheckSupervisor>,
    owner: crate::timeline::TimelineOwner,
    exchange_id: String,
    scope: Option<InvocationScope>,
    revision: Option<u64>,
    shell: crate::session::CheckShell,
    sink: Option<SessionUpdateSink>,
    cancelled: Arc<AtomicBool>,
}

/// Record whether an existing check is still running or discovery is ready.
pub(crate) enum AutomaticCheckRefreshResult {
    Running(AutomaticCheckRefresh),
    Ready(AutomaticCheckRefresh),
}

/// Own one admitted check process and its revision evidence.
pub(crate) struct AutomaticCheckRun {
    capture: AutomaticCheckRefresh,
    run: PreparedCheckRun,
}

/// Record one completed check without allowing it to settle a newer execution.
pub(crate) struct AutomaticCheckResult {
    capture: AutomaticCheckRefresh,
    run: CheckRun,
}

/// Own one task verification process and the authority that admitted it.
pub(crate) struct TaskCheckRun {
    capture: AutomaticCheckRefresh,
    permit: crate::task::TaskPermit,
    run: PreparedCheckRun,
}

/// Retain task and execution authority with completed verification evidence.
pub(crate) struct TaskCheckResult {
    capture: AutomaticCheckRefresh,
    permit: crate::task::TaskPermit,
    run: CheckRun,
}

/// Distinguish invalidated work from a prepared check or an empty selection.
pub(crate) enum AutomaticCheckAdmission {
    Stale,
    Empty,
    Run(Box<AutomaticCheckRun>),
}

/// Own automatic validation state while refresh and check processes run independently.
pub(crate) struct AutomaticCheckTransaction {
    interaction: Exchange,
    automatic: AutomaticChecks,
    change_owner: String,
    result: (Value, Vec<SessionEvent>),
}

/// Route an automatic check phase without retaining a broker borrow.
pub(crate) enum AutomaticCheckAction {
    Finish((Value, Vec<SessionEvent>)),
    /// Drain reopened execution evidence and refresh host prerequisites before recapturing.
    Revalidate,
    Refresh(AutomaticCheckRefresh),
    Run(Box<AutomaticCheckRun>),
    /// Deliver a blocked-check diagnostic before returning the completed result.
    Publish {
        update: super::publication::SessionUpdate,
        result: (Value, Vec<SessionEvent>),
    },
    Repair {
        prompt: String,
        change_owner: String,
    },
}

impl HarnessBroker {
    /// Retain response events while an invalidated check revalidates host prerequisites.
    pub(crate) fn take_automatic_check_result(
        &self,
        tx: &mut AutomaticCheckTransaction,
    ) -> (Value, Vec<SessionEvent>) {
        std::mem::take(&mut tx.result)
    }

    /// Record the exchange that owns the pending automatic repair before dispatch.
    pub(crate) fn record_automatic_repair_exchange(
        &self,
        tx: &mut AutomaticCheckTransaction,
        exchange: &Exchange,
        owner: &str,
    ) -> Result<()> {
        ensure!(
            tx.change_owner == owner && exchange.check_owner.as_deref() == Some(owner),
            "automatic repair owner changed"
        );
        tx.automatic.repair_exchange = Some(exchange.id.clone());
        tx.automatic.save()
    }

    /// Transfer a prepared task verification process after checking its current authority.
    pub(crate) fn capture_task_check(
        &self,
        interaction: &Exchange,
        permit: crate::task::TaskPermit,
        run: PreparedCheckRun,
    ) -> Result<TaskCheckRun> {
        permit.validate(self.task_controller.current()?.as_ref())?;
        let capture = self.capture_automatic_check(interaction)?;
        ensure!(
            self.automatic_check_current(interaction, &capture, true)?,
            "task verification owner is not settled"
        );
        Ok(TaskCheckRun {
            capture,
            permit,
            run,
        })
    }

    /// Reject task verification evidence if its permit or execution revision has changed.
    pub(crate) fn commit_task_check(
        &self,
        interaction: &Exchange,
        result: TaskCheckResult,
    ) -> Result<Option<(crate::task::TaskPermit, CheckRun)>> {
        result
            .permit
            .validate(self.task_controller.current()?.as_ref())?;
        if !self.automatic_check_current(interaction, &result.capture, false)? {
            return Ok(None);
        }
        Ok(Some((result.permit, result.run)))
    }
    /// Select and capture automatic completion checks after their host prerequisites settle.
    pub(crate) fn begin_automatic_checks(
        &mut self,
        mut result: (Value, Vec<SessionEvent>),
        mode: crate::provider::PromptMode,
    ) -> Result<(Option<AutomaticCheckTransaction>, AutomaticCheckAction)> {
        if !matches!(
            mode,
            crate::provider::PromptMode::Chat
                | crate::provider::PromptMode::RequestChanges
                | crate::provider::PromptMode::GoalContinuation
        ) || self.check_setup
            || self.agent_coordinator.active_count() != 0
            || self.find_active_interaction_elicitation()?.is_some()
            || self.task_controller.current()?.is_some()
        {
            return Ok((None, AutomaticCheckAction::Finish(result)));
        }
        let Some(interaction) = self.store.latest_exchange(&self.session.id)? else {
            return Ok((None, AutomaticCheckAction::Finish(result)));
        };
        if interaction.state() != ExchangeState::Complete
            || interaction.awaiting_input()
            || interaction.check_setup.is_some()
        {
            return Ok((None, AutomaticCheckAction::Finish(result)));
        }
        let settings = crate::settings::SettingsStore::open(&self.data_root)?;
        let desired = settings
            .get(
                &self.session.workspace,
                &self.session.id,
                &self.session.provider,
            )?
            .desired;
        if !desired.automatic_checks_enabled() {
            self.observer.completion_queue.finish()?;
            return Ok((None, AutomaticCheckAction::Finish(result)));
        }
        if crate::checks::ConfigSnapshot::load(std::path::Path::new(&self.session.workspace))
            .is_err()
        {
            let catalog = self.check_runner.catalog(None)?;
            if catalog["configured"] == false {
                return Ok((None, AutomaticCheckAction::Finish(result)));
            }
            anyhow::bail!(
                "Automatic checks could not load configuration: {}",
                catalog["error"]
            );
        }
        let Some(change_owner) = interaction.check_owner.clone() else {
            return Ok((None, AutomaticCheckAction::Finish(result)));
        };
        let mut automatic = AutomaticChecks::open(&self.check_runner.root, &change_owner)?;
        if self
            .observer
            .completion_queue
            .defer(desired.check_timing.unwrap_or_default(), &change_owner)?
        {
            automatic.pending = true;
            automatic.save()?;
            result.0["checks_deferred"] = json!(true);
            return Ok((None, AutomaticCheckAction::Finish(result)));
        }
        let refresh = self.capture_automatic_check(&interaction)?;
        Ok((
            Some(AutomaticCheckTransaction {
                interaction,
                automatic,
                change_owner,
                result,
            }),
            AutomaticCheckAction::Refresh(refresh),
        ))
    }

    /// Revalidate a completed discovery before admitting a check process.
    pub(crate) fn apply_automatic_check_refresh(
        &mut self,
        tx: &mut AutomaticCheckTransaction,
        refresh: AutomaticCheckRefreshResult,
    ) -> Result<AutomaticCheckAction> {
        if self.task_controller.current()?.is_some()
            || self.agent_coordinator.active_count() != 0
            || self.find_active_interaction_elicitation()?.is_some()
        {
            return Ok(AutomaticCheckAction::Revalidate);
        }
        let desired = crate::settings::SettingsStore::open(&self.data_root)?
            .get(
                &self.session.workspace,
                &self.session.id,
                &self.session.provider,
            )?
            .desired;
        if !desired.automatic_checks_enabled() {
            tx.automatic.pending = false;
            tx.automatic.blocked = None;
            tx.automatic.delivered.clear();
            tx.automatic.repair_prompt = None;
            tx.automatic.repair_exchange = None;
            tx.automatic.save()?;
            self.observer.completion_queue.finish()?;
            return Ok(AutomaticCheckAction::Finish(std::mem::take(&mut tx.result)));
        }
        let capture = match refresh {
            AutomaticCheckRefreshResult::Running(capture) => {
                return Ok(
                    if self.automatic_check_current(&tx.interaction, &capture, true)? {
                        AutomaticCheckAction::Refresh(capture)
                    } else {
                        AutomaticCheckAction::Revalidate
                    },
                );
            }
            AutomaticCheckRefreshResult::Ready(capture) => capture,
        };
        match self.admit_automatic_check(&tx.interaction, capture)? {
            AutomaticCheckAdmission::Stale => Ok(AutomaticCheckAction::Revalidate),
            AutomaticCheckAdmission::Empty => {
                self.observer.completion_queue.finish()?;
                tx.automatic.pending = false;
                tx.automatic.blocked = None;
                tx.automatic.delivered.clear();
                tx.automatic.repair_prompt = None;
                tx.automatic.repair_exchange = None;
                tx.automatic.save()?;
                Ok(AutomaticCheckAction::Finish(std::mem::take(&mut tx.result)))
            }
            AutomaticCheckAdmission::Run(run) => {
                tx.automatic.pending = true;
                tx.automatic.save()?;
                Ok(AutomaticCheckAction::Run(run))
            }
        }
    }

    /// Apply current check evidence or schedule another round after invalidation.
    pub(crate) fn apply_automatic_check_result(
        &mut self,
        tx: &mut AutomaticCheckTransaction,
        result: AutomaticCheckResult,
    ) -> Result<AutomaticCheckAction> {
        if self.task_controller.current()?.is_some()
            || self.agent_coordinator.active_count() != 0
            || self.find_active_interaction_elicitation()?.is_some()
        {
            return Ok(AutomaticCheckAction::Revalidate);
        }
        let Some(run) = self.commit_automatic_check(&tx.interaction, result)? else {
            return Ok(AutomaticCheckAction::Revalidate);
        };
        if run.state == CheckState::Interrupted {
            return Err(TurnCancelled.into());
        }
        if run.state == CheckState::Stale {
            return Ok(AutomaticCheckAction::Refresh(
                self.capture_automatic_check(&tx.interaction)?,
            ));
        }
        let desired = crate::settings::SettingsStore::open(&self.data_root)?
            .get(
                &self.session.workspace,
                &self.session.id,
                &self.session.provider,
            )?
            .desired;
        if !desired.automatic_checks_enabled() {
            tx.automatic.pending = false;
            tx.automatic.blocked = None;
            tx.automatic.delivered.clear();
            tx.automatic.repair_prompt = None;
            tx.automatic.repair_exchange = None;
            tx.automatic.save()?;
            self.observer.completion_queue.finish()?;
            return Ok(AutomaticCheckAction::Finish(std::mem::take(&mut tx.result)));
        }
        let repair = tx.automatic.observe(&run)?;
        if matches!(run.state, CheckState::Stopped | CheckState::ExecutionError) {
            tx.result.0["automatic_checks"] = json!({"run_id":run.id,"state":run.state});
            return Ok(AutomaticCheckAction::Finish(std::mem::take(&mut tx.result)));
        }
        if let Some(reason) = &tx.automatic.blocked {
            let update = super::publication::SessionUpdate::Diagnostic {
                diagnostic: crate::provider::ProviderDiagnostic {
                    kind: forge_provider_api::ProviderFailureKind::Unknown,
                    message: forge_provider_api::SanitizedMessage::new(reason.clone())
                        .expect("host diagnostic is nonempty"),
                    fatal: false,
                },
            };
            tx.result.0["automatic_checks"] = json!({"run_id":run.id,"blocked":reason});
            if self.event_sink.is_some() {
                return Ok(AutomaticCheckAction::Publish {
                    update,
                    result: std::mem::take(&mut tx.result),
                });
            }
            tx.result.1.push(self.event(
                "provider_event",
                super::publication::encode_session_update(&update)?,
            )?);
            return Ok(AutomaticCheckAction::Finish(std::mem::take(&mut tx.result)));
        }
        let Some(prompt) = repair else {
            self.observer.completion_queue.finish()?;
            tx.result.0["automatic_checks"] = json!({"run_id":run.id,"state":run.state});
            return Ok(AutomaticCheckAction::Finish(std::mem::take(&mut tx.result)));
        };
        Ok(AutomaticCheckAction::Repair {
            prompt,
            change_owner: tx.change_owner.clone(),
        })
    }

    /// Record a completed repair and either resume checking or return its pending interaction.
    pub(crate) fn apply_automatic_check_repair(
        &mut self,
        tx: &mut AutomaticCheckTransaction,
        repaired: (Value, Vec<SessionEvent>),
    ) -> Result<AutomaticCheckAction> {
        tx.result.0 = repaired.0;
        tx.result.1.extend(repaired.1);
        let latest = self
            .store
            .latest_exchange(&self.session.id)?
            .ok_or_else(|| anyhow::anyhow!("repair exchange disappeared"))?;
        tx.automatic.repair_exchange = Some(latest.id.clone());
        tx.automatic.save()?;
        if latest.state() != ExchangeState::Complete
            || latest.awaiting_input()
            || self.find_active_interaction_elicitation()?.is_some()
            || self.agent_coordinator.active_count() != 0
            || self.task_controller.current()?.is_some()
        {
            return Ok(AutomaticCheckAction::Finish(std::mem::take(&mut tx.result)));
        }
        tx.interaction = latest;
        Ok(AutomaticCheckAction::Refresh(
            self.capture_automatic_check(&tx.interaction)?,
        ))
    }

    /// Refresh an invalidated check only after reopened work has settled on its original exchange.
    pub(crate) fn revalidate_automatic_checks(
        &self,
        tx: &mut AutomaticCheckTransaction,
    ) -> Result<AutomaticCheckAction> {
        let interaction = self
            .store
            .load_exchange(&tx.interaction.id)?
            .ok_or_else(|| anyhow::anyhow!("automatic check exchange disappeared"))?;
        if interaction.turn.last().is_some_and(|turn| {
            self.reopened_execution.active(&turn.provider().scope)
                || self.reopened_execution.is_settling(&turn.provider().scope)
        }) {
            return Ok(AutomaticCheckAction::Revalidate);
        }
        if interaction.state() != ExchangeState::Complete
            || interaction.awaiting_input()
            || interaction.check_owner.as_deref() != Some(tx.change_owner.as_str())
            || self.find_active_interaction_elicitation()?.is_some()
            || self.agent_coordinator.active_count() != 0
            || self.task_controller.current()?.is_some()
        {
            return Ok(AutomaticCheckAction::Finish(std::mem::take(&mut tx.result)));
        }
        tx.interaction = interaction;
        Ok(AutomaticCheckAction::Refresh(
            self.capture_automatic_check(&tx.interaction)?,
        ))
    }
    /// Capture the exchange and execution revision before starting check discovery.
    pub(crate) fn capture_automatic_check(
        &self,
        interaction: &Exchange,
    ) -> Result<AutomaticCheckRefresh> {
        let scope = interaction
            .turn
            .last()
            .map(|turn| turn.provider().scope.clone());
        let revision = scope
            .as_ref()
            .map(|scope| self.reopened_execution.revision(scope));
        Ok(AutomaticCheckRefresh {
            supervisor: self.check_runner.clone(),
            owner: self
                .store
                .capture_timeline_owner(&self.session.id, &interaction.id)?,
            exchange_id: interaction.id.clone(),
            scope,
            revision,
            shell: self.session.shell,
            sink: self.event_sink.clone(),
            cancelled: self.turn_cancellation.requested.clone(),
        })
    }

    /// Admit refreshed checks only if the captured execution is still current.
    pub(crate) fn admit_automatic_check(
        &self,
        interaction: &Exchange,
        capture: AutomaticCheckRefresh,
    ) -> Result<AutomaticCheckAdmission> {
        if !self.automatic_check_current(interaction, &capture, true)? {
            return Ok(AutomaticCheckAdmission::Stale);
        }
        if capture.cancelled.load(Ordering::Acquire) {
            return Err(TurnCancelled.into());
        }
        let mut run = capture.supervisor.prepare_request(
            capture.owner.clone(),
            RunRequest::default(),
            None,
            capture.shell,
            "Automatic completion checks".into(),
        )?;
        if !run
            .commands
            .iter()
            .any(|command| command.state != CheckState::Skipped)
        {
            return Ok(AutomaticCheckAdmission::Empty);
        }
        run.context.automatic = true;
        Ok(AutomaticCheckAdmission::Run(Box::new(AutomaticCheckRun {
            capture,
            run,
        })))
    }

    /// Reject check evidence if reopening changed its execution before the result arrived.
    pub(crate) fn commit_automatic_check(
        &self,
        interaction: &Exchange,
        result: AutomaticCheckResult,
    ) -> Result<Option<CheckRun>> {
        if !self.automatic_check_current(interaction, &result.capture, false)? {
            return Ok(None);
        }
        ensure!(
            !result.capture.cancelled.load(Ordering::Acquire),
            TurnCancelled
        );
        Ok(Some(result.run))
    }

    fn automatic_check_current(
        &self,
        interaction: &Exchange,
        capture: &AutomaticCheckRefresh,
        check_timeline: bool,
    ) -> Result<bool> {
        Ok(interaction.id == capture.exchange_id
            && (!check_timeline
                || self
                    .store
                    .capture_timeline_owner(&self.session.id, &interaction.id)?
                    == capture.owner)
            && interaction.turn.last().map(|turn| &turn.provider().scope) == capture.scope.as_ref()
            && capture.scope.as_ref().is_none_or(|scope| {
                capture.revision == Some(self.reopened_execution.revision(scope))
                    && !self.reopened_execution.active(scope)
                    && !self.reopened_execution.is_settling(scope)
            }))
    }
}

impl AutomaticCheckRefresh {
    /// Wait for an existing run or refresh discovery without holding the broker.
    pub(crate) async fn run(self) -> Result<AutomaticCheckRefreshResult> {
        if self.cancelled.load(Ordering::Acquire) {
            return Err(TurnCancelled.into());
        }
        if let Some(id) = self.supervisor.active()?
            && self.supervisor.status(&id, 30_000, false).await?.state == CheckState::Running
        {
            return Ok(AutomaticCheckRefreshResult::Running(self));
        }
        self.supervisor.refresh_changes().await?;
        Ok(AutomaticCheckRefreshResult::Ready(self))
    }
}

impl AutomaticCheckRun {
    /// Execute admitted checks independently of broker ownership.
    pub(crate) async fn run(self) -> Result<AutomaticCheckResult> {
        let run = self
            .capture
            .supervisor
            .run(
                self.run,
                self.capture.sink.clone(),
                Some(self.capture.cancelled.clone()),
            )
            .await?;
        Ok(AutomaticCheckResult {
            capture: self.capture,
            run,
        })
    }
}

impl TaskCheckRun {
    /// Run the prepared task verification without holding its admitting broker.
    pub(crate) async fn run(self) -> Result<TaskCheckResult> {
        let run = self
            .capture
            .supervisor
            .run(
                self.run,
                self.capture.sink.clone(),
                Some(self.capture.cancelled.clone()),
            )
            .await?;
        Ok(TaskCheckResult {
            capture: self.capture,
            permit: self.permit,
            run,
        })
    }
}
