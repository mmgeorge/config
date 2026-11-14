//! Persist cancellation phase and blockers before exposing them to observers.

use super::*;
use crate::exchange::{CancellationBlockers, CancellationPhase, CancellationProgress};

impl ExchangeDriver {
    /// Publish only changed cancellation facts; response expiry never ends the operation.
    pub(super) fn cancellation_publish(
        &mut self,
        env: &DriverEnv,
        state: &DriverState,
        step: u64,
    ) -> Result<()> {
        let previous = self.cancellation_progress.as_ref();
        let (operation_id, phase, exchange_id, response_timed_out) = match &state.command {
            Some(ActiveCommand::Cancel {
                command,
                transition,
            }) => {
                let (phase, exchange) = transition.cancellation_progress();
                (
                    command.record.request.id.clone(),
                    phase,
                    exchange,
                    transition.response_expired(),
                )
            }
            _ => {
                let Some(previous) = previous.filter(|previous| previous.phase.active()) else {
                    return Ok(());
                };
                let phase = match self.execution_state {
                    ExecutionState::Stopping | ExecutionState::Restarting => {
                        CancellationPhase::Recovering
                    }
                    ExecutionState::Ready => CancellationPhase::Settled,
                    ExecutionState::Failed => CancellationPhase::Failed,
                };
                (
                    previous.operation_id.clone(),
                    phase,
                    previous.exchange_id.clone(),
                    previous.response_timed_out,
                )
            }
        };
        let foreground = state.foreground.as_ref();
        let blockers = CancellationBlockers {
            foreground: foreground.is_some(),
            running_jobs: foreground.map_or(0, |runtime| runtime.jobs.len()),
            staged_effects: foreground
                .map(|runtime| {
                    runtime
                        .staged_effects
                        .iter()
                        .map(|job| job.diagnostic_name().to_owned())
                        .collect()
                })
                .unwrap_or_default(),
            queued_publications: foreground.map_or(0, |runtime| runtime.publications.len())
                + state.deferred_publications.updates.len(),
            publication_active: foreground
                .is_some_and(|runtime| !runtime.publication_job.is_empty()),
            checks_running: !state.check_runs.is_empty()
                || state.automatic_checks.is_some()
                || state.task_verification.is_some(),
            children: env.services.children.active_count(),
            input_delivery: self.input_delivery.is_some() || !self.steering.is_empty(),
            recovery_jobs: !state.tasks.is_idle() || self.observation_recovery.active(),
        };
        if previous.is_some_and(|previous| {
            previous.operation_id == operation_id
                && previous.phase == phase
                && previous.exchange_id == exchange_id
                && previous.response_timed_out == response_timed_out
                && previous.blockers == blockers
        }) {
            return Ok(());
        }
        let now = crate::broker::Clock::now_ms(&crate::broker::SystemClock);
        let progress = CancellationProgress {
            started_at_ms: previous
                .filter(|previous| previous.operation_id == operation_id)
                .map_or(now, |previous| previous.started_at_ms),
            updated_at_ms: now,
            revision: previous
                .map_or(Some(1), |previous| previous.revision.checked_add(1))
                .context("cancellation revision exhausted")?,
            operation_id,
            exchange_id,
            phase,
            response_timed_out,
            blockers,
        };
        self.commands
            .append_core(&super::super::core_log::CoreRecord::CancellationProgress {
                progress: progress.clone(),
            })?;
        let snapshot = env.services.observer.snapshot()?;
        let event = crate::broker::publication::encode_session_event(
            &env.session_id,
            &crate::broker::publication::SessionUpdate::ExecutionState {
                snapshot: Box::new(snapshot),
            },
        )?;
        retain_publication(
            event,
            state.command_messages.as_ref(),
            &env.services,
            &env.trace,
            &env.session_id,
            step,
        )?;
        self.cancellation_progress = Some(progress);
        Ok(())
    }
}
