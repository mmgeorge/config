//! Hold the exchange driver's loop-owned work so step phases and wake handlers share one owner.

use std::collections::VecDeque;

use forge_protocol::outbound::MessageSender;
use tokio::sync::{oneshot, watch};
use tokio::task::JoinSet;

use super::super::command::{CommandId, InputId};
use super::super::input_delivery::{InputDeliveryEvent, RetainedInput};
use super::super::publication::{PublicationDelivery, PublicationError};
use super::super::rollback_run::RollbackRecovery;
use super::super::shutdown::OwnerShutdown;
use super::super::transition::RetainedTransition;
use super::{
    ActiveCommand, ActiveTurn, AutomaticPublication, CollectedCancellation, CollectedInput,
    DeferredPublications, DriverWake, FailedForeground, QueuedTurn, TaskVerification,
};
use crate::broker::DispatchResult;

/// Own every task, queue, and retained resource the driver loop keeps between update steps.
pub(super) struct DriverState {
    /// Admitted foreground turns waiting for the active turn to finish.
    pub(super) pending: VecDeque<QueuedTurn>,
    /// The turn that currently owns provider execution.
    pub(super) foreground: Option<ActiveTurn>,
    /// The single caller command the driver is executing.
    pub(super) command: Option<ActiveCommand>,
    /// The transport that receives command and publication output.
    pub(super) command_messages: Option<MessageSender>,
    /// Offer and admission state for committed session publications.
    pub(super) publication_delivery: PublicationDelivery,
    /// When the driver next retries opening publication storage.
    pub(super) publication_recovery_at: Option<tokio::time::Instant>,
    /// Publications the transport has not accepted yet.
    pub(super) deferred_publications: DeferredPublications,
    /// Background jobs that must finish before a restart or shutdown.
    pub(super) tasks: RunningTasks,
    /// Drained input work kept alive until the driver exits.
    pub(super) retained_inputs: Vec<RetainedInput>,
    /// The last drained or failed command stop.
    pub(super) retained_cancellation: Option<RetainedTransition>,
    /// Rollback recoveries that failed and keep their resources.
    pub(super) retained_rollbacks: Vec<RollbackRecovery>,
    /// Failed work whose provider restart has not succeeded yet.
    pub(super) failed_work: Option<FailedWork>,
    /// A steering event that became ready while the driver polled ahead.
    pub(super) steering_ready: Option<(InputId, InputDeliveryEvent)>,
    /// The shutdown owner once the session inbox closes.
    pub(super) shutdown: Option<OwnerShutdown>,
    /// A wake selected in one pass and applied on the next.
    pub(super) pending_wake: Option<DriverWake>,
    /// Question revision changes from the session's question controller.
    pub(super) question_changes: watch::Receiver<u64>,
    /// Requests from tool-started and automatic check owners.
    pub(super) check_events: tokio::sync::mpsc::Receiver<crate::checks::owner::CheckOwnerEvent>,
    /// Check runs that tools started.
    pub(super) check_runs: crate::checks::owner::CheckOwnerRuns,
    /// The invocation whose tool checks the driver currently admits.
    pub(super) check_scope: Option<crate::provider::InvocationScope>,
    /// Tool check requests waiting for admission.
    pub(super) pending_check_tools: VecDeque<crate::checks::owner::ToolCheckRequest>,
    // Tool-started runs outlive the provider turn that requested them, so their progress and
    // configuration errors publish through this owner-lifetime stream rather than the turn sink.
    /// The sink tool-started check runs publish through.
    pub(super) tool_check_sink: crate::broker::publication::SessionUpdateSink,
    /// The stream that receives tool-started check output.
    pub(super) tool_check_updates: crate::broker::publication::SessionUpdateStream,
    /// Automatic check completions waiting to start.
    pub(super) pending_automatic_checks: VecDeque<crate::checks::owner::AutomaticCheckRequest>,
    /// The automatic check completion the driver is running.
    pub(super) automatic_checks: Option<crate::broker::OwnedCheckCompletion>,
    /// The next action the automatic check completion requested.
    pub(super) automatic_action: Option<crate::broker::OwnedCheckProgress>,
    /// The publication stream of the running automatic check.
    pub(super) automatic_publication: Option<AutomaticPublication>,
    /// The response of a repair turn that automatic checks started.
    pub(super) automatic_repair: Option<oneshot::Receiver<anyhow::Result<DispatchResult>>>,
    /// The task check run and the permits it holds.
    pub(super) task_verification: Option<TaskVerification>,
    /// The output stream of the running task check.
    pub(super) task_check_updates: Option<crate::broker::publication::SessionUpdateStream>,
    /// A finished task check waiting to publish its events.
    pub(super) task_check_completion: Option<(
        Vec<forge_protocol::message::SessionEvent>,
        Option<crate::checks::owner::CheckOwnerFailure>,
    )>,
}

/// Own the background jobs whose results the driver must collect before it restarts the provider
/// or shuts down.
///
/// `is_idle` is the only definition of "no background job is running", so a new job kind added
/// here is automatically one that restarts and shutdown wait for.
#[derive(Default)]
pub(super) struct RunningTasks {
    /// Collection jobs for turns that failed.
    pub(super) failed_turns: JoinSet<FailedForeground>,
    /// Native collection and host drain jobs for command stops.
    pub(super) stops: JoinSet<CollectedCancellation>,
    /// Recovery jobs for rollback commands.
    pub(super) rollbacks: JoinSet<(CommandId, RollbackRecovery)>,
    /// Response forwarding for turns the driver started internally.
    pub(super) internal_completions: JoinSet<(uuid::Uuid, Result<(), PublicationError>)>,
    /// Native collection and host drain jobs for input delivery stops.
    pub(super) input_stops: JoinSet<CollectedInput>,
}

impl RunningTasks {
    /// Get whether every background job has been collected.
    pub(super) fn is_idle(&self) -> bool {
        let Self {
            failed_turns,
            stops,
            rollbacks,
            internal_completions,
            input_stops,
        } = self;
        failed_turns.is_empty()
            && stops.is_empty()
            && rollbacks.is_empty()
            && internal_completions.is_empty()
            && input_stops.is_empty()
    }
}

/// Record work whose failure left provider state unknown, while the restart that replaces its
/// provider generation runs.
pub(super) enum FailedWork {
    /// A failed turn's collected runtime, kept so its native handles outlive the old generation.
    Turn(Box<ActiveTurn>),
    /// A stop that could not prove the provider idle, or a collection task that ended without
    /// reporting its result.
    Unknown,
}

impl DriverState {
    /// Create empty loop state bound to the session's question and check owners.
    pub(super) fn new(
        services: &super::super::services::OwnerServices,
        check_events: tokio::sync::mpsc::Receiver<crate::checks::owner::CheckOwnerEvent>,
    ) -> Self {
        let (tool_check_sink, tool_check_updates) = crate::broker::publication::channel();
        Self {
            pending: VecDeque::new(),
            foreground: None,
            command: None,
            command_messages: None,
            publication_delivery: PublicationDelivery::default(),
            publication_recovery_at: None,
            deferred_publications: DeferredPublications::default(),
            tasks: RunningTasks::default(),
            retained_inputs: Vec::new(),
            retained_cancellation: None,
            retained_rollbacks: Vec::new(),
            failed_work: None,
            steering_ready: None,
            shutdown: None,
            pending_wake: None,
            question_changes: services.questions.changed.subscribe(),
            check_events,
            check_runs: crate::checks::owner::CheckOwnerRuns::new(services.checks.clone()),
            check_scope: None,
            pending_check_tools: VecDeque::new(),
            tool_check_sink,
            tool_check_updates,
            pending_automatic_checks: VecDeque::new(),
            automatic_checks: None,
            automatic_action: None,
            automatic_publication: None,
            automatic_repair: None,
            task_verification: None,
            task_check_updates: None,
            task_check_completion: None,
        }
    }
}

/// Hold the session-lifetime handles every driver step and wake handler reads.
pub(super) struct DriverEnv {
    /// The session broker that owns domain state.
    pub(super) broker: std::sync::Arc<tokio::sync::Mutex<crate::broker::HarnessBroker>>,
    /// The session's diagnostic trace.
    pub(super) trace: std::sync::Arc<crate::trace::TraceStore>,
    /// The session this driver owns.
    pub(super) session_id: String,
    /// The shared services the driver coordinates.
    pub(super) services: super::super::services::OwnerServices,
}

impl DriverEnv {
    /// Create the context that one update step records and publishes through.
    pub(super) fn context(&self, step: u64) -> super::OwnerUpdateContext<'_> {
        super::OwnerUpdateContext {
            services: &self.services,
            trace: &self.trace,
            session: &self.session_id,
            step,
        }
    }
}
