//! Keep the active command's typed stage and completion ownership together.

use super::agent_command::{AgentCommandEvent, AgentCommandRun};
use super::command::{Command, CommandFailure, ConfigurationChange, PermissionChange};
use super::input_command::{InputCommandRun, PreparedInput};
use super::native_command::{NativeCommandEvent, NativeCommandRun};
use super::queue::QueuedCommand;
use super::rollback_run::{RollbackEvent, RollbackRun};
use super::services::OwnerServices;
use super::task_command::{TaskCommandEvent, TaskCommandRun};
use super::transition::{ExchangeTransition, ForegroundStatus, TransitionEvent, TransitionIntent};

/// Own exactly one caller command while its asynchronous stages progress.
pub(super) enum ActiveCommand {
    /// Child commands retain their finite coordinator operation independently of its response waiters.
    Agent(Box<AgentCommandRun>),
    /// Rollback retains native collection and detached filesystem work.
    Rollback(Box<RollbackRun>),
    /// Task control retains collection, atomic mutation, and journaled input scheduling.
    Task(Box<TaskCommandRun>),
    /// Input admission/editing has a finite preparation and commit boundary.
    Input(InputCommandRun),
    /// Local configuration has a synchronous owner commit boundary.
    Configuration(QueuedCommand),
    /// Execution permission changes retain collection, commit, and replacement handoff stages.
    NativeCommand(Box<NativeCommandRun>),
    /// Cancellation retains collection and checkpoint ownership through settlement.
    Cancel {
        command: QueuedCommand,
        transition: Box<ExchangeTransition>,
    },
}

impl ActiveCommand {
    /// Permit a command's exact replacement foreground without reopening its prior invocation's tool gate.
    pub(super) fn authorizes_foreground(&self, work: uuid::Uuid) -> bool {
        match self {
            Self::NativeCommand(run) => run.authorizes_foreground(work),
            Self::Task(_)
            | Self::Rollback(_)
            | Self::Cancel { .. }
            | Self::Agent(_)
            | Self::Input(_)
            | Self::Configuration(_) => false,
        }
    }
    /// Transfer owner decisions without exposing the transition's writable state.
    pub(super) fn take_collection_signal(&mut self) -> Option<crate::broker::ForegroundSignal> {
        match self {
            Self::Task(run) => run.take_collection_signal(),
            Self::NativeCommand(run) => run.take_collection_signal(),
            Self::Rollback(run) => run.take_collection_signal(),
            Self::Cancel { transition, .. } => transition.take_collection_signal(),
            Self::Agent(_) | Self::Input(_) | Self::Configuration(_) => None,
        }
    }
    /// Close new invocation tools only for stages that require execution quiescence.
    pub(super) fn closes_tool_admission(&self) -> bool {
        match self {
            Self::Cancel { .. } => true,
            Self::Rollback(run) => run.closes_tool_admission(),
            Self::NativeCommand(run) => run.closes_tool_admission(),
            Self::Task(run) => run.closes_tool_admission(),
            Self::Agent(_) | Self::Input(_) | Self::Configuration(_) => false,
        }
    }
    /// Install the domain stage after the command's active revision commits.
    pub(super) fn start(command: QueuedCommand, services: &OwnerServices) -> Self {
        match &command.record.request.command {
            Command::Agent(_) => Self::Agent(Box::new(AgentCommandRun::start(command, services))),
            Command::Rollback(_) => Self::Rollback(Box::new(RollbackRun::start(command, services))),
            Command::Task(_) | Command::ContinueWorkflow(_) => {
                Self::Task(Box::new(TaskCommandRun::start(command, services)))
            }
            Command::Input(_) => Self::Input(InputCommandRun::start(command, services)),
            Command::CompactContext(_)
            | Command::ConfigureSession(
                ConfigurationChange::Settings(_)
                | ConfigurationChange::Permission(PermissionChange::ExecutionPolicy { .. }),
            ) => Self::NativeCommand(Box::new(NativeCommandRun::start(command, services))),
            Command::ConfigureSession(
                ConfigurationChange::Permission(PermissionChange::SetCheckEditing { .. })
                | ConfigurationChange::CheckPreferences(_)
                | ConfigurationChange::InputTimeouts(_),
            ) => Self::Configuration(command),
            Command::CancelExchange(request) => {
                let transition = ExchangeTransition::start_at(
                    command.record.request.id.clone(),
                    TransitionIntent::Cancel(request.input),
                    services,
                    None,
                    None,
                    command.admitted_at,
                );
                Self::Cancel {
                    command,
                    transition: Box::new(transition),
                }
            }
        }
    }

    /// Commit cancellation's local intent before any asynchronous acquisition can time out.
    ///
    /// Only cancellation locks the broker. Every other command starts without waiting on a
    /// busy session's broker, so local policy commits while a turn holds it.
    pub(super) async fn pin_cancellation_target(
        &mut self,
        foreground: &ForegroundStatus,
        dispatch: Option<&mut crate::broker::ForegroundDispatch>,
        broker: &tokio::sync::Mutex<crate::broker::HarnessBroker>,
        services: &OwnerServices,
    ) -> Result<
        (
            Vec<forge_protocol::message::SessionEvent>,
            Option<crate::input::InputUpdate>,
        ),
        super::transition::TransitionFailure,
    > {
        match self {
            Self::Cancel {
                command,
                transition,
            } => {
                let mut broker = broker.lock().await;
                let broker = &mut *broker;
                transition.pin_cancellation_target(
                    foreground,
                    broker,
                    services.observer.session_id(),
                )?;
                let Command::CancelExchange(request) = &command.record.request.command else {
                    unreachable!("the active cancellation retains its cancellation request")
                };
                let foreground_absent = matches!(
                    dispatch.as_deref(),
                    None | Some(crate::broker::ForegroundDispatch::Done(_))
                );
                // Mark the canonical foreground before pausing its task revokes callback authority.
                // Otherwise the same update can misclassify that intentional revocation as failure.
                if let Some(dispatch) = dispatch {
                    match dispatch {
                        crate::broker::ForegroundDispatch::Admitting(_)
                        | crate::broker::ForegroundDispatch::Running { .. }
                        | crate::broker::ForegroundDispatch::Finalizing { .. } => {
                            let action = broker
                                .step_foreground(
                                    dispatch,
                                    crate::broker::ForegroundSignal::CollectionStarted(
                                        crate::broker::ForegroundCollection::Cancel(request.input),
                                    ),
                                )
                                .map_err(super::transition::TransitionFailure::Domain)?;
                            if !matches!(action, crate::broker::ForegroundAction::Wait) {
                                return Err(super::transition::TransitionFailure::Uncertain);
                            }
                        }
                        crate::broker::ForegroundDispatch::Done(_)
                        | crate::broker::ForegroundDispatch::Transitioning => {}
                    }
                }
                broker
                    .cancel_question_delivery()
                    .map_err(super::transition::TransitionFailure::Domain)?;
                let mut input = services
                    .input
                    .suspend(true, Some(command.record.request.id.as_str()))
                    .map_err(super::transition::TransitionFailure::Domain)?;
                // Without foreground work, a restoring cancel targets the prompt that was
                // submitted but not yet claimed; only retraction keeps it from being
                // stranded behind the suspension.
                if foreground_absent
                    && request.input == super::command::CancelledInputPolicy::RestoreIfUnconsumed
                    && let Some(retracted) = services
                        .input
                        .retract_unclaimed_submission(command.record.request.id.as_str())
                        .map_err(super::transition::TransitionFailure::Domain)?
                {
                    input = retracted;
                }
                let (prior, events) = broker
                    .owner_cancel_local(None)
                    .map_err(super::transition::TransitionFailure::Domain)?;
                debug_assert!(
                    prior.is_none(),
                    "task-only cancellation has no exchange finalization"
                );
                Ok((events, Some(input)))
            }
            Self::Agent(_)
            | Self::Rollback(_)
            | Self::Task(_)
            | Self::Input(_)
            | Self::Configuration(_)
            | Self::NativeCommand(_) => Ok((Vec::new(), None)),
        }
    }

    /// Get the current command's resolver owner for idempotent observer attachment.
    pub(super) fn queued_mut(&mut self) -> &mut QueuedCommand {
        match self {
            Self::Agent(run) => &mut run.command,
            Self::Rollback(run) => &mut run.command,
            Self::Task(run) => &mut run.command,
            Self::Input(run) => &mut run.command,
            Self::Configuration(command) => command,
            Self::NativeCommand(run) => &mut run.command,
            Self::Cancel { command, .. } => command,
        }
    }

    /// Apply confirmed dispatch to the task operation awaiting its exact handoff.
    pub(super) fn observe_dispatch(
        &mut self,
        work: uuid::Uuid,
        confirmation: &forge_provider_api::submission::DispatchConfirmation,
    ) {
        match self {
            Self::Task(run) => run.observe_dispatch(work, confirmation),
            Self::Agent(_)
            | Self::Rollback(_)
            | Self::Input(_)
            | Self::Configuration(_)
            | Self::NativeCommand(_)
            | Self::Cancel { .. } => {}
        }
    }

    /// Fail the dispatch wait if its foreground ends without confirmed transport handoff.
    pub(super) fn dispatch_lost(&mut self, work: uuid::Uuid) {
        match self {
            Self::Task(run) => run.dispatch_lost(work),
            Self::Agent(_)
            | Self::Rollback(_)
            | Self::Input(_)
            | Self::Configuration(_)
            | Self::NativeCommand(_)
            | Self::Cancel { .. } => {}
        }
    }

    /// Await one fact without blocking the driver's provider event or command intake.
    pub(super) async fn next(
        &mut self,
        foreground: ForegroundStatus,
        delivery: super::task_command::TaskDeliveryGate,
    ) -> CommandEvent {
        match self {
            Self::Agent(run) => CommandEvent::Agent(run.next().await),
            Self::Rollback(run) => CommandEvent::Rollback(run.next(foreground.clone()).await),
            Self::Task(run) => CommandEvent::Task(run.next(foreground.clone(), delivery).await),
            Self::Input(run) => CommandEvent::Input(run.next().await),
            Self::Configuration(_) => CommandEvent::Configuration,
            Self::NativeCommand(run) => {
                CommandEvent::NativeCommand(run.next(foreground.clone()).await)
            }
            Self::Cancel { transition, .. } => {
                CommandEvent::Cancel(transition.next(foreground.clone()).await)
            }
        }
    }
}

/// Route an identified asynchronous result back to the matching active command stage.
pub(super) enum CommandEvent {
    /// Child coordinator completion or its nonterminal observation deadline.
    Agent(AgentCommandEvent),
    /// Rollback collection or detached file work produced one retained fact.
    Rollback(RollbackEvent),
    /// Task control has a correlated collection, commit, or input scheduling fact.
    Task(TaskCommandEvent),
    /// A local configuration command is ready for its owner-controlled commit.
    Configuration,
    /// An internal policy transition has new identified evidence.
    NativeCommand(NativeCommandEvent),
    /// Prepared input evidence must be revalidated before committing input state.
    Input(Result<PreparedInput, CommandFailure>),
    /// Collection/settlement evidence advances only the selected cancellation.
    Cancel(TransitionEvent),
}
