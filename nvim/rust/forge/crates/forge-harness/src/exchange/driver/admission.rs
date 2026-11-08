//! Decide what the exchange driver may start, from one snapshot of owner facts.
//!
//! Every admission rule lives in `Admission::evaluate`, a pure function of `AdmissionFacts`, so
//! two call sites can no longer apply drifted copies of the same rule. A copy that drifted let a
//! model change start while a turn's send was in flight, which wedged a session.

use super::*;
use crate::broker::DispatchStage;
use crate::exchange::task_command::TaskDeliveryGate;

/// Classify the next queued command for the dispatch hold.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum NextCommand {
    /// The next command cancels the exchange.
    Cancel,
    /// The next command is any other kind.
    Other,
}

/// Record the active turn's facts that admission rules read.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct TurnFacts {
    /// How far the turn's provider send has progressed.
    pub(super) dispatch: DispatchStage,
    /// Whether the driver already observed the send as lost.
    pub(super) dispatch_lost: bool,
    /// Whether the active command closes tool admission without authorizing this turn.
    pub(super) effects_closed_by_command: bool,
}

/// Snapshot the owner state that every admission rule reads.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct AdmissionFacts {
    /// The owner's execution state.
    pub(super) execution: ExecutionState,
    /// The session owner closed and is shutting down.
    pub(super) owner_closed: bool,
    /// The owner event inbox has closed.
    pub(super) inbox_closed: bool,
    /// Owner storage failed.
    pub(super) storage_failed: bool,
    /// A provider restart is requested or running.
    pub(super) restart_active: bool,
    /// The provider's observation stream has closed.
    pub(super) observation_closed: bool,
    /// A command is active.
    pub(super) command_active: bool,
    /// The active command closes tool admission.
    pub(super) command_closes_tools: bool,
    /// The next queued command, if any.
    pub(super) next_command: Option<NextCommand>,
    /// An input delivery is preparing its turn.
    pub(super) delivery_preparing: bool,
    /// An input delivery is active.
    pub(super) input_delivery_active: bool,
    /// A steering delivery is active.
    pub(super) steering_active: bool,
    /// The active turn, if any.
    pub(super) turn: Option<TurnFacts>,
    /// An admitted turn is queued.
    pub(super) turn_queued: bool,
    /// A failed turn is still being collected.
    pub(super) failed_turn_collecting: bool,
    /// A task check run is active.
    pub(super) task_verification: bool,
    /// An automatic check run is active.
    pub(super) automatic_checks_active: bool,
    /// A tool check run is active.
    pub(super) check_runs_active: bool,
    /// A background job in `RunningTasks` has not been collected yet.
    pub(super) tasks_running: bool,
    /// An automatic check's repair turn has not answered yet.
    pub(super) automatic_repair_active: bool,
    /// An automatic check run's publication stream is still open.
    pub(super) automatic_publication_active: bool,
    /// A finished task check has not published its result yet.
    pub(super) task_check_finishing: bool,
    /// Tool check requests wait for admission.
    pub(super) check_tools_queued: bool,
    /// Shutdown collection reported completion.
    pub(super) shutdown_complete: bool,
    /// Work that only shutdown waits for remains: task check output, retained rollbacks,
    /// question waits, or queued question tools.
    pub(super) shutdown_blocked: bool,
}

/// Record what the driver may start under one snapshot of owner facts.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct Admission {
    /// The next queued command may start.
    pub(super) start_command: bool,
    /// The next queued turn may start.
    pub(super) start_turn: bool,
    /// The active turn may run its ordinary effects, not only collection.
    pub(super) run_turn_effects: bool,
    /// Question tool callbacks may open.
    pub(super) open_question_tools: bool,
    /// Running automatic checks must stop.
    pub(super) cancel_automatic_checks: bool,
    /// Automatic checks may start or continue.
    pub(super) run_automatic_checks: bool,
    /// Running checks may be interrupted for direct input.
    pub(super) interrupt_checks_for_input: bool,
    /// A deferred approval timeout may start its turn.
    pub(super) start_approval_timeout: bool,
    /// A running task check must stop.
    pub(super) cancel_task_checks: bool,
    /// A requested task check run may start.
    pub(super) start_task_verification: bool,
    /// A queued check tool call may run.
    pub(super) open_check_tools: bool,
    /// A deferred configuration change may be retried.
    pub(super) retry_deferred_configuration: bool,
    /// How task input delivery is admitted.
    pub(super) task_delivery: TaskDeliveryGate,
    /// The owner should request a provider restart because its observation stream closed.
    pub(super) request_restart: bool,
    /// The owner is idle enough for a requested restart to begin.
    pub(super) restart_idle: bool,
    /// Shutdown may finish once no wake is ready.
    pub(super) finish_shutdown: bool,
}

impl Admission {
    /// Evaluate every admission rule from one snapshot.
    pub(super) fn evaluate(facts: &AdmissionFacts) -> Self {
        let ready = facts.execution == ExecutionState::Ready;
        let commands_waiting = facts.next_command.is_some();
        // A failed observation stream reports its failure to every later reader until a restart
        // replaces it, and a stop collects the provider, so commands wait for both. Commands
        // other than cancel also wait while a send is unsettled: stopping a send in flight leaves
        // its delivery unknown, and stopping an unsent turn makes the exchange restart and the
        // input queue both send the prompt again. Cancel prevents an unsent prompt and waits
        // inside the provider runtime for a send in flight, so it is exempt from that wait.
        let start_command = !facts.command_active
            && !facts.restart_active
            && !facts.observation_closed
            && facts.execution != ExecutionState::Stopping
            && !facts.delivery_preparing
            && match (facts.next_command, facts.turn) {
                (None, _) => false,
                (Some(_), None) => true,
                (Some(_), Some(turn)) if turn.dispatch_lost => false,
                (Some(next), Some(turn)) => match turn.dispatch {
                    DispatchStage::Settled => true,
                    DispatchStage::Lost => false,
                    DispatchStage::Unsent | DispatchStage::InFlight => next == NextCommand::Cancel,
                },
            };
        // Every rule that waits for background work reads this one definition, so a new kind of
        // job cannot be forgotten by one of them.
        let background_idle = !facts.tasks_running
            && !facts.check_runs_active
            && !facts.automatic_checks_active
            && !facts.automatic_repair_active
            && !facts.task_verification
            && !facts.task_check_finishing;
        let restart_idle = !facts.command_active
            && facts.turn.is_none()
            && !facts.turn_queued
            && !facts.input_delivery_active
            && !facts.steering_active
            && background_idle;
        let automatic_idle = !facts.owner_closed
            && !facts.command_active
            && !commands_waiting
            && facts.turn.is_none()
            && !facts.turn_queued
            && !facts.input_delivery_active
            && !facts.steering_active
            && !facts.task_verification
            && !facts.storage_failed
            && ready;
        Self {
            start_command,
            start_turn: facts.turn.is_none()
                && facts.turn_queued
                && !facts.failed_turn_collecting
                && !facts.storage_failed
                && ready,
            run_turn_effects: !facts.owner_closed
                && !facts.storage_failed
                && ready
                && !facts
                    .turn
                    .is_some_and(|turn| turn.effects_closed_by_command),
            open_question_tools: !facts.owner_closed
                && !facts.inbox_closed
                && !facts.storage_failed
                && !facts.restart_active
                && ready
                && !facts.command_closes_tools,
            cancel_automatic_checks: facts.command_closes_tools || !ready,
            run_automatic_checks: automatic_idle,
            interrupt_checks_for_input: !facts.owner_closed
                && !facts.command_active
                && !commands_waiting
                && !facts.storage_failed
                && ready,
            start_approval_timeout: automatic_idle
                && background_idle
                && !facts.automatic_publication_active
                && !facts.check_tools_queued,
            cancel_task_checks: facts.command_closes_tools,
            start_task_verification: !facts.task_verification
                && !facts.automatic_checks_active
                && !facts.check_runs_active
                && automatic_idle,
            open_check_tools: !facts.owner_closed
                && !facts.storage_failed
                && ready
                && !facts.command_closes_tools,
            retry_deferred_configuration: !facts.owner_closed
                && !facts.command_active
                && !commands_waiting,
            task_delivery: if facts.execution == ExecutionState::Failed {
                TaskDeliveryGate::Unavailable
            } else if ready
                && !facts.input_delivery_active
                && !facts.steering_active
                && !facts.turn_queued
                && facts.turn.is_none()
            {
                TaskDeliveryGate::Open
            } else {
                TaskDeliveryGate::Waiting
            },
            request_restart: !facts.owner_closed
                && !facts.restart_active
                && !facts.storage_failed
                && facts.execution != ExecutionState::Failed
                && facts.observation_closed,
            restart_idle,
            finish_shutdown: facts.owner_closed
                && facts.shutdown_complete
                && !facts.restart_active
                && restart_idle
                && !facts.automatic_publication_active
                && !facts.check_tools_queued
                && !facts.shutdown_blocked,
        }
    }
}

impl ExchangeDriver {
    /// Evaluate admission from the owner's current state.
    pub(super) fn admission(
        &self,
        services: &OwnerServices,
        state: &DriverState,
        closed: bool,
    ) -> Admission {
        let command = state.command.as_ref();
        let turn = state.foreground.as_ref().map(|turn| TurnFacts {
            dispatch: turn.transaction.dispatch_stage(),
            dispatch_lost: matches!(turn.handoff, TurnDispatch::Lost),
            effects_closed_by_command: command.is_some_and(|command| {
                command.closes_tool_admission() && !command.authorizes_foreground(turn.id)
            }),
        });
        let facts = AdmissionFacts {
            execution: self.execution_state,
            owner_closed: closed,
            inbox_closed: self.receiver.is_closed(),
            storage_failed: self.commands.storage_unavailable(),
            restart_active: self.observation_recovery.active(),
            observation_closed: services.observer.session_events().is_closed(),
            command_active: command.is_some(),
            command_closes_tools: command.is_some_and(ActiveCommand::closes_tool_admission),
            next_command: self.commands.next_command().map(|next| match next {
                Command::CancelExchange(_) => NextCommand::Cancel,
                _ => NextCommand::Other,
            }),
            delivery_preparing: self
                .input_delivery
                .as_ref()
                .is_some_and(InputDeliveryRun::preparing),
            input_delivery_active: self.input_delivery.is_some(),
            steering_active: !self.steering.is_empty(),
            turn,
            turn_queued: !state.pending.is_empty(),
            failed_turn_collecting: !state.tasks.failed_turns.is_empty(),
            task_verification: state.task_verification.is_some(),
            automatic_checks_active: state.automatic_checks.is_some(),
            check_runs_active: !state.check_runs.is_empty(),
            tasks_running: !state.tasks.is_idle(),
            automatic_repair_active: state.automatic_repair.is_some(),
            automatic_publication_active: state.automatic_publication.is_some(),
            task_check_finishing: state.task_check_completion.is_some(),
            check_tools_queued: !state.pending_check_tools.is_empty(),
            shutdown_complete: state
                .shutdown
                .as_ref()
                .is_some_and(crate::exchange::shutdown::OwnerShutdown::complete),
            shutdown_blocked: state.task_check_updates.is_some()
                || !state.retained_rollbacks.is_empty()
                || !self.question_waits.is_idle()
                || !self.question_tools.is_empty(),
        };
        Admission::evaluate(&facts)
    }
}

#[cfg(test)]
mod tests {
    use super::{Admission, AdmissionFacts, NextCommand, TurnFacts};
    use crate::broker::DispatchStage;
    use crate::exchange::driver::ExecutionState;
    use crate::exchange::task_command::TaskDeliveryGate;

    /// Create an idle, ready owner with nothing queued.
    fn idle() -> AdmissionFacts {
        AdmissionFacts {
            execution: ExecutionState::Ready,
            owner_closed: false,
            inbox_closed: false,
            storage_failed: false,
            restart_active: false,
            observation_closed: false,
            command_active: false,
            command_closes_tools: false,
            next_command: None,
            delivery_preparing: false,
            input_delivery_active: false,
            steering_active: false,
            turn: None,
            turn_queued: false,
            failed_turn_collecting: false,
            task_verification: false,
            automatic_checks_active: false,
            check_runs_active: false,
            tasks_running: false,
            automatic_repair_active: false,
            automatic_publication_active: false,
            task_check_finishing: false,
            check_tools_queued: false,
            shutdown_complete: false,
            shutdown_blocked: false,
        }
    }

    /// Create a turn at one dispatch stage with no command restricting its effects.
    fn turn(dispatch: DispatchStage) -> Option<TurnFacts> {
        Some(TurnFacts {
            dispatch,
            dispatch_lost: false,
            effects_closed_by_command: false,
        })
    }

    /// Hold every command except cancel while a send is unsettled, and every command once it is lost.
    #[test]
    fn command_start_follows_the_dispatch_hold() {
        let rows = [
            (DispatchStage::Unsent, NextCommand::Other, false),
            (DispatchStage::Unsent, NextCommand::Cancel, true),
            (DispatchStage::InFlight, NextCommand::Other, false),
            (DispatchStage::InFlight, NextCommand::Cancel, true),
            (DispatchStage::Lost, NextCommand::Other, false),
            (DispatchStage::Lost, NextCommand::Cancel, false),
            (DispatchStage::Settled, NextCommand::Other, true),
            (DispatchStage::Settled, NextCommand::Cancel, true),
        ];
        for (dispatch, next, expected) in rows {
            let facts = AdmissionFacts {
                next_command: Some(next),
                turn: turn(dispatch),
                ..idle()
            };
            assert_eq!(
                Admission::evaluate(&facts).start_command,
                expected,
                "{dispatch:?} with {next:?}"
            );
        }
    }

    /// Hold commands while the provider is stopping, restarting, or its stream has failed.
    #[test]
    fn command_start_waits_for_stops_and_restarts() {
        let waiting = AdmissionFacts {
            next_command: Some(NextCommand::Cancel),
            ..idle()
        };
        assert!(Admission::evaluate(&waiting).start_command);
        let rows = [
            AdmissionFacts {
                execution: ExecutionState::Stopping,
                ..waiting
            },
            AdmissionFacts {
                restart_active: true,
                ..waiting
            },
            AdmissionFacts {
                observation_closed: true,
                ..waiting
            },
            AdmissionFacts {
                command_active: true,
                ..waiting
            },
            AdmissionFacts {
                delivery_preparing: true,
                ..waiting
            },
            AdmissionFacts {
                turn: Some(TurnFacts {
                    dispatch_lost: true,
                    ..turn(DispatchStage::Settled).unwrap()
                }),
                ..waiting
            },
        ];
        for facts in rows {
            assert!(!Admission::evaluate(&facts).start_command, "{facts:?}");
        }
        // A failed owner still starts cancel, so it fails immediately instead of waiting.
        let failed = AdmissionFacts {
            execution: ExecutionState::Failed,
            ..waiting
        };
        assert!(Admission::evaluate(&failed).start_command);
    }

    /// Start turns and their effects only while execution is ready.
    #[test]
    fn turns_start_and_run_only_when_ready() {
        let queued = AdmissionFacts {
            turn_queued: true,
            ..idle()
        };
        assert!(Admission::evaluate(&queued).start_turn);
        for execution in [
            ExecutionState::Stopping,
            ExecutionState::Restarting,
            ExecutionState::Failed,
        ] {
            let facts = AdmissionFacts {
                execution,
                ..queued
            };
            let admission = Admission::evaluate(&facts);
            assert!(!admission.start_turn, "{execution:?}");
            assert!(!admission.run_turn_effects, "{execution:?}");
        }
        let restricted = AdmissionFacts {
            turn: Some(TurnFacts {
                effects_closed_by_command: true,
                ..turn(DispatchStage::Settled).unwrap()
            }),
            ..idle()
        };
        assert!(!Admission::evaluate(&restricted).run_turn_effects);
    }

    /// Request a restart once the stream closes, and begin it only when the owner is idle.
    #[test]
    fn restart_requests_and_waits_for_idle() {
        let closed_stream = AdmissionFacts {
            observation_closed: true,
            ..idle()
        };
        let admission = Admission::evaluate(&closed_stream);
        assert!(admission.request_restart);
        assert!(admission.restart_idle);
        let failed = AdmissionFacts {
            execution: ExecutionState::Failed,
            ..closed_stream
        };
        assert!(!Admission::evaluate(&failed).request_restart);
        let busy = AdmissionFacts {
            tasks_running: true,
            ..closed_stream
        };
        assert!(!Admission::evaluate(&busy).restart_idle);
    }

    /// Open task delivery only for an idle ready owner, and report a failed owner as unavailable.
    #[test]
    fn task_delivery_follows_execution_state() {
        assert_eq!(
            Admission::evaluate(&idle()).task_delivery,
            TaskDeliveryGate::Open
        );
        let busy = AdmissionFacts {
            turn: turn(DispatchStage::Settled),
            ..idle()
        };
        assert_eq!(
            Admission::evaluate(&busy).task_delivery,
            TaskDeliveryGate::Waiting
        );
        let failed = AdmissionFacts {
            execution: ExecutionState::Failed,
            ..idle()
        };
        assert_eq!(
            Admission::evaluate(&failed).task_delivery,
            TaskDeliveryGate::Unavailable
        );
    }

    /// Finish shutdown only after its collection completes and no owned work remains.
    #[test]
    fn shutdown_finishes_only_when_idle() {
        let complete = AdmissionFacts {
            owner_closed: true,
            shutdown_complete: true,
            ..idle()
        };
        assert!(Admission::evaluate(&complete).finish_shutdown);
        for facts in [
            AdmissionFacts {
                shutdown_blocked: true,
                ..complete
            },
            AdmissionFacts {
                restart_active: true,
                ..complete
            },
            AdmissionFacts {
                turn_queued: true,
                ..complete
            },
            AdmissionFacts {
                shutdown_complete: false,
                ..complete
            },
        ] {
            assert!(!Admission::evaluate(&facts).finish_shutdown, "{facts:?}");
        }
    }
}
