//! Own the driver's execution state and the shared admission fence that mirrors it.

use super::*;

/// Record whether the owner may start provider execution.
///
/// Only the transition methods on `ExchangeDriver` change this state, and each one updates
/// `OwnerServices::execution` in the same call, so the owner's state and the admission fence that
/// other session components read can never disagree.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum ExecutionState {
    /// Execution may start.
    Ready,
    /// A stop is collecting native and host work; execution reopens when it proves the provider
    /// idle, or the provider restarts when it cannot.
    Stopping,
    /// The provider is restarting after a failure left its state unknown.
    Restarting,
    /// A restart or owner storage failed; execution never reopens for this session.
    Failed,
}

impl ExchangeDriver {
    /// Pause execution admission while a stop collects its work.
    ///
    /// Only a ready owner starts stopping: a restart already holds admission, and a failed owner
    /// never reopens.
    pub(super) fn execution_stop_begin(&mut self, services: &OwnerServices) {
        services.execution.fence_collection_pending();
        if self.execution_state == ExecutionState::Ready {
            self.execution_state = ExecutionState::Stopping;
        }
    }

    /// Pause execution admission while the provider restarts.
    pub(super) fn execution_restart_begin(&mut self, services: &OwnerServices) {
        services.execution.fence_collection_pending();
        if self.execution_state != ExecutionState::Failed {
            self.execution_state = ExecutionState::Restarting;
        }
    }

    /// Resume execution after a stop proved the provider idle or a restart succeeded.
    pub(super) fn execution_reopen(&mut self, services: &OwnerServices) {
        if self.execution_state == ExecutionState::Failed {
            return;
        }
        self.execution_state = ExecutionState::Ready;
        services.execution.lift_collection_pending();
        services.input.changed.notify_one();
    }

    /// Close execution for the session's lifetime.
    pub(super) fn execution_fail(&mut self, services: &OwnerServices) {
        services.execution.close();
        self.execution_state = ExecutionState::Failed;
    }
}
