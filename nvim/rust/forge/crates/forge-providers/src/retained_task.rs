//! Retain a native worker until its completion has been joined.

use forge_provider_api::{ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation};
use tokio::{sync::Mutex, task::JoinHandle};

/// Keep the original native worker owned across cancelled and repeated collection waits.
pub(crate) struct RetainedTask<T = ()> {
    state: Mutex<TaskState<T>>,
    operation: ProviderOperation,
}

enum TaskState<T> {
    Running(JoinHandle<Result<T, ProviderError>>),
    Collected(Result<T, ProviderError>),
}

impl<T> RetainedTask<T> {
    /// Create the sole retained owner of a spawned native worker.
    pub(crate) fn new(
        operation: ProviderOperation,
        task: JoinHandle<Result<T, ProviderError>>,
    ) -> Self {
        Self {
            state: Mutex::new(TaskState::Running(task)),
            operation,
        }
    }

    /// Check completion without taking ownership from an active collector.
    pub(crate) fn is_finished(&self) -> bool {
        self.state.try_lock().is_ok_and(|state| match &*state {
            TaskState::Running(task) => task.is_finished(),
            TaskState::Collected(_) => true,
        })
    }

    /// Join the same worker across cancelled waiters and retain its normalized result.
    pub(crate) async fn join(&self) -> Result<T, ProviderError>
    where
        T: Clone,
    {
        let mut state = self.state.lock().await;
        let result = match &mut *state {
            TaskState::Running(task) => task.await.unwrap_or_else(|_| {
                Err(ProviderError::Failure(
                    ProviderFailure::new(self.operation, ProviderFailureKind::Runtime).at_phase(
                        forge_provider_api::ProviderPhase::Collection,
                        forge_provider_api::FailureDisposition::Invalidated,
                    ),
                ))
            }),
            TaskState::Collected(result) => return result.clone(),
        };
        *state = TaskState::Collected(result.clone());
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Keep a typed native failure unchanged across repeated collection.
    #[tokio::test]
    async fn native_failure_survives_repeated_collection() {
        let task: RetainedTask = RetainedTask::new(
            ProviderOperation::TurnStart,
            tokio::spawn(async { Err(ProviderError::DeliveryUncertain) }),
        );
        for _ in 0..2 {
            assert!(matches!(
                task.join().await,
                Err(ProviderError::DeliveryUncertain)
            ));
        }
    }
}
