//! Retain a native worker until its completion has been joined.

use crate::{ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation};
use tokio::{sync::Mutex, task::JoinHandle};

/// Keep the original native worker owned across cancelled and repeated collection waits.
pub struct RetainedTask<T = ()> {
    state: Mutex<TaskState<T>>,
    operation: ProviderOperation,
}

enum TaskState<T> {
    Running(JoinHandle<Result<T, ProviderError>>),
    Collected(Result<T, ProviderError>),
}

impl<T> RetainedTask<T> {
    /// Create the sole retained owner of a spawned native worker.
    pub fn new(operation: ProviderOperation, task: JoinHandle<Result<T, ProviderError>>) -> Self {
        Self {
            state: Mutex::new(TaskState::Running(task)),
            operation,
        }
    }

    /// Check completion without taking ownership from an active collector.
    pub fn is_finished(&self) -> bool {
        self.state.try_lock().is_ok_and(|state| match &*state {
            TaskState::Running(task) => task.is_finished(),
            TaskState::Collected(_) => true,
        })
    }

    /// Join the same worker across cancelled waiters and retain its normalized result.
    pub async fn join(&self) -> Result<T, ProviderError>
    where
        T: Clone,
    {
        let mut state = self.state.lock().await;
        let result = match &mut *state {
            TaskState::Running(task) => task.await.unwrap_or_else(|_| {
                Err(ProviderError::Failure(
                    ProviderFailure::new(self.operation, ProviderFailureKind::Runtime).at_phase(
                        crate::ProviderPhase::Collection,
                        crate::FailureDisposition::Invalidated,
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

    /// Preserve the worker when a pending collection future is dropped.
    #[tokio::test]
    async fn cancelled_join_keeps_the_original_prompt_worker() {
        let (release, wait) = tokio::sync::oneshot::channel();
        let task = RetainedTask::new(
            ProviderOperation::TurnStart,
            tokio::spawn(async move {
                let _ = wait.await;
                Ok(())
            }),
        );
        let mut collecting = Box::pin(task.join());
        assert!(futures_util::poll!(collecting.as_mut()).is_pending());
        drop(collecting);
        assert!(!task.is_finished());
        assert!(
            tokio::time::timeout(std::time::Duration::from_millis(1), task.join())
                .await
                .is_err()
        );
        assert!(!task.is_finished());
        release.send(()).unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(2), task.join())
            .await
            .unwrap()
            .unwrap();
        assert!(task.is_finished());
        task.join().await.unwrap();
    }

    /// A cancelled result waiter cannot replace the native outcome retained by the owner.
    #[tokio::test]
    async fn native_outcome_survives_cancelled_and_repeated_collection() {
        let (release, wait) = tokio::sync::oneshot::channel();
        let task = RetainedTask::new(
            ProviderOperation::TurnStart,
            tokio::spawn(async move {
                wait.await.unwrap();
                Ok(crate::turn::TurnOutcome::Completed {
                    stop_reason: crate::turn::TurnCompletionReason::Refusal,
                })
            }),
        );
        let mut collecting = Box::pin(task.join());
        assert!(futures_util::poll!(collecting.as_mut()).is_pending());
        drop(collecting);
        release.send(()).unwrap();
        for _ in 0..2 {
            assert_eq!(
                task.join().await.unwrap(),
                crate::turn::TurnOutcome::Completed {
                    stop_reason: crate::turn::TurnCompletionReason::Refusal,
                }
            );
        }
    }

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

    /// Report a panicked worker without exposing its native panic text.
    #[tokio::test]
    async fn worker_panic_is_normalized_and_retained() {
        let task: RetainedTask = RetainedTask::new(
            ProviderOperation::TurnStart,
            tokio::spawn(async {
                panic!("private prompt panic marker");
            }),
        );
        for _ in 0..2 {
            let error = task.join().await.unwrap_err();
            assert!(matches!(&error, ProviderError::Failure(failure)
                if failure.kind() == ProviderFailureKind::Runtime));
            assert!(!format!("{error:?} {error}").contains("private prompt panic marker"));
            assert!(
                std::error::Error::source(&error)
                    .unwrap()
                    .is::<ProviderFailure>()
            );
        }
    }
}
