use std::{
    collections::VecDeque,
    future::Future,
    sync::{Arc, Mutex},
};
use tokio::{
    sync::{Mutex as AsyncMutex, oneshot},
    task::JoinHandle,
};

const MAX_RETAINED: usize = 64;

/// Share an original typed failure between its waiter and retained task owner.
#[derive(Clone, Debug)]
pub(crate) struct RetainedCleanupFailure(Arc<anyhow::Error>);

impl std::fmt::Display for RetainedCleanupFailure {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.0.as_ref(), formatter)
    }
}

impl std::error::Error for RetainedCleanupFailure {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(self.0.as_ref().as_ref())
    }
}

struct RetainedTask {
    handle: AsyncMutex<Option<JoinHandle<Result<(), RetainedCleanupFailure>>>>,
}

#[derive(Default)]
struct LedgerState {
    tasks: VecDeque<Arc<RetainedTask>>,
    sealed: bool,
}

/// Own provider cleanup and write tasks until their result is collected.
#[derive(Default)]
pub(super) struct CleanupLedger {
    state: Mutex<LedgerState>,
    failure: Mutex<Option<RetainedCleanupFailure>>,
    collecting: AsyncMutex<()>,
}

impl CleanupLedger {
    pub(super) fn start<F>(
        &self,
        future: F,
    ) -> anyhow::Result<oneshot::Receiver<Result<(), RetainedCleanupFailure>>>
    where
        F: Future<Output = anyhow::Result<()>> + Send + 'static,
    {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("configuration cleanup ledger poisoned"))?;
        anyhow::ensure!(
            !state.sealed && state.tasks.len() < MAX_RETAINED,
            "configuration cleanup ledger is full"
        );
        let runtime = tokio::runtime::Handle::try_current().map_err(|error| {
            anyhow::anyhow!("configuration cleanup runtime unavailable: {error}")
        })?;
        let (sender, receiver) = oneshot::channel();
        let handle = runtime.spawn(async move {
            let result = future
                .await
                .map_err(|error| RetainedCleanupFailure(Arc::new(error)));
            let _ = sender.send(result.clone());
            result
        });
        state.tasks.push_back(Arc::new(RetainedTask {
            handle: AsyncMutex::new(Some(handle)),
        }));
        Ok(receiver)
    }

    /// Forbid new work atomically with task admission before shutdown collection.
    pub(super) fn seal(&self) -> anyhow::Result<()> {
        self.state
            .lock()
            .map_err(|_| anyhow::anyhow!("configuration cleanup ledger poisoned"))?
            .sealed = true;
        Ok(())
    }

    /// Preserve a registration error that could not be returned from a drop guard.
    pub(super) fn record_failure(&self, error: anyhow::Error) {
        self.record_retained_failure(RetainedCleanupFailure(Arc::new(error)));
    }

    fn record_retained_failure(&self, error: RetainedCleanupFailure) {
        let mut failure = self
            .failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if failure.is_none() {
            *failure = Some(error);
        }
    }

    /// Join each task without removing ownership if this collection is cancelled.
    pub(super) async fn collect(&self) -> anyhow::Result<()> {
        let _collecting = self.collecting.lock().await;
        loop {
            let task = self
                .state
                .lock()
                .map_err(|_| anyhow::anyhow!("configuration cleanup ledger poisoned"))?
                .tasks
                .front()
                .cloned();
            let Some(task) = task else {
                break;
            };
            let mut handle = task.handle.lock().await;
            if let Some(join) = handle.as_mut() {
                match join.await {
                    Ok(Ok(())) => {}
                    Ok(Err(error)) => self.record_retained_failure(error),
                    Err(error) => self.record_failure(
                        anyhow::Error::new(error).context("configuration cleanup task failed"),
                    ),
                }
                *handle = None;
            }
            self.state
                .lock()
                .map_err(|_| anyhow::anyhow!("configuration cleanup ledger poisoned"))?
                .tasks
                .pop_front();
        }
        if let Some(error) = self
            .failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take()
        {
            return Err(anyhow::Error::new(error).context("configuration cleanup failed"));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn abandoned_waiter_keeps_work_until_collection() -> anyhow::Result<()> {
        let ledger = Arc::new(CleanupLedger::default());
        let (release, wait) = oneshot::channel::<()>();
        let receiver = ledger.start(async move {
            wait.await?;
            Ok(())
        })?;
        drop(receiver);
        let collector = {
            let ledger = Arc::clone(&ledger);
            tokio::spawn(async move { ledger.collect().await })
        };
        tokio::task::yield_now().await;
        collector.abort();
        let _ = collector.await;
        release
            .send(())
            .map_err(|_| anyhow::anyhow!("retained work was dropped"))?;
        ledger.collect().await?;
        Ok(())
    }

    #[tokio::test]
    async fn cleanup_failure_is_reported_after_waiter_is_dropped() -> anyhow::Result<()> {
        let ledger = CleanupLedger::default();
        let receiver = ledger.start(async {
            Err(crate::provider::ProviderError::Rejected {
                message: "native fence failed".into(),
            }
            .into())
        })?;
        drop(receiver);
        let error = ledger.collect().await.unwrap_err();
        assert!(error.chain().any(|cause| matches!(cause.downcast_ref::<crate::provider::ProviderError>(), Some(crate::provider::ProviderError::Rejected { message }) if message == "native fence failed")));
        Ok(())
    }

    #[tokio::test]
    async fn poisoned_failure_lock_still_retains_typed_error() -> anyhow::Result<()> {
        let ledger = Arc::new(CleanupLedger::default());
        let thread_ledger = Arc::clone(&ledger);
        let thread = std::thread::spawn(move || {
            let _guard = thread_ledger.failure.lock().unwrap();
            panic!("poison failure lock");
        });
        assert!(thread.join().is_err());
        ledger.record_failure(crate::provider::ProviderError::Cancelled.into());
        let error = ledger.collect().await.unwrap_err();
        assert!(error.chain().any(|cause| matches!(
            cause.downcast_ref::<crate::provider::ProviderError>(),
            Some(crate::provider::ProviderError::Cancelled)
        )));
        Ok(())
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn concurrent_start_and_seal_cannot_escape_collection() -> anyhow::Result<()> {
        let ledger = Arc::new(CleanupLedger::default());
        let barrier = Arc::new(tokio::sync::Barrier::new(3));
        let ran = Arc::new(std::sync::atomic::AtomicBool::new(false));
        let starter = {
            let ledger = Arc::clone(&ledger);
            let barrier = Arc::clone(&barrier);
            let ran = Arc::clone(&ran);
            tokio::spawn(async move {
                barrier.wait().await;
                ledger
                    .start(async move {
                        ran.store(true, std::sync::atomic::Ordering::Release);
                        Ok(())
                    })
                    .is_ok()
            })
        };
        let sealer = {
            let ledger = Arc::clone(&ledger);
            let barrier = Arc::clone(&barrier);
            tokio::spawn(async move {
                barrier.wait().await;
                ledger.seal()
            })
        };
        barrier.wait().await;
        let accepted = starter.await?;
        sealer.await??;
        ledger.collect().await?;
        assert_eq!(ran.load(std::sync::atomic::Ordering::Acquire), accepted);
        assert!(ledger.start(async { Ok(()) }).is_err());
        Ok(())
    }
}
