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

/// Identify the owned obligation independently of error text.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum CleanupWorkKind {
    ConfigurationWrite,
    OwnerFence,
    CheckpointCollection,
}

/// Preserve the concrete result produced by an owned cleanup operation.
#[derive(Debug)]
pub(crate) enum CleanupWorkFailure {
    Provider(crate::provider::ProviderError),
    Checkpoint(crate::checkpoint::CheckpointFailure),
}

impl From<crate::provider::ProviderError> for CleanupWorkFailure {
    /// Preserve provider recovery classification while transferring retained work.
    fn from(error: crate::provider::ProviderError) -> Self {
        Self::Provider(error)
    }
}

impl std::fmt::Display for CleanupWorkFailure {
    /// Describe a retained worker's original cause.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Provider(error) => std::fmt::Display::fmt(error, f),
            Self::Checkpoint(error) => std::fmt::Display::fmt(error, f),
        }
    }
}
impl std::error::Error for CleanupWorkFailure {
    /// Retain the original provider or checkpoint failure.
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Provider(error) => Some(error),
            Self::Checkpoint(error) => Some(error),
        }
    }
}

/// Classify task termination without retaining a panic payload or worker internals.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum CleanupJoinFailure {
    Cancelled,
    Panicked,
}

impl From<tokio::task::JoinError> for CleanupJoinFailure {
    /// Normalize the terminal task result before it reaches retained diagnostics.
    fn from(error: tokio::task::JoinError) -> Self {
        if error.is_cancelled() {
            Self::Cancelled
        } else {
            Self::Panicked
        }
    }
}

impl std::fmt::Display for CleanupJoinFailure {
    /// Render the normalized task exit without the original panic text.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            Self::Cancelled => "cleanup worker was cancelled",
            Self::Panicked => "cleanup worker panicked",
        })
    }
}

impl std::error::Error for CleanupJoinFailure {}

/// Distinguish admission, worker, and collection failures without erased engine errors.
#[derive(Debug)]
pub(crate) enum CleanupError {
    Sealed,
    CapacityExceeded {
        limit: usize,
    },
    Poisoned,
    RuntimeUnavailable(tokio::runtime::TryCurrentError),
    Work {
        kind: CleanupWorkKind,
        source: CleanupWorkFailure,
    },
    Join {
        kind: CleanupWorkKind,
        failure: CleanupJoinFailure,
    },
}
impl std::fmt::Display for CleanupError {
    /// Render the concrete failed phase at a host reporting boundary.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Sealed => f.write_str("retained cleanup admission is sealed"),
            Self::CapacityExceeded { limit } => {
                write!(f, "retained cleanup capacity {limit} exceeded")
            }
            Self::Poisoned => f.write_str("retained cleanup state poisoned"),
            Self::RuntimeUnavailable(source) => {
                write!(f, "retained cleanup runtime unavailable: {source}")
            }
            Self::Work { kind, source } => write!(f, "retained {kind:?} failed: {source}"),
            Self::Join { kind, failure } => {
                write!(f, "retained {kind:?} worker did not complete: {failure}")
            }
        }
    }
}
impl std::error::Error for CleanupError {
    /// Expose every concrete worker and admission source.
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::RuntimeUnavailable(error) => Some(error),
            Self::Work { source, .. } => Some(source),
            Self::Join { failure, .. } => Some(failure),
            Self::Sealed | Self::CapacityExceeded { .. } | Self::Poisoned => None,
        }
    }
}

/// Share distinct typed failures between waiters and the retained task owner.
#[derive(Clone, Debug)]
pub(crate) struct RetainedCleanupFailure {
    primary: Arc<CleanupError>,
    additional: Vec<Arc<CleanupError>>,
}
impl RetainedCleanupFailure {
    /// Get the primary failure followed by independent cleanup failures.
    pub(crate) fn failures(&self) -> impl Iterator<Item = &CleanupError> {
        std::iter::once(self.primary.as_ref()).chain(self.additional.iter().map(Arc::as_ref))
    }

    /// Append shared causes once without nesting an earlier aggregate.
    fn append(&mut self, failure: Self) {
        for error in std::iter::once(failure.primary).chain(failure.additional) {
            if !Arc::ptr_eq(&self.primary, &error)
                && !self
                    .additional
                    .iter()
                    .any(|known| Arc::ptr_eq(known, &error))
            {
                self.additional.push(error);
            }
        }
    }
}
impl From<CleanupError> for RetainedCleanupFailure {
    /// Share a failed obligation without flattening its source chain.
    fn from(error: CleanupError) -> Self {
        Self {
            primary: Arc::new(error),
            additional: Vec::new(),
        }
    }
}
impl std::fmt::Display for RetainedCleanupFailure {
    /// Describe each retained failure once at a host reporting boundary.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (index, error) in self.failures().enumerate() {
            if index != 0 {
                f.write_str("; ")?;
            }
            std::fmt::Display::fmt(error, f)?;
        }
        Ok(())
    }
}
impl std::error::Error for RetainedCleanupFailure {
    /// Get the original primary obligation while retaining all causes for inspection.
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(self.primary.as_ref())
    }
}

struct RetainedTask {
    kind: CleanupWorkKind,
    handle: AsyncMutex<Option<JoinHandle<Result<(), RetainedCleanupFailure>>>>,
}
#[derive(Default)]
struct LedgerState {
    tasks: VecDeque<Arc<RetainedTask>>,
    sealed: bool,
}
/// Own cleanup and write tasks until their typed result is collected.
#[derive(Default)]
pub(super) struct CleanupLedger {
    state: Mutex<LedgerState>,
    failure: Mutex<Option<RetainedCleanupFailure>>,
    collecting: AsyncMutex<()>,
}
impl CleanupLedger {
    /// Retain a typed obligation before exposing its optional waiter.
    pub(super) fn start<F>(
        &self,
        kind: CleanupWorkKind,
        future: F,
    ) -> Result<oneshot::Receiver<Result<(), RetainedCleanupFailure>>, CleanupError>
    where
        F: Future<Output = Result<(), CleanupWorkFailure>> + Send + 'static,
    {
        let mut state = self.state.lock().map_err(|_| CleanupError::Poisoned)?;
        if state.sealed {
            return Err(CleanupError::Sealed);
        }
        if state.tasks.len() >= MAX_RETAINED {
            return Err(CleanupError::CapacityExceeded {
                limit: MAX_RETAINED,
            });
        }
        let runtime =
            tokio::runtime::Handle::try_current().map_err(CleanupError::RuntimeUnavailable)?;
        let (sender, receiver) = oneshot::channel();
        let handle = runtime.spawn(async move {
            let result = future.await.map_err(|source| {
                RetainedCleanupFailure::from(CleanupError::Work { kind, source })
            });
            let _ = sender.send(result.clone());
            result
        });
        state.tasks.push_back(Arc::new(RetainedTask {
            kind,
            handle: AsyncMutex::new(Some(handle)),
        }));
        Ok(receiver)
    }
    /// Forbid new work atomically with task admission before shutdown collection.
    pub(super) fn seal(&self) -> Result<(), CleanupError> {
        self.state
            .lock()
            .map_err(|_| CleanupError::Poisoned)?
            .sealed = true;
        Ok(())
    }
    /// Preserve registration failures that could not be returned by a drop guard.
    pub(super) fn record_failure(&self, error: CleanupError) {
        self.record_retained_failure(error.into());
    }
    /// Accumulate independent failures even when the failure lock was poisoned.
    fn record_retained_failure(&self, error: RetainedCleanupFailure) {
        let mut failure = self
            .failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match failure.as_mut() {
            Some(failure) => failure.append(error),
            None => *failure = Some(error),
        }
    }
    /// Join each task without removing ownership if this collector is cancelled.
    pub(super) async fn collect(&self) -> Result<(), RetainedCleanupFailure> {
        let _collecting = self.collecting.lock().await;
        loop {
            let task = self
                .state
                .lock()
                .map_err(|_| RetainedCleanupFailure::from(CleanupError::Poisoned))?
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
                    Err(source) => self.record_failure(CleanupError::Join {
                        kind: task.kind,
                        failure: source.into(),
                    }),
                }
                *handle = None;
            }
            self.state
                .lock()
                .map_err(|_| RetainedCleanupFailure::from(CleanupError::Poisoned))?
                .tasks
                .pop_front();
        }
        if let Some(error) = self
            .failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take()
        {
            return Err(error);
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
        let receiver = ledger.start(CleanupWorkKind::OwnerFence, async move {
            wait.await.map_err(|_| {
                CleanupWorkFailure::Provider(crate::provider::ProviderError::Cancelled)
            })?;
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
                    .start(CleanupWorkKind::OwnerFence, async move {
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
        assert!(
            ledger
                .start(CleanupWorkKind::OwnerFence, async { Ok(()) })
                .is_err()
        );
        Ok(())
    }
}
