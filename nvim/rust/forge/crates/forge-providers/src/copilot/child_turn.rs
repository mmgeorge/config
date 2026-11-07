//! Retain native child response cycles and their own callback collection independently.

use std::sync::Arc;

use async_trait::async_trait;
use forge_provider_api::{
    NativeTurnStart, ProviderError, ProviderEventSink, TurnAddress, TurnResult, turn::TurnOutcome,
};
use tokio::sync::{Mutex, Notify, RwLock, watch};
use tokio_util::sync::CancellationToken;

/// Own callback revocation and collection for one original child invocation.
#[derive(Clone)]
pub(super) struct ChildResources {
    pub(super) revoked: CancellationToken,
    pub(super) callbacks: Arc<RwLock<()>>,
    observation_lost: CancellationToken,
}

impl ChildResources {
    /// Create independent observation and callback lifetimes for one native child.
    pub(super) fn new() -> Self {
        Self {
            revoked: CancellationToken::new(),
            callbacks: Arc::new(RwLock::new(())),
            observation_lost: CancellationToken::new(),
        }
    }

    /// Revoke future callbacks and join every callback already admitted to this child.
    pub(super) async fn collect(&self) {
        self.revoked.cancel();
        let _collected = self.callbacks.write().await;
    }
}

/// Keep the child's sole shared lifecycle worker alive after its parent returns.
pub(super) struct CopilotChildTurn {
    driver: Arc<ChildDriver>,
    start: watch::Receiver<Option<NativeTurnStart>>,
    runner: crate::turn_runner::RetainedTurn,
}

impl CopilotChildTurn {
    /// Create an observed driver only after native ancestry and processing were established.
    pub(super) fn new(
        sink: ProviderEventSink,
        address: TurnAddress,
        resources: ChildResources,
        callbacks: Option<Arc<dyn super::provider::NativeCallbackRouter>>,
    ) -> Result<Self, ProviderError> {
        let (start, receiver) = watch::channel(None);
        let driver = Arc::new(ChildDriver {
            start,
            terminal: Mutex::new(None),
            ended: Notify::new(),
            resources,
            callbacks,
            address: address.clone(),
        });
        let runner =
            crate::turn_runner::RetainedTurn::spawn_observed(driver.clone(), sink, address)?;
        Ok(Self {
            driver,
            start: receiver,
            runner,
        })
    }

    /// Publish positive native processing before any child item activity.
    pub(super) async fn observe(&self, observed_at_ms: i64) -> Result<(), ProviderError> {
        let mut start = self.start.clone();
        loop {
            let current = start.borrow().clone();
            if let Some(current) = current {
                return current.observed(observed_at_ms).await;
            }
            start.changed().await.map_err(|_| {
                ProviderError::Observation(
                    forge_provider_api::ObservationFailure::LifecycleUnavailable,
                )
            })?;
        }
    }

    /// Retain exact terminal evidence without waiting for independent child callback collection.
    pub(super) async fn complete(
        &self,
        outcome: TurnOutcome,
        completed_at_ms: i64,
    ) -> Result<(), ProviderError> {
        let result = TurnResult::new(outcome, completed_at_ms, None, Vec::new(), Vec::new(), None)
            .map_err(|_| {
                ProviderError::Observation(forge_provider_api::ObservationFailure::Protocol)
            })?;
        let mut terminal = self.driver.terminal.lock().await;
        if let Some(previous) = terminal.as_ref() {
            return match previous {
                Ok(previous) if previous.outcome() == result.outcome() => Ok(()),
                Ok(_) | Err(_) => Err(ProviderError::Observation(
                    forge_provider_api::ObservationFailure::OwnershipConflict,
                )),
            };
        }
        *terminal = Some(Ok(result));
        drop(terminal);
        self.driver.ended.notify_one();
        Ok(())
    }

    /// Revoke lost child observation and await its original retained callback collection.
    pub(super) async fn close(&self) {
        self.driver.resources.revoked.cancel();
        self.driver.resources.observation_lost.cancel();
        let _collected = self.runner.join().await;
    }
}

impl Drop for CopilotChildTurn {
    /// Fail lost observation instead of manufacturing a terminal child result.
    fn drop(&mut self) {
        self.driver.resources.observation_lost.cancel();
    }
}

struct ChildDriver {
    start: watch::Sender<Option<NativeTurnStart>>,
    terminal: Mutex<Option<Result<TurnResult, ProviderError>>>,
    ended: Notify,
    resources: ChildResources,
    callbacks: Option<Arc<dyn super::provider::NativeCallbackRouter>>,
    address: TurnAddress,
}

#[async_trait]
impl crate::turn_runner::NativeTurnDriver for ChildDriver {
    /// Wait for this child's original native terminal observation or binding loss.
    async fn run(&self, start: NativeTurnStart) -> Result<TurnResult, ProviderError> {
        self.start.send_replace(Some(start));
        loop {
            let ended = self.ended.notified();
            tokio::pin!(ended);
            ended.as_mut().enable();
            if let Some(result) = self.terminal.lock().await.clone() {
                return result;
            }
            tokio::select! {
                _ = ended => {},
                _ = self.resources.observation_lost.cancelled() => {
                    if let Some(result) = self.terminal.lock().await.clone() { return result; }
                    return Err(ProviderError::Observation(forge_provider_api::ObservationFailure::Disconnected));
                }
            }
        }
    }

    /// Cancel only this independently owned child observation.
    async fn interrupt(&self) -> Result<(), ProviderError> {
        self.resources.observation_lost.cancel();
        Ok(())
    }

    /// Collect only callbacks retained under this original child's authority.
    async fn collect(&self) -> Result<(), ProviderError> {
        self.resources.collect().await;
        if let Some(callbacks) = &self.callbacks {
            callbacks.collect_native_child(&self.address).await?;
        }
        Ok(())
    }
}
