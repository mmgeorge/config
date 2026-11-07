//! Own observed native child work independently of its parent's foreground prompt.

use async_trait::async_trait;
use forge_provider_api::{
    NativeTurnStart, ProviderError, ProviderEventSink, TurnAddress, TurnResult, turn::TurnOutcome,
};
use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};
use tokio::sync::{Mutex, Notify, RwLock, watch};
use tokio_util::sync::CancellationToken;

/// Retain callback resources whose collection belongs to one native child only.
#[derive(Clone)]
pub(super) struct ChildResources {
    revocation: CancellationToken,
    callbacks: Arc<RwLock<()>>,
}

impl ChildResources {
    /// Create independent callback admission and collection for one trusted native child.
    pub(super) fn new() -> Self {
        Self {
            revocation: CancellationToken::new(),
            callbacks: Arc::new(RwLock::new(())),
        }
    }
    /// Get the child's independent callback revocation signal.
    pub(super) fn revocation(&self) -> CancellationToken {
        self.revocation.clone()
    }
    /// Get the internal child callback collection barrier.
    pub(super) fn callbacks(&self) -> Arc<RwLock<()>> {
        self.callbacks.clone()
    }
    /// Revoke callbacks before waiting for their original work to finish.
    async fn collect(&self) {
        self.revocation.cancel();
        let _collected = self.callbacks.write().await;
    }
}

/// Retain native child terminal observation and its sole shared lifecycle runner.
pub(super) struct AcpChildTurn {
    driver: Arc<ChildDriver>,
    start: watch::Receiver<Option<NativeTurnStart>>,
    observed: AtomicBool,
    relationship: watch::Sender<bool>,
    runner: crate::turn_runner::RetainedTurn,
}

impl AcpChildTurn {
    /// Register trusted child ownership without claiming that metadata alone started processing.
    pub(super) fn new(
        sink: ProviderEventSink,
        address: TurnAddress,
        resources: ChildResources,
    ) -> Result<Self, ProviderError> {
        let (start, receiver) = watch::channel(None);
        let driver = Arc::new(ChildDriver {
            start,
            terminal: Mutex::new(None),
            ended: Notify::new(),
            resources,
        });
        let runner =
            crate::turn_runner::RetainedTurn::spawn_observed(driver.clone(), sink, address)?;
        let (relationship, _) = watch::channel(false);
        Ok(Self {
            driver,
            start: receiver,
            observed: AtomicBool::new(false),
            relationship,
            runner,
        })
    }
    /// Publish the child's first positively attributed processing observation once.
    pub(super) async fn observe(&self, observed_at_ms: i64) -> Result<(), ProviderError> {
        if self.runner.is_finished() && !self.observed.load(Ordering::Acquire) {
            return Err(ProviderError::Observation(
                forge_provider_api::ObservationFailure::OwnershipConflict,
            ));
        }
        let mut relationship = self.relationship.subscribe();
        while !*relationship.borrow() {
            tokio::select! {
                _ = self.driver.resources.revocation.cancelled() => return Err(ProviderError::Cancelled),
                changed = relationship.changed() => changed.map_err(|_| ProviderError::Observation(forge_provider_api::ObservationFailure::LifecycleUnavailable))?,
            }
        }
        let mut receiver = self.start.clone();
        loop {
            let start = receiver.borrow().clone();
            if let Some(start) = start {
                start.observed(observed_at_ms).await?;
                self.observed.store(true, Ordering::Release);
                return Ok(());
            }
            tokio::select! {
                _ = self.driver.resources.revocation.cancelled() => return Err(ProviderError::Cancelled),
                changed = receiver.changed() => changed.map_err(|_| ProviderError::Observation(forge_provider_api::ObservationFailure::LifecycleUnavailable))?,
            }
        }
    }
    /// Open child activity only after its initial ancestry relationship is in the ordered stream.
    pub(super) fn relationship_published(&self) {
        self.relationship.send_replace(true);
    }

    /// Preserve explicit native terminal evidence after final child item snapshots were published.
    pub(super) async fn complete(
        &self,
        outcome: TurnOutcome,
        completed_at_ms: i64,
    ) -> Result<(), ProviderError> {
        let result = if outcome == TurnOutcome::Interrupted
            && !self.observed.load(Ordering::Acquire)
        {
            Err(ProviderError::Cancelled)
        } else {
            self.observe(completed_at_ms).await?;
            TurnResult::new(outcome, completed_at_ms, None, Vec::new(), Vec::new(), None).map_err(
                |_| ProviderError::Observation(forge_provider_api::ObservationFailure::Protocol),
            )
        };
        let mut terminal = self.driver.terminal.lock().await;
        if terminal.is_some() {
            return Err(ProviderError::Observation(
                forge_provider_api::ObservationFailure::OwnershipConflict,
            ));
        }
        *terminal = Some(result);
        drop(terminal);
        self.driver.ended.notify_one();
        match self.runner.join().await {
            Ok(_) | Err(ProviderError::Cancelled) => Ok(()),
            Err(error) => Err(error),
        }
    }
    /// Get the independent callback resources used by the child's native permission requests.
    pub(super) fn resources(&self) -> ChildResources {
        self.driver.resources.clone()
    }
    /// Check whether explicit native end and original callback collection have both finished.
    pub(super) fn is_finished(&self) -> bool {
        self.runner.is_finished()
    }
    /// Revoke binding-owned observation without fabricating a child terminal certificate.
    pub(super) async fn close(&self) {
        self.driver.resources.revocation.cancel();
        let _collected = self.runner.join().await;
    }
}

/// Wait for native child end while retaining the exact callback resource lifetime.
struct ChildDriver {
    start: watch::Sender<Option<NativeTurnStart>>,
    terminal: Mutex<Option<Result<TurnResult, ProviderError>>>,
    ended: Notify,
    resources: ChildResources,
}

#[async_trait]
impl crate::turn_runner::NativeTurnDriver for ChildDriver {
    /// Wait for the original child terminal observation or explicit binding observation loss.
    async fn run(&self, start: NativeTurnStart) -> Result<TurnResult, ProviderError> {
        self.start.send_replace(Some(start));
        loop {
            let ended = self.ended.notified();
            if let Some(result) = self.terminal.lock().await.clone() {
                return result;
            }
            tokio::select! {
                biased;
                _ = ended => {},
                _ = self.resources.revocation.cancelled() => {
                    if let Some(result) = self.terminal.lock().await.clone() { return result; }
                    return Err(ProviderError::Observation(forge_provider_api::ObservationFailure::Disconnected));
                }
            }
        }
    }
    /// Revoke only this child's retained observation and callback admission.
    async fn interrupt(&self) -> Result<(), ProviderError> {
        self.resources.revocation.cancel();
        Ok(())
    }
    /// Join original child callbacks before the shared runner can publish completion.
    async fn collect(&self) -> Result<(), ProviderError> {
        self.resources.collect().await;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::*;

    /// Callback-first child processing must wait until native ancestry publication has completed.
    #[tokio::test]
    async fn child_processing_waits_for_relationship_publication_before_started() {
        let parent = ThreadOwner::root("relationship");
        let owner =
            ThreadOwner::new(parent.session_id().clone(), AgentId::new("child").unwrap()).unwrap();
        let scope = InvocationScope::new(owner.clone(), "original-exchange");
        let address = TurnAddress {
            thread_id: owner.thread_id(),
            turn_id: scope.turn_id().unwrap(),
            scope,
        };
        let (sink, mut stream) = events::channel(
            BindingId::new("child-relationship").unwrap(),
            EventScope::Binding { owner: parent },
        );
        let child = AcpChildTurn::new(sink, address, ChildResources::new()).unwrap();
        let processing = child.observe(1);
        tokio::pin!(processing);
        assert!(futures_util::poll!(&mut processing).is_pending());
        assert!(stream.try_next().unwrap().is_none());
        child.relationship_published();
        processing.await.unwrap();
        assert!(matches!(
            stream.recv().await.unwrap().unwrap().event,
            ProviderEvent::Turn {
                event: TurnEvent::Started(_),
                ..
            }
        ));
        child
            .complete(
                TurnOutcome::Completed {
                    stop_reason: turn::TurnCompletionReason::Normal,
                },
                2,
            )
            .await
            .unwrap();
        assert!(matches!(
            stream.recv().await.unwrap().unwrap().event,
            ProviderEvent::Turn {
                event: TurnEvent::Completed(_),
                ..
            }
        ));
    }
}
