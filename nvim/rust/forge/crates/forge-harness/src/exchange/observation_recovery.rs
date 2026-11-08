//! Retain observation-generation recovery inside the exchange owner.

use super::{
    provider_lifecycle::{ProviderInitialization, ProviderLifecycleFailure},
    services::OwnerServices,
};
use crate::{
    broker::HarnessBroker,
    provider::{
        Provider, ThreadOwner,
        session_execution::{SessionExecutionRegistry, SessionObservationInbox},
    },
};
use std::sync::Arc;
use tokio::{
    sync::{OwnedRwLockWriteGuard, OwnedSemaphorePermit, oneshot},
    task::JoinHandle,
};

type Reply = oneshot::Sender<Result<(), ProviderLifecycleFailure>>;

/// Keep the selected inbox, recovery work, and all observers until committed handoff.
#[derive(Default)]
pub(super) struct ObservationRecovery {
    target: Option<Arc<SessionObservationInbox>>,
    replies: Vec<Reply>,
    stage: Stage,
    deadline: Option<tokio::time::Instant>,
    storage_failed: bool,
    uncommitted: Option<RecoveryEvidence>,
}

#[derive(Default)]
enum Stage {
    #[default]
    Idle,
    Requested,
    Collecting(JoinHandle<Result<Lease, ProviderLifecycleFailure>>),
    Draining(Lease),
    Replacing(JoinHandle<Result<Replacement, ProviderLifecycleFailure>>),
    Initializing(JoinHandle<Result<ProviderInitialization, ProviderLifecycleFailure>>),
    Failed,
}

pub(super) struct Lease {
    provider: Arc<Provider>,
    registry: Arc<SessionExecutionRegistry>,
    inbox: Arc<SessionObservationInbox>,
    owner: ThreadOwner,
    _control: OwnedSemaphorePermit,
    retirement: OwnedRwLockWriteGuard<()>,
    configuration: tokio::sync::OwnedMutexGuard<()>,
}

pub(super) struct Replacement {
    lease: Lease,
    inbox: Arc<SessionObservationInbox>,
}

/// Keep collected native evidence without publishing an uncommitted replacement.
enum RecoveryEvidence {
    Fenced {
        _provider: Arc<Provider>,
        _inbox: Arc<SessionObservationInbox>,
    },
    Replaced {
        _provider: Arc<Provider>,
        _inbox: Arc<SessionObservationInbox>,
    },
    Initialized {
        _initialized: ProviderInitialization,
    },
}

/// Carry one completed job back to the owner without transferring lifecycle authority.
pub(super) enum RecoveryEvent {
    /// A caller observation expired; retain the original work and keep admission blocked.
    Deadline,
    Collected(Result<Lease, ProviderLifecycleFailure>),
    Replaced(Result<Replacement, ProviderLifecycleFailure>),
    Initialized(Result<ProviderInitialization, ProviderLifecycleFailure>),
}

impl ObservationRecovery {
    /// Attach recovery observers to the exact inbox selected by the service listener.
    pub(super) fn request(
        &mut self,
        target: Arc<SessionObservationInbox>,
        current: &Arc<SessionObservationInbox>,
        reply: Reply,
    ) {
        if self.storage_failed {
            let _ = reply.send(Err(ProviderLifecycleFailure::StateUnavailable));
            return;
        }
        if !Arc::ptr_eq(&target, current) {
            let _ = reply.send(Err(ProviderLifecycleFailure::GenerationChanged));
            return;
        }
        if self
            .target
            .as_ref()
            .is_some_and(|current| !Arc::ptr_eq(current, &target))
        {
            let _ = reply.send(Err(ProviderLifecycleFailure::GenerationChanged));
            return;
        }
        self.target = Some(target);
        self.replies.push(reply);
        if matches!(self.stage, Stage::Idle | Stage::Failed) {
            self.stage = Stage::Requested;
        }
    }

    /// Prevent new caller/native work from overtaking a requested generation recovery.
    pub(super) fn active(&self) -> bool {
        !matches!(self.stage, Stage::Idle | Stage::Failed)
    }

    /// Start resource collection only after prior owner work has settled.
    pub(super) fn advance(
        &mut self,
        quiescent: bool,
        broker: &mut HarnessBroker,
        services: &OwnerServices,
    ) -> Result<bool, ProviderLifecycleFailure> {
        if self.storage_failed {
            return Ok(false);
        }
        if matches!(self.stage, Stage::Requested) && quiescent {
            let target = self
                .target
                .as_ref()
                .expect("requested recovery retains its inbox");
            if !Arc::ptr_eq(target, &services.observer.session_events()) {
                // No recovery effect started: reject this stale request without changing the healthy replacement.
                self.finish(Err(ProviderLifecycleFailure::GenerationChanged));
                return Ok(true);
            }
            if !target.is_closed() && !services.tasks.configuration_is_uncertain() {
                self.finish(Ok(()));
                return Ok(true);
            }
            if services.tasks.configuration_is_uncertain() {
                broker
                    .recover_pending_configuration()
                    .map_err(|_| ProviderLifecycleFailure::StateUnavailable)?;
            }
            let provider = broker.provider_handle();
            let registry = broker.execution_registry();
            let inbox = target.clone();
            let owner = ThreadOwner::root(services.observer.session_id());
            let control = services.control.clone();
            let lane = services.provider_lane.clone();
            let configuration = services.tasks.configuration_lane();
            self.deadline = Some(tokio::time::Instant::now() + std::time::Duration::from_secs(60));
            self.stage = Stage::Collecting(tokio::spawn(async move {
                let control = control
                    .acquire_owned()
                    .await
                    .map_err(|_| ProviderLifecycleFailure::ControllerClosed)?;
                let retirement = lane.write_owned().await;
                let configuration = configuration.lock_owned().await;
                registry.fence(&owner).await?;
                Ok(Lease {
                    provider,
                    registry,
                    inbox,
                    owner,
                    _control: control,
                    retirement,
                    configuration,
                })
            }));
        }
        if let Stage::Draining(lease) = &self.stage {
            if !lease
                .inbox
                .ready_for_replacement()
                .map_err(|_| ProviderLifecycleFailure::ObservationUnavailable)?
            {
                // Only the owner consumes the last old-generation observations.
                let events = broker.drain_session_events();
                match events {
                    Ok(events) => {
                        for event in events {
                            services
                                .publications
                                .publish(event, None)
                                .map_err(|_| ProviderLifecycleFailure::StateUnavailable)?;
                        }
                    }
                    Err(error) if lease.inbox.ready_for_replacement().unwrap_or(false) => {
                        services.publications.publish(forge_protocol::message::SessionEvent {
                            session_id: services.observer.session_id().into(), event: "provider_event".into(),
                            payload: serde_json::json!({"kind":"error","code":"recovered_observation_failure","text":error.to_string()}),
                        }, None).map_err(|_| ProviderLifecycleFailure::StateUnavailable)?;
                    }
                    Err(_) => return Err(ProviderLifecycleFailure::ObservationUnavailable),
                }
            }
            if lease
                .inbox
                .ready_for_replacement()
                .map_err(|_| ProviderLifecycleFailure::ObservationUnavailable)?
            {
                let Stage::Draining(lease) = std::mem::replace(&mut self.stage, Stage::Idle) else {
                    unreachable!()
                };
                self.stage = Stage::Replacing(tokio::spawn(async move {
                    let inbox =
                        lease
                            .registry
                            .replace(lease.owner.clone())
                            .await
                            .map_err(|error| {
                                ProviderLifecycleFailure::Provider(
                                    crate::provider::lifecycle_error::classify(
                                        forge_provider_api::ProviderOperation::ThreadOpen,
                                        error.into(),
                                    ),
                                )
                            })?;
                    Ok(Replacement { lease, inbox })
                }));
            }
        }
        Ok(false)
    }

    /// Get whether resource retirement has started rather than merely being requested.
    pub(super) fn started(&self) -> bool {
        matches!(
            self.stage,
            Stage::Collecting(_)
                | Stage::Draining(_)
                | Stage::Replacing(_)
                | Stage::Initializing(_)
        )
    }

    /// Fail observers without discarding a live native recovery worker.
    pub(super) fn fail_storage(&mut self) {
        self.storage_failed = true;
        self.deadline = None;
        for reply in self.replies.drain(..) {
            let _ = reply.send(Err(ProviderLifecycleFailure::StateUnavailable));
        }
        if matches!(self.stage, Stage::Requested | Stage::Idle | Stage::Failed) {
            self.stage = Stage::Failed;
        } else if matches!(self.stage, Stage::Draining(_)) {
            let Stage::Draining(lease) = std::mem::replace(&mut self.stage, Stage::Failed) else {
                unreachable!()
            };
            self.retain_fenced(lease);
        }
    }

    /// Retain collected evidence while releasing only guards whose native work has actually settled.
    fn retain_fenced(&mut self, lease: Lease) {
        self.uncommitted = Some(RecoveryEvidence::Fenced {
            _provider: lease.provider.clone(),
            _inbox: lease.inbox.clone(),
        });
        drop(lease);
    }

    /// Reject a not-yet-started recovery when shutdown closes admission.
    pub(super) fn close_pending(&mut self) {
        if matches!(self.stage, Stage::Requested) {
            self.finish(Err(ProviderLifecycleFailure::ControllerClosed));
        }
    }

    /// Await the current job while event intake and cleanup continue in the owner loop.
    pub(super) async fn next(&mut self) -> RecoveryEvent {
        let deadline = self.deadline;
        let event = async {
            match &mut self.stage {
                Stage::Collecting(task) => RecoveryEvent::Collected(
                    task.await
                        .unwrap_or(Err(ProviderLifecycleFailure::ObservationUnavailable)),
                ),
                Stage::Replacing(task) => RecoveryEvent::Replaced(
                    task.await
                        .unwrap_or(Err(ProviderLifecycleFailure::ObservationUnavailable)),
                ),
                Stage::Initializing(task) => RecoveryEvent::Initialized(
                    task.await
                        .unwrap_or(Err(ProviderLifecycleFailure::ObservationUnavailable)),
                ),
                Stage::Idle | Stage::Requested | Stage::Draining(_) | Stage::Failed => {
                    std::future::pending().await
                }
            }
        };
        tokio::select! {
            result = event => result,
            _ = async { match deadline { Some(deadline) => tokio::time::sleep_until(deadline).await, None => std::future::pending().await } } => RecoveryEvent::Deadline,
        }
    }

    /// Commit replacement identity and epoch only after the matching old resources collected.
    pub(super) fn apply(
        &mut self,
        event: RecoveryEvent,
        broker: &mut HarnessBroker,
        services: &OwnerServices,
    ) -> Result<bool, ProviderLifecycleFailure> {
        if matches!(event, RecoveryEvent::Deadline) {
            self.deadline = None;
            for reply in self.replies.drain(..) {
                let _ = reply.send(Err(ProviderLifecycleFailure::ObservationUnavailable));
            }
            return Ok(false);
        }
        if self.storage_failed {
            self.stage = Stage::Failed;
            match event {
                RecoveryEvent::Collected(Ok(lease)) => self.retain_fenced(lease),
                RecoveryEvent::Replaced(Ok(replacement)) => {
                    self.uncommitted = Some(RecoveryEvidence::Replaced {
                        _provider: replacement.lease.provider.clone(),
                        _inbox: replacement.inbox,
                    });
                    drop(replacement.lease);
                }
                RecoveryEvent::Initialized(Ok(initialized)) => {
                    self.uncommitted = Some(RecoveryEvidence::Initialized {
                        _initialized: initialized,
                    });
                }
                RecoveryEvent::Collected(Err(_))
                | RecoveryEvent::Replaced(Err(_))
                | RecoveryEvent::Initialized(Err(_)) => {}
                RecoveryEvent::Deadline => unreachable!("deadline keeps its original stage"),
            }
            return Ok(false);
        }
        self.stage = Stage::Idle;
        match event {
            RecoveryEvent::Deadline => unreachable!("deadline retains its original recovery stage"),
            RecoveryEvent::Collected(lease) => self.stage = Stage::Draining(lease?),
            RecoveryEvent::Replaced(replacement) => {
                let Replacement { lease, inbox } = replacement?;
                if !Arc::ptr_eq(&lease.provider, &broker.provider_handle())
                    || !Arc::ptr_eq(&lease.inbox, &services.observer.session_events())
                {
                    return Err(ProviderLifecycleFailure::GenerationChanged);
                }
                services
                    .tasks
                    .reset_configuration_generation()
                    .map_err(|_| ProviderLifecycleFailure::StateUnavailable)?;
                broker
                    .clear_effective_capabilities()
                    .map_err(|_| ProviderLifecycleFailure::StateUnavailable)?;
                services.observer.replace_session_events(inbox);
                let provider = lease.provider.clone();
                // Publish the new inbox under retirement; release guards before initialization observes it.
                drop(lease.configuration);
                drop(lease.retirement);
                drop(lease._control);
                self.stage = Stage::Initializing(tokio::spawn(
                    services.provider_lifecycle.prepare(provider),
                ));
            }
            RecoveryEvent::Initialized(initialized) => {
                initialized?.commit(broker)?;
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// Release every observer only after success or an explicit retained recovery failure.
    pub(super) fn finish(&mut self, result: Result<(), ProviderLifecycleFailure>) {
        if matches!(
            self.stage,
            Stage::Collecting(_) | Stage::Replacing(_) | Stage::Initializing(_)
        ) {
            // An observation result cannot dispose the only handle to native work still running.
            self.fail_storage();
            return;
        }
        for reply in self.replies.drain(..) {
            let _ = reply.send(result.clone());
        }
        self.target = None;
        self.deadline = None;
        self.stage = if result.is_ok() {
            Stage::Idle
        } else {
            Stage::Failed
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Storage failure releases waiters but leaves the original collection worker observable.
    #[tokio::test]
    async fn storage_failure_retains_pending_collection() {
        let (completed, outcome) = oneshot::channel();
        let (reply, observed) = oneshot::channel();
        let mut recovery = ObservationRecovery::default();
        recovery.replies.push(reply);
        recovery.stage = Stage::Collecting(tokio::spawn(async move {
            outcome
                .await
                .unwrap_or(Err(ProviderLifecycleFailure::ObservationUnavailable))
        }));
        recovery.fail_storage();
        assert!(matches!(
            observed.await.unwrap(),
            Err(ProviderLifecycleFailure::StateUnavailable)
        ));
        assert!(matches!(recovery.stage, Stage::Collecting(_)));
        assert!(recovery.active());
        assert!(
            completed
                .send(Err(ProviderLifecycleFailure::ObservationUnavailable))
                .is_ok()
        );
        assert!(matches!(
            recovery.next().await,
            RecoveryEvent::Collected(Err(ProviderLifecycleFailure::ObservationUnavailable))
        ));
    }
}
