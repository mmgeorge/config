//! Retain owner shutdown collection through native timeouts and database failures.

use futures_util::FutureExt;

use super::{
    services::OwnerServices,
    transition::{
        ExchangeStop, RetainedTransition, TransitionEvent, TransitionFailure, TransitionProgress,
        TurnStatus,
    },
};

/// Keep the session lease alive until all shutdown effects and terminal state settle.
pub(super) struct OwnerShutdown {
    state: ShutdownState,
    attempt_count: u8,
    signals: std::collections::VecDeque<crate::broker::ForegroundSignal>,
}

impl OwnerShutdown {
    /// Start internal cancellation after the admitted command has reached its finite result.
    pub(super) fn start(services: &OwnerServices) -> Self {
        Self {
            state: ShutdownState::Collecting(ExchangeStop::for_shutdown(services)),
            attempt_count: 1,
            signals: Default::default(),
        }
    }

    /// Drain the collection fact for the exact foreground being retired during shutdown.
    pub(super) fn take_collection_signal(&mut self) -> Option<crate::broker::ForegroundSignal> {
        if let Some(signal) = self.signals.pop_front() {
            return Some(signal);
        }
        match &mut self.state {
            ShutdownState::Collecting(transition) => transition.take_collection_signal(),
            ShutdownState::Recovering(_)
            | ShutdownState::Retiring(_)
            | ShutdownState::RetirementFailed { .. }
            | ShutdownState::Settled { .. }
            | ShutdownState::Blocked { .. }
            | ShutdownState::Lost
            | ShutdownState::Complete
            | ShutdownState::Consumed => None,
        }
    }

    /// Retry a settled shutdown transaction only after a new explicit close request.
    pub(super) fn retry(&mut self, services: &OwnerServices) {
        if matches!(self.state, ShutdownState::RetirementFailed { .. }) {
            let ShutdownState::RetirementFailed {
                _resources: retirement,
            } = std::mem::replace(&mut self.state, ShutdownState::Consumed)
            else {
                unreachable!("failed retirement retains its exact generation")
            };
            self.state = ShutdownState::Retiring(Self::retire(retirement));
        } else if matches!(self.state, ShutdownState::Blocked { .. }) {
            let ShutdownState::Blocked {
                _resources: retained,
            } = std::mem::replace(&mut self.state, ShutdownState::Consumed)
            else {
                unreachable!("blocked shutdown retains its original resources")
            };
            self.attempt_count = 1;
            self.state = ShutdownState::Recovering(Self::collect(retained, services.clone()));
        } else if matches!(self.state, ShutdownState::Settled { .. }) {
            self.state = ShutdownState::Collecting(ExchangeStop::for_shutdown(services));
            self.attempt_count = 1;
        }
    }

    /// Get whether resource collection and the shutdown domain commit both completed.
    pub(super) fn complete(&self) -> bool {
        matches!(self.state, ShutdownState::Complete)
    }

    /// Await one retained shutdown fact without blocking foreground or provider event intake.
    pub(super) async fn next(&mut self, foreground: TurnStatus) -> ShutdownEvent {
        match &mut self.state {
            ShutdownState::Collecting(transition) => {
                ShutdownEvent::Transition(transition.next(foreground.clone()).await)
            }
            ShutdownState::Recovering(task) => ShutdownEvent::Recovered(Box::new(task.await)),
            ShutdownState::Retiring(task) => ShutdownEvent::Retired(task.await),
            ShutdownState::RetirementFailed { .. }
            | ShutdownState::Settled { .. }
            | ShutdownState::Blocked { .. }
            | ShutdownState::Lost
            | ShutdownState::Complete
            | ShutdownState::Consumed => std::future::pending().await,
        }
    }

    /// Apply shutdown evidence only at the owner update boundary and retain failed cleanup ownership.
    pub(super) fn update(
        &mut self,
        event: ShutdownEvent,
        foreground: TurnStatus,
        broker: &mut crate::broker::HarnessBroker,
        services: &OwnerServices,
    ) -> ShutdownProgress {
        match event {
            ShutdownEvent::Transition(event) => {
                let ShutdownState::Collecting(transition) = &mut self.state else {
                    unreachable!("shutdown transition retains its evidence owner")
                };
                let result = transition.update(event, foreground, broker, services);
                while let Some(signal) = transition.take_collection_signal() {
                    self.signals.push_back(signal);
                }
                match result {
                    Ok(TransitionProgress::Waiting { events, input }) => ShutdownProgress {
                        events,
                        input: input.map(|input| *input),
                        failure: None,
                    },
                    Ok(TransitionProgress::Finished { events, .. }) => {
                        // Runtime shutdown may only know the initial generation. Rollback
                        // installs a replacement, whose observer must also be retired here.
                        let retirement = ShutdownRetirement {
                            registry: broker.execution_registry(),
                            owner: crate::provider::ThreadOwner::root(
                                services.observer.session_id(),
                            ),
                            inbox: services.observer.session_events(),
                        };
                        self.state = ShutdownState::Retiring(Self::retire(retirement));
                        ShutdownProgress {
                            events,
                            input: None,
                            failure: None,
                        }
                    }
                    Err(failure) => {
                        let ShutdownState::Collecting(transition) =
                            std::mem::replace(&mut self.state, ShutdownState::Consumed)
                        else {
                            unreachable!("failed shutdown retains its transition")
                        };
                        self.state = ShutdownState::Recovering(Self::collect(
                            transition.into_retained(),
                            services.clone(),
                        ));
                        ShutdownProgress {
                            events: Vec::new(),
                            input: None,
                            failure: Some(failure),
                        }
                    }
                }
            }
            ShutdownEvent::Recovered(result) => {
                match *result {
                    Ok((retained, Ok(()))) if self.attempt_count < 2 => {
                        drop(retained);
                        self.attempt_count += 1;
                        self.state =
                            ShutdownState::Collecting(ExchangeStop::for_shutdown(services));
                    }
                    Ok((retained, Ok(()))) => {
                        self.state = ShutdownState::Settled {
                            _resources: retained,
                        }
                    }
                    Ok((retained, Err(_))) => {
                        self.state = ShutdownState::Blocked {
                            _resources: retained,
                        }
                    }
                    Err(_) => self.state = ShutdownState::Lost,
                }
                ShutdownProgress {
                    events: Vec::new(),
                    input: None,
                    failure: None,
                }
            }
            ShutdownEvent::Retired(result) => {
                let failure = match result {
                    Ok((_, Ok(()))) => {
                        self.state = ShutdownState::Complete;
                        None
                    }
                    Ok((retirement, Err(error))) => {
                        self.state = ShutdownState::RetirementFailed {
                            _resources: retirement,
                        };
                        Some(TransitionFailure::Domain(error))
                    }
                    Err(_) => {
                        self.state = ShutdownState::Lost;
                        Some(TransitionFailure::Uncertain)
                    }
                };
                ShutdownProgress {
                    events: Vec::new(),
                    input: None,
                    failure,
                }
            }
        }
    }
    /// Preserve collection ownership even if a cleanup future panics.
    fn collect(
        mut retained: RetainedTransition,
        services: OwnerServices,
    ) -> tokio::task::JoinHandle<ShutdownCollection> {
        tokio::spawn(async move {
            let result = std::panic::AssertUnwindSafe(retained.collect(&services))
                .catch_unwind()
                .await
                .unwrap_or(Err(crate::provider::ProviderError::DeliveryUncertain));
            (retained, result)
        })
    }

    /// Preserve the retired generation after a panic so explicit close can retry collection.
    fn retire(
        retirement: ShutdownRetirement,
    ) -> tokio::task::JoinHandle<(ShutdownRetirement, anyhow::Result<()>)> {
        tokio::spawn(async move {
            let result = std::panic::AssertUnwindSafe(retirement.collect())
                .catch_unwind()
                .await
                .unwrap_or_else(|_| Err(crate::provider::ProviderError::DeliveryUncertain.into()));
            (retirement, result)
        })
    }
}

/// Preserve retained resources alongside the result of collecting their native work.
type ShutdownCollection = (
    RetainedTransition,
    Result<(), crate::provider::ProviderError>,
);

/// Distinguish collection failure from losing observation of the retained worker.
type ShutdownCollectionObservation = Result<ShutdownCollection, tokio::task::JoinError>;

/// Carry one shutdown fact while the runtime continues draining all other owned resources.
pub(super) enum ShutdownEvent {
    /// Carry one result from the bounded shutdown transition.
    Transition(TransitionEvent),
    /// Carry actual retained collection after its observation deadline expired.
    Recovered(Box<ShutdownCollectionObservation>),
    Retired(Result<(ShutdownRetirement, anyhow::Result<()>), tokio::task::JoinError>),
}

impl ShutdownEvent {
    /// Name the exact retained boundary for shutdown diagnostics without exposing native payloads.
    pub(super) fn name(&self) -> &'static str {
        match self {
            Self::Transition(event) => event.name(),
            Self::Recovered(_) => "shutdown_recovered",
            Self::Retired(_) => "generation_retired",
        }
    }
}

/// Publish committed shutdown observations while keeping failures out of success responses.
pub(super) struct ShutdownProgress {
    pub(super) events: Vec<forge_protocol::message::SessionEvent>,
    pub(super) input: Option<crate::input::InputUpdate>,
    pub(super) failure: Option<TransitionFailure>,
}

enum ShutdownState {
    Collecting(ExchangeStop),
    Recovering(tokio::task::JoinHandle<ShutdownCollection>),
    Retiring(tokio::task::JoinHandle<(ShutdownRetirement, anyhow::Result<()>)>),
    RetirementFailed { _resources: ShutdownRetirement },
    // Retain collected ownership while a later explicit close may retry its domain commit.
    Settled { _resources: RetainedTransition },
    // Keep uncertain native ownership alive; the session lease must not be released.
    Blocked { _resources: RetainedTransition },
    Lost,
    Complete,
    Consumed,
}

/// Keep the exact final generation alive until native and observation collection succeeds.
pub(super) struct ShutdownRetirement {
    registry: std::sync::Arc<crate::provider::session_execution::SessionExecutionRegistry>,
    owner: crate::provider::ThreadOwner,
    inbox: std::sync::Arc<crate::provider::session_execution::SessionObservationInbox>,
}

impl ShutdownRetirement {
    async fn collect(&self) -> anyhow::Result<()> {
        // A registry and provider can serve sibling sessions. Retire only this
        // session's binding; service runtime shutdown owns process-wide teardown.
        self.registry
            .retire(
                &self.owner,
                crate::provider::session_execution::OwnerCloseReason::Shutdown,
            )
            .await?;
        self.inbox
            .close_collected(crate::provider::session_execution::OwnerCloseReason::Shutdown);
        self.inbox.retire_background().await?;
        Ok(())
    }
}
