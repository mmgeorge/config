//! Retain rollback collection and detached file work inside the active owner command.

use forge_protocol::message::SessionEvent;
use tokio::{
    sync::{OwnedRwLockWriteGuard, OwnedSemaphorePermit},
    task::JoinHandle,
};

use super::{
    command::{CancelledInputPolicy, Command, CommandFailure},
    failure::OperationFailure,
    queue::QueuedCommand,
    rollback::{RollbackFailure, RollbackOutcome},
    services::OwnerServices,
    transition::{
        ExchangeStop, RetainedTransition, StopIntent, TransitionEvent, TransitionOutcome,
        TransitionProgress, TurnStatus,
    },
};

/// Own the admitted rollback and every resource required to finish or collect it.
pub(super) struct RollbackRun {
    /// Retain the admitted identity and completion observers until its finite outcome commits.
    pub(super) command: QueuedCommand,
    stage: RollbackStage,
    transition: Option<ExchangeStop>,
    control: Option<OwnedSemaphorePermit>,
    retirement: Option<OwnedRwLockWriteGuard<()>>,
    pending_finish: Option<(RollbackOutcome, Vec<SessionEvent>)>,
}

impl RollbackRun {
    /// Transfer collection decisions to the owner-held foreground in source order.
    pub(super) fn take_collection_signal(&mut self) -> Option<crate::broker::ForegroundSignal> {
        self.transition
            .as_mut()
            .and_then(ExchangeStop::take_collection_signal)
    }
    /// Get whether this rollback changes history and therefore closes invocation tool admission.
    pub(super) fn closes_tool_admission(&self) -> bool {
        let Command::Rollback(intent) = &self.command.record.request.command else {
            unreachable!("rollback stage retains its typed intent")
        };
        intent.requires_collection()
    }
    /// Get whether a committed rollback still awaits its replacement provider's readiness.
    pub(super) fn initializing(&self) -> bool {
        self.pending_finish.is_some()
    }
    /// Install collection before file mutation, or acquire control for read-only preparation.
    pub(super) fn start(command: QueuedCommand, services: &OwnerServices) -> Self {
        let Command::Rollback(intent) = &command.record.request.command else {
            unreachable!("rollback stages receive rollback commands")
        };
        let (stage, transition) = if intent.requires_collection() {
            (
                RollbackStage::Collecting,
                Some(ExchangeStop::start(
                    command.record.request.id.clone(),
                    StopIntent::Cancel(CancelledInputPolicy::Retain),
                    services,
                    None,
                    None,
                )),
            )
        } else {
            let control = services.control.clone();
            (
                RollbackStage::Acquiring(tokio::spawn(async move {
                    tokio::time::timeout(
                        std::time::Duration::from_secs(60),
                        control.acquire_owned(),
                    )
                    .await
                    .ok()
                    .and_then(Result::ok)
                    .ok_or(CommandFailure::OwnershipUnavailable)
                })),
                None,
            )
        };
        Self {
            command,
            stage,
            transition,
            control: None,
            retirement: None,
            pending_finish: None,
        }
    }

    /// Await one owned fact while the driver remains free to process provider events.
    pub(super) async fn next(&mut self, foreground: TurnStatus) -> RollbackEvent {
        match &mut self.stage {
            RollbackStage::Acquiring(task) => RollbackEvent::Control(
                task.await
                    .unwrap_or(Err(CommandFailure::OwnershipUnavailable)),
            ),
            RollbackStage::Retiring(task) => RollbackEvent::Retired(
                task.await
                    .unwrap_or(Err(CommandFailure::OwnershipUnavailable)),
            ),
            RollbackStage::Collecting => RollbackEvent::Transition(
                self.transition
                    .as_mut()
                    .expect("rollback collection retains its transition")
                    .next(foreground.clone())
                    .await,
            ),
            RollbackStage::Prepare => RollbackEvent::Prepare,
            RollbackStage::Working { task, deadline } => tokio::select! {
                biased;
                result = task => RollbackEvent::Worked(result.unwrap_or(Err(RollbackFailure::RecoveryRequired)).map(Box::new)),
                _ = tokio::time::sleep_until(*deadline) => RollbackEvent::Deadline,
            },
            RollbackStage::Commit(_) => RollbackEvent::Commit,
            RollbackStage::Initializing { task, deadline } => tokio::select! {
                biased;
                result = task => RollbackEvent::Initialized(result.unwrap_or(Err(super::provider_lifecycle::ProviderLifecycleFailure::ObservationUnavailable))),
                _ = tokio::time::sleep_until(*deadline) => RollbackEvent::Deadline,
            },
            RollbackStage::InitializationReady(_) => RollbackEvent::CommitInitialization,
            RollbackStage::InitializationFailed(_) | RollbackStage::Consumed => {
                std::future::pending().await
            }
        }
    }

    /// Apply only the active stage's evidence and install detached work after owner validation.
    pub(super) fn update(
        &mut self,
        event: RollbackEvent,
        foreground: TurnStatus,
        broker: &mut crate::broker::HarnessBroker,
        services: &OwnerServices,
        can_commit: bool,
    ) -> Result<RollbackProgress, OperationFailure> {
        if !can_commit {
            match event {
                RollbackEvent::Control(result) => {
                    self.control = result.ok();
                    self.stage = RollbackStage::Consumed;
                }
                RollbackEvent::Retired(result) => {
                    self.retirement = result.ok();
                    self.stage = RollbackStage::Consumed;
                }
                RollbackEvent::Worked(result) => {
                    self.stage = match result {
                        Ok(result) => RollbackStage::Commit(result),
                        Err(_) => RollbackStage::Consumed,
                    };
                }
                RollbackEvent::Initialized(result) => {
                    self.stage = RollbackStage::InitializationReady(result);
                }
                RollbackEvent::Transition(event) => {
                    self.transition
                        .as_mut()
                        .expect("rollback transition retains yielded facts")
                        .retain_uncommitted(event);
                }
                RollbackEvent::Prepare
                | RollbackEvent::Commit
                | RollbackEvent::CommitInitialization
                | RollbackEvent::Deadline => {}
            }
            return Err(OperationFailure::StorageUncommitted);
        }
        if matches!(
            &event,
            RollbackEvent::Control(_) | RollbackEvent::Retired(_) | RollbackEvent::Worked(_)
        ) {
            self.stage = RollbackStage::Consumed;
        }
        match event {
            RollbackEvent::Control(result) => {
                self.control = Some(result.map_err(|failure| {
                    OperationFailure::Transition(super::transition::TransitionFailure::Rejected(
                        failure,
                    ))
                })?);
                self.stage = RollbackStage::Prepare;
                Ok(RollbackProgress::waiting())
            }
            RollbackEvent::Transition(event) => {
                match self
                    .transition
                    .as_mut()
                    .expect("rollback transition remains owned")
                    .update(event, foreground, broker, services)
                    .map_err(OperationFailure::Transition)?
                {
                    TransitionProgress::Waiting { events, input } => {
                        Ok(RollbackProgress::Waiting { events, input })
                    }
                    TransitionProgress::Finished {
                        outcome: TransitionOutcome::Cancelled(_),
                        events,
                    } => {
                        self.stage = RollbackStage::Prepare;
                        Ok(RollbackProgress::Waiting {
                            events,
                            input: None,
                        })
                    }
                    TransitionProgress::Finished {
                        outcome: TransitionOutcome::Reconfigured { .. },
                        ..
                    } => {
                        unreachable!("rollback cancels execution before changing its history")
                    }
                }
            }
            RollbackEvent::Prepare => {
                let Command::Rollback(intent) = &self.command.record.request.command else {
                    unreachable!("rollback retains its original typed intent")
                };
                if intent.requires_collection() && self.retirement.is_none() {
                    let lane = services.provider_lane.clone();
                    self.stage = RollbackStage::Retiring(tokio::spawn(async move {
                        tokio::time::timeout(std::time::Duration::from_secs(60), lane.write_owned())
                            .await
                            .map_err(|_| CommandFailure::OwnershipUnavailable)
                    }));
                    return Ok(RollbackProgress::waiting());
                }
                match broker.prepare_rollback_effect(intent.clone()) {
                    Ok(effect) => {
                        self.stage = RollbackStage::Working {
                            task: tokio::spawn(effect.run()),
                            deadline: tokio::time::Instant::now()
                                + std::time::Duration::from_secs(60),
                        };
                        Ok(RollbackProgress::waiting())
                    }
                    Err(failure) => Ok(RollbackProgress::Finished {
                        outcome: Err(failure),
                        events: Vec::new(),
                    }),
                }
            }
            RollbackEvent::Retired(result) => {
                self.retirement = Some(result.map_err(|failure| {
                    OperationFailure::Transition(super::transition::TransitionFailure::Rejected(
                        failure,
                    ))
                })?);
                self.stage = RollbackStage::Prepare;
                Ok(RollbackProgress::waiting())
            }
            RollbackEvent::Worked(result) => match result {
                Ok(result) => {
                    self.stage = RollbackStage::Commit(result);
                    Ok(RollbackProgress::waiting())
                }
                Err(failure) => Ok(RollbackProgress::Finished {
                    outcome: Err(failure),
                    events: Vec::new(),
                }),
            },
            RollbackEvent::Commit => {
                let RollbackStage::Commit(result) =
                    std::mem::replace(&mut self.stage, RollbackStage::Consumed)
                else {
                    unreachable!("rollback commit consumes one completed effect")
                };
                match broker.commit_rollback_effect(*result) {
                    Ok((outcome @ RollbackOutcome::Restored, events)) => {
                        let provider = broker.provider_handle();
                        *services
                            .provider
                            .write()
                            .unwrap_or_else(std::sync::PoisonError::into_inner) = provider.clone();
                        self.retirement.take();
                        self.pending_finish = Some((outcome, events));
                        self.stage = RollbackStage::Initializing {
                            task: tokio::spawn(services.provider_lifecycle.prepare(provider)),
                            deadline: tokio::time::Instant::now()
                                + std::time::Duration::from_secs(60),
                        };
                        Ok(RollbackProgress::waiting())
                    }
                    Ok((outcome, events)) => Ok(RollbackProgress::Finished {
                        outcome: Ok(outcome),
                        events,
                    }),
                    Err(failure) => Ok(RollbackProgress::Finished {
                        outcome: Err(failure),
                        events: Vec::new(),
                    }),
                }
            }
            RollbackEvent::Initialized(result) => {
                self.stage = RollbackStage::InitializationReady(result);
                Ok(RollbackProgress::waiting())
            }
            RollbackEvent::CommitInitialization => {
                let RollbackStage::InitializationReady(result) =
                    std::mem::replace(&mut self.stage, RollbackStage::Consumed)
                else {
                    unreachable!("initialization commit retains its exact native result")
                };
                let (outcome, events) = self
                    .pending_finish
                    .take()
                    .expect("replacement initialization retains the committed rollback outcome");
                match result.and_then(|initialized| initialized.commit(broker)) {
                    Ok(()) => Ok(RollbackProgress::Finished {
                        outcome: Ok(outcome),
                        events,
                    }),
                    Err(error) => {
                        self.stage = RollbackStage::InitializationFailed(error);
                        self.pending_finish = Some((outcome, events.clone()));
                        Ok(RollbackProgress::Finished {
                            outcome: Err(RollbackFailure::RecoveryRequired),
                            events,
                        })
                    }
                }
            }
            RollbackEvent::Deadline => Err(OperationFailure::JobDeadline),
        }
    }

    /// Preserve a timed-out file worker and its authority until its actual result is collected.
    pub(super) fn into_recovery(self) -> (QueuedCommand, RollbackRecovery) {
        (
            self.command,
            RollbackRecovery {
                stage: self.stage,
                transition: self.transition,
                control: self.control,
                retirement: self.retirement,
                pending_finish: self.pending_finish,
            },
        )
    }
}

/// Hold rollback work after command completion without granting a replacement execution permit.
pub(super) struct RollbackRecovery {
    stage: RollbackStage,
    transition: Option<ExchangeStop>,
    control: Option<OwnedSemaphorePermit>,
    retirement: Option<OwnedRwLockWriteGuard<()>>,
    pending_finish: Option<(RollbackOutcome, Vec<SessionEvent>)>,
}

impl RollbackRecovery {
    /// Join file work without discarding its completed durable restoration evidence.
    pub(super) async fn collect(mut self) -> Self {
        if let RollbackStage::Working { task, .. } = &mut self.stage {
            let result = task
                .await
                .unwrap_or(Err(RollbackFailure::RecoveryRequired))
                .map(Box::new);
            self.stage = match result {
                Ok(result) => RollbackStage::Commit(result),
                Err(_) => RollbackStage::Consumed,
            };
        }
        if let RollbackStage::Acquiring(task) = &mut self.stage {
            self.control = task.await.ok().and_then(Result::ok);
            self.stage = RollbackStage::Consumed;
        }
        if let RollbackStage::Retiring(task) = &mut self.stage {
            self.retirement = task.await.ok().and_then(Result::ok);
            self.stage = RollbackStage::Consumed;
        }
        if let RollbackStage::Initializing { task, .. } = &mut self.stage {
            self.stage = RollbackStage::InitializationReady(task.await.unwrap_or(Err(
                super::provider_lifecycle::ProviderLifecycleFailure::ObservationUnavailable,
            )));
        }
        self
    }

    /// Commit late file evidence before releasing the retained transition's control ownership.
    pub(super) fn apply(
        &mut self,
        broker: &mut crate::broker::HarnessBroker,
        services: &OwnerServices,
    ) -> Result<RollbackRecoveryProgress, RollbackFailure> {
        let stage = std::mem::replace(&mut self.stage, RollbackStage::Consumed);
        match stage {
            RollbackStage::Commit(result) => {
                let (outcome, events) = broker.commit_rollback_effect(*result)?;
                if matches!(outcome, RollbackOutcome::Restored) {
                    let provider = broker.provider_handle();
                    *services
                        .provider
                        .write()
                        .unwrap_or_else(std::sync::PoisonError::into_inner) = provider.clone();
                    self.retirement.take();
                    self.pending_finish = Some((outcome, events));
                    self.stage = RollbackStage::Initializing {
                        task: tokio::spawn(services.provider_lifecycle.prepare(provider)),
                        deadline: tokio::time::Instant::now() + std::time::Duration::from_secs(60),
                    };
                    return Ok(RollbackRecoveryProgress::Waiting);
                }
                Ok(RollbackRecoveryProgress::Finished(events))
            }
            RollbackStage::InitializationReady(result) => {
                let events = self
                    .pending_finish
                    .take()
                    .map_or_else(Vec::new, |(_, events)| events);
                if let Err(error) = result.and_then(|initialized| initialized.commit(broker)) {
                    self.stage = RollbackStage::InitializationFailed(error);
                    return Err(RollbackFailure::RecoveryRequired);
                }
                Ok(RollbackRecoveryProgress::Finished(events))
            }
            RollbackStage::InitializationFailed(error) => {
                self.stage = RollbackStage::InitializationFailed(error);
                Err(RollbackFailure::RecoveryRequired)
            }
            RollbackStage::Acquiring(_)
            | RollbackStage::Retiring(_)
            | RollbackStage::Working { .. }
            | RollbackStage::Collecting
            | RollbackStage::Prepare
            | RollbackStage::Initializing { .. }
            | RollbackStage::Consumed => Ok(RollbackRecoveryProgress::Finished(Vec::new())),
        }
    }

    /// Transfer uncertain native collection to the owner's established cleanup machinery.
    pub(super) fn into_transition(self) -> RetainedTransition {
        drop(self.retirement);
        self.transition
            .map(ExchangeStop::into_retained)
            .unwrap_or_else(|| RetainedTransition::configuration(self.control))
    }
}

/// Keep late replacement initialization owned before releasing the collection fence.
pub(super) enum RollbackRecoveryProgress {
    Waiting,
    Finished(Vec<SessionEvent>),
}

enum RollbackStage {
    Acquiring(JoinHandle<Result<OwnedSemaphorePermit, CommandFailure>>),
    Retiring(JoinHandle<Result<OwnedRwLockWriteGuard<()>, CommandFailure>>),
    Collecting,
    Prepare,
    Working {
        task: JoinHandle<Result<crate::broker::RollbackEffectResult, RollbackFailure>>,
        deadline: tokio::time::Instant,
    },
    Commit(Box<crate::broker::RollbackEffectResult>),
    Initializing {
        task: JoinHandle<
            Result<
                super::provider_lifecycle::ProviderInitialization,
                super::provider_lifecycle::ProviderLifecycleFailure,
            >,
        >,
        deadline: tokio::time::Instant,
    },
    InitializationReady(
        Result<
            super::provider_lifecycle::ProviderInitialization,
            super::provider_lifecycle::ProviderLifecycleFailure,
        >,
    ),
    InitializationFailed(super::provider_lifecycle::ProviderLifecycleFailure),
    Consumed,
}

/// Carry one correlated collection or file-work fact to the active command.
pub(super) enum RollbackEvent {
    Control(Result<OwnedSemaphorePermit, CommandFailure>),
    Retired(Result<OwnedRwLockWriteGuard<()>, CommandFailure>),
    Transition(TransitionEvent),
    Prepare,
    Worked(Result<Box<crate::broker::RollbackEffectResult>, RollbackFailure>),
    Commit,
    Initialized(
        Result<
            super::provider_lifecycle::ProviderInitialization,
            super::provider_lifecycle::ProviderLifecycleFailure,
        >,
    ),
    CommitInitialization,
    Deadline,
}

impl RollbackEvent {
    /// Get the finite effect label without recording restore paths or payloads.
    pub(super) fn name(&self) -> &'static str {
        match self {
            Self::Control(_) => "rollback_control_acquired",
            Self::Retired(_) => "rollback_generation_retired",
            Self::Transition(event) => event.name(),
            Self::Prepare => "rollback_effect_preparation",
            Self::Worked(_) => "rollback_effect_collected",
            Self::Commit => "rollback_effect_commit",
            Self::Initialized(_) => "rollback_provider_initialized",
            Self::CommitInitialization => "rollback_provider_commit",
            Self::Deadline => "rollback_effect_deadline",
        }
    }
}

/// Distinguish retained rollback progress from its finite typed command outcome.
pub(super) enum RollbackProgress {
    Waiting {
        events: Vec<SessionEvent>,
        input: Option<Box<crate::input::InputUpdate>>,
    },
    Finished {
        outcome: Result<RollbackOutcome, RollbackFailure>,
        events: Vec<SessionEvent>,
    },
}

impl RollbackProgress {
    /// Wait for another owner update without publishing uncommitted content.
    fn waiting() -> Self {
        Self::Waiting {
            events: Vec::new(),
            input: None,
        }
    }
}
