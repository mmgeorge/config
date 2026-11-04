//! Apply native configuration through owner-controlled validation, collection, and replacement handoff.

use super::command::{
    Command, CommandFailure, CommandOutcome, ConfigurationChange, ConfigurationOutcome,
    ContextCompactionOutcome, InvocationDisposition, PermissionChange,
};
use super::compaction::{CompactionEvent, CompactionProgress, ContextCompactionRun};
use super::failure::OperationFailure;
use super::queue::QueuedCommand;
use super::services::{OwnerServices, ProviderGenerationPermit};
use super::settings::{SettingsEvent, SettingsProgress, SettingsRun};
use super::transition::{
    ChildCollectionPolicy, ExchangeTransition, ForegroundStatus, RetainedTransition,
    TransitionEvent, TransitionFailure, TransitionIntent, TransitionOutcome, TransitionProgress,
};
use crate::broker::{
    ForegroundDispatch, HarnessBroker,
    publication::{SessionUpdate, SessionUpdateStream},
};
use crate::command::ExecutionPermit;
use crate::provider::ExchangeId;
use forge_protocol::message::SessionEvent;

/// Retain one configuration command through validation, collection, commit, and replacement handoff.
pub(super) struct NativeCommandRun {
    /// Durable command identity and every attached completion observer.
    pub(super) command: QueuedCommand,
    transition: Option<ExchangeTransition>,
    work: NativeWork,
    stage: NativeCommandStage,
    selected: Option<ExchangeId>,
    execution: Option<ExecutionPermit>,
    generation: Option<ProviderGenerationPermit>,
    control: Option<tokio::sync::OwnedSemaphorePermit>,
}

impl NativeCommandRun {
    /// Authorize only the replacement foreground prepared by this active command's current stage.
    pub(super) fn authorizes_foreground(&self, work: uuid::Uuid) -> bool {
        matches!(self.stage, NativeCommandStage::AwaitingDispatch { work: expected, .. } if expected == work)
    }
    /// Transfer collection decisions to the owner-held foreground in source order.
    pub(super) fn take_collection_signal(&mut self) -> Option<crate::broker::ForegroundSignal> {
        self.transition
            .as_mut()
            .and_then(ExchangeTransition::take_collection_signal)
    }
    /// Keep new tool effects closed while this command owns native collection or replacement.
    pub(super) fn closes_tool_admission(&self) -> bool {
        self.transition.is_some()
    }
    /// Preserve an already-yielded collection result when its owner cannot certify storage.
    pub(super) fn retain_transition_event(&mut self, event: TransitionEvent) {
        self.transition
            .as_mut()
            .expect("native collection retains its transition")
            .retain_uncommitted(event);
    }
    /// Retire yielded acquisition handles before fallible owner checks can transfer the command to cleanup.
    pub(super) fn consume_completed_worker(&mut self, event: &NativeCommandEvent) {
        if matches!(
            event,
            NativeCommandEvent::Control(_)
                | NativeCommandEvent::Observation(_)
                | NativeCommandEvent::Execution(_)
        ) {
            self.stage = NativeCommandStage::Consumed;
        }
    }

    /// Get a compaction attempt identity without exposing context contents or native tokens.
    pub(super) fn compaction_id(&self) -> Option<&super::command::ContextCompactionId> {
        match &self.work {
            NativeWork::Compaction(run) => run.attempt_id(),
            NativeWork::Settings(_) | NativeWork::Permission => None,
        }
    }

    /// Acquire compatibility control before generation observation and any configuration effect.
    pub(super) fn start(command: QueuedCommand, services: &OwnerServices) -> Self {
        let (work, native_authority) = match &command.record.request.command {
            Command::ConfigureSession(ConfigurationChange::Settings(patch)) => (
                NativeWork::Settings(Box::new(SettingsRun::new(
                    command.record.request.id.clone(),
                    patch.clone(),
                ))),
                patch.requires_native_authority(),
            ),
            Command::ConfigureSession(ConfigurationChange::Permission(
                PermissionChange::ExecutionPolicy { .. },
            )) => (NativeWork::Permission, true),
            Command::CompactContext(_) => {
                (NativeWork::Compaction(ContextCompactionRun::new()), true)
            }
            Command::ConfigureSession(
                ConfigurationChange::Permission(PermissionChange::SetCheckEditing { .. })
                | ConfigurationChange::CheckPreferences(_)
                | ConfigurationChange::InputTimeouts(_),
            )
            | Command::Agent(_)
            | Command::ContinueWorkflow(_)
            | Command::Rollback(_)
            | Command::Task(_)
            | Command::CancelExchange(_)
            | Command::Input(_) => unreachable!(
                "native lifecycle owns only settings, execution policy, and compaction"
            ),
        };
        let stage = if native_authority {
            let control = services.control.clone();
            NativeCommandStage::AcquiringControl(tokio::spawn(async move {
                match tokio::time::timeout(
                    std::time::Duration::from_secs(60),
                    control.acquire_owned(),
                )
                .await
                {
                    Ok(Ok(control)) => Ok(control),
                    Ok(Err(_)) | Err(_) => Err(CommandFailure::OwnershipUnavailable),
                }
            }))
        } else {
            NativeCommandStage::Work
        };
        Self {
            command,
            transition: None,
            work,
            stage,
            selected: None,
            execution: None,
            generation: None,
            control: None,
        }
    }

    /// Await one fact while the session driver continues consuming provider events and commands.
    pub(super) async fn next(&mut self, foreground: ForegroundStatus) -> NativeCommandEvent {
        match &mut self.stage {
            NativeCommandStage::AcquiringControl(task) => NativeCommandEvent::Control(
                task.await
                    .unwrap_or(Err(CommandFailure::OwnershipUnavailable)),
            ),
            NativeCommandStage::AcquiringObservation(task) => NativeCommandEvent::Observation(
                task.await
                    .unwrap_or(Err(CommandFailure::OwnershipUnavailable)),
            ),
            NativeCommandStage::Work => match &mut self.work {
                NativeWork::Settings(run) => NativeCommandEvent::Settings(run.next().await),
                NativeWork::Compaction(run) => NativeCommandEvent::Compaction(run.next().await),
                NativeWork::Permission => {
                    unreachable!("permission intent resolves before its native work stage")
                }
            },
            NativeCommandStage::Collecting => NativeCommandEvent::Transition(
                self.transition
                    .as_mut()
                    .expect("collection retains its transition")
                    .next(foreground)
                    .await,
            ),
            NativeCommandStage::AcquiringExecution(task) => NativeCommandEvent::Execution(
                task.await
                    .unwrap_or(Err(CommandFailure::OwnershipUnavailable)),
            ),
            NativeCommandStage::PreparingReplacement { .. }
            | NativeCommandStage::Ready(_)
            | NativeCommandStage::DispatchLost => NativeCommandEvent::Ready,
            NativeCommandStage::AwaitingDispatch { deadline, .. } => {
                tokio::time::sleep_until(*deadline).await;
                NativeCommandEvent::DispatchDeadline
            }
            NativeCommandStage::Consumed => std::future::pending().await,
        }
    }

    /// Consume a confirmed handoff only for this command's exact replacement work item.
    pub(super) fn observe_dispatch(
        &mut self,
        work: uuid::Uuid,
        confirmation: &forge_provider_api::submission::DispatchConfirmation,
    ) {
        if matches!(&self.stage, NativeCommandStage::AwaitingDispatch { work: expected, .. } if *expected == work)
        {
            self.stage = NativeCommandStage::Ready(InvocationDisposition::Dispatched {
                scope: confirmation.scope().clone(),
                binding: confirmation.binding().clone(),
            });
        }
    }

    /// Preserve uncertainty if its replacement ends without a confirmed handoff.
    pub(super) fn dispatch_lost(&mut self, work: uuid::Uuid) {
        if matches!(&self.stage, NativeCommandStage::AwaitingDispatch { work: expected, .. } if *expected == work)
        {
            self.stage = NativeCommandStage::DispatchLost;
        }
    }

    /// Commit policy and stage effects only in the unique session update owner.
    pub(super) fn update(
        &mut self,
        event: NativeCommandEvent,
        foreground: ForegroundStatus,
        broker: &mut HarnessBroker,
        services: &OwnerServices,
        can_commit: bool,
    ) -> Result<NativeCommandProgress, OperationFailure> {
        let event = match event {
            NativeCommandEvent::Transition(event) if !can_commit => {
                self.retain_transition_event(event);
                return Err(OperationFailure::StorageUncommitted);
            }
            event => event,
        };
        self.consume_completed_worker(&event);
        match event {
            NativeCommandEvent::Control(result) => {
                self.control = Some(result.map_err(|error| {
                    OperationFailure::Transition(TransitionFailure::Rejected(error))
                })?);
                // Legacy retirement acquires control before its provider write permit.
                // Preserve that order until those writers have moved into this owner.
                let observation = services.clone();
                self.stage = NativeCommandStage::AcquiringObservation(tokio::spawn(async move {
                    tokio::time::timeout(
                        std::time::Duration::from_secs(60),
                        observation.observe_provider_generation(),
                    )
                    .await
                    .map_err(|_| CommandFailure::OwnershipUnavailable)
                }));
                Ok(NativeCommandProgress::Waiting {
                    events: Vec::new(),
                    updates: Vec::new(),
                })
            }
            NativeCommandEvent::Observation(result) => {
                self.generation = Some(result.map_err(|error| {
                    OperationFailure::Transition(TransitionFailure::Rejected(error))
                })?);
                if matches!(self.work, NativeWork::Permission) {
                    let Command::ConfigureSession(ConfigurationChange::Permission(
                        PermissionChange::ExecutionPolicy { change },
                    )) = &self.command.record.request.command
                    else {
                        unreachable!(
                            "only permission intent requires execution-time settings resolution"
                        )
                    };
                    let patch = broker
                        .owner_permission_patch(*change)
                        .map_err(OperationFailure::Validation)?;
                    self.work = NativeWork::Settings(Box::new(SettingsRun::new(
                        self.command.record.request.id.clone(),
                        patch,
                    )));
                }
                self.stage = NativeCommandStage::Work;
                Ok(NativeCommandProgress::Waiting {
                    events: Vec::new(),
                    updates: Vec::new(),
                })
            }
            NativeCommandEvent::Settings(event) => {
                let NativeWork::Settings(settings) = &mut self.work else {
                    unreachable!("settings facts belong to the active settings work")
                };
                match settings.update(event, broker, can_commit)? {
                    SettingsProgress::Waiting => Ok(NativeCommandProgress::Waiting {
                        events: Vec::new(),
                        updates: Vec::new(),
                    }),
                    SettingsProgress::Collect(children) => {
                        self.begin_collection(children, foreground, services)
                    }
                    SettingsProgress::Committed { updates } => {
                        self.prepare_completion(broker, updates)
                    }
                }
            }
            NativeCommandEvent::Compaction(event) => {
                let NativeWork::Compaction(compaction) = &mut self.work else {
                    unreachable!("compaction facts belong to the active compaction work")
                };
                match compaction.update(event, broker, can_commit)? {
                    CompactionProgress::Collect(children) => {
                        self.begin_collection(children, foreground, services)
                    }
                    CompactionProgress::Waiting { updates } => Ok(NativeCommandProgress::Waiting {
                        events: Vec::new(),
                        updates,
                    }),
                    CompactionProgress::Committed { updates } => {
                        self.prepare_completion(broker, updates)
                    }
                }
            }
            NativeCommandEvent::Transition(event) => match self
                .transition
                .as_mut()
                .expect("collection facts belong to the active transition")
                .update(event, foreground, broker, services)?
            {
                TransitionProgress::Waiting { events, input } => {
                    debug_assert!(
                        input.is_none(),
                        "internal reconfiguration does not suspend caller input"
                    );
                    Ok(NativeCommandProgress::Waiting {
                        events,
                        updates: Vec::new(),
                    })
                }
                TransitionProgress::Finished {
                    outcome: TransitionOutcome::Reconfigured { exchange },
                    events,
                } => {
                    self.selected = exchange;
                    let execution = services.execution.clone();
                    self.stage = NativeCommandStage::AcquiringExecution(tokio::spawn(async move {
                        match tokio::time::timeout(
                            std::time::Duration::from_secs(60),
                            execution.acquire_owned(),
                        )
                        .await
                        {
                            Ok(Ok(permit)) => Ok(permit),
                            Ok(Err(_)) | Err(_) => Err(CommandFailure::OwnershipUnavailable),
                        }
                    }));
                    Ok(NativeCommandProgress::Waiting {
                        events,
                        updates: Vec::new(),
                    })
                }
                TransitionProgress::Finished {
                    outcome: TransitionOutcome::Cancelled(_),
                    ..
                } => {
                    unreachable!("permission changes never run explicit cancellation settlement")
                }
            },
            NativeCommandEvent::Execution(result) => {
                self.execution = Some(result.map_err(|_| OperationFailure::ExecutionUnavailable)?);
                match &mut self.work {
                    NativeWork::Settings(run) => run.collected(),
                    NativeWork::Compaction(run) => run.collected(),
                    NativeWork::Permission => {
                        unreachable!("permission intent resolves before collection")
                    }
                }
                self.stage = NativeCommandStage::Work;
                Ok(NativeCommandProgress::Waiting {
                    events: Vec::new(),
                    updates: Vec::new(),
                })
            }
            NativeCommandEvent::DispatchDeadline => {
                if matches!(self.stage, NativeCommandStage::Ready(_)) {
                    return Ok(NativeCommandProgress::Waiting {
                        events: Vec::new(),
                        updates: Vec::new(),
                    });
                }
                Err(OperationFailure::DispatchUnconfirmed)
            }
            NativeCommandEvent::Ready => {
                if !can_commit {
                    return Err(OperationFailure::StorageUncommitted);
                }
                let stage = std::mem::replace(&mut self.stage, NativeCommandStage::Consumed);
                match stage {
                    NativeCommandStage::PreparingReplacement { prompt } => {
                        let eligible = match self.selected.as_ref() {
                            Some(selected) => broker
                                .owner_resume_selected(selected)
                                .map_err(OperationFailure::Domain)?,
                            None => false,
                        };
                        let Some(prompt) = prompt.filter(|_| eligible) else {
                            self.stage =
                                NativeCommandStage::Ready(InvocationDisposition::Committed);
                            return Ok(NativeCommandProgress::Waiting {
                                events: Vec::new(),
                                updates: Vec::new(),
                            });
                        };
                        let (publication, updates) = crate::broker::publication::channel();
                        let Some(transaction) = broker
                            .begin_selected_continuation_dispatch(
                                self.selected
                                    .as_ref()
                                    .expect("eligible continuation retains its selected exchange"),
                                prompt,
                                publication,
                            )
                            .map_err(OperationFailure::Domain)?
                        else {
                            self.stage =
                                NativeCommandStage::Ready(InvocationDisposition::Committed);
                            return Ok(NativeCommandProgress::Waiting {
                                events: Vec::new(),
                                updates: Vec::new(),
                            });
                        };
                        let work = uuid::Uuid::new_v4();
                        self.stage = NativeCommandStage::AwaitingDispatch {
                            work,
                            deadline: tokio::time::Instant::now()
                                + std::time::Duration::from_secs(30),
                        };
                        Ok(NativeCommandProgress::Replacement(NativeCommandReplacement {
                            id: work, transaction, updates,
                            execution: self.execution.take().expect("collected policy changes acquired exclusive execution before commit"),
                        }))
                    }
                    NativeCommandStage::Ready(outcome) => {
                        self.stage = NativeCommandStage::Ready(outcome.clone());
                        let outcome = match &self.command.record.request.command {
                            Command::ConfigureSession(ConfigurationChange::Settings(patch)) => {
                                CommandOutcome::Configured(ConfigurationOutcome::Settings {
                                    disposition: outcome,
                                    ask_model: patch.question_model(),
                                })
                            }
                            Command::ConfigureSession(ConfigurationChange::Permission(
                                PermissionChange::ExecutionPolicy { .. },
                            )) => {
                                CommandOutcome::Configured(ConfigurationOutcome::ExecutionPolicy {
                                    disposition: outcome,
                                })
                            }
                            Command::CompactContext(_) => {
                                let NativeWork::Compaction(run) = &self.work else {
                                    unreachable!(
                                        "compaction completion retains its confirmed operation"
                                    )
                                };
                                CommandOutcome::Compacted(ContextCompactionOutcome {
                                    compaction: run.completed_id().clone(),
                                    disposition: outcome,
                                })
                            }
                            Command::ConfigureSession(
                                ConfigurationChange::Permission(
                                    PermissionChange::SetCheckEditing { .. },
                                )
                                | ConfigurationChange::CheckPreferences(_)
                                | ConfigurationChange::InputTimeouts(_),
                            )
                            | Command::Agent(_)
                            | Command::ContinueWorkflow(_)
                            | Command::Rollback(_)
                            | Command::Task(_)
                            | Command::CancelExchange(_)
                            | Command::Input(_) => unreachable!(
                                "native configuration completion matches its immutable command"
                            ),
                        };
                        Ok(NativeCommandProgress::Finished(outcome))
                    }
                    NativeCommandStage::DispatchLost => Err(OperationFailure::DispatchUnconfirmed),
                    NativeCommandStage::AcquiringControl(_)
                    | NativeCommandStage::AcquiringObservation(_)
                    | NativeCommandStage::Work
                    | NativeCommandStage::Collecting
                    | NativeCommandStage::AcquiringExecution(_)
                    | NativeCommandStage::AwaitingDispatch { .. }
                    | NativeCommandStage::Consumed => {
                        unreachable!("only owner-ready permission phases produce a ready event")
                    }
                }
            }
        }
    }

    /// Authorize one collection scope while retaining existing generation and control capabilities.
    fn begin_collection(
        &mut self,
        children: ChildCollectionPolicy,
        foreground: ForegroundStatus,
        services: &OwnerServices,
    ) -> Result<NativeCommandProgress, OperationFailure> {
        if self.generation.is_none() || self.control.is_none() {
            return Err(OperationFailure::ExecutionUnavailable);
        }
        if matches!(foreground, ForegroundStatus::Unavailable) {
            return Err(OperationFailure::Transition(TransitionFailure::Rejected(
                CommandFailure::CollectionUncertain,
            )));
        }
        self.transition = Some(ExchangeTransition::start(
            self.command.record.request.id.clone(),
            TransitionIntent::Reconfigure { children },
            services,
            self.generation.clone(),
            self.control.take(),
        ));
        self.stage = NativeCommandStage::Collecting;
        Ok(NativeCommandProgress::Waiting {
            events: Vec::new(),
            updates: Vec::new(),
        })
    }

    /// Publish committed operation state and let the owner select any surviving exchange continuation.
    fn prepare_completion(
        &mut self,
        broker: &mut HarnessBroker,
        mut updates: Vec<SessionUpdate>,
    ) -> Result<NativeCommandProgress, OperationFailure> {
        let completed = broker
            .owner_finish_configuration(
                Some(&self.command.record.request.id),
                self.selected.as_ref(),
            )
            .map_err(OperationFailure::Domain)?;
        updates.extend(completed.delivery.into_updates());
        self.stage = NativeCommandStage::PreparingReplacement {
            prompt: completed.resume,
        };
        Ok(NativeCommandProgress::Waiting {
            events: Vec::new(),
            updates,
        })
    }

    /// Transfer uncertain collection while the caller's durable failure is committed separately.
    pub(super) fn into_retained(self) -> (QueuedCommand, RetainedTransition) {
        let mut transition = self
            .transition
            .map(ExchangeTransition::into_retained)
            .unwrap_or_else(|| RetainedTransition::configuration(self.control));
        transition.retain_generation(self.generation);
        match self.stage {
            NativeCommandStage::AcquiringControl(task) => {
                transition.retain_job(tokio::spawn(async move {
                    task.await
                        .map(drop)
                        .map_err(|_| crate::provider::ProviderError::DeliveryUncertain)
                }))
            }
            NativeCommandStage::AcquiringObservation(task) => {
                transition.retain_job(tokio::spawn(async move {
                    task.await
                        .map(drop)
                        .map_err(|_| crate::provider::ProviderError::DeliveryUncertain)
                }))
            }
            NativeCommandStage::AcquiringExecution(task) => {
                transition.retain_job(tokio::spawn(async move {
                    task.await
                        .map(drop)
                        .map_err(|_| crate::provider::ProviderError::DeliveryUncertain)
                }))
            }
            NativeCommandStage::Work
            | NativeCommandStage::Collecting
            | NativeCommandStage::PreparingReplacement { .. }
            | NativeCommandStage::AwaitingDispatch { .. }
            | NativeCommandStage::Ready(_)
            | NativeCommandStage::DispatchLost
            | NativeCommandStage::Consumed => {}
        }
        match self.work {
            NativeWork::Settings(run) => {
                if let Some(job) = (*run).into_collection_job() {
                    transition.retain_job(job);
                }
            }
            NativeWork::Compaction(run) => run.retain(&mut transition),
            NativeWork::Permission => {}
        }
        (self.command, transition)
    }
}

/// Carry one identified native policy stage result back to the session owner.
pub(super) enum NativeCommandEvent {
    Control(Result<tokio::sync::OwnedSemaphorePermit, CommandFailure>),
    Observation(Result<ProviderGenerationPermit, CommandFailure>),
    Settings(SettingsEvent),
    Compaction(CompactionEvent),
    Transition(TransitionEvent),
    Execution(Result<ExecutionPermit, CommandFailure>),
    Ready,
    DispatchDeadline,
}

impl NativeCommandEvent {
    /// Get the finite log label without permission/prompt payloads.
    pub(super) fn name(&self) -> &'static str {
        match self {
            Self::Control(_) => "configuration_control_acquired",
            Self::Observation(_) => "configuration_observation_acquired",
            Self::Settings(event) => event.name(),
            Self::Compaction(event) => event.name(),
            Self::Transition(event) => event.name(),
            Self::Execution(_) => "execution_acquired",
            Self::Ready => "configuration_ready",
            Self::DispatchDeadline => "dispatch_deadline",
        }
    }
}

/// Distinguish published commit facts, replacement work, and command completion.
pub(super) enum NativeCommandProgress {
    Waiting {
        events: Vec<SessionEvent>,
        updates: Vec<SessionUpdate>,
    },
    Replacement(NativeCommandReplacement),
    Finished(CommandOutcome),
}

/// Transfer replacement resources into foreground ownership before confirming command completion.
pub(super) struct NativeCommandReplacement {
    pub(super) id: uuid::Uuid,
    pub(super) transaction: ForegroundDispatch,
    pub(super) updates: SessionUpdateStream,
    pub(super) execution: ExecutionPermit,
}

enum NativeCommandStage {
    AcquiringControl(
        tokio::task::JoinHandle<Result<tokio::sync::OwnedSemaphorePermit, CommandFailure>>,
    ),
    AcquiringObservation(tokio::task::JoinHandle<Result<ProviderGenerationPermit, CommandFailure>>),
    Work,
    Collecting,
    AcquiringExecution(tokio::task::JoinHandle<Result<ExecutionPermit, CommandFailure>>),
    PreparingReplacement {
        prompt: Option<String>,
    },
    AwaitingDispatch {
        work: uuid::Uuid,
        deadline: tokio::time::Instant,
    },
    Ready(InvocationDisposition),
    DispatchLost,
    Consumed,
}

/// Keep mutually exclusive operation work together instead of parallel optional stage fields.
enum NativeWork {
    Settings(Box<SettingsRun>),
    Permission,
    Compaction(ContextCompactionRun),
}
