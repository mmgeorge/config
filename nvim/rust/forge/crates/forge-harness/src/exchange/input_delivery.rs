//! Own queued input preparation, captured steering, native work, and receipt settlement in the driver.

use std::sync::Arc;

use forge_protocol::{message::SessionEvent, outbound::MessageSender};
use tokio::{
    sync::{OwnedSemaphorePermit, oneshot},
    task::JoinHandle,
};

use super::{
    command::{InputId, InputRevision, QueuedModelSettings},
    compaction::{CompactionEvent, CompactionProgress, ContextCompactionRun},
    failure::OperationFailure,
    input_receipts::{
        InputAbandonment, InputDispatchActivity, InputExecutionResult, InputReceiptFact,
        InputReceipts,
    },
    services::{OwnerServices, ProviderGenerationPermit},
    settings::{SettingsEvent, SettingsProgress, SettingsRun},
    transition::{
        ChildCollectionPolicy, ExchangeTransition, ForegroundStatus, RetainedTransition,
        TransitionEvent, TransitionOutcome, TransitionProgress,
    },
};
use crate::{
    broker::{
        DispatchResult, ForegroundDispatch, HarnessBroker,
        publication::{SessionUpdate, SessionUpdateStream},
    },
    command::ExecutionPermit,
    input::{InputDelivery, InputDeliveryGuard, InputMessage, InputUpdate},
    provider::submission::{
        Submission, SubmissionOrigin, SubmissionReconciliation, SubmissionState,
    },
};

/// Retain owner-acquired root capabilities until a queue claim installs its delivery stages.
pub(crate) struct InputDeliveryOffer {
    /// Exact queued revision selected at the owner mutation boundary.
    pub(crate) input: InputRevision,
    /// Exclude overlapping root execution before the owner claims the input.
    pub(crate) execution: ExecutionPermit,
    /// Preserve control-before-generation lock order during the remaining migration.
    pub(crate) control: OwnedSemaphorePermit,
}

/// Retain owner-acquired steering capabilities until its claimed input is installed.
pub(crate) struct SteeringDeliveryOffer {
    /// Exact queued revision selected at the owner mutation boundary.
    pub(crate) input: InputRevision,
    /// Preserve control-before-generation ordering throughout preparation.
    pub(crate) control: OwnedSemaphorePermit,
    /// Keep admission ordered until the selected queue owns this steering message.
    pub(crate) order: OwnedSemaphorePermit,
}

/// Capture a steering destination without selecting a newer invocation after asynchronous preparation.
#[derive(Clone)]
pub(super) struct SteeringDispatchTarget {
    parent: crate::provider::SteeringTarget,
    registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
    owner: crate::provider::ThreadOwner,
    engine: Arc<tokio::sync::OnceCell<Arc<crate::provider::session_execution::SessionExecution>>>,
}

impl SteeringDispatchTarget {
    /// Capture one active parent receiver, retaining the corresponding execution registry.
    pub(super) fn parent(
        target: crate::provider::SteeringTarget,
        registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
        owner: crate::provider::ThreadOwner,
    ) -> Self {
        Self {
            parent: target,
            registry,
            owner,
            engine: Default::default(),
        }
    }

    /// Deliver to the captured receiver/scope using one retained engine for dispatch and collection.
    async fn steer(
        &self,
        generation: &ProviderGenerationPermit,
        input: crate::provider::SteeringInput,
    ) -> Result<(), crate::provider::ProviderError> {
        self.execution(generation).await?;
        self.parent.steer(input).await
    }

    /// Fence the exact engine captured before dispatch, rather than looking up its replacement.
    async fn fence(
        &self,
        generation: &ProviderGenerationPermit,
    ) -> Result<(), crate::provider::ProviderError> {
        self.execution(generation).await?.close().await
    }

    /// Get one immutable engine binding under the admitted provider generation.
    async fn execution(
        &self,
        generation: &ProviderGenerationPermit,
    ) -> Result<
        &Arc<crate::provider::session_execution::SessionExecution>,
        crate::provider::ProviderError,
    > {
        if !self.registry.matches(generation.provider()) {
            return Err(crate::provider::ProviderError::Failure(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Protocol,
                ),
            ));
        }
        self.engine
            .get_or_try_init(|| self.registry.owner(self.owner.clone()))
            .await
    }
}

enum InputDestination {
    Foreground,
    Steering {
        target: SteeringDispatchTarget,
        order: Option<OwnedSemaphorePermit>,
    },
}

impl InputDestination {
    /// Query exact activity only when a prepared receipt could safely reenter the queue.
    fn receipt_activity(
        &self,
        delivery: &SubmissionState,
    ) -> Result<InputDispatchActivity, OperationFailure> {
        if !matches!(delivery, SubmissionState::Prepared) {
            return Ok(InputDispatchActivity::Active);
        }
        Ok(match self {
            Self::Foreground => InputDispatchActivity::Active,
            Self::Steering { target, .. } => {
                if target.parent.is_open() {
                    InputDispatchActivity::Active
                } else {
                    InputDispatchActivity::Idle
                }
            }
        })
    }
}

/// Retain one claimed input from preparation through its final receipt application.
pub(super) struct InputDeliveryRun {
    pub(super) input: InputRevision,
    pub(super) messages: Option<MessageSender>,
    message: InputMessage,
    kind: DeliveryKind,
    work: PreparationWork,
    stage: DeliveryStage,
    observation: InputObservation,
    transition: Option<ExchangeTransition>,
    selected: Option<crate::provider::ExchangeId>,
    collected: bool,
    execution: Option<ExecutionPermit>,
    control: Option<OwnedSemaphorePermit>,
    generation: Option<ProviderGenerationPermit>,
    terminal_events: Vec<SessionEvent>,
    destination: InputDestination,
}

impl InputDeliveryRun {
    /// Install identified delivery and owned capabilities before starting preparation I/O.
    pub(super) fn start(
        message: InputMessage,
        execution: ExecutionPermit,
        control: OwnedSemaphorePermit,
        messages: Option<MessageSender>,
        services: &OwnerServices,
    ) -> Self {
        Self::start_preparation(
            message,
            Some(execution),
            control,
            messages,
            services,
            InputDestination::Foreground,
        )
    }

    /// Install steering under the same owner preparation and native receipt pipeline as queued prompts.
    pub(super) fn start_steering(
        message: InputMessage,
        control: OwnedSemaphorePermit,
        order: OwnedSemaphorePermit,
        target: SteeringDispatchTarget,
        messages: MessageSender,
        services: &OwnerServices,
    ) -> Self {
        Self::start_preparation(
            message,
            None,
            control,
            Some(messages),
            services,
            InputDestination::Steering {
                target,
                order: Some(order),
            },
        )
    }

    /// Retain one input's preparation resources before any asynchronous work begins.
    fn start_preparation(
        message: InputMessage,
        execution: Option<ExecutionPermit>,
        control: OwnedSemaphorePermit,
        messages: Option<MessageSender>,
        services: &OwnerServices,
        destination: InputDestination,
    ) -> Self {
        let input = InputRevision {
            id: InputId::new(message.id.clone()).expect("accepted input identities are nonempty"),
            revision: message.revision,
        };
        let kind = if message.text.trim().is_empty() {
            DeliveryKind::ConfigurationOnly
        } else if message.text == "/compact" {
            DeliveryKind::Compact
        } else {
            DeliveryKind::Prompt
        };
        let configured = message
            .config
            .clone()
            .map(QueuedModelSettings::decode)
            .transpose()
            .map_err(|error| OperationFailure::Validation(error.into()))
            .and_then(|selection| {
                selection
                    .filter(|settings| !settings.is_empty())
                    .map(SettingsRun::for_input)
                    .transpose()
                    .map_err(|error| OperationFailure::Validation(error.into()))
            });
        let (work, stage) = match configured {
            Ok(settings) => {
                let observation = services.clone();
                (
                    settings
                        .map(|run| PreparationWork::Settings(Box::new(run)))
                        .unwrap_or(PreparationWork::None),
                    DeliveryStage::Observation {
                        task: tokio::spawn(async move {
                            observation.observe_provider_generation().await
                        }),
                    },
                )
            }
            Err(error) => (PreparationWork::None, DeliveryStage::Rejected(Some(error))),
        };
        Self {
            input,
            messages,
            message,
            kind,
            work,
            stage,
            observation: InputObservation::Waiting,
            transition: None,
            selected: None,
            collected: false,
            execution,
            control: Some(control),
            generation: None,
            terminal_events: Vec::new(),
            destination,
        }
    }

    /// Keep configuration effects exclusive only through finite preparation, never through the answer.
    pub(super) fn preparing(&self) -> bool {
        !matches!(
            self.stage,
            DeliveryStage::Foreground { .. }
                | DeliveryStage::Reconcile { .. }
                | DeliveryStage::Steering { .. }
                | DeliveryStage::Replacement { .. }
                | DeliveryStage::Consumed
        )
    }

    /// Prevent later input from overtaking this input's initial native handoff.
    pub(super) fn blocks_input_admission(&self) -> bool {
        self.preparing()
            || matches!(
                self.stage,
                DeliveryStage::Foreground {
                    confirmed: false,
                    ..
                }
            )
    }

    /// Await owned I/O or foreground completion without blocking owner intake and native events.
    pub(super) async fn next(&mut self, foreground: ForegroundStatus) -> InputDeliveryEvent {
        if matches!(self.observation, InputObservation::Pending) {
            return std::future::pending().await;
        }
        let event = self.poll_next(foreground).await;
        // The owner can buffer this event while applying other ready facts.
        // Its completed worker must stay unpolled until the event is applied.
        self.observation = InputObservation::Pending;
        event
    }

    /// Observe the current stage once without changing its owner-applied state.
    async fn poll_next(&mut self, foreground: ForegroundStatus) -> InputDeliveryEvent {
        match &mut self.stage {
            DeliveryStage::Observation { task } => InputDeliveryEvent::Observation(task.await),
            DeliveryStage::Work => match &mut self.work {
                PreparationWork::Settings(run) => InputDeliveryEvent::Settings(run.next().await),
                PreparationWork::Compaction(run) => {
                    InputDeliveryEvent::Compaction(run.next().await)
                }
                PreparationWork::None => InputDeliveryEvent::Ready,
            },
            DeliveryStage::Collecting => InputDeliveryEvent::Transition(
                self.transition
                    .as_mut()
                    .expect("input collection retains its transition")
                    .next(foreground)
                    .await,
            ),
            DeliveryStage::Attachments { task } => InputDeliveryEvent::Attachments(task.await),
            DeliveryStage::Receipt { task, .. } => InputDeliveryEvent::Receipt(task.await),
            DeliveryStage::Foreground { response, .. } => {
                InputDeliveryEvent::Foreground(response.await)
            }
            DeliveryStage::Steering { task, queued, .. } => tokio::select! {
                result = task => InputDeliveryEvent::Steered(result),
                handoff = async { match queued {
                    Some(queued) => { let mut observed = queued.clone(); observed.wait_for(|queued| *queued).await.map(drop) },
                    None => std::future::pending().await,
                }} => InputDeliveryEvent::SteeringQueued(handoff),
            },
            DeliveryStage::Reconcile { task, .. } => {
                InputDeliveryEvent::Reconciled(Box::new(task.await))
            }
            DeliveryStage::Replacement {
                response,
                confirmed,
                ..
            } => {
                if *confirmed {
                    return InputDeliveryEvent::Ready;
                }
                InputDeliveryEvent::Foreground(response.await)
            }
            DeliveryStage::Local | DeliveryStage::Rejected(_) => InputDeliveryEvent::Ready,
            DeliveryStage::Consumed => std::future::pending().await,
        }
    }

    /// Release preparation only after this input's exact foreground work confirms native handoff.
    pub(super) fn observe_dispatch(&mut self, id: uuid::Uuid) {
        if let DeliveryStage::Replacement {
            work, confirmed, ..
        }
        | DeliveryStage::Foreground {
            work, confirmed, ..
        } = &mut self.stage
            && *work == id
        {
            *confirmed = true;
        }
    }

    /// Apply facts and authorize the next input effect only inside the persistent lifecycle owner.
    pub(super) fn update(
        &mut self,
        event: InputDeliveryEvent,
        foreground: ForegroundStatus,
        broker: &mut HarnessBroker,
        services: &OwnerServices,
        receipts: &mut InputReceipts,
        can_commit: bool,
    ) -> Result<InputDeliveryProgress, OperationFailure> {
        self.observation = InputObservation::Waiting;
        match event {
            InputDeliveryEvent::Observation(result) => {
                self.stage = DeliveryStage::Consumed;
                self.generation = Some(result.map_err(OperationFailure::JobObservationLost)?);
                if !can_commit {
                    return Err(OperationFailure::StorageUncommitted);
                }
                if let Some(reason) = self.steering_block_reason(services)? {
                    return self.finish_unprepared_steering(reason, services);
                }
                if let Some(reason) = self.interruption(services)? {
                    return self.finish_interrupted(reason, None, receipts, services);
                }
                self.stage = DeliveryStage::Work;
                if matches!(self.work, PreparationWork::None) {
                    self.after_settings(services);
                }
                Ok(InputDeliveryProgress::waiting())
            }
            InputDeliveryEvent::Settings(event) => {
                let PreparationWork::Settings(run) = &mut self.work else {
                    unreachable!("settings fact belongs to input settings preparation")
                };
                match run.update(event, broker, can_commit)? {
                    SettingsProgress::Waiting => Ok(InputDeliveryProgress::waiting()),
                    SettingsProgress::Collect(children) => {
                        self.collect(children, services);
                        Ok(InputDeliveryProgress::waiting())
                    }
                    SettingsProgress::Committed { mut updates } => {
                        let configured = broker
                            .owner_finish_configuration(None, None, None)
                            .map_err(OperationFailure::Domain)?;
                        updates.extend(configured.delivery.into_updates());
                        self.work = PreparationWork::None;
                        self.after_settings(services);
                        Ok(InputDeliveryProgress::Waiting {
                            events: Vec::new(),
                            updates,
                            input: None,
                        })
                    }
                }
            }
            InputDeliveryEvent::Compaction(event) => {
                let PreparationWork::Compaction(run) = &mut self.work else {
                    unreachable!("compaction fact belongs to queued compaction")
                };
                match run.update(event, broker, can_commit)? {
                    CompactionProgress::Collect(children) => {
                        self.collect(children, services);
                        Ok(InputDeliveryProgress::waiting())
                    }
                    CompactionProgress::Waiting { updates } => Ok(InputDeliveryProgress::Waiting {
                        events: Vec::new(),
                        updates,
                        input: None,
                    }),
                    CompactionProgress::Committed { mut updates } => {
                        let completed = broker
                            .owner_finish_configuration(None, self.selected.as_ref(), None)
                            .map_err(OperationFailure::Domain)?;
                        updates.extend(completed.delivery.into_updates());
                        if let Some(prompt) = completed.resume {
                            let (publication, stream) = crate::broker::publication::channel();
                            if let Some(transaction) = broker
                                .begin_selected_continuation_dispatch(
                                    self.selected
                                        .as_ref()
                                        .expect("resumed compaction retains its selected exchange"),
                                    prompt,
                                    publication,
                                )
                                .map_err(OperationFailure::Domain)?
                            {
                                let id = uuid::Uuid::new_v4();
                                let (completion, response) = oneshot::channel();
                                self.stage = DeliveryStage::Replacement {
                                    work: id,
                                    response,
                                    confirmed: false,
                                };
                                self.release_preparation();
                                return Ok(InputDeliveryProgress::Foreground(
                                    InputForegroundStart {
                                        id,
                                        source: InputForegroundSource::Continuation,
                                        transaction,
                                        updates: stream,
                                        execution: self
                                            .execution
                                            .take()
                                            .expect("queued compaction retains root execution"),
                                        completion,
                                        committed: updates,
                                        input: None,
                                    },
                                ));
                            }
                        }
                        self.stage = DeliveryStage::Local;
                        Ok(InputDeliveryProgress::Waiting {
                            events: Vec::new(),
                            updates,
                            input: None,
                        })
                    }
                }
            }
            InputDeliveryEvent::Transition(event) => {
                let progress = self
                    .transition
                    .as_mut()
                    .expect("input collection retains its state")
                    .update(event, foreground, broker, services)?;
                if !can_commit {
                    return Err(OperationFailure::StorageUncommitted);
                }
                match progress {
                    TransitionProgress::Waiting { events, input } => {
                        Ok(InputDeliveryProgress::Waiting {
                            events,
                            updates: Vec::new(),
                            input: input.map(|input| *input),
                        })
                    }
                    TransitionProgress::Finished {
                        outcome: TransitionOutcome::Reconfigured { exchange },
                        events,
                    } => {
                        self.selected = exchange.or_else(|| self.selected.take());
                        self.collected = true;
                        match &mut self.work {
                            PreparationWork::Settings(run) => run.collected(),
                            PreparationWork::Compaction(run) => run.collected(),
                            PreparationWork::None => self.after_settings(services),
                        }
                        if !matches!(self.work, PreparationWork::None) {
                            self.stage = DeliveryStage::Work;
                        }
                        Ok(InputDeliveryProgress::Waiting {
                            events,
                            updates: Vec::new(),
                            input: None,
                        })
                    }
                    TransitionProgress::Finished {
                        outcome: TransitionOutcome::Cancelled(_),
                        ..
                    } => unreachable!("input preparation does not issue user cancellation"),
                }
            }
            InputDeliveryEvent::Attachments(result) => {
                self.stage = DeliveryStage::Consumed;
                let attachments = result
                    .map_err(OperationFailure::JobObservationLost)?
                    .map_err(|error| {
                        OperationFailure::Validation(
                            error.context(super::command::CommandFailure::AttachmentUnavailable),
                        )
                    })?;
                if !can_commit {
                    return Err(OperationFailure::StorageUncommitted);
                }
                if let Some(reason) = self.steering_block_reason(services)? {
                    return self.finish_unprepared_steering(reason, services);
                }
                if let Some(reason) = self.interruption(services)? {
                    return self.finish_interrupted(reason, None, receipts, services);
                }
                let provider = self
                    .generation
                    .as_ref()
                    .expect("input preparation owns its generation")
                    .provider()
                    .descriptor()
                    .kind;
                let input = match &self.message.purpose {
                    crate::input::InputPurpose::Question { reply, .. } => {
                        crate::provider::ProviderInput::QuestionReply {
                            context: String::new(),
                            reply: reply.clone(),
                            attachments: attachments.clone(),
                        }
                    }
                    crate::input::InputPurpose::Clarification { .. }
                    | crate::input::InputPurpose::User
                    | crate::input::InputPurpose::QuestionContinuation { .. }
                    | crate::input::InputPurpose::WorkflowContinuation { .. }
                    | crate::input::InputPurpose::TaskEntry { .. }
                    | crate::input::InputPurpose::TaskContinuation { .. } => {
                        crate::provider::ProviderInput::from_text(self.message.text.clone())
                            .with_attachments(attachments.clone())
                    }
                };
                let purpose = self.message.purpose.clone();
                let request = broker
                    .input_submission_request(input, crate::provider::PromptMode::Chat)
                    .map_err(OperationFailure::Domain)?;
                let origin = match purpose {
                    crate::input::InputPurpose::TaskEntry { id, .. }
                    | crate::input::InputPurpose::TaskContinuation { id, .. } => {
                        SubmissionOrigin::TaskInput(id)
                    }
                    crate::input::InputPurpose::Clarification { .. }
                    | crate::input::InputPurpose::User
                    | crate::input::InputPurpose::Question { .. }
                    | crate::input::InputPurpose::QuestionContinuation { .. }
                    | crate::input::InputPurpose::WorkflowContinuation { .. } => {
                        SubmissionOrigin::QueuedInput(self.input.id.clone())
                    }
                };
                let journal = services.submissions.clone();
                let task = tokio::spawn(async move {
                    journal
                        .persist_input_request(request, provider, origin)
                        .await
                });
                self.stage = DeliveryStage::Receipt { task, attachments };
                Ok(InputDeliveryProgress::waiting())
            }
            InputDeliveryEvent::Receipt(result) => {
                let DeliveryStage::Receipt { attachments, .. } =
                    std::mem::replace(&mut self.stage, DeliveryStage::Consumed)
                else {
                    unreachable!("receipt result belongs to its attachment preparation")
                };
                let submission = result
                    .map_err(OperationFailure::JobObservationLost)?
                    .map_err(|error| {
                        OperationFailure::Validation(
                            anyhow::Error::new(error)
                                .context(super::command::CommandFailure::PrerequisiteFailed),
                        )
                    })?;
                if !can_commit {
                    return Err(OperationFailure::StorageUncommitted);
                }
                let input = receipts
                    .apply(
                        InputReceiptFact::Prepared {
                            input: self.input.clone(),
                            submission: submission.clone(),
                        },
                        services,
                    )
                    .map_err(|error| OperationFailure::Domain(error.into()))?;
                if matches!(
                    submission
                        .record()
                        .map_err(OperationFailure::Domain)?
                        .delivery,
                    SubmissionState::Accepted { .. }
                ) {
                    self.begin_reconcile(
                        submission,
                        InputExecutionResult::Completed,
                        Vec::new(),
                        None,
                        services,
                    );
                    self.release_preparation();
                    self.execution.take();
                    return Ok(InputDeliveryProgress::Waiting {
                        events: Vec::new(),
                        updates: Vec::new(),
                        input,
                    });
                }
                if matches!(self.destination, InputDestination::Steering { .. })
                    && let Some(reason) = self.steering_block_reason(services)?
                {
                    let changed = receipts
                        .apply(
                            InputReceiptFact::Abandoned {
                                submission: submission.receipt_id().clone(),
                                reason,
                            },
                            services,
                        )
                        .map_err(|error| OperationFailure::Domain(error.into()))?;
                    self.release_steering_order(services);
                    return Ok(InputDeliveryProgress::Finished {
                        events: Vec::new(),
                        input: changed,
                        completion: None,
                    });
                }
                if let Some(reason) = self.interruption(services)? {
                    return self.finish_interrupted(reason, Some(&submission), receipts, services);
                }
                let message = receipts
                    .capture_dispatch(&self.input, submission.receipt_id(), services)
                    .map_err(|error| OperationFailure::Domain(error.into()))?;
                if let InputDestination::Steering { target, .. } = &self.destination {
                    let flight = Arc::new(SteeringFlight {
                        target: target.clone(),
                        generation: self
                            .generation
                            .as_ref()
                            .expect("steering preparation owns a provider generation")
                            .clone(),
                        _receipt: submission.begin_steering().map_err(|error| {
                            OperationFailure::InputReconciliation(
                                crate::provider::submission::journal_failure(error),
                            )
                        })?,
                    });
                    let steering = match &message.purpose {
                        crate::input::InputPurpose::TaskEntry {
                            start: Some(start), ..
                        } => crate::provider::SteeringInput::TaskEntry {
                            start: start.clone(),
                            text: message.text,
                            submission: submission.clone(),
                        },
                        crate::input::InputPurpose::TaskEntry { start: None, .. } => {
                            crate::provider::SteeringInput::User {
                                text: message.text,
                                attachments: Vec::new(),
                                submission: Some(submission.clone()),
                            }
                        }
                        crate::input::InputPurpose::Clarification { .. }
                        | crate::input::InputPurpose::User
                        | crate::input::InputPurpose::Question { .. }
                        | crate::input::InputPurpose::QuestionContinuation { .. }
                        | crate::input::InputPurpose::WorkflowContinuation { .. }
                        | crate::input::InputPurpose::TaskContinuation { .. } => {
                            crate::provider::SteeringInput::User {
                                text: message.text,
                                attachments,
                                submission: Some(submission.clone()),
                            }
                        }
                    };
                    let transport = flight.clone();
                    let queued = submission.steering_queued();
                    self.stage = DeliveryStage::Steering {
                        task: tokio::spawn(async move {
                            transport
                                .target
                                .steer(&transport.generation, steering)
                                .await
                        }),
                        submission,
                        queued: Some(queued),
                        flight,
                    };
                    self.control.take();
                    return Ok(InputDeliveryProgress::Waiting {
                        events: Vec::new(),
                        updates: Vec::new(),
                        input,
                    });
                }
                let (publication, stream) = crate::broker::publication::channel();
                let transaction = broker
                    .begin_queued_input_dispatch(
                        message,
                        submission.clone(),
                        attachments,
                        publication,
                    )
                    .map_err(OperationFailure::Domain)?;
                let id = uuid::Uuid::new_v4();
                let receipt = submission.receipt_id().clone();
                let (completion, response) = oneshot::channel();
                self.stage = DeliveryStage::Foreground {
                    work: id,
                    response,
                    submission,
                    confirmed: false,
                };
                self.release_preparation();
                services.cancellation.arm();
                Ok(InputDeliveryProgress::Foreground(InputForegroundStart {
                    id,
                    source: InputForegroundSource::Input(receipt),
                    transaction,
                    updates: stream,
                    execution: self
                        .execution
                        .take()
                        .expect("claimed root input retains execution until foreground transfer"),
                    completion,
                    committed: Vec::new(),
                    input,
                }))
            }
            InputDeliveryEvent::Foreground(result) => {
                if matches!(self.stage, DeliveryStage::Replacement { .. }) {
                    self.stage = DeliveryStage::Consumed;
                    if let Ok(Ok(result)) = result {
                        self.terminal_events.extend(result.event);
                    }
                    return Err(OperationFailure::DispatchUnconfirmed);
                }
                let DeliveryStage::Foreground { submission, .. } =
                    std::mem::replace(&mut self.stage, DeliveryStage::Consumed)
                else {
                    unreachable!("foreground completion belongs to its input")
                };
                let result = result.map_err(|error| OperationFailure::Domain(error.into()))?;
                if !can_commit {
                    return Err(OperationFailure::StorageUncommitted);
                }
                let (execution, events) = match result {
                    Ok(result)
                        if result.response.error().is_some_and(|error| {
                            matches!(error.code.as_str(), "turn_retracted" | "turn_reconfigured")
                        }) =>
                    {
                        let input = receipts
                            .apply(
                                InputReceiptFact::Abandoned {
                                    submission: submission.receipt_id().clone(),
                                    reason: if result
                                        .response
                                        .error()
                                        .is_some_and(|error| error.code == "turn_reconfigured")
                                    {
                                        InputAbandonment::Reconfigured
                                    } else {
                                        InputAbandonment::Retracted
                                    },
                                },
                                services,
                            )
                            .map_err(|error| OperationFailure::Domain(error.into()))?;
                        return Ok(InputDeliveryProgress::Finished {
                            events: result.event,
                            input,
                            completion: None,
                        });
                    }
                    Ok(result) => (
                        match result.response.error() {
                            None => InputExecutionResult::Completed,
                            Some(error) if error.code == "turn_cancelled" => {
                                InputExecutionResult::Cancelled
                            }
                            Some(_) => InputExecutionResult::Failed,
                        },
                        result.event,
                    ),
                    Err(error) if error.is::<crate::broker::TurnCancelled>() => {
                        (InputExecutionResult::Cancelled, Vec::new())
                    }
                    Err(_) => (InputExecutionResult::Failed, Vec::new()),
                };
                self.begin_reconcile(submission, execution, events, None, services);
                Ok(InputDeliveryProgress::waiting())
            }
            InputDeliveryEvent::SteeringQueued(result) => {
                let DeliveryStage::Steering { queued, .. } = &mut self.stage else {
                    unreachable!("handoff belongs to its retained steering worker")
                };
                // Observe handoff once; the native acknowledgment and actual worker remain owned.
                queued.take();
                result.map_err(|_| {
                    OperationFailure::InputReconciliation(
                        crate::provider::ProviderError::DeliveryUncertain,
                    )
                })?;
                self.release_steering_order(services);
                Ok(InputDeliveryProgress::waiting())
            }
            InputDeliveryEvent::Steered(result) => {
                let DeliveryStage::Steering {
                    submission, flight, ..
                } = std::mem::replace(&mut self.stage, DeliveryStage::Consumed)
                else {
                    unreachable!("steering result retains its receipt until owner settlement")
                };
                let outcome = result.map_err(OperationFailure::JobObservationLost)?;
                if !can_commit {
                    return Err(OperationFailure::StorageUncommitted);
                }
                self.release_steering_order(services);
                let prepared = submission
                    .record()
                    .map_err(OperationFailure::Domain)?
                    .delivery
                    == SubmissionState::Prepared;
                let retired_parent = !flight.target.parent.is_open();
                if outcome.is_err()
                    && prepared
                    && (retired_parent || *services.cancellation.subscribe().borrow())
                {
                    let input = receipts
                        .apply(
                            InputReceiptFact::Abandoned {
                                submission: submission.receipt_id().clone(),
                                reason: InputAbandonment::Paused,
                            },
                            services,
                        )
                        .map_err(|error| OperationFailure::Domain(error.into()))?;
                    return Ok(InputDeliveryProgress::Finished {
                        events: Vec::new(),
                        input,
                        completion: None,
                    });
                }
                let (execution, failure) = match outcome {
                    Ok(()) => (InputExecutionResult::Completed, None),
                    Err(error) if error.is_clean_cancellation() => {
                        (InputExecutionResult::Cancelled, None)
                    }
                    Err(error) => (InputExecutionResult::Failed, Some(error)),
                };
                self.begin_reconcile(submission, execution, Vec::new(), failure, services);
                Ok(InputDeliveryProgress::waiting())
            }
            InputDeliveryEvent::Reconciled(result) => {
                let DeliveryStage::Reconcile {
                    submission,
                    result: execution,
                    mut events,
                    steering_failure,
                    ..
                } = std::mem::replace(&mut self.stage, DeliveryStage::Consumed)
                else {
                    unreachable!("receipt reconciliation belongs to settled input work")
                };
                let ReconciledInput { observation } = (*result)
                    .map_err(OperationFailure::JobObservationLost)?
                    .map_err(OperationFailure::InputReconciliation)?;
                if !can_commit {
                    return Err(OperationFailure::StorageUncommitted);
                }
                // Receipt observation can follow acceptance, cancellation, or a task revision change.
                // Reconcile the original attempt and let the receipt guard authorize the queue commit.
                let record = submission
                    .apply_reconciliation(observation)
                    .map_err(|error| {
                        OperationFailure::InputReconciliation(
                            crate::provider::submission::journal_failure(error),
                        )
                    })?;
                let activity = self.destination.receipt_activity(&record.delivery)?;
                if let Some(error) = steering_failure
                    && !(matches!(record.delivery, SubmissionState::Prepared)
                        && matches!(activity, InputDispatchActivity::Idle))
                {
                    events.push(SessionEvent { session_id: services.observer.session_id().into(), event: "provider_event".into(),
                        payload: serde_json::json!({"kind":"error","code":"input_steering_failed","text":error.to_string()}) });
                }
                let input = receipts
                    .apply(
                        InputReceiptFact::Settled {
                            submission: submission.receipt_id().clone(),
                            result: execution,
                            activity,
                        },
                        services,
                    )
                    .map_err(|error| OperationFailure::Domain(error.into()))?;
                if matches!(record.delivery, SubmissionState::Accepted { .. })
                    && matches!(
                        self.message.purpose,
                        crate::input::InputPurpose::TaskEntry { .. }
                            | crate::input::InputPurpose::TaskContinuation { .. }
                    )
                {
                    events.push(SessionEvent {
                        session_id: services.observer.session_id().into(),
                        event: "session_changed".into(),
                        payload: serde_json::json!({"snapshot":
                            services
                                .observer
                                .snapshot()
                                .map_err(OperationFailure::Domain)?
                        }),
                    });
                }
                Ok(InputDeliveryProgress::Finished {
                    events,
                    input,
                    completion: None,
                })
            }
            InputDeliveryEvent::Ready => {
                if !can_commit {
                    return Err(OperationFailure::StorageUncommitted);
                }
                match &mut self.stage {
                    DeliveryStage::Rejected(error) => Err(error
                        .take()
                        .expect("input preflight rejection is consumed once")),
                    DeliveryStage::Local
                    | DeliveryStage::Replacement {
                        confirmed: true, ..
                    } => {
                        let input = services
                            .input
                            .delivery_if_current(
                                self.input.id.as_str(),
                                InputDeliveryGuard::Claimed(self.input.revision),
                                None,
                                None,
                                None,
                            )
                            .map_err(OperationFailure::Domain)?;
                        let completion =
                            match std::mem::replace(&mut self.stage, DeliveryStage::Consumed) {
                                DeliveryStage::Local => None,
                                DeliveryStage::Replacement { work, response, .. } => {
                                    Some(InputDeliveryCompletion { work, response })
                                }
                                DeliveryStage::Observation { .. }
                                | DeliveryStage::Work
                                | DeliveryStage::Collecting
                                | DeliveryStage::Attachments { .. }
                                | DeliveryStage::Receipt { .. }
                                | DeliveryStage::Foreground { .. }
                                | DeliveryStage::Steering { .. }
                                | DeliveryStage::Reconcile { .. }
                                | DeliveryStage::Rejected(_)
                                | DeliveryStage::Consumed => unreachable!(
                                    "only local or confirmed replacement input completes here"
                                ),
                            };
                        Ok(InputDeliveryProgress::Finished {
                            events: Vec::new(),
                            input,
                            completion,
                        })
                    }
                    DeliveryStage::Work => {
                        self.after_settings(services);
                        Ok(InputDeliveryProgress::waiting())
                    }
                    DeliveryStage::Observation { .. }
                    | DeliveryStage::Collecting
                    | DeliveryStage::Attachments { .. }
                    | DeliveryStage::Receipt { .. }
                    | DeliveryStage::Foreground { .. }
                    | DeliveryStage::Steering { .. }
                    | DeliveryStage::Reconcile { .. }
                    | DeliveryStage::Replacement { .. }
                    | DeliveryStage::Consumed => {
                        unreachable!("only synchronous input stages produce ready facts")
                    }
                }
            }
        }
    }

    /// Transfer all uncertain jobs and capabilities rather than detaching timed-out work.
    pub(super) fn retain(self) -> RetainedInput {
        let mut completion = None;
        // A consumed send result must not erase the engine needed after a later commit failure.
        let settled_steering = match (&self.destination, &self.stage) {
            (InputDestination::Steering { target, .. }, stage)
                if !matches!(stage, DeliveryStage::Steering { .. }) =>
            {
                target.engine.get().cloned()
            }
            (InputDestination::Foreground, _) | (InputDestination::Steering { .. }, _) => None,
        };
        let mut retained = self
            .transition
            .map(ExchangeTransition::into_retained)
            .unwrap_or_else(|| RetainedTransition::configuration(self.control));
        retained.retain_generation(self.generation);
        if let Some(engine) = settled_steering {
            retained.retain_job(tokio::spawn(async move { engine.close().await }));
        }
        match self.work {
            PreparationWork::Settings(run) => {
                if let Some(job) = (*run).into_collection_job() {
                    retained.retain_job(job);
                }
            }
            PreparationWork::Compaction(run) => run.retain(&mut retained),
            PreparationWork::None => {}
        }
        match self.stage {
            DeliveryStage::Observation { task, .. } => {
                retained.retain_job(tokio::spawn(async move {
                    task.await
                        .map(drop)
                        .map_err(|_| crate::provider::ProviderError::DeliveryUncertain)
                }))
            }
            DeliveryStage::Attachments { task, .. } => {
                retained.retain_job(tokio::spawn(async move {
                    task.await
                        .map(drop)
                        .map_err(|_| crate::provider::ProviderError::DeliveryUncertain)
                }))
            }
            DeliveryStage::Receipt { task, .. } => retained.retain_job(tokio::spawn(async move {
                task.await
                    .map(drop)
                    .map_err(|_| crate::provider::ProviderError::DeliveryUncertain)
            })),
            DeliveryStage::Reconcile { task, .. } => {
                retained.retain_job(tokio::spawn(async move {
                    match task.await {
                        Ok(Ok(_)) => Ok(()),
                        Ok(Err(_)) | Err(_) => {
                            Err(crate::provider::ProviderError::DeliveryUncertain)
                        }
                    }
                }))
            }
            DeliveryStage::Steering { task, flight, .. } => {
                // Fence the captured engine before joining an overdue native send that may need cancellation.
                retained.retain_job(tokio::spawn(async move {
                    let collected = flight.target.fence(&flight.generation).await;
                    let joined = task.await;
                    match (collected, joined) {
                        (Ok(()), Ok(_)) => Ok(()),
                        (Err(error), _) => Err(error),
                        (Ok(()), Err(_)) => Err(crate::provider::ProviderError::DeliveryUncertain),
                    }
                }));
            }
            DeliveryStage::Work
            | DeliveryStage::Collecting
            | DeliveryStage::Foreground { .. }
            | DeliveryStage::Local
            | DeliveryStage::Rejected(_)
            | DeliveryStage::Consumed => {}
            DeliveryStage::Replacement { work, response, .. } => {
                completion = Some(InputDeliveryCompletion { work, response })
            }
        }
        RetainedInput {
            input: self.input,
            messages: self.messages,
            retained,
            execution: self.execution,
            completion,
            terminal_events: self.terminal_events,
        }
    }

    /// Detect steering deferral without failing or discarding a provably unsent draft.
    fn steering_block_reason(
        &self,
        services: &OwnerServices,
    ) -> Result<Option<InputAbandonment>, OperationFailure> {
        if matches!(self.destination, InputDestination::Foreground) {
            return Ok(None);
        }
        if !crate::input::input_purpose_is_current(
            &services.tasks,
            &services.questions,
            &self.message.purpose,
            &services.root,
            services.observer.session_id(),
        )
        .map_err(|error| OperationFailure::Domain(error.into()))?
        {
            return Ok(Some(InputAbandonment::Superseded));
        }
        let queue = services
            .input
            .snapshot()
            .map_err(OperationFailure::Domain)?;
        let ready = services
            .observer
            .input_readiness()
            .map_err(OperationFailure::Domain)?;
        Ok((queue.suspended
            && !matches!(
                self.message.purpose,
                crate::input::InputPurpose::Question { .. }
            )
            || *services.cancellation.subscribe().borrow()
            || ready.has_approval
            || ready.checks_running
            || services.tasks.configuration_is_uncertain())
        .then_some(InputAbandonment::Paused))
    }

    /// Preserve an unsent draft or retire obsolete authority only at its original claim revision.
    fn finish_unprepared_steering(
        &mut self,
        reason: InputAbandonment,
        services: &OwnerServices,
    ) -> Result<InputDeliveryProgress, OperationFailure> {
        let delivery = match reason {
            InputAbandonment::Paused
            | InputAbandonment::Retracted
            | InputAbandonment::Reconfigured => Some(InputDelivery::Queued),
            InputAbandonment::Cancelled | InputAbandonment::Superseded => None,
        };
        let input = services
            .input
            .delivery_if_current(
                self.input.id.as_str(),
                InputDeliveryGuard::Claimed(self.input.revision),
                delivery,
                None,
                None,
            )
            .map_err(OperationFailure::Domain)?;
        self.release_steering_order(services);
        Ok(InputDeliveryProgress::Finished {
            events: Vec::new(),
            input,
            completion: None,
        })
    }

    /// Settle deliberate preparation interruption without classifying it as failed acceptance.
    fn finish_interrupted(
        &mut self,
        reason: InputAbandonment,
        submission: Option<&Arc<Submission>>,
        receipts: &mut InputReceipts,
        services: &OwnerServices,
    ) -> Result<InputDeliveryProgress, OperationFailure> {
        let input = if let Some(submission) = submission {
            receipts
                .apply(
                    InputReceiptFact::Abandoned {
                        submission: submission.receipt_id().clone(),
                        reason,
                    },
                    services,
                )
                .map_err(|error| OperationFailure::Domain(error.into()))?
        } else {
            let delivery = match reason {
                InputAbandonment::Paused | InputAbandonment::Reconfigured => {
                    Some(InputDelivery::Queued)
                }
                InputAbandonment::Cancelled
                | InputAbandonment::Superseded
                | InputAbandonment::Retracted => None,
            };
            let mut input = services
                .input
                .delivery_if_current(
                    self.input.id.as_str(),
                    InputDeliveryGuard::Claimed(self.input.revision),
                    delivery,
                    None,
                    None,
                )
                .map_err(OperationFailure::Domain)?;
            if matches!(
                reason,
                InputAbandonment::Cancelled | InputAbandonment::Retracted
            ) && let Some(update) = &mut input
            {
                update.retracted = Some(self.input.id.as_str().into());
            }
            input
        };
        self.release_steering_order(services);
        self.release_preparation();
        Ok(InputDeliveryProgress::Finished {
            events: Vec::new(),
            input,
            completion: None,
        })
    }

    /// Classify owner-observed interruption before issuing another input effect.
    fn interruption(
        &self,
        services: &OwnerServices,
    ) -> Result<Option<InputAbandonment>, OperationFailure> {
        let snapshot = services
            .input
            .snapshot()
            .map_err(OperationFailure::Domain)?;
        let current = snapshot
            .messages
            .iter()
            .find(|message| message.id == self.input.id.as_str());
        if current.is_none_or(|message| message.delivery() != InputDelivery::Delivering) {
            return Ok(Some(InputAbandonment::Superseded));
        }
        if *services.cancellation.subscribe().borrow() {
            return Ok(Some(InputAbandonment::Cancelled));
        }
        if snapshot.suspended
            && !matches!(
                self.message.purpose,
                crate::input::InputPurpose::Question { .. }
            )
        {
            return Ok(Some(InputAbandonment::Paused));
        }
        if !crate::input::input_purpose_is_current(
            &services.tasks,
            &services.questions,
            &self.message.purpose,
            &services.root,
            services.observer.session_id(),
        )
        .map_err(|error| OperationFailure::Domain(error.into()))?
        {
            return Ok(Some(InputAbandonment::Superseded));
        }
        Ok(None)
    }

    /// Reuse root collection without waiting for this input preparation to collect itself.
    fn collect(&mut self, children: ChildCollectionPolicy, services: &OwnerServices) {
        let control = self.control.take().or_else(|| {
            self.transition
                .as_mut()
                .and_then(ExchangeTransition::take_control)
        });
        self.transition = Some(ExchangeTransition::for_input(
            self.input.clone(),
            children,
            services,
            self.generation.clone(),
            control,
        ));
        self.stage = DeliveryStage::Collecting;
    }

    /// Select local compaction or capture attachments after queued settings have committed.
    fn after_settings(&mut self, services: &OwnerServices) {
        match self.kind {
            DeliveryKind::Compact => {
                self.work = PreparationWork::Compaction(ContextCompactionRun::new());
                self.stage = DeliveryStage::Work;
            }
            DeliveryKind::ConfigurationOnly => self.stage = DeliveryStage::Local,
            DeliveryKind::Prompt => {
                if matches!(self.destination, InputDestination::Foreground) && !self.collected {
                    self.collect(ChildCollectionPolicy::Preserve, services);
                    return;
                }
                let root = services.root.clone();
                let attachments = self.message.attachments.clone();
                self.stage = DeliveryStage::Attachments {
                    task: tokio::spawn(async move {
                        crate::attachment::resolve_input(
                            &root,
                            &serde_json::json!({"attachments":attachments}),
                        )
                        .await
                    }),
                };
            }
        }
    }

    /// Reconcile native evidence with no control/execution capability held by the input observer.
    fn begin_reconcile(
        &mut self,
        submission: Arc<Submission>,
        result: InputExecutionResult,
        events: Vec<SessionEvent>,
        steering_failure: Option<crate::provider::ProviderError>,
        services: &OwnerServices,
    ) {
        let reconciliation = submission.clone();
        let observation = services.clone();
        let retained_generation = self.generation.clone();
        self.stage = DeliveryStage::Reconcile {
            task: tokio::spawn(async move {
                let generation = match retained_generation {
                    Some(generation) => generation,
                    None => observation.observe_provider_generation().await,
                };
                let observation = reconciliation
                    .observe_reconciliation(generation.provider())
                    .await
                    .map_err(crate::provider::submission::journal_failure)?;
                Ok(ReconciledInput { observation })
            }),
            submission,
            result,
            events,
            steering_failure,
        };
    }

    /// Release ordering at queue handoff while retaining native receipt and transport ownership.
    fn release_steering_order(&mut self, services: &OwnerServices) {
        if let InputDestination::Steering { order, .. } = &mut self.destination
            && order.take().is_some()
        {
            services.input.changed.notify_one();
        }
    }

    /// Release preparation-only gates before native foreground or continuation dispatch.
    fn release_preparation(&mut self) {
        self.transition.take();
        self.control.take();
        self.generation.take();
    }
}

/// Retain failed internal input work until actual collection and owner-side fact application.
pub(super) struct RetainedInput {
    pub(super) input: InputRevision,
    pub(super) messages: Option<MessageSender>,
    pub(super) retained: RetainedTransition,
    pub(super) execution: Option<ExecutionPermit>,
    pub(super) completion: Option<InputDeliveryCompletion>,
    pub(super) terminal_events: Vec<SessionEvent>,
}

/// Retain response-only publications after an internal replacement's dispatch is confirmed.
pub(super) struct InputDeliveryCompletion {
    pub(super) work: uuid::Uuid,
    pub(super) response: oneshot::Receiver<anyhow::Result<DispatchResult>>,
}

impl InputDeliveryCompletion {
    /// Retain terminal publications independently of the completed input's dispatch wait and UI delivery.
    pub(super) async fn forward(
        self,
        publications: Arc<super::publication::SessionPublications>,
        messages: Option<MessageSender>,
    ) -> (uuid::Uuid, Result<(), super::publication::PublicationError>) {
        let outcome = async {
            if let Ok(Ok(result)) = self.response.await {
                for event in result.event {
                    publications.publish(event, messages.as_ref())?;
                }
            }
            Ok(())
        }
        .await;
        (self.work, outcome)
    }
}

/// Transfer a prepared foreground with its output and exclusive execution lifetime.
pub(super) struct InputForegroundStart {
    pub(super) id: uuid::Uuid,
    /// Distinguish original input dispatch from a continuation after local compaction.
    pub(super) source: InputForegroundSource,
    pub(super) transaction: ForegroundDispatch,
    pub(super) updates: SessionUpdateStream,
    pub(super) execution: ExecutionPermit,
    pub(super) completion: oneshot::Sender<anyhow::Result<DispatchResult>>,
    pub(super) committed: Vec<SessionUpdate>,
    pub(super) input: Option<InputUpdate>,
}

/// Identify whether prepared foreground work carries a new input acceptance obligation.
pub(super) enum InputForegroundSource {
    /// Original input's durable receipt must match its confirmed handoff.
    Input(forge_provider_api::SubmissionId),
    /// Compaction resumed existing work without submitting a new input.
    Continuation,
}

/// Describe the committed publications, foreground handoff, or terminal input effect of one update.
pub(super) enum InputDeliveryProgress {
    Waiting {
        events: Vec<SessionEvent>,
        updates: Vec<SessionUpdate>,
        input: Option<InputUpdate>,
    },
    Foreground(InputForegroundStart),
    Finished {
        events: Vec<SessionEvent>,
        input: Option<InputUpdate>,
        completion: Option<InputDeliveryCompletion>,
    },
}

impl InputDeliveryProgress {
    /// Advance an internal phase without manufacturing an outward state change.
    fn waiting() -> Self {
        Self::Waiting {
            events: Vec::new(),
            updates: Vec::new(),
            input: None,
        }
    }
}

/// Correlate a retained input job or preparation stage with its unique delivery owner.
pub(super) enum InputDeliveryEvent {
    Observation(Result<ProviderGenerationPermit, tokio::task::JoinError>),
    Settings(SettingsEvent),
    Compaction(CompactionEvent),
    Transition(TransitionEvent),
    Attachments(
        Result<anyhow::Result<Vec<crate::attachment::InputAttachment>>, tokio::task::JoinError>,
    ),
    Receipt(
        Result<Result<Arc<Submission>, crate::provider::ProviderError>, tokio::task::JoinError>,
    ),
    Foreground(Result<anyhow::Result<DispatchResult>, oneshot::error::RecvError>),
    /// Observe steering completion independently of its selected queue's earlier handoff.
    Steered(Result<Result<(), crate::provider::ProviderError>, tokio::task::JoinError>),
    /// Release serial admission once the captured queue owns this steering input.
    SteeringQueued(Result<(), tokio::sync::watch::error::RecvError>),
    Reconciled(
        Box<
            Result<Result<ReconciledInput, crate::provider::ProviderError>, tokio::task::JoinError>,
        >,
    ),
    Ready,
}

impl InputDeliveryEvent {
    /// Get a finite event name without input text, settings values, or native tokens.
    pub(super) fn name(&self) -> &'static str {
        match self {
            Self::Observation(_) => "input_generation_observed",
            Self::Settings(event) => event.name(),
            Self::Compaction(event) => event.name(),
            Self::Transition(event) => event.name(),
            Self::Attachments(_) => "input_attachments_prepared",
            Self::Receipt(_) => "input_receipt_prepared",
            Self::Foreground(_) => "input_foreground_settled",
            Self::Steered(_) => "input_steering_settled",
            Self::SteeringQueued(_) => "input_steering_queued",
            Self::Reconciled(_) => "input_receipt_reconciled",
            Self::Ready => "input_preparation_ready",
        }
    }
}

enum DeliveryKind {
    Prompt,
    ConfigurationOnly,
    Compact,
}

enum InputObservation {
    Waiting,
    Pending,
}
enum PreparationWork {
    Settings(Box<SettingsRun>),
    Compaction(ContextCompactionRun),
    None,
}

/// Retain the actual send's generation and receipt flight, independently of any waiting observer.
struct SteeringFlight {
    target: SteeringDispatchTarget,
    generation: ProviderGenerationPermit,
    _receipt: crate::provider::submission::SubmissionFlight,
}

/// Reconcile native evidence separately from the activity that permits safe steering requeue.
pub(super) struct ReconciledInput {
    observation: SubmissionReconciliation,
}

enum DeliveryStage {
    Observation {
        task: JoinHandle<ProviderGenerationPermit>,
    },
    Work,
    Collecting,
    Attachments {
        task: JoinHandle<anyhow::Result<Vec<crate::attachment::InputAttachment>>>,
    },
    Receipt {
        task: JoinHandle<Result<Arc<Submission>, crate::provider::ProviderError>>,
        attachments: Vec<crate::attachment::InputAttachment>,
    },
    Foreground {
        work: uuid::Uuid,
        response: oneshot::Receiver<anyhow::Result<DispatchResult>>,
        submission: Arc<Submission>,
        confirmed: bool,
    },
    Steering {
        task: JoinHandle<Result<(), crate::provider::ProviderError>>,
        submission: Arc<Submission>,
        queued: Option<tokio::sync::watch::Receiver<bool>>,
        flight: Arc<SteeringFlight>,
    },
    Reconcile {
        task: JoinHandle<Result<ReconciledInput, crate::provider::ProviderError>>,
        submission: Arc<Submission>,
        result: InputExecutionResult,
        events: Vec<SessionEvent>,
        steering_failure: Option<crate::provider::ProviderError>,
    },
    Replacement {
        work: uuid::Uuid,
        response: oneshot::Receiver<anyhow::Result<DispatchResult>>,
        confirmed: bool,
    },
    Local,
    Rejected(Option<OperationFailure>),
    Consumed,
}
