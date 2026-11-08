//! Advance task control through owner collection, atomic task commits, and journaled input scheduling.

use forge_protocol::message::SessionEvent;
use tokio::sync::OwnedSemaphorePermit;
use tokio::task::JoinHandle;

use super::{
    command::{
        Command, CommandFailure, InputId, InputRevision, NewGoal, NewPlan, PlanRevisionSelection,
        PlanSelection, PlanSourceGuard, TaskCommand, TaskCommandCheckpoint, TaskCommandFailure,
        TaskCommandOutcome, TaskSynchronizationFailure,
    },
    failure::OperationFailure,
    queue::QueuedCommand,
    services::OwnerServices,
    transition::{
        ChildCollectionPolicy, ExchangeStop, RetainedTransition, StopIntent, TransitionEvent,
        TransitionFailure, TransitionOutcome, TransitionProgress, TurnStatus,
    },
};

/// Read the single owner's complete boundary for installing task input.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum TaskDeliveryGate {
    /// No foreground, steering, or queued preparation occupies the delivery slot.
    Open,
    /// Retain acquired resources while the owner finishes earlier work.
    Waiting,
    /// Surface a finite failure when earlier cleanup cannot restore the delivery slot.
    Unavailable,
}

/// Retain one task command and its resources independently of response observers.
pub(super) struct TaskCommandRun {
    pub(super) command: QueuedCommand,
    stage: TaskStage,
    checkpoint: Option<TaskCommandCheckpoint>,
    control: Option<OwnedSemaphorePermit>,
    transition: Option<ExchangeStop>,
}

impl TaskCommandRun {
    /// Acquire compatibility control once for lifecycle commands; collection inherits that capability.
    /// Local policy changes rely on the task mutation fence and do not acquire native lanes.
    pub(super) fn start(command: QueuedCommand, services: &OwnerServices) -> Self {
        let local = matches!(
            &command.record.request.command,
            Command::Task(TaskCommand::SetAutoAccept(_))
        );
        let stage = if local {
            TaskStage::Preflight
        } else {
            let control = services.control.clone();
            TaskStage::Acquiring(tokio::spawn(async move {
                tokio::time::timeout(std::time::Duration::from_secs(60), control.acquire_owned())
                    .await
                    .ok()
                    .and_then(Result::ok)
                    .ok_or(CommandFailure::OwnershipUnavailable)
            }))
        };
        Self {
            command,
            checkpoint: None,
            control: None,
            transition: None,
            stage,
        }
    }

    /// Drain the collection fact for the foreground that this task operation owns.
    pub(super) fn take_collection_signal(&mut self) -> Option<crate::broker::ForegroundSignal> {
        self.transition
            .as_mut()
            .and_then(ExchangeStop::take_collection_signal)
    }

    /// Close tool admission only while old invocation work is being collected or replaced.
    pub(super) fn closes_tool_admission(&self) -> bool {
        match &self.stage {
            TaskStage::Acquiring(_) | TaskStage::Preflight => !matches!(
                &self.command.record.request.command,
                Command::Task(TaskCommand::SetAutoAccept(_) | TaskCommand::EditPlan(_))
            ),
            TaskStage::Collecting(_)
            | TaskStage::SettlingFinalization { .. }
            | TaskStage::PreparingCreation { .. }
            | TaskStage::PreparingExecution { .. } => true,
            TaskStage::PreparingOperation { .. } | TaskStage::Commit(_) => {
                self.transition.is_some()
            }
            TaskStage::AcquiringDelivery(_)
            | TaskStage::AwaitingInput(_)
            | TaskStage::AwaitingDispatch { .. }
            | TaskStage::Ready(_)
            | TaskStage::Consumed => false,
        }
    }

    /// Get committed task evidence for failures whose native resources require collection.
    pub(super) fn checkpoint(&self) -> Option<&TaskCommandCheckpoint> {
        self.checkpoint.as_ref()
    }

    /// Trace captured task authority without changing the durable outer command kind.
    pub(super) fn captured_task(&self) -> Option<&super::command::TaskSelection> {
        match &self.command.record.request.command {
            Command::Task(intent) => intent.target(),
            Command::ContinueWorkflow(request) => request
                .execution()
                .and_then(|selection| selection.prior())
                .or_else(|| {
                    self.checkpoint
                        .as_ref()
                        .and_then(TaskCommandCheckpoint::task)
                }),
            _ => None,
        }
    }

    /// Trace the plan source shared by direct execution and explicit workflow continuation.
    pub(super) fn captured_plan(&self) -> Option<PlanSourceGuard> {
        match &self.command.record.request.command {
            Command::Task(intent) => intent.plan_source(),
            Command::ContinueWorkflow(request) => request
                .execution()
                .map(|selection| PlanSourceGuard::Working(selection.source().clone())),
            _ => None,
        }
    }

    /// Identify local task preparation whose timeout must not be reported as native collection failure.
    pub(super) fn preparing(&self) -> bool {
        matches!(
            self.stage,
            TaskStage::PreparingCreation { .. }
                | TaskStage::PreparingExecution { .. }
                | TaskStage::PreparingOperation { .. }
        )
    }

    /// Retire yielded worker handles even when a prior owner-storage check prevents applying their result.
    pub(super) fn consume_completed_worker(&mut self, event: &TaskCommandEvent) {
        if matches!(
            event,
            TaskCommandEvent::Control(_)
                | TaskCommandEvent::Delivery(_)
                | TaskCommandEvent::Prepared(_)
                | TaskCommandEvent::ExecutionPrepared(_)
                | TaskCommandEvent::OperationPrepared(_)
        ) {
            self.stage = TaskStage::Consumed;
        }
    }

    /// Correlate shared input preparation with this command before its foreground can dispatch.
    pub(super) fn input_foreground(
        &mut self,
        input: &InputRevision,
        work: uuid::Uuid,
        receipt: forge_provider_api::SubmissionId,
    ) -> bool {
        if matches!(&self.stage, TaskStage::AwaitingInput(expected) if expected == input) {
            self.stage = TaskStage::AwaitingDispatch {
                input: input.clone(),
                work,
                receipt,
                deadline: tokio::time::Instant::now() + std::time::Duration::from_secs(30),
            };
            return true;
        }
        false
    }

    /// Complete startup only on the handoff of the exact foreground installed for its input.
    pub(super) fn observe_dispatch(
        &mut self,
        work: uuid::Uuid,
        confirmation: &forge_provider_api::submission::DispatchConfirmation,
    ) {
        if !matches!(&self.stage, TaskStage::AwaitingDispatch { work: expected, .. } if *expected == work)
        {
            return;
        }
        let TaskStage::AwaitingDispatch { receipt, .. } = &self.stage else {
            unreachable!("matching task handoff retains its expected input receipt")
        };
        if !matches!(confirmation.target(), forge_provider_api::submission::DispatchTarget::Input(id) if id == receipt)
        {
            self.dispatch_lost(work);
            return;
        }
        let checkpoint = self
            .checkpoint
            .as_ref()
            .expect("confirmed task handoff retains its checkpoint");
        let outcome = match checkpoint {
            TaskCommandCheckpoint::InputPending { task, input }
                if matches!(&self.command.record.request.command, Command::Task(_)) =>
            {
                TaskCommandOutcome::Started {
                    task: task.clone(),
                    input: input.clone(),
                    scope: confirmation.scope().clone(),
                    binding: confirmation.binding().clone(),
                }
            }
            TaskCommandCheckpoint::InputPending { task, input } => TaskCommandOutcome::Continued {
                task: Some(task.clone()),
                input: InputId::new(input.as_str()).expect("task input identity is validated"),
                scope: confirmation.scope().clone(),
                binding: confirmation.binding().clone(),
            },
            TaskCommandCheckpoint::WorkflowInputPending { task, input } => {
                TaskCommandOutcome::Continued {
                    task: task.clone(),
                    input: input.clone(),
                    scope: confirmation.scope().clone(),
                    binding: confirmation.binding().clone(),
                }
            }
            TaskCommandCheckpoint::Control { .. } => {
                unreachable!("finite controls do not dispatch input")
            }
        };
        self.stage = TaskStage::Ready(Box::new(Ok(outcome)));
    }

    /// Wake the command when its exact foreground loses handoff observation.
    pub(super) fn dispatch_lost(&mut self, work: uuid::Uuid) {
        if let TaskStage::AwaitingDispatch {
            work: expected,
            deadline,
            ..
        } = &mut self.stage
            && *expected == work
        {
            *deadline = tokio::time::Instant::now();
        }
    }

    /// Preserve task commit evidence when shared input work fails before confirmed dispatch.
    pub(super) fn input_ended(&mut self, input: &InputRevision) {
        if matches!(&self.stage, TaskStage::AwaitingInput(expected) if expected == input) {
            let failure = self.failure(TaskSynchronizationFailure::InputPreparationUnavailable);
            self.stage = TaskStage::Ready(Box::new(Err(failure)));
        } else if let TaskStage::AwaitingDispatch {
            input: expected,
            deadline,
            ..
        } = &mut self.stage
            && expected == input
        {
            // Once foreground owns the send, missing handoff requires the uncertainty/collection path.
            *deadline = tokio::time::Instant::now();
        }
    }

    /// Await one retained fact while provider observations and caller intake continue to advance.
    pub(super) async fn next(
        &mut self,
        foreground: TurnStatus,
        delivery: TaskDeliveryGate,
    ) -> TaskCommandEvent {
        match &mut self.stage {
            TaskStage::Preflight => TaskCommandEvent::Preflight,
            TaskStage::Acquiring(task) => TaskCommandEvent::Control(
                task.await
                    .unwrap_or(Err(CommandFailure::OwnershipUnavailable)),
            ),
            TaskStage::PreparingCreation { task, deadline } => tokio::select! {
                biased;
                prepared = task => TaskCommandEvent::Prepared(prepared.unwrap_or(Err(TaskCommandFailure::PreparationUnavailable))),
                _ = tokio::time::sleep_until(*deadline) => TaskCommandEvent::PreparationDeadline,
            },
            TaskStage::PreparingExecution { task, deadline } => tokio::select! {
                biased;
                prepared = task => TaskCommandEvent::ExecutionPrepared(prepared.unwrap_or(Err(TaskCommandFailure::PreparationUnavailable))),
                _ = tokio::time::sleep_until(*deadline) => TaskCommandEvent::PreparationDeadline,
            },
            TaskStage::PreparingOperation { task, deadline } => tokio::select! {
                biased;
                prepared = task => TaskCommandEvent::OperationPrepared(prepared.unwrap_or_else(|_| Err(rejected(TaskCommandFailure::PreparationUnavailable)))),
                _ = tokio::time::sleep_until(*deadline) => TaskCommandEvent::PreparationDeadline,
            },
            TaskStage::Collecting(_) | TaskStage::SettlingFinalization { .. } => {
                TaskCommandEvent::Transition(
                    self.transition
                        .as_mut()
                        .expect("task collection retains its transition")
                        .next(foreground.clone())
                        .await,
                )
            }
            TaskStage::Commit(_) => TaskCommandEvent::Commit,
            // A ready semaphore acquisition does not imply that the owner released all prior work.
            TaskStage::AcquiringDelivery(_) if delivery == TaskDeliveryGate::Waiting => {
                std::future::pending().await
            }
            TaskStage::AcquiringDelivery(task) => TaskCommandEvent::Delivery(
                task.await
                    .unwrap_or(Err(CommandFailure::OwnershipUnavailable)),
            ),
            TaskStage::AwaitingInput(_) => std::future::pending().await,
            TaskStage::AwaitingDispatch { deadline, .. } => {
                tokio::time::sleep_until(*deadline).await;
                TaskCommandEvent::DispatchDeadline
            }
            TaskStage::Ready(_) => TaskCommandEvent::Ready,
            TaskStage::Consumed => std::future::pending().await,
        }
    }

    /// Apply facts and mutate task authority only while the session owner holds its update boundary.
    pub(super) fn update(
        &mut self,
        event: TaskCommandEvent,
        foreground: TurnStatus,
        broker: &mut crate::broker::HarnessBroker,
        services: &OwnerServices,
        can_commit: bool,
        can_deliver: bool,
    ) -> Result<TaskCommandProgress, OperationFailure> {
        // Acquisitions have already yielded their result; failure retention must not await their handles twice.
        self.consume_completed_worker(&event);
        let preparation_failed = matches!(
            &event,
            TaskCommandEvent::Prepared(Err(_))
                | TaskCommandEvent::ExecutionPrepared(Err(_))
                | TaskCommandEvent::OperationPrepared(Err(_))
        );
        if preparation_failed
            || !can_commit
                && matches!(
                    &event,
                    TaskCommandEvent::Prepared(_)
                        | TaskCommandEvent::ExecutionPrepared(_)
                        | TaskCommandEvent::OperationPrepared(_)
                )
        {
            self.reconcile_source_artifacts(services)?;
        }
        if !can_commit {
            if let TaskCommandEvent::Transition(event) = event
                && let Some(transition) = self.transition.as_mut()
            {
                transition.retain_uncommitted(event);
            }
            return Err(OperationFailure::StorageUncommitted);
        }
        match event {
            TaskCommandEvent::Control(result) => {
                self.control = Some(result.map_err(|error| {
                    OperationFailure::Transition(TransitionFailure::Rejected(error))
                })?);
                self.prepare(broker, services)
            }
            TaskCommandEvent::Preflight => {
                self.stage = TaskStage::Consumed;
                self.prepare(broker, services)
            }
            TaskCommandEvent::Prepared(result) => {
                let prepared = result.map_err(rejected)?;
                let current = services.tasks.current().map_err(OperationFailure::Domain)?;
                match &prepared.prior {
                    Some(prior) => prior
                        .validate_observation(current.as_ref())
                        .map_err(|_| rejected(TaskCommandFailure::Changed))?,
                    None if current.is_none() => {}
                    None => return Err(rejected(TaskCommandFailure::Changed)),
                }
                self.stage = TaskStage::Commit(TaskCommit::Creation(prepared));
                Ok(TaskCommandProgress::waiting())
            }
            TaskCommandEvent::OperationPrepared(result) => {
                self.stage = TaskStage::Commit(result?);
                Ok(TaskCommandProgress::waiting())
            }
            TaskCommandEvent::ExecutionPrepared(result) => {
                self.stage = TaskStage::Commit(TaskCommit::Execution(result.map_err(rejected)?));
                Ok(TaskCommandProgress::waiting())
            }
            TaskCommandEvent::PreparationDeadline => Err(OperationFailure::JobDeadline),
            TaskCommandEvent::Transition(event) => {
                match self
                    .transition
                    .as_mut()
                    .expect("task transition events retain their collection")
                    .update(event, foreground, broker, services)
                    .map_err(OperationFailure::Transition)?
                {
                    TransitionProgress::Waiting { events, input } => {
                        Ok(TaskCommandProgress::Waiting {
                            events,
                            input: input.map(|input| *input),
                        })
                    }
                    TransitionProgress::Finished {
                        outcome: TransitionOutcome::Cancelled(_),
                        events,
                    } if matches!(self.stage, TaskStage::SettlingFinalization { .. }) => {
                        let TaskStage::SettlingFinalization { outcome } =
                            std::mem::replace(&mut self.stage, TaskStage::Consumed)
                        else {
                            unreachable!("finalization settlement retains its committed outcome")
                        };
                        self.transition.take();
                        Ok(TaskCommandProgress::Finished {
                            outcome: Ok(*outcome),
                            events,
                        })
                    }
                    TransitionProgress::Finished {
                        outcome: TransitionOutcome::Reconfigured { .. },
                        events,
                    } => {
                        let TaskStage::Collecting(mutation) =
                            std::mem::replace(&mut self.stage, TaskStage::Consumed)
                        else {
                            unreachable!("task collection retains its pending mutation")
                        };
                        match mutation {
                            TaskCollection::Creation { request, prior } => {
                                self.prepare_creation(*request, prior, services)?
                            }
                            TaskCollection::Mutation { intent, prior } => {
                                self.prepare_mutation(*intent, prior, services)?
                            }
                            TaskCollection::Execution {
                                selection,
                                validated,
                                prior,
                            } => {
                                self.prepare_execution(*selection, *validated, prior, services)?;
                            }
                            TaskCollection::Control(permit) => {
                                self.stage = TaskStage::Commit(TaskCommit::Control(permit));
                            }
                        }
                        Ok(TaskCommandProgress::Waiting {
                            events,
                            input: None,
                        })
                    }
                    TransitionProgress::Finished {
                        outcome: TransitionOutcome::Cancelled(_),
                        ..
                    } => unreachable!(
                        "task controls use parked collection, not exchange-cancel outcomes"
                    ),
                }
            }
            TaskCommandEvent::Commit => {
                let TaskStage::Commit(mutation) =
                    std::mem::replace(&mut self.stage, TaskStage::Consumed)
                else {
                    unreachable!("task commit consumes its prepared mutation once")
                };
                // Commit task state with its finite result or journaled input checkpoint.
                let mut store = crate::storage::SqliteStore::open(&services.root)
                    .map_err(OperationFailure::Domain)?;
                let now = std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .map_err(|error| OperationFailure::Domain(error.into()))?;
                let now_ms = i64::try_from(now.as_millis())
                    .map_err(|error| OperationFailure::Domain(error.into()))?;
                let committed = match mutation {
                    TaskCommit::Mutation {
                        intent,
                        prior,
                        change,
                    } => {
                        let finite = matches!(intent.as_ref(), TaskCommand::EditPlan(_))
                            || matches!(intent.as_ref(), TaskCommand::CancelPlan(_))
                                && matches!(&change.change, crate::task::TaskChange::Preserve);
                        services
                            .tasks
                            .apply_owner_mutation(
                                &mut store,
                                &self.command.record.request.id,
                                &intent,
                                prior.as_ref(),
                                *change,
                                now_ms,
                            )
                            .map(|checkpoint| match checkpoint {
                                TaskCommandCheckpoint::Control { outcome } if finite => {
                                    TaskCommitResult::Complete(*outcome)
                                }
                                checkpoint => TaskCommitResult::ScheduleInput(checkpoint),
                            })
                    }
                    TaskCommit::QuestionWait {
                        request,
                        prior,
                        questions,
                    } => services
                        .tasks
                        .apply_owner_question_wait(
                            &mut store,
                            &self.command.record.request.id,
                            &request,
                            prior.as_ref(),
                            questions,
                            now_ms,
                        )
                        .map(TaskCommitResult::Complete),
                    TaskCommit::Continuation(prepared) => {
                        let PreparedContinuation {
                            request,
                            prior,
                            change,
                            instruction,
                        } = *prepared;
                        services
                            .tasks
                            .apply_owner_continuation(
                                &mut store,
                                crate::task::OwnerTaskCommit {
                                    id: &self.command.record.request.id,
                                    permit: prior.as_ref(),
                                    now_ms,
                                },
                                &request,
                                change,
                                instruction,
                            )
                            .map(|checkpoint| match checkpoint {
                                TaskCommandCheckpoint::Control { outcome } => {
                                    TaskCommitResult::Complete(*outcome)
                                }
                                checkpoint => TaskCommitResult::ScheduleInput(checkpoint),
                            })
                    }
                    TaskCommit::Execution(prepared) => {
                        let PreparedExecution {
                            selection,
                            prior,
                            mut change,
                            question_set,
                        } = *prepared;
                        if let Command::ContinueWorkflow(request) =
                            &self.command.record.request.command
                        {
                            change.effects.continuation_guard.push(request.clone());
                        }
                        services
                            .tasks
                            .apply_owner_execution(
                                &mut store,
                                crate::task::OwnerTaskCommit {
                                    id: &self.command.record.request.id,
                                    permit: prior.as_ref(),
                                    now_ms,
                                },
                                &selection,
                                change,
                                question_set,
                            )
                            .map(|checkpoint| match checkpoint {
                                TaskCommandCheckpoint::Control { outcome } => {
                                    TaskCommitResult::Complete(*outcome)
                                }
                                checkpoint @ (TaskCommandCheckpoint::InputPending { .. }
                                | TaskCommandCheckpoint::WorkflowInputPending {
                                    ..
                                }) => TaskCommitResult::ScheduleInput(checkpoint),
                            })
                    }

                    TaskCommit::Control(permit) => services
                        .tasks
                        .apply_owner_command(
                            &mut store,
                            &self.command.record.request.id,
                            self.intent(),
                            &permit,
                            now_ms,
                        )
                        .map(|outcome| match outcome {
                            outcome @ TaskCommandOutcome::AutoAcceptChanged { .. } => {
                                TaskCommitResult::Complete(outcome)
                            }
                            outcome @ (TaskCommandOutcome::Paused { .. }
                            | TaskCommandOutcome::Cleared { .. }
                            | TaskCommandOutcome::Unchanged { .. }) => {
                                TaskCommitResult::Complete(outcome)
                            }
                            TaskCommandOutcome::PlanEdited { .. }
                            | TaskCommandOutcome::PlanChangesRequested { .. }
                            | TaskCommandOutcome::Resumed { .. }
                            | TaskCommandOutcome::PlanCancelled { .. }
                            | TaskCommandOutcome::WorkflowAwaitingInput { .. }
                            | TaskCommandOutcome::ContinuationReserved { .. }
                            | TaskCommandOutcome::Continued { .. }
                            | TaskCommandOutcome::ExecutionUpdated { .. }
                            | TaskCommandOutcome::AwaitingInput { .. }
                            | TaskCommandOutcome::Started { .. }
                            | TaskCommandOutcome::Attached { .. } => unreachable!(
                                "local task controls cannot create an installation outcome"
                            ),
                        }),
                    TaskCommit::AttachmentCurrent { permit, source } => services
                        .tasks
                        .confirm_owner_attachment(
                            &mut store,
                            &self.command.record.request.id,
                            &permit,
                            &source,
                            now_ms,
                        )
                        .map(TaskCommitResult::Complete),
                    TaskCommit::Creation(prepared) => {
                        let PreparedCreation { prior, prepared } = *prepared;
                        services
                            .tasks
                            .apply_owner_creation(
                                &mut store,
                                &self.command.record.request.id,
                                prior.as_ref(),
                                prepared,
                                now_ms,
                            )
                            .map(TaskCommitResult::ScheduleInput)
                    }
                }
                .map_err(|error| {
                    if matches!(error, TaskCommandFailure::StateUnavailable) {
                        OperationFailure::Domain(error.into())
                    } else {
                        rejected(error)
                    }
                })?;
                // Collection parked any running exchange for a later admission to close. A finite
                // control that removed the task schedules none, so close it while control is held.
                let closed = match &committed {
                    TaskCommitResult::Complete(_)
                    | TaskCommitResult::ScheduleInput(TaskCommandCheckpoint::Control { .. }) => {
                        broker
                            .owner_close_orphaned_parked_exchange()
                            .map_err(OperationFailure::Domain)?
                    }
                    TaskCommitResult::ScheduleInput(
                        TaskCommandCheckpoint::InputPending { .. }
                        | TaskCommandCheckpoint::WorkflowInputPending { .. },
                    ) => Vec::new(),
                };
                self.transition.take();
                self.control.take();
                if matches!(
                    &committed,
                    TaskCommitResult::ScheduleInput(
                        TaskCommandCheckpoint::WorkflowInputPending { .. }
                    )
                ) {
                    services
                        .input
                        .refresh_workflow_reservation()
                        .map_err(OperationFailure::Domain)?;
                }
                services.input.changed.notify_one();
                // Parked collection defers the final checkpoint to the next admission. A finite
                // control that removed the task schedules none, so the collected exchange would
                // stay finalizing (and its transcript "Saving exchange") until unrelated work.
                // Settle it through ordinary cancellation settlement before reporting completion.
                let settle = |task: &mut Self, outcome: TaskCommandOutcome, events| {
                    if !broker
                        .owner_has_orphaned_finalization()
                        .map_err(OperationFailure::Domain)?
                    {
                        return Ok(TaskCommandProgress::Finished {
                            outcome: Ok(outcome),
                            events,
                        });
                    }
                    task.transition = Some(ExchangeStop::start(
                        task.command.record.request.id.clone(),
                        StopIntent::Cancel(super::command::CancelledInputPolicy::Retain),
                        services,
                        None,
                        None,
                    ));
                    task.stage = TaskStage::SettlingFinalization {
                        outcome: Box::new(outcome),
                    };
                    Ok(TaskCommandProgress::Waiting {
                        events,
                        input: None,
                    })
                };
                match committed {
                    TaskCommitResult::Complete(outcome) => settle(self, outcome, closed),
                    TaskCommitResult::ScheduleInput(committed) => {
                        self.checkpoint = Some(committed.clone());
                        match committed {
                            TaskCommandCheckpoint::Control { outcome } => {
                                settle(self, *outcome, closed)
                            }
                            TaskCommandCheckpoint::InputPending { .. }
                            | TaskCommandCheckpoint::WorkflowInputPending { .. } => {
                                let services = services.clone();
                                self.stage =
                                    TaskStage::AcquiringDelivery(tokio::spawn(async move {
                                        tokio::time::timeout(
                                            std::time::Duration::from_secs(60),
                                            async {
                                                let control = services
                                                    .control
                                                    .clone()
                                                    .acquire_owned()
                                                    .await
                                                    .map_err(|_| {
                                                        CommandFailure::OwnershipUnavailable
                                                    })?;
                                                let execution =
                                                    services.execution.acquire().await.map_err(
                                                        |_| CommandFailure::OwnershipUnavailable,
                                                    )?;
                                                Ok(TaskDeliveryCapabilities { execution, control })
                                            },
                                        )
                                        .await
                                        .unwrap_or(Err(CommandFailure::OwnershipUnavailable))
                                    }));
                                Ok(TaskCommandProgress::waiting())
                            }
                        }
                    }
                }
            }
            TaskCommandEvent::Delivery(result) => {
                if !can_deliver {
                    drop(result);
                    return Ok(self.failed(TaskSynchronizationFailure::InputPreparationUnavailable));
                }
                let capabilities = match result {
                    Ok(capabilities) => capabilities,
                    Err(_) => {
                        return Ok(
                            self.failed(TaskSynchronizationFailure::InputPreparationUnavailable)
                        );
                    }
                };
                self.prepare_input(services, capabilities)
            }
            TaskCommandEvent::DispatchDeadline => {
                if matches!(self.stage, TaskStage::Ready(_)) {
                    return Ok(TaskCommandProgress::waiting());
                }
                Err(OperationFailure::DispatchLost)
            }
            TaskCommandEvent::Ready => {
                let TaskStage::Ready(outcome) =
                    std::mem::replace(&mut self.stage, TaskStage::Consumed)
                else {
                    unreachable!("task completion retains its finite outcome")
                };
                Ok(TaskCommandProgress::Finished {
                    outcome: *outcome,
                    events: Vec::new(),
                })
            }
        }
    }

    /// Retain collection and local acquisition resources after observation or storage failure.
    pub(super) fn into_retained(self) -> (QueuedCommand, RetainedTransition) {
        let mut retained = self
            .transition
            .map(ExchangeStop::into_retained)
            .unwrap_or_else(|| RetainedTransition::configuration(self.control));
        match self.stage {
            TaskStage::PreparingCreation { task, .. } => {
                retained.retain_job(tokio::spawn(async move {
                    task.await
                        .map(drop)
                        .map_err(|_| crate::provider::ProviderError::DeliveryUncertain)
                }));
            }
            TaskStage::PreparingExecution { task, .. } => {
                retained.retain_job(tokio::spawn(async move {
                    task.await
                        .map(drop)
                        .map_err(|_| crate::provider::ProviderError::DeliveryUncertain)
                }));
            }
            TaskStage::PreparingOperation { task, .. } => {
                retained.retain_job(tokio::spawn(async move {
                    task.await
                        .map(drop)
                        .map_err(|_| crate::provider::ProviderError::DeliveryUncertain)
                }));
            }
            TaskStage::Acquiring(task) => {
                retained.retain_job(tokio::spawn(async move {
                    task.await
                        .map(drop)
                        .map_err(|_| crate::provider::ProviderError::DeliveryUncertain)
                }));
            }
            TaskStage::AcquiringDelivery(task) => {
                retained.retain_job(tokio::spawn(async move {
                    task.await
                        .map(drop)
                        .map_err(|_| crate::provider::ProviderError::DeliveryUncertain)
                }));
            }
            TaskStage::Preflight
            | TaskStage::Collecting(_)
            | TaskStage::SettlingFinalization { .. }
            | TaskStage::Commit(_)
            | TaskStage::AwaitingInput(_)
            | TaskStage::AwaitingDispatch { .. }
            | TaskStage::Ready(_)
            | TaskStage::Consumed => {}
        }
        (self.command, retained)
    }

    /// Validate captured task authority before installing local commit or collection stages.
    fn prepare(
        &mut self,
        broker: &mut crate::broker::HarnessBroker,
        services: &OwnerServices,
    ) -> Result<TaskCommandProgress, OperationFailure> {
        let current = services.tasks.current().map_err(OperationFailure::Domain)?;
        if let Command::ContinueWorkflow(request) = &self.command.record.request.command {
            let request = request.clone();
            validate_question_continuation(&request, current.as_ref(), services)?;
            if request.execution().is_none() {
                let services = services.clone();
                self.stage = TaskStage::PreparingOperation {
                    task: tokio::task::spawn_blocking(move || {
                        prepare_workflow_continuation(request, current.as_ref(), &services)
                    }),
                    deadline: tokio::time::Instant::now() + std::time::Duration::from_secs(60),
                };
                return Ok(TaskCommandProgress::waiting());
            }
        }
        if let Command::Task(intent) = &self.command.record.request.command
            && let Some(target) = intent.target()
        {
            target.validate(current.as_ref()).map_err(rejected)?;
        }
        let source = match &self.command.record.request.command {
            Command::Task(intent) => intent.plan_source(),
            Command::ContinueWorkflow(request) => request
                .execution()
                .map(|selection| PlanSourceGuard::Working(selection.source().clone())),
            _ => None,
        };
        if let Some(source) = source {
            let store = crate::storage::SqliteStore::read_only(&services.root)
                .map_err(|_| rejected(TaskCommandFailure::StateUnavailable))?;
            if let Some(plan) = store
                .load_plan(source.id().as_str())
                .map_err(OperationFailure::Domain)?
            {
                services
                    .observer
                    .reconcile_plan_artifacts(&plan)
                    .map_err(OperationFailure::Domain)?;
            }
            source
                .validate_storage(&store, services.observer.session_id())
                .map_err(rejected)?;
        }
        let execution = match &self.command.record.request.command {
            Command::Task(TaskCommand::ExecutePlan(selection)) => Some(selection.clone()),
            Command::ContinueWorkflow(request) => request.execution().cloned(),
            _ => None,
        };
        if let Some(selection) = execution {
            selection
                .validate_target(current.as_ref())
                .map_err(rejected)?;
            let now_ms = owner_now_ms()?;
            match services
                .observer
                .prepare_plan_execution(&self.command.record.request.id, &selection, now_ms)
                .map_err(rejected)?
            {
                crate::broker::PlanExecutionPreparation::AwaitingInput {
                    change,
                    question_set,
                } => {
                    self.stage =
                        TaskStage::Commit(TaskCommit::Execution(Box::new(PreparedExecution {
                            selection,
                            prior: current.as_ref().map(|task| task.permit()),
                            change: *change,
                            question_set: Some(question_set),
                        })));
                    return Ok(TaskCommandProgress::waiting());
                }
                crate::broker::PlanExecutionPreparation::Ready(validated) => {
                    return self.begin_collection(
                        TaskCollection::Execution {
                            selection: Box::new(selection),
                            validated: Box::new(validated),
                            prior: current.as_ref().map(|task| task.permit()),
                        },
                        broker,
                        services,
                    );
                }
            }
        }
        if matches!(
            self.intent(),
            TaskCommand::Resume(_)
                | TaskCommand::RequestPlanChanges(_)
                | TaskCommand::CancelPlan(_)
                | TaskCommand::EditPlan(_)
        ) {
            let intent = self.intent().clone();
            let prior = current.as_ref().map(|task| task.permit());
            if let TaskCommand::RequestPlanChanges(request) = &intent {
                request
                    .selection()
                    .validate_target(current.as_ref())
                    .map_err(rejected)?;
            }
            if let TaskCommand::CancelPlan(request) = &intent {
                request
                    .selection()
                    .validate_target(current.as_ref())
                    .map_err(rejected)?;
            }
            let should_collect = match &intent {
                TaskCommand::Resume(_) => true,
                TaskCommand::RequestPlanChanges(request) => {
                    task_owns_plan(current.as_ref(), request.selection().source())
                }
                TaskCommand::CancelPlan(request) => {
                    task_owns_plan(current.as_ref(), request.selection().source())
                }
                TaskCommand::EditPlan(_) => false,
                _ => unreachable!("mutation routing selected a supported operation"),
            };
            if should_collect {
                return self.begin_collection(
                    TaskCollection::Mutation {
                        intent: Box::new(intent),
                        prior,
                    },
                    broker,
                    services,
                );
            }
            self.prepare_mutation(intent, prior, services)?;
            return Ok(TaskCommandProgress::waiting());
        }
        if let TaskCommand::AttachPlan(source) = self.intent() {
            source.validate_attachment().map_err(rejected)?;
            if let Some(task) = current.as_ref()
                && matches!(&task.workflow, crate::task::WorkflowSnapshot::Plan { plan }
                    if plan.artifact.id == source.id().as_str())
            {
                self.stage = TaskStage::Commit(TaskCommit::AttachmentCurrent {
                    permit: task.permit(),
                    source: source.clone(),
                });
                return Ok(TaskCommandProgress::waiting());
            }
        }
        let creation = match self.intent() {
            TaskCommand::StartGoal(goal) => Some(TaskCreation::Goal(goal.clone())),
            TaskCommand::StartPlan(plan) => Some(TaskCreation::Plan(plan.clone())),
            TaskCommand::AttachPlan(source) => Some(TaskCreation::Attach(source.clone())),
            TaskCommand::ForkPlan(source) => Some(TaskCreation::Fork(source.clone())),
            TaskCommand::Replan(selection) => Some(TaskCreation::Replan(selection.clone())),
            TaskCommand::Resume(_)
            | TaskCommand::RequestPlanChanges(_)
            | TaskCommand::CancelPlan(_)
            | TaskCommand::EditPlan(_)
            | TaskCommand::ExecutePlan(_)
            | TaskCommand::Pause(_)
            | TaskCommand::Clear(_)
            | TaskCommand::SetAutoAccept(_) => None,
        };
        if let Some(request) = creation {
            return self.begin_collection(
                TaskCollection::Creation {
                    request: Box::new(request),
                    prior: current.as_ref().map(|task| task.permit()),
                },
                broker,
                services,
            );
        }
        let current = current
            .as_ref()
            .ok_or_else(|| rejected(TaskCommandFailure::MissingTask))?;
        let permit = current.permit();
        match self.intent() {
            TaskCommand::SetAutoAccept(_) => {
                if !matches!(
                    current.kind(),
                    crate::task::TaskKind::Plan | crate::task::TaskKind::Execute
                ) {
                    return Err(rejected(TaskCommandFailure::InvalidPolicyTask));
                }
                self.stage = TaskStage::Commit(TaskCommit::Control(permit));
                Ok(TaskCommandProgress::waiting())
            }
            TaskCommand::Pause(_) if current.status().terminal() => {
                self.stage = TaskStage::Commit(TaskCommit::Control(permit));
                Ok(TaskCommandProgress::waiting())
            }
            TaskCommand::Pause(_) | TaskCommand::Clear(_) => {
                self.begin_collection(TaskCollection::Control(permit), broker, services)
            }
            TaskCommand::Resume(_)
            | TaskCommand::RequestPlanChanges(_)
            | TaskCommand::CancelPlan(_)
            | TaskCommand::EditPlan(_)
            | TaskCommand::ExecutePlan(_)
            | TaskCommand::StartGoal(_)
            | TaskCommand::StartPlan(_)
            | TaskCommand::AttachPlan(_)
            | TaskCommand::ForkPlan(_)
            | TaskCommand::Replan(_) => {
                unreachable!("creation has already installed its preparation job")
            }
        }
    }

    /// Prepare a captured task mutation after its collection boundary without giving the worker commit authority.
    fn prepare_mutation(
        &mut self,
        intent: TaskCommand,
        prior: Option<crate::task::TaskPermit>,
        services: &OwnerServices,
    ) -> Result<(), OperationFailure> {
        let current = services.tasks.current().map_err(OperationFailure::Domain)?;
        match &prior {
            Some(prior) => prior
                .validate_observation(current.as_ref())
                .map_err(|_| rejected(TaskCommandFailure::Changed))?,
            None if current.is_none() => {}
            None => return Err(rejected(TaskCommandFailure::Changed)),
        }
        let services = services.clone();
        let now_ms = owner_now_ms()?;
        if let TaskCommand::EditPlan(request) = &intent {
            let request = request.clone();
            let operation_id = self.command.record.request.id.as_str().to_owned();
            self.stage = TaskStage::PreparingOperation {
                task: tokio::spawn(async move {
                    let change = services
                        .observer
                        .prepare_owner_plan_edit(&request, &operation_id, current.as_ref(), now_ms)
                        .await
                        .map_err(rejected)?;
                    Ok(TaskCommit::Mutation {
                        intent: Box::new(intent),
                        prior,
                        change: Box::new(change),
                    })
                }),
                deadline: tokio::time::Instant::now() + std::time::Duration::from_secs(60),
            };
            return Ok(());
        }
        self.stage = TaskStage::PreparingOperation {
            task: tokio::task::spawn_blocking(move || {
                if let Some(source) = intent.plan_source() {
                    let store = crate::storage::SqliteStore::read_only(&services.root)
                        .map_err(OperationFailure::Domain)?;
                    source
                        .validate_storage(&store, services.observer.session_id())
                        .map_err(rejected)?;
                }
                let action = match &intent {
                    TaskCommand::Resume(request) => crate::task::command::SessionAction::Resume {
                        request: request.request(),
                    },
                    TaskCommand::RequestPlanChanges(request) => {
                        crate::task::command::SessionAction::RequestChanges {
                            review: request.selection().normalized_request(),
                        }
                    }
                    TaskCommand::CancelPlan(request) => {
                        crate::task::command::SessionAction::CancelPlan {
                            review: request.selection().normalized_request(),
                        }
                    }
                    TaskCommand::EditPlan(_) => unreachable!(
                        "asynchronous edit preparation was selected before the blocking worker"
                    ),
                    _ => unreachable!("only prepared task mutations enter this worker"),
                };
                let change = services
                    .observer
                    .prepare_task_change(&action, current.as_ref(), services.goal_max_turns, now_ms)
                    .map_err(|_| rejected(TaskCommandFailure::PreparationUnavailable))?;
                Ok(TaskCommit::Mutation {
                    intent: Box::new(intent),
                    prior,
                    change: Box::new(change),
                })
            }),
            deadline: tokio::time::Instant::now() + std::time::Duration::from_secs(60),
        };
        Ok(())
    }

    /// Prepare only the validated execution after its displaced resources have been collected.
    fn prepare_execution(
        &mut self,
        selection: super::command::PlanExecutionSelection,
        validated: crate::broker::ValidatedPlanExecution,
        prior: Option<crate::task::TaskPermit>,
        services: &OwnerServices,
    ) -> Result<(), OperationFailure> {
        let current = services.tasks.current().map_err(OperationFailure::Domain)?;
        match &prior {
            Some(prior) => prior
                .validate_observation(current.as_ref())
                .map_err(|_| rejected(TaskCommandFailure::Changed))?,
            None if current.is_none() => {}
            None => return Err(rejected(TaskCommandFailure::Changed)),
        }
        let observer = services.observer.clone();
        let root = services.root.clone();
        let budget = services.goal_max_turns;
        let now_ms = owner_now_ms()?;
        self.stage = TaskStage::PreparingExecution {
            task: tokio::task::spawn_blocking(move || {
                let store = crate::storage::SqliteStore::read_only(&root)
                    .map_err(|_| TaskCommandFailure::StateUnavailable)?;
                PlanSourceGuard::Working(selection.source().clone())
                    .validate_storage(&store, observer.session_id())?;
                let (task, effects) = observer
                    .prepare_execution(validated.request(), budget, now_ms)
                    .map_err(|_| TaskCommandFailure::PreparationUnavailable)?;
                let change = crate::broker::plan_entry::PreparedTaskChange {
                    change: crate::task::TaskChange::prepared(current.as_ref(), task),
                    effects,
                };
                Ok(Box::new(PreparedExecution {
                    selection,
                    prior,
                    change,
                    question_set: None,
                }))
            }),
            deadline: tokio::time::Instant::now() + std::time::Duration::from_secs(60),
        };
        Ok(())
    }

    /// Capture plan source only after replaced tools and native work have settled.
    fn prepare_creation(
        &mut self,
        request: TaskCreation,
        prior: Option<crate::task::TaskPermit>,
        services: &OwnerServices,
    ) -> Result<(), OperationFailure> {
        let current = services.tasks.current().map_err(OperationFailure::Domain)?;
        match &prior {
            Some(prior) => prior
                .validate_observation(current.as_ref())
                .map_err(|_| rejected(TaskCommandFailure::Changed))?,
            None if current.is_none() => {}
            None => return Err(rejected(TaskCommandFailure::Changed)),
        }
        let elapsed = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_err(|error| OperationFailure::Domain(error.into()))?;
        let now_ms = i64::try_from(elapsed.as_millis())
            .map_err(|error| OperationFailure::Domain(error.into()))?;
        let observer = services.observer.clone();
        let root = services.root.clone();
        let budget = services.goal_max_turns;
        self.stage = TaskStage::PreparingCreation {
            task: tokio::task::spawn_blocking(move || {
                let (action, source) = match request {
                    TaskCreation::Goal(goal) => (
                        crate::task::command::SessionAction::NewGoal {
                            text: goal.objective().into(),
                        },
                        None,
                    ),
                    TaskCreation::Plan(plan) => (
                        crate::task::command::SessionAction::NewPlan {
                            text: plan.objective().into(),
                        },
                        None,
                    ),
                    TaskCreation::Fork(source) => (
                        crate::task::command::SessionAction::Fork {
                            plan_id: source.id().as_str().into(),
                        },
                        Some(PlanSourceGuard::Working(source)),
                    ),
                    TaskCreation::Attach(source) => (
                        crate::task::command::SessionAction::Attach {
                            plan_id: source.id().as_str().into(),
                        },
                        Some(PlanSourceGuard::Working(source)),
                    ),
                    TaskCreation::Replan(selection) => (
                        crate::task::command::SessionAction::Replan {
                            plan_id: selection.source().id().as_str().into(),
                            revision: selection.revision(),
                        },
                        Some(PlanSourceGuard::Historical(selection)),
                    ),
                };
                if let Some(source) = &source {
                    let store = crate::storage::SqliteStore::read_only(&root)
                        .map_err(|_| TaskCommandFailure::StateUnavailable)?;
                    source.validate_storage(&store, observer.session_id())?;
                }
                observer
                    .prepare_task_change(&action, current.as_ref(), budget, now_ms)
                    .map_err(|_| TaskCommandFailure::PreparationUnavailable)
                    .and_then(|prepared| {
                        let crate::broker::plan_entry::PreparedTaskChange {
                            change,
                            mut effects,
                        } = prepared;
                        if let Some(source) = source {
                            effects.source_plan.push(source);
                        }
                        let task = match change {
                            crate::task::TaskChange::Install(task) => *task,
                            crate::task::TaskChange::Update(_)
                            | crate::task::TaskChange::SetPlanAcceptance(_)
                            | crate::task::TaskChange::Preserve
                            | crate::task::TaskChange::Pause
                            | crate::task::TaskChange::Resume
                            | crate::task::TaskChange::ResumeWithEntry(_)
                            | crate::task::TaskChange::ResumeWithChecks(_)
                            | crate::task::TaskChange::Clear => {
                                return Err(TaskCommandFailure::PreparationUnavailable);
                            }
                        };
                        let prepared = crate::task::TaskInstallation::new(task, effects)?;
                        Ok(Box::new(PreparedCreation { prior, prepared }))
                    })
            }),
            deadline: tokio::time::Instant::now() + std::time::Duration::from_secs(60),
        };
        Ok(())
    }

    /// Park affected work under inherited control before committing a prepared task mutation.
    fn begin_collection(
        &mut self,
        mutation: TaskCollection,
        broker: &mut crate::broker::HarnessBroker,
        services: &OwnerServices,
    ) -> Result<TaskCommandProgress, OperationFailure> {
        // Revoke delivery before collection releases waiting model tools.
        broker
            .cancel_question_delivery()
            .map_err(OperationFailure::Domain)?;
        let input = if matches!(
            &self.command.record.request.command,
            Command::Task(TaskCommand::Pause(_))
        ) {
            Some(
                services
                    .input
                    .suspend(true, Some(self.command.record.request.id.as_str()))
                    .map_err(OperationFailure::Domain)?,
            )
        } else {
            None
        };
        self.transition = Some(ExchangeStop::start(
            self.command.record.request.id.clone(),
            StopIntent::Reconfigure {
                children: ChildCollectionPolicy::Collect,
            },
            services,
            None,
            self.control.take(),
        ));
        self.stage = TaskStage::Collecting(mutation);
        Ok(TaskCommandProgress::Waiting {
            events: Vec::new(),
            input,
        })
    }

    /// Claim the committed task's original input and transfer capabilities into shared delivery.
    fn prepare_input(
        &mut self,
        services: &OwnerServices,
        capabilities: TaskDeliveryCapabilities,
    ) -> Result<TaskCommandProgress, OperationFailure> {
        // Revalidate task authority before enabling delivery or producing any task input.
        let checkpoint = self
            .checkpoint
            .as_ref()
            .expect("input preparation follows its committed reservation");
        let (expected_task, expected_input, workflow) = match checkpoint {
            TaskCommandCheckpoint::InputPending { task, input } => (
                Some(task.clone()),
                InputId::new(input.as_str()).expect("task input identity is validated"),
                false,
            ),
            TaskCommandCheckpoint::WorkflowInputPending { task, input } => {
                (task.clone(), input.clone(), true)
            }
            TaskCommandCheckpoint::Control { .. } => {
                unreachable!("finite control never prepares input")
            }
        };
        let prepared = (|| -> Result<_, TaskSynchronizationFailure> {
            let current = services
                .tasks
                .current()
                .map_err(|_| TaskSynchronizationFailure::StateUnavailable)?;
            if let Some(expected_task) = &expected_task {
                expected_task
                    .validate(current.as_ref())
                    .map_err(|_| TaskSynchronizationFailure::TargetChanged)?;
            } else if current.is_some() {
                return Err(TaskSynchronizationFailure::TargetChanged);
            }
            if !workflow
                && !current.as_ref().is_some_and(|task| {
                    task.entry.as_ref().is_some_and(|entry| {
                        entry.id.as_str() == expected_input.as_str()
                            && entry.delivery == crate::task::TaskInputDelivery::Pending
                    }) || task
                        .turn
                        .as_ref()
                        .and_then(|turn| turn.continuation.as_ref())
                        .is_some_and(|input| {
                            input.id.as_str() == expected_input.as_str()
                                && input.delivery == crate::task::TaskInputDelivery::Pending
                        })
                })
            {
                return Err(TaskSynchronizationFailure::TargetChanged);
            }
            if workflow && expected_task.is_none() {
                let Command::ContinueWorkflow(request) = &self.command.record.request.command
                else {
                    unreachable!("workflow checkpoint retains ContinueWorkflow");
                };
                let source = match &request.target {
                    super::command::WorkflowContinuationTarget::Exchange(source) => source,
                    super::command::WorkflowContinuationTarget::Questions(
                        super::command::QuestionContinuationSelection {
                            workflow: super::command::QuestionWorkflow::Exchange(source),
                            ..
                        },
                    ) => source,
                    _ => return Err(TaskSynchronizationFailure::TargetChanged),
                };
                let store = crate::storage::SqliteStore::read_only(&services.root)
                    .map_err(|_| TaskSynchronizationFailure::StateUnavailable)?;
                let exchange = store
                    .load_exchange(source.id().as_str())
                    .map_err(|_| TaskSynchronizationFailure::StateUnavailable)?
                    .ok_or(TaskSynchronizationFailure::TargetChanged)?;
                source
                    .validate(&exchange, None)
                    .map_err(|_| TaskSynchronizationFailure::TargetChanged)?;
            }
            let mut updates = Vec::new();
            if services
                .input
                .snapshot()
                .map_err(|_| TaskSynchronizationFailure::StateUnavailable)?
                .suspended
            {
                updates.push(
                    services
                        .input
                        .suspend(false, Some(self.command.record.request.id.as_str()))
                        .map_err(|_| TaskSynchronizationFailure::StateUnavailable)?,
                );
            }
            if !workflow {
                match super::task_input::reconcile(services)
                    .map_err(|_| TaskSynchronizationFailure::StateUnavailable)?
                {
                    super::task_input::TaskInputProduction::Updated(produced) => {
                        updates.extend(produced)
                    }
                    super::task_input::TaskInputProduction::Idle => {}
                    super::task_input::TaskInputProduction::ContinuationCancelled { .. }
                    | super::task_input::TaskInputProduction::Superseded => {
                        return Err(TaskSynchronizationFailure::TargetChanged);
                    }
                }
            }
            let snapshot = services
                .input
                .snapshot()
                .map_err(|_| TaskSynchronizationFailure::StateUnavailable)?;
            let selected = snapshot
                .messages
                .iter()
                .find(|message| {
                    message.id == expected_input.as_str()
                        && (workflow || matches!(&message.purpose,
                    crate::input::InputPurpose::TaskEntry { revision, id, .. }
                    | crate::input::InputPurpose::TaskContinuation { revision, id, .. }
                    if expected_task.as_ref().is_some_and(|task| revision == task.revision()) && id.as_str() == expected_input.as_str()))
                })
                .ok_or(TaskSynchronizationFailure::InputPreparationUnavailable)?;
            let message = services
                .input
                .claim(
                    &selected.id,
                    selected.revision,
                    crate::input::InputRoute::Turn,
                )
                .map_err(|_| TaskSynchronizationFailure::StateUnavailable)?
                .ok_or(TaskSynchronizationFailure::InputPreparationUnavailable)?;
            Ok((message, updates))
        })();
        let (message, updates) = match prepared {
            Ok(prepared) => prepared,
            Err(failure) => return Ok(self.failed(failure)),
        };
        self.stage = TaskStage::AwaitingInput(InputRevision {
            id: InputId::new(message.id.clone())
                .expect("committed task input has a validated identity"),
            revision: message.revision,
        });
        Ok(TaskCommandProgress::DeliverInput(Box::new(
            TaskInputStart {
                message,
                capabilities,
                updates,
            },
        )))
    }

    /// Reconcile a settled preparation against persisted authority before releasing its command.
    fn reconcile_source_artifacts(&self, services: &OwnerServices) -> Result<(), OperationFailure> {
        let source = match &self.command.record.request.command {
            Command::Task(intent) => intent
                .plan_source()
                .map(|source| source.id().as_str().to_owned()),
            Command::ContinueWorkflow(request) => request
                .execution()
                .map(|selection| selection.source().id().as_str().to_owned()),
            _ => None,
        };
        if let Some(id) = source {
            let store = crate::storage::SqliteStore::read_only(&services.root)
                .map_err(OperationFailure::Domain)?;
            if let Some(plan) = store.load_plan(&id).map_err(OperationFailure::Domain)? {
                services
                    .observer
                    .reconcile_plan_artifacts(&plan)
                    .map_err(OperationFailure::Domain)?;
            }
        }
        Ok(())
    }

    /// Preserve committed task facts independently of a failed native preparation or observation.
    fn failure(&self, failure: TaskSynchronizationFailure) -> CommandFailure {
        CommandFailure::TaskSynchronization {
            committed: self
                .checkpoint
                .clone()
                .expect("post-commit failures retain their task checkpoint"),
            failure,
        }
    }

    /// Finish a post-commit stage without claiming that initial input dispatched.
    fn failed(&self, failure: TaskSynchronizationFailure) -> TaskCommandProgress {
        TaskCommandProgress::Finished {
            outcome: Err(self.failure(failure)),
            events: Vec::new(),
        }
    }

    /// Get the task intent captured in this command's immutable journal request.
    fn intent(&self) -> &TaskCommand {
        let Command::Task(intent) = &self.command.record.request.command else {
            unreachable!("task command stages only receive Task intents")
        };
        intent
    }
}

/// Carry one task fact without granting its worker task mutation authority.
pub(super) enum TaskCommandEvent {
    /// A local policy request can validate task authority without acquiring a native lane.
    Preflight,
    /// The retained acquisition settled before any lifecycle effect began.
    Control(Result<OwnedSemaphorePermit, CommandFailure>),
    /// An owner-issued filesystem/preparation worker settled without mutating task authority.
    Prepared(Result<Box<PreparedCreation>, TaskCommandFailure>),
    /// Execution artifacts settled without granting the worker task mutation authority.
    ExecutionPrepared(Result<Box<PreparedExecution>, TaskCommandFailure>),
    /// An owned local preparation completed while the owner kept admitting facts.
    OperationPrepared(Result<TaskCommit, OperationFailure>),
    /// Preparation observation expired; its actual worker remains retained for collection.
    PreparationDeadline,
    /// Shared collection produced a fact for this active task command.
    Transition(TransitionEvent),
    /// Required collection/preflight completed; the owner may attempt its task transaction.
    Commit,
    /// Local acquisition settled; only the owner may claim and install the task input.
    Delivery(Result<TaskDeliveryCapabilities, CommandFailure>),
    /// Initial foreground handoff did not settle within its bounded observation period.
    DispatchDeadline,
    /// A correlated handoff or input failure produced a finite command outcome.
    Ready,
}

impl TaskCommandEvent {
    /// Get prepared task identity for correlated logs without exposing mutation authority.
    pub(super) fn prepared_task(&self) -> Option<&crate::task::Task> {
        match self {
            Self::Prepared(result) => result
                .as_ref()
                .ok()
                .map(|prepared| prepared.prepared.task()),
            Self::OperationPrepared(_)
            | Self::ExecutionPrepared(_)
            | Self::Preflight
            | Self::Control(_)
            | Self::PreparationDeadline
            | Self::Transition(_)
            | Self::Commit
            | Self::Delivery(_)
            | Self::DispatchDeadline
            | Self::Ready => None,
        }
    }

    /// Get a finite event label without recording native tokens or task contents.
    pub(super) fn name(&self) -> &'static str {
        match self {
            Self::Preflight => "task_preflight",
            Self::Control(_) => "task_control_acquired",
            Self::Prepared(_) => "task_prepared",
            Self::ExecutionPrepared(_) => "task_execution_prepared",
            Self::OperationPrepared(_) => "task_operation_prepared",
            Self::PreparationDeadline => "task_preparation_deadline",
            Self::Transition(_) => "task_collection",
            Self::Commit => "task_commit_ready",
            Self::Delivery(_) => "task_delivery_acquired",
            Self::DispatchDeadline => "task_dispatch_deadline",
            Self::Ready => "task_completion_ready",
        }
    }
}

/// Distinguish task progress, an internal effect request, and finite command completion.
pub(super) enum TaskCommandProgress {
    /// Retain the command slot while publishing already-committed observations.
    Waiting {
        events: Vec<SessionEvent>,
        input: Option<crate::input::InputUpdate>,
    },
    /// Install this exact claimed input in the existing owner delivery pipeline before publication.
    DeliverInput(Box<TaskInputStart>),
    /// Persist the finite control result; this never represents the eventual task answer.
    Finished {
        outcome: Result<TaskCommandOutcome, CommandFailure>,
        events: Vec<SessionEvent>,
    },
}

impl TaskCommandProgress {
    /// Advance a local stage without manufacturing output or another command.
    fn waiting() -> Self {
        Self::Waiting {
            events: Vec::new(),
            input: None,
        }
    }
}

enum TaskStage {
    Preflight,
    Acquiring(JoinHandle<Result<OwnedSemaphorePermit, CommandFailure>>),
    PreparingCreation {
        task: JoinHandle<Result<Box<PreparedCreation>, TaskCommandFailure>>,
        deadline: tokio::time::Instant,
    },
    PreparingExecution {
        task: JoinHandle<Result<Box<PreparedExecution>, TaskCommandFailure>>,
        deadline: tokio::time::Instant,
    },
    PreparingOperation {
        task: JoinHandle<Result<TaskCommit, OperationFailure>>,
        deadline: tokio::time::Instant,
    },
    Collecting(TaskCollection),
    Commit(TaskCommit),
    /// Settle the exchange a finite control left finalizing, then report the committed outcome.
    SettlingFinalization {
        outcome: Box<TaskCommandOutcome>,
    },
    AcquiringDelivery(JoinHandle<Result<TaskDeliveryCapabilities, CommandFailure>>),
    AwaitingInput(InputRevision),
    AwaitingDispatch {
        input: InputRevision,
        work: uuid::Uuid,
        receipt: forge_provider_api::SubmissionId,
        deadline: tokio::time::Instant,
    },
    Ready(Box<Result<TaskCommandOutcome, CommandFailure>>),
    Consumed,
}

enum TaskCollection {
    Mutation {
        intent: Box<TaskCommand>,
        prior: Option<crate::task::TaskPermit>,
    },
    Execution {
        selection: Box<super::command::PlanExecutionSelection>,
        validated: Box<crate::broker::ValidatedPlanExecution>,
        prior: Option<crate::task::TaskPermit>,
    },
    Control(crate::task::TaskPermit),
    Creation {
        request: Box<TaskCreation>,
        prior: Option<crate::task::TaskPermit>,
    },
}

pub(super) enum TaskCommit {
    Mutation {
        intent: Box<TaskCommand>,
        prior: Option<crate::task::TaskPermit>,
        change: Box<crate::broker::plan_entry::PreparedTaskChange>,
    },
    QuestionWait {
        request: Box<super::command::WorkflowContinuationRequest>,
        prior: Option<crate::task::TaskPermit>,
        questions: super::command::PlanQuestionSetSelection,
    },
    Continuation(Box<PreparedContinuation>),
    Execution(Box<PreparedExecution>),
    Control(crate::task::TaskPermit),
    AttachmentCurrent {
        permit: crate::task::TaskPermit,
        source: PlanSelection,
    },
    Creation(Box<PreparedCreation>),
}

enum TaskCreation {
    Goal(NewGoal),
    Plan(NewPlan),
    Attach(PlanSelection),
    Fork(PlanSelection),
    Replan(PlanRevisionSelection),
}

enum TaskCommitResult {
    Complete(TaskCommandOutcome),
    ScheduleInput(TaskCommandCheckpoint),
}

/// Retain the captured workflow alongside its prepared reservation until one atomic commit.
pub(super) struct PreparedContinuation {
    request: super::command::WorkflowContinuationRequest,
    prior: Option<crate::task::TaskPermit>,
    change: crate::broker::plan_entry::PreparedTaskChange,
    instruction: String,
}

/// Retain execution authority and prepared effects until their atomic owner commit.
pub(super) struct PreparedExecution {
    selection: super::command::PlanExecutionSelection,
    prior: Option<crate::task::TaskPermit>,
    change: crate::broker::plan_entry::PreparedTaskChange,
    question_set: Option<super::command::PlanQuestionSetSelection>,
}

/// Retain the prior task fence alongside a complete preparation result until the owner can install it.
pub(super) struct PreparedCreation {
    prior: Option<crate::task::TaskPermit>,
    prepared: crate::task::TaskInstallation,
}

/// Retain exclusive root delivery capabilities acquired in compatibility lock order.
pub(super) struct TaskDeliveryCapabilities {
    pub(super) execution: crate::command::ExecutionPermit,
    pub(super) control: OwnedSemaphorePermit,
}

/// Transfer a claimed task input into the shared root preparation and receipt pipeline.
pub(super) struct TaskInputStart {
    pub(super) message: crate::input::InputMessage,
    pub(super) capabilities: TaskDeliveryCapabilities,
    pub(super) updates: Vec<crate::input::InputUpdate>,
}

/// Identify whether a saved-plan mutation also owns the current task lifecycle.
fn task_owns_plan(current: Option<&crate::task::Task>, source: &PlanSelection) -> bool {
    current.is_some_and(|task| match &task.workflow {
        crate::task::WorkflowSnapshot::Plan { plan } => plan.artifact.id == source.id().as_str(),
        crate::task::WorkflowSnapshot::Execute { execution } => {
            execution.record.plan_id == source.id().as_str()
        }
        crate::task::WorkflowSnapshot::Goal { .. } => false,
    })
}

/// Validate the exact question revision before choosing a continuation transition.
fn validate_question_continuation(
    request: &super::command::WorkflowContinuationRequest,
    current: Option<&crate::task::Task>,
    services: &OwnerServices,
) -> Result<(), OperationFailure> {
    use super::command::{QuestionWorkflow, WorkflowContinuationTarget};
    let WorkflowContinuationTarget::Questions(selection) = &request.target else {
        return Ok(());
    };
    let store =
        crate::storage::SqliteStore::read_only(&services.root).map_err(OperationFailure::Domain)?;
    match &selection.workflow {
        QuestionWorkflow::PlanReview(source) => {
            source.validate_target(current).map_err(rejected)?;
            let plan = store
                .load_plan(source.source().id().as_str())
                .map_err(OperationFailure::Domain)?
                .ok_or_else(|| rejected(TaskCommandFailure::PlanMissing))?;
            source.source().validate(&plan).map_err(rejected)?;
            selection
                .questions
                .validate(
                    &plan
                        .review
                        .as_ref()
                        .ok_or_else(|| rejected(TaskCommandFailure::Changed))?
                        .elicitation,
                )
                .map_err(rejected)
        }
        QuestionWorkflow::Plan { source, task } => {
            task.validate(current).map_err(rejected)?;
            let plan = store
                .load_plan(source.id().as_str())
                .map_err(OperationFailure::Domain)?
                .ok_or_else(|| rejected(TaskCommandFailure::PlanMissing))?;
            source.validate(&plan).map_err(rejected)?;
            selection
                .questions
                .validate(
                    plan.elicitation
                        .as_ref()
                        .ok_or_else(|| rejected(TaskCommandFailure::Changed))?,
                )
                .map_err(rejected)
        }
        QuestionWorkflow::Exchange(source) => {
            let exchange = store
                .load_exchange(source.id().as_str())
                .map_err(OperationFailure::Domain)?
                .ok_or_else(|| rejected(TaskCommandFailure::Changed))?;
            source.validate(&exchange, current).map_err(rejected)?;
            selection
                .questions
                .validate(
                    exchange
                        .elicitation()
                        .ok_or_else(|| rejected(TaskCommandFailure::Changed))?,
                )
                .map_err(rejected)
        }
    }
}

/// Prepare continuation input and question consumption without starting native work.
fn prepare_workflow_continuation(
    request: super::command::WorkflowContinuationRequest,
    current: Option<&crate::task::Task>,
    services: &OwnerServices,
) -> Result<TaskCommit, OperationFailure> {
    use super::command::{QuestionWorkflow, WorkflowContinuationTarget};
    let store =
        crate::storage::SqliteStore::read_only(&services.root).map_err(OperationFailure::Domain)?;
    let now_ms = owner_now_ms()?;
    let prior = current.map(|task| task.permit());
    let mut effects = crate::storage::TaskEffects::default();
    let instruction;
    let change;
    match &request.target {
        WorkflowContinuationTarget::Questions(selection) => match &selection.workflow {
            QuestionWorkflow::Plan { source, .. } => {
                let mut plan = store
                    .load_plan(source.id().as_str())
                    .map_err(OperationFailure::Domain)?
                    .ok_or_else(|| rejected(TaskCommandFailure::PlanMissing))?;
                let elicitation = plan
                    .elicitation
                    .as_ref()
                    .ok_or_else(|| rejected(TaskCommandFailure::Changed))?;
                if elicitation.has_pending_decisions() {
                    return Ok(TaskCommit::QuestionWait {
                        request: Box::new(request.clone()),
                        prior,
                        questions: selection.questions.clone(),
                    });
                }
                let elicitation = plan
                    .elicitation
                    .take()
                    .expect("validated plan retains elicitation");
                let feedback = elicitation.feedback();
                for question in &elicitation.question_set.questions {
                    let response = elicitation
                        .answer
                        .iter()
                        .find(|answer| answer.question_id == question.id)
                        .map(|answer| answer.response.clone());
                    plan.question_ledger.resolve(question, response, now_ms);
                }
                crate::plan::state_machine::PlanStateMachine::apply(
                    &mut plan,
                    crate::plan::state_machine::PlanEvent::FeedbackConsumed,
                    now_ms,
                )
                .map_err(OperationFailure::Domain)?;
                plan.generation.reset_no_progress();
                instruction = crate::plan::PlanPrompt::feedback(&plan.request, &feedback);
                effects.plan.push(plan.clone());
                effects.lifecycle.push(crate::plan::PlanLifecycleRecord {
                    id: uuid::Uuid::new_v4().to_string(),
                    session_id: services.observer.session_id().into(),
                    plan_id: plan.id.clone(),
                    title: plan.title.clone(),
                    anchor: store
                        .latest_exchange(services.observer.session_id())
                        .map_err(OperationFailure::Domain)?
                        .as_ref()
                        .map(crate::plan::ExchangeAnchor::capture),
                    kind: crate::plan::PlanLifecycleKind::QuestionAnswered,
                    model_revision: plan.model_revision,
                    user_revision: plan.user_revision,
                    overall_comment: None,
                    annotation: Vec::new(),
                    question: Some(elicitation.question_set.clone()),
                    answer: Some(feedback.clone()),
                    created_at_ms: now_ms,
                });
                effects
                    .source_plan
                    .push(PlanSourceGuard::Working(source.clone()));
                let mut task = current
                    .ok_or_else(|| rejected(TaskCommandFailure::MissingTask))?
                    .draft();
                let crate::task::WorkflowSnapshot::Plan { plan: active } = &mut task.workflow
                else {
                    return Err(rejected(TaskCommandFailure::Changed));
                };
                active.artifact = plan;
                let completed = store
                    .latest_exchange(services.observer.session_id())
                    .map_err(OperationFailure::Domain)?
                    .ok_or_else(|| rejected(TaskCommandFailure::Changed))?;
                task.turn = Some(crate::task::TaskTurn {
                    id: crate::task::TaskTurnId {
                        exchange_ordinal: completed.ordinal,
                        turn_ordinal: completed.turn.len(),
                    },
                    continuation: Some(crate::task::TaskContinuation {
                        id: crate::task::TaskInputId::fresh(),
                        reason: "The planning questions were answered".into(),
                        delivery: crate::task::TaskInputDelivery::Pending,
                        work: crate::task::TaskWork::Provider {
                            instruction: instruction.clone(),
                        },
                    }),
                });
                change = crate::task::TaskChange::Update(Box::new(
                    crate::task::Task::try_from(task).map_err(OperationFailure::Domain)?,
                ));
            }
            QuestionWorkflow::Exchange(source) => {
                let mut exchange = store
                    .load_exchange(source.id().as_str())
                    .map_err(OperationFailure::Domain)?
                    .ok_or_else(|| rejected(TaskCommandFailure::Changed))?;
                source.validate(&exchange, current).map_err(rejected)?;
                if exchange
                    .turn
                    .iter()
                    .any(|turn| turn.state() == crate::turn::TurnState::Running)
                {
                    return Err(rejected(TaskCommandFailure::Changed));
                }
                let elicitation = exchange
                    .elicitation()
                    .ok_or_else(|| rejected(TaskCommandFailure::Changed))?;
                if elicitation.has_pending_decisions() {
                    return Ok(TaskCommit::QuestionWait {
                        request: Box::new(request.clone()),
                        prior,
                        questions: selection.questions.clone(),
                    });
                }
                let elicitation = exchange
                    .take_elicitation()
                    .expect("validated exchange retains elicitation");
                let feedback = elicitation.feedback();
                if exchange.check_setup.is_some()
                    && elicitation.answer.iter().any(|answer| {
                        answer.source == crate::session::ResolutionSource::User
                            && !matches!(
                                answer.response,
                                crate::plan::PlanQuestionResponse::Skipped
                            )
                    })
                {
                    exchange.check_setup = Some(crate::checks::CheckSetupState::Reviewed);
                }
                exchange.set_awaiting_input(false);
                for node in &mut exchange.node_list {
                    if let crate::exchange::ExchangeNode::QuestionPresented {
                        question, answer, ..
                    } = node
                        && question.id == elicitation.question_set.id
                    {
                        *answer = Some(feedback.clone());
                    }
                }
                exchange
                    .append_question_input(
                        crate::exchange::InputIntent::Answer,
                        feedback.clone(),
                        crate::exchange::QuestionInput {
                            set_id: elicitation.question_set.id,
                            question_id: None,
                            answer: elicitation.answer,
                        },
                        now_ms,
                    )
                    .map_err(OperationFailure::Domain)?;
                effects.exchange.push(exchange);
                instruction = format!(
                    "Harness resolved the pending questions and recorded their response sources. Continue the original request using these responses.\n\n{feedback}"
                );
                change = continuation_task_change(
                    current,
                    source.completed_turn(),
                    &instruction,
                    now_ms,
                )?;
            }
            QuestionWorkflow::PlanReview(_) => {
                unreachable!("review continuation uses execution preparation")
            }
        },
        WorkflowContinuationTarget::Exchange(source) => {
            let exchange = store
                .load_exchange(source.id().as_str())
                .map_err(OperationFailure::Domain)?
                .ok_or_else(|| rejected(TaskCommandFailure::Changed))?;
            source.validate(&exchange, current).map_err(rejected)?;
            if !exchange.is_finished() {
                return Err(rejected(TaskCommandFailure::Changed));
            }
            if current.is_some() && request.message.is_none() {
                let prepared = services
                    .observer
                    .prepare_task_change(
                        &crate::task::command::SessionAction::ReserveContinuation {
                            completed_turn: Some(source.completed_turn().clone()),
                        },
                        current,
                        services.goal_max_turns,
                        now_ms,
                    )
                    .map_err(OperationFailure::Domain)?;
                instruction = match &prepared.change {
                    crate::task::TaskChange::Update(task) => task
                        .turn
                        .as_ref()
                        .and_then(|turn| turn.continuation.as_ref())
                        .and_then(|input| input.work.instruction())
                        .unwrap_or("")
                        .into(),
                    _ => return Err(rejected(TaskCommandFailure::Changed)),
                };
                change = prepared.change;
                effects = prepared.effects;
            } else {
                instruction = request.message.clone().unwrap_or_else(|| {
                    "Continue the previous request from its completed state.".into()
                });
                change = continuation_task_change(
                    current,
                    source.completed_turn(),
                    &instruction,
                    now_ms,
                )?;
            }
        }
        WorkflowContinuationTarget::Plan(_) => {
            unreachable!("plan continuation uses execution preparation")
        }
    }
    Ok(TaskCommit::Continuation(Box::new(PreparedContinuation {
        request,
        prior,
        change: crate::broker::plan_entry::PreparedTaskChange { change, effects },
        instruction,
    })))
}

/// Reserve explicit continuation work while preserving the completed-turn boundary and task identity.
fn continuation_task_change(
    current: Option<&crate::task::Task>,
    completed: &crate::task::TaskTurnId,
    instruction: &str,
    now_ms: i64,
) -> Result<crate::task::TaskChange, OperationFailure> {
    let Some(current) = current else {
        return Ok(crate::task::TaskChange::Preserve);
    };
    if !current.entry_accepted()
        || !matches!(
            current.status(),
            crate::task::TaskStatus::Running | crate::task::TaskStatus::Waiting
        )
    {
        return Err(rejected(TaskCommandFailure::Changed));
    }
    if current.turn.as_ref().is_some_and(|turn| {
        turn.id > *completed || turn.id == *completed && turn.continuation.is_some()
    }) {
        return Err(rejected(TaskCommandFailure::Changed));
    }
    let mut task = current.draft();
    task.updated_at_ms = now_ms;
    task.turn = Some(crate::task::TaskTurn {
        id: completed.clone(),
        continuation: Some(crate::task::TaskContinuation {
            id: crate::task::TaskInputId::fresh(),
            reason: "The selected workflow was continued".into(),
            delivery: crate::task::TaskInputDelivery::Pending,
            work: crate::task::TaskWork::Provider {
                instruction: instruction.into(),
            },
        }),
    });
    Ok(crate::task::TaskChange::Update(Box::new(
        crate::task::Task::try_from(task).map_err(OperationFailure::Domain)?,
    )))
}

/// Read a checked timestamp for task preparation and atomic owner transitions.
fn owner_now_ms() -> Result<i64, OperationFailure> {
    let elapsed = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_err(|error| OperationFailure::Domain(error.into()))?;
    i64::try_from(elapsed.as_millis()).map_err(|error| OperationFailure::Domain(error.into()))
}

/// Preserve a rejected task target as a safe, typed command failure before committing a mutation.
fn rejected(failure: TaskCommandFailure) -> OperationFailure {
    OperationFailure::Transition(TransitionFailure::Rejected(CommandFailure::Task(failure)))
}
