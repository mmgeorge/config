use anyhow::{Context, Result};
use std::sync::{Arc, Mutex};
use tokio::sync::{OwnedSemaphorePermit, Semaphore, mpsc, oneshot};

const MAX_STEERING_REQUESTS: usize = 256;

use super::{HarnessEventSink, ProviderError};

#[derive(Clone, Debug)]
/// Distinguishes user steering from child results delivered without user-prompt rendering.
pub enum SteeringInput {
    /// Records an explicit user addition in the parent timeline.
    User {
        text: String,
        attachments: Vec<crate::attachment::InputAttachment>,
        /// Durable input ownership retained through queueing and native acknowledgment.
        submission: Option<Arc<super::submission::Submission>>,
    },
    TaskEntry {
        start: crate::exchange::TaskStart,
        text: String,
        submission: Arc<super::submission::Submission>,
    },
    /// Adds an attributed child result while its transcript remains under the child.
    AgentResult(String),
}

impl SteeringInput {
    /// Returns the provider-visible message without its presentation origin.
    pub fn text(&self) -> &str {
        match self {
            Self::User { text, .. } | Self::TaskEntry { text, .. } | Self::AgentResult(text) => {
                text
            }
        }
    }

    /// Return explicit files carried by user steering, excluding generated child reports.
    pub(crate) fn attachments(&self) -> &[crate::attachment::InputAttachment] {
        match self {
            Self::User { attachments, .. } => attachments,
            Self::TaskEntry { .. } | Self::AgentResult(_) => &[],
        }
    }

    pub(crate) fn submission(&self) -> Option<Arc<super::submission::Submission>> {
        match self {
            Self::User { submission, .. } => submission.clone(),
            Self::TaskEntry { submission, .. } => Some(submission.clone()),
            Self::AgentResult(_) => None,
        }
    }

    pub(crate) fn task_start(&self) -> Option<&crate::exchange::TaskStart> {
        match self {
            Self::TaskEntry { start, .. } => Some(start),
            _ => None,
        }
    }
}

impl From<String> for SteeringInput {
    fn from(text: String) -> Self {
        Self::User {
            text,
            attachments: Vec::new(),
            submission: None,
        }
    }
}

impl From<&str> for SteeringInput {
    fn from(text: &str) -> Self {
        text.to_owned().into()
    }
}

/// Routes input into the provider turn that currently owns the provider transport.
#[derive(Clone, Default)]
pub struct SteeringLane {
    state: Arc<Mutex<SteeringState>>,
}

#[derive(Default)]
struct SteeringState {
    generation: u64,
    sender: Option<SteeringSender>,
}

#[derive(Clone)]
struct SteeringSender {
    sender: mpsc::Sender<SteerCommand>,
    ordinary: Arc<Semaphore>,
    #[cfg(test)]
    interrupt: Arc<Semaphore>,
}

/// Retain one admitted receiver so delayed preparation cannot select a later active turn.
#[derive(Clone)]
pub(crate) struct SteeringTarget {
    sender: SteeringSender,
}

impl SteeringTarget {
    /// Get whether the exact captured turn still accepts queued steering.
    pub(crate) fn is_open(&self) -> bool {
        !self.sender.sender.is_closed()
    }

    /// Deliver only to the captured receiver, preserving normalized admission and receipt errors.
    pub(crate) async fn steer(
        &self,
        input: SteeringInput,
    ) -> std::result::Result<(), ProviderError> {
        SteeringLane::submit_to(self.sender.clone(), ActiveTurnOperation::Steer, Some(input))
            .await
            .map_err(|error| {
                super::lifecycle_error::classify(
                    forge_provider_api::ProviderOperation::TurnSteer,
                    error,
                )
            })
    }
}

/// Owns one active turn's steering receiver and clears it when that turn ends.
pub struct ActiveSteering {
    lane: SteeringLane,
    generation: u64,
    receiver: mpsc::Receiver<SteerCommand>,
    event_sink: Option<HarnessEventSink>,
}

/// Carries one steering input and its provider acknowledgement.
pub struct SteerCommand {
    _admission: OwnedSemaphorePermit,
    pub text: String,
    pub attachments: Vec<crate::attachment::InputAttachment>,
    /// Durable input ownership shared with the native adapter.
    pub submission: Option<Arc<super::submission::Submission>>,
    pub operation: ActiveTurnOperation,
    completion: Option<oneshot::Sender<std::result::Result<(), ProviderError>>>,
    event_sink: Option<HarnessEventSink>,
    publish_input: bool,
    task_start: Option<crate::exchange::TaskStart>,
}

/// Defines the active-turn operation carried over the shared provider process.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ActiveTurnOperation {
    Steer,
    #[cfg(test)]
    Interrupt,
    #[cfg(test)]
    Cancel,
}

impl SteeringLane {
    /// Capture the current receiver without deferring turn selection until an asynchronous send.
    pub(crate) fn capture(&self) -> Option<SteeringTarget> {
        self.state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .sender
            .clone()
            .map(|sender| SteeringTarget { sender })
    }

    /// Maximum number of ordinary controls owned by an active turn.
    pub(crate) const CAPACITY: usize = MAX_STEERING_REQUESTS;

    /// Reports whether a provider turn or Harness continuation owns the input receiver.
    pub(crate) fn is_active(&self) -> bool {
        self.state
            .lock()
            .expect("steering lane lock poisoned")
            .sender
            .is_some()
    }

    /// Attach a receiver to the active provider turn before accepting steering input.
    pub fn activate(&self, event_sink: Option<HarnessEventSink>) -> Result<ActiveSteering> {
        let (sender, receiver) = mpsc::channel(MAX_STEERING_REQUESTS + 1);
        let mut state = self.state.lock().expect("steering lane lock poisoned");
        anyhow::ensure!(
            state.sender.is_none(),
            "provider already has an active turn"
        );
        state.generation += 1;
        let generation = state.generation;
        state.sender = Some(SteeringSender {
            sender,
            ordinary: Arc::new(Semaphore::new(MAX_STEERING_REQUESTS)),
            #[cfg(test)]
            interrupt: Arc::new(Semaphore::new(1)),
        });
        Ok(ActiveSteering {
            lane: self.clone(),
            generation,
            receiver,
            event_sink,
        })
    }

    /// Delivers input to the active turn and publishes only acknowledged user additions.
    #[cfg(test)]
    pub async fn steer(&self, input: SteeringInput) -> Result<()> {
        self.submit(ActiveTurnOperation::Steer, Some(input)).await
    }

    /// Interrupt the parent and descendants and wait for terminal provider evidence.
    #[cfg(test)]
    pub async fn cancel_pending_control(&self) -> Result<()> {
        self.submit(ActiveTurnOperation::Cancel, None).await
    }

    #[cfg(test)]
    async fn submit(
        &self,
        operation: ActiveTurnOperation,
        input: Option<SteeringInput>,
    ) -> Result<()> {
        let sender = self
            .state
            .lock()
            .expect("steering lane lock poisoned")
            .sender
            .clone();
        let Some(sender) = sender else {
            #[cfg(test)]
            if operation == ActiveTurnOperation::Cancel {
                return Ok(());
            }
            return Err(not_admitted().into());
        };
        Self::submit_to(sender, operation, input).await
    }

    /// Admit a control to one captured receiver without consulting the lane's later generation.
    async fn submit_to(
        sender: SteeringSender,
        operation: ActiveTurnOperation,
        input: Option<SteeringInput>,
    ) -> Result<()> {
        let admission = match operation {
            ActiveTurnOperation::Steer => sender
                .ordinary
                .acquire_owned()
                .await
                .context("active turn control admission is closed")?,
            #[cfg(test)]
            ActiveTurnOperation::Interrupt | ActiveTurnOperation::Cancel => sender
                .interrupt
                .try_acquire_owned()
                .context("active turn control admission is full")?,
        };
        let (completion, response) = oneshot::channel();
        let publish_input = input
            .as_ref()
            .is_some_and(|input| !matches!(input, SteeringInput::AgentResult(_)));
        let text = input
            .as_ref()
            .map(|input| input.text().to_owned())
            .unwrap_or_default();
        let attachments = input
            .as_ref()
            .map(|input| input.attachments().to_vec())
            .unwrap_or_default();
        let submission = input.as_ref().and_then(SteeringInput::submission);
        let task_start = input.as_ref().and_then(SteeringInput::task_start).cloned();
        sender
            .sender
            .try_send(SteerCommand {
                _admission: admission,
                text,
                attachments,
                submission: submission.clone(),
                operation,
                completion: Some(completion),
                event_sink: None,
                publish_input,
                task_start,
            })
            .map_err(|_| not_admitted())?;
        if let Some(submission) = submission {
            submission.mark_steering_queued();
        }
        response
            .await
            .context("active turn completed before steering input was acknowledged")?
            .map_err(anyhow::Error::from)
    }
}

impl ActiveSteering {
    /// Release this completed lane while retaining its receiver until its owner is dropped.
    pub(crate) fn retire(&mut self) {
        self.receiver.close();
        let mut state = self.lane.state.lock().expect("steering lane lock poisoned");
        if state.generation == self.generation
            && let Some(sender) = state.sender.take()
        {
            sender.ordinary.close();
            #[cfg(test)]
            sender.interrupt.close();
        }
    }
    /// Drains input already admitted before a provider-turn boundary.
    pub(crate) fn try_receive(&mut self) -> Option<SteerCommand> {
        let mut command = self.receiver.try_recv().ok()?;
        command.event_sink.clone_from(&self.event_sink);
        Some(command)
    }

    /// Receive the next steering input in submission order.
    pub async fn receive(&mut self) -> Option<SteerCommand> {
        // Retirement fences new admission while settlement still polls the owner.
        // Its already-admitted commands are drained explicitly through try_receive.
        if self.receiver.is_closed() {
            return std::future::pending().await;
        }
        let mut command = self.receiver.recv().await?;
        command.event_sink.clone_from(&self.event_sink);
        Some(command)
    }
}

impl Drop for ActiveSteering {
    fn drop(&mut self) {
        let mut state = self.lane.state.lock().expect("steering lane lock poisoned");
        if state.generation == self.generation
            && let Some(sender) = state.sender.take()
        {
            sender.ordinary.close();
            #[cfg(test)]
            sender.interrupt.close();
        }
    }
}

impl SteerCommand {
    /// Release a terminal owner's undispatched input without authorizing automatic replay.
    pub(crate) async fn cancel_undispatched(self) -> Result<()> {
        if let Some(submission) = &self.submission {
            submission.persist(|submission| submission.cancel()).await?;
        }
        self.complete(Err(ProviderError::Cancelled.into())).await;
        Ok(())
    }

    pub(crate) fn input(&self) -> SteeringInput {
        if !self.publish_input {
            return SteeringInput::AgentResult(self.text.clone());
        }
        match (&self.task_start, &self.submission) {
            (Some(start), Some(submission)) => SteeringInput::TaskEntry {
                start: start.clone(),
                text: self.text.clone(),
                submission: submission.clone(),
            },
            _ => SteeringInput::User {
                text: self.text.clone(),
                attachments: self.attachments.clone(),
                submission: self.submission.clone(),
            },
        }
    }
    /// Retain continuation ownership until provider acceptance or a terminal failure.
    pub(crate) async fn complete_with(
        mut self,
        operation: impl std::future::Future<Output = Result<super::retry::RetryAttempt>>,
        event_sink: HarnessEventSink,
    ) -> Result<(super::retry::RetryAttempt, Option<Self>)> {
        self.event_sink = Some(event_sink);
        let mut operation = Box::pin(operation);
        let result = if let Some(submission) = &self.submission {
            let mut acceptance = submission.acceptance();
            tokio::select! {
                biased;
                accepted = async { acceptance.wait_for(|accepted| *accepted).await.map(drop) } => {
                    accepted.context("continuation acceptance channel closed")?;
                    self.complete(Ok(())).await;
                    let result = operation.await?;
                    return Ok((result, None));
                }
                result = &mut operation => result,
            }
        } else {
            operation.await
        };
        if matches!(&result, Ok(super::retry::RetryAttempt::Stale(..))) {
            return Ok((result?, Some(self)));
        }
        let accepted = self
            .submission
            .as_ref()
            .map(|submission| submission.record())
            .transpose()?
            .is_some_and(|record| {
                matches!(
                    record.delivery,
                    super::submission::SubmissionState::Accepted { .. }
                )
            });
        self.complete(if accepted {
            Ok(())
        } else {
            result.as_ref().map(|_| ()).map_err(|error| {
                super::lifecycle_error::classify_source(
                    forge_provider_api::ProviderOperation::TurnSteer,
                    error.as_ref(),
                )
                .into()
            })
        })
        .await;
        Ok((result?, None))
    }

    /// Defers only an input that never crossed native dispatch, and settles all other outcomes.
    pub(crate) async fn settle_delivery(self, result: Result<()>) -> Result<Option<Self>> {
        match result {
            Ok(()) => {
                self.complete(Ok(())).await;
                Ok(None)
            }
            Err(error) => {
                let not_consumed = error.downcast_ref::<ProviderError>().is_some_and(|error| {
                    matches!(
                        error,
                        ProviderError::NotConsumed | ProviderError::PreparationFailed(_)
                    )
                });
                let prepared = self
                    .submission
                    .as_ref()
                    .map(|submission| submission.record())
                    .transpose()?
                    .is_some_and(|record| {
                        matches!(
                            record.delivery,
                            super::submission::SubmissionState::Prepared
                        )
                    });
                if not_consumed && prepared {
                    return Ok(Some(self));
                }
                self.complete(Err(super::lifecycle_error::classify_source(
                    forge_provider_api::ProviderOperation::TurnSteer,
                    error.as_ref(),
                )
                .into()))
                    .await;
                Err(error)
            }
        }
    }

    /// Resolve the originating Harness request with the provider result.
    pub async fn complete(mut self, mut result: Result<()>) {
        if let Err(error) = &result
            && let Some(submission) = &self.submission
        {
            let failure = format!("{error:#}");
            if let Err(persist) = submission
                .persist(move |submission| submission.failed(failure))
                .await
            {
                result = Err(persist.context(format!("{error:#}")));
            }
        }
        if result.is_ok()
            && self.publish_input
            && self.operation == ActiveTurnOperation::Steer
            && let Some(event_sink) = self.event_sink.take()
        {
            let publication = async {
                let receipt = self
                    .submission
                    .clone()
                    .context("successful steering requires its journal receipt")?;
                let presentation = match self.task_start.take() {
                    Some(start) => super::session_execution::InputPresentation::TaskStarted {
                        start,
                        prompt: self.text.clone(),
                    },
                    None => super::session_execution::InputPresentation::Steering {
                        text: self.text.clone(),
                    },
                };
                event_sink
                    .send_acceptance(
                        event_sink.invocation_scope()?.clone(),
                        receipt,
                        presentation,
                    )
                    .await?;
                Ok::<(), anyhow::Error>(())
            }
            .await;
            if let Err(error) = publication {
                result = Err(error);
            }
        }
        if let Some(completion) = self.completion.take() {
            let _ = completion.send(result.map_err(|error| {
                super::lifecycle_error::classify(
                    forge_provider_api::ProviderOperation::TurnSteer,
                    error,
                )
            }));
        }
    }
}

impl Drop for SteerCommand {
    fn drop(&mut self) {
        if let Some(completion) = self.completion.take() {
            let error = if self
                .submission
                .as_ref()
                .and_then(|submission| submission.record().ok())
                .is_some_and(|record| {
                    matches!(
                        record.delivery,
                        super::submission::SubmissionState::Prepared
                    )
                }) {
                not_admitted()
            } else {
                ProviderError::DeliveryUncertain
            };
            let _ = completion.send(Err(error));
        }
    }
}

/// Prove local rejection without fabricating a provider continuation capability.
fn not_admitted() -> ProviderError {
    ProviderError::PreparationFailed(Box::new(ProviderError::Failure(
        forge_provider_api::ProviderFailure::new(
            forge_provider_api::ProviderOperation::TurnSteer,
            forge_provider_api::ProviderFailureKind::Rejected,
        ),
    )))
}

#[cfg(test)]
mod test {
    use super::SteeringLane;
    use crate::provider::session_execution::{InputPresentation, OwnerObservation};
    use crate::provider::{InvocationScope, ThreadOwner};

    /// A captured sender must reject retirement instead of delivering to the replacement turn.
    #[tokio::test]
    async fn captured_steering_target_does_not_retarget_replacement_turn() -> anyhow::Result<()> {
        let lane = SteeringLane::default();
        let original = lane.activate(None)?;
        let captured = lane
            .capture()
            .expect("active receiver has a capture capability");
        drop(original);
        let mut replacement = lane.activate(None)?;
        assert!(!captured.is_open());
        assert!(
            captured
                .steer(crate::provider::SteeringInput::AgentResult(
                    "old target".into()
                ))
                .await
                .is_err()
        );
        assert!(replacement.try_receive().is_none());
        Ok(())
    }

    fn scope() -> InvocationScope {
        InvocationScope {
            owner: ThreadOwner::root("steering"),
            exchange_id: "exchange".into(),
            invocation_id: "invocation".into(),
        }
    }

    #[tokio::test]
    async fn stale_continuation_retains_unconsumed_command() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = std::sync::Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let store = crate::provider::submission::SubmissionStore::open(directory.path(), trace)?;
        let catalog = crate::provider::ProviderCatalogRequest {
            execution_owner: None,
            harness_session_id: "steering".into(),
            workspace: "workspace".into(),
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            provider_session_id: None,
        };
        let submission = store.prepare_steering(
            &catalog,
            crate::provider::ProviderInput::from_text("once"),
            forge_providers::registry::default_id(),
        )?;
        let lane = SteeringLane::default();
        let mut active = lane.activate(None)?;
        let mut sending = Box::pin(lane.steer(super::SteeringInput::User {
            text: "once".into(),
            attachments: Vec::new(),
            submission: Some(submission.clone()),
        }));
        assert!(futures_util::poll!(sending.as_mut()).is_pending());
        let command = active.receive().await.unwrap();
        let request = submission.record()?.request;
        let (sink, _events) = crate::provider::events::channel(scope());
        let (attempt, command) = command
            .complete_with(
                async move {
                    Ok(crate::provider::retry::RetryAttempt::Stale(
                        Box::new(request),
                        Default::default(),
                    ))
                },
                sink,
            )
            .await?;
        assert!(matches!(
            attempt,
            crate::provider::retry::RetryAttempt::Stale(..)
        ));
        assert!(futures_util::poll!(sending.as_mut()).is_pending());
        assert!(matches!(
            submission.record()?.delivery,
            crate::provider::submission::SubmissionState::Prepared
        ));
        command
            .expect("stale command retained")
            .complete(Ok(()))
            .await;
        sending.await?;
        Ok(())
    }

    #[tokio::test]
    async fn uncertain_steering_is_never_returned_to_the_fallback_queue() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = std::sync::Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let store = crate::provider::submission::SubmissionStore::open(directory.path(), trace)?;
        let catalog = crate::provider::ProviderCatalogRequest {
            execution_owner: None,
            harness_session_id: "steering".into(),
            workspace: "workspace".into(),
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            provider_session_id: None,
        };
        let submission = store.prepare_steering(
            &catalog,
            crate::provider::ProviderInput::from_text("once"),
            forge_providers::registry::default_id(),
        )?;
        let lane = SteeringLane::default();
        let mut active = lane.activate(None)?;
        let mut sending = Box::pin(lane.steer(super::SteeringInput::User {
            text: "once".into(),
            attachments: Vec::new(),
            submission: Some(submission.clone()),
        }));
        assert!(futures_util::poll!(sending.as_mut()).is_pending());
        let command = active.receive().await.unwrap();
        submission.sending(None)?;
        let failure = forge_provider_api::ProviderFailure::new(
            forge_provider_api::ProviderOperation::TurnSteer,
            forge_provider_api::ProviderFailureKind::Timeout,
        )
        .at_phase(
            forge_provider_api::ProviderPhase::Observation,
            forge_provider_api::FailureDisposition::Uncertain,
        );
        let settlement_error = match command
            .settle_delivery(Err(crate::provider::ProviderError::Failure(failure).into()))
            .await
        {
            Err(error) => error,
            Ok(_) => panic!("uncertain steering returned to fallback ownership"),
        };
        assert!(
            matches!(settlement_error.downcast_ref::<crate::provider::ProviderError>(), Some(crate::provider::ProviderError::Failure(actual)) if *actual == failure)
        );
        let acknowledgement_error = sending.await.unwrap_err();
        assert!(
            matches!(acknowledgement_error.downcast_ref::<crate::provider::ProviderError>(), Some(crate::provider::ProviderError::Failure(actual)) if *actual == failure)
        );
        assert!(active.try_receive().is_none());
        assert!(matches!(
            submission.record()?.delivery,
            crate::provider::submission::SubmissionState::Sending { .. }
        ));
        Ok(())
    }

    #[tokio::test]
    async fn control_collection_after_provider_settlement_does_not_require_a_live_steering_receiver()
     {
        let lane = SteeringLane::default();
        lane.cancel_pending_control().await.unwrap();
        let mut active = lane.activate(None).unwrap();
        let mut cleanup = Box::pin(lane.cancel_pending_control());
        assert!(futures_util::poll!(cleanup.as_mut()).is_pending());
        let command = active.receive().await.unwrap();
        assert_eq!(command.operation, super::ActiveTurnOperation::Cancel);
        command.complete(Ok(())).await;
        cleanup.await.unwrap();
        drop(active);
        lane.cancel_pending_control().await.unwrap();
        assert!(lane.steer("late input".into()).await.is_err());
    }

    #[tokio::test]
    async fn bounded_steering_preserves_interrupt_admission_and_inflight_accounting() {
        let lane = SteeringLane::default();
        let mut active = lane.activate(None).unwrap();
        let mut pending = Vec::new();
        for sequence in 0..super::MAX_STEERING_REQUESTS {
            let mut request = Box::pin(lane.steer(sequence.to_string().into()));
            assert!(futures_util::poll!(request.as_mut()).is_pending());
            pending.push(request);
            tokio::task::yield_now().await;
        }
        let mut overflow = Box::pin(lane.steer("overflow".into()));
        assert!(futures_util::poll!(overflow.as_mut()).is_pending());
        let first = active.receive().await.unwrap();
        assert!(futures_util::poll!(overflow.as_mut()).is_pending());
        let mut interrupt = Box::pin(lane.submit(super::ActiveTurnOperation::Interrupt, None));
        assert!(futures_util::poll!(interrupt.as_mut()).is_pending());
        assert!(
            lane.submit(super::ActiveTurnOperation::Interrupt, None)
                .await
                .is_err()
        );
        for sequence in 1..super::MAX_STEERING_REQUESTS {
            let command = active.receive().await.unwrap();
            assert_eq!(command.text, sequence.to_string());
            command.complete(Ok(())).await;
        }
        let command = active.receive().await.unwrap();
        assert_eq!(command.operation, super::ActiveTurnOperation::Interrupt);
        command.complete(Ok(())).await;
        interrupt.await.unwrap();
        first.complete(Ok(())).await;
        assert!(futures_util::poll!(overflow.as_mut()).is_pending());
        let command = active.receive().await.unwrap();
        assert_eq!(command.text, "overflow");
        command.complete(Ok(())).await;
        overflow.await.unwrap();
        for request in pending {
            request.await.unwrap();
        }
        let mut request = Box::pin(lane.steer("after completion".into()));
        assert!(futures_util::poll!(request.as_mut()).is_pending());
        drop(active);
        assert!(request.await.is_err());
        assert!(lane.activate(None).is_ok());
    }

    /// Retire capacity waiters with their captured invocation instead of admitting a successor.
    #[tokio::test]
    async fn saturated_steering_waiter_cannot_enter_replacement_lane() {
        let lane = SteeringLane::default();
        let active = lane.activate(None).unwrap();
        let mut pending = Vec::new();
        for sequence in 0..super::MAX_STEERING_REQUESTS {
            let mut request = Box::pin(lane.steer(sequence.to_string().into()));
            assert!(futures_util::poll!(request.as_mut()).is_pending());
            pending.push(request);
        }
        let mut waiting = Box::pin(lane.steer("waiting".into()));
        assert!(futures_util::poll!(waiting.as_mut()).is_pending());
        drop(active);
        let mut replacement = lane.activate(None).unwrap();
        assert!(waiting.await.is_err());
        for request in pending {
            assert!(request.await.is_err());
        }
        assert!(replacement.try_receive().is_none());
    }

    #[tokio::test]
    async fn large_steering_preserves_complete_content() {
        let lane = SteeringLane::default();
        let mut active = lane.activate(None).unwrap();
        let text = "λ".repeat(65_537);
        let mut request = Box::pin(lane.submit(
            super::ActiveTurnOperation::Steer,
            Some(super::SteeringInput::User {
                text: text.clone(),
                attachments: Vec::new(),
                submission: None,
            }),
        ));
        assert!(futures_util::poll!(request.as_mut()).is_pending());
        let command = active.receive().await.unwrap();
        assert_eq!(command.text, text);
        command.complete(Ok(())).await;
        request.await.unwrap();
    }

    #[tokio::test]
    async fn preserves_submission_order_across_one_active_turn() {
        let lane = SteeringLane::default();
        let mut active = lane.activate(None).unwrap();
        let first_lane = lane.clone();
        let first = tokio::spawn(async move { first_lane.steer("first".into()).await });
        tokio::task::yield_now().await;
        let second_lane = lane.clone();
        let second = tokio::spawn(async move { second_lane.steer("second".into()).await });
        tokio::task::yield_now().await;

        let first_command = active.receive().await.unwrap();
        assert_eq!(first_command.text, "first");
        first_command.complete(Ok(())).await;
        let second_command = active.receive().await.unwrap();
        assert_eq!(second_command.text, "second");
        second_command.complete(Ok(())).await;

        first.await.unwrap().unwrap();
        second.await.unwrap().unwrap();
    }

    #[tokio::test]
    async fn rejects_input_after_the_active_turn_ends() {
        let lane = SteeringLane::default();
        drop(lane.activate(None).unwrap());
        let error = lane.steer("too late".into()).await.unwrap_err();
        assert!(
            matches!(error.downcast_ref::<crate::provider::ProviderError>(), Some(crate::provider::ProviderError::PreparationFailed(cause))
            if matches!(cause.as_ref(), crate::provider::ProviderError::Failure(failure) if failure.operation() == forge_provider_api::ProviderOperation::TurnSteer && failure.kind() == forge_provider_api::ProviderFailureKind::Rejected && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged))
        );
    }

    #[tokio::test]
    async fn child_results_reach_the_provider_without_user_steering_events() {
        let lane = SteeringLane::default();
        let (event_sink, mut event_stream) = crate::provider::events::channel(scope());
        let mut active = lane.activate(Some(event_sink)).unwrap();
        let sender = lane.clone();
        let delivery = tokio::spawn(async move {
            sender
                .steer(super::SteeringInput::AgentResult("child report".into()))
                .await
        });
        let command = active.receive().await.unwrap();
        assert_eq!(command.text, "child report");
        assert!(matches!(
            command.input(),
            super::SteeringInput::AgentResult(_)
        ));
        command.complete(Ok(())).await;
        delivery.await.unwrap().unwrap();
        assert!(event_stream.try_recv().is_err());
    }

    #[tokio::test]
    async fn task_start_preserves_origin_and_publishes_only_after_acknowledgement()
    -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = std::sync::Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let store = crate::provider::submission::SubmissionStore::open(directory.path(), trace)?;
        let catalog = crate::provider::ProviderCatalogRequest {
            execution_owner: None,
            harness_session_id: "steering".into(),
            workspace: "workspace".into(),
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            provider_session_id: None,
        };
        let prompt = "Complete the startup work.\nVerify the result.";
        let start = crate::exchange::TaskStart {
            kind: crate::task::TaskKind::Goal,
            title: "Reduce startup time".into(),
        };
        let submission = store.prepare_input(
            &catalog,
            crate::provider::ProviderInput::from_text(prompt),
            forge_providers::registry::default_id(),
            Some("task-entry".into()),
        )?;
        let lane = SteeringLane::default();
        let (event_sink, mut events) = crate::provider::events::channel(scope());
        let mut active = lane.activate(Some(event_sink))?;
        let mut delivery = Box::pin(lane.steer(super::SteeringInput::TaskEntry {
            start: start.clone(),
            text: prompt.into(),
            submission,
        }));
        assert!(futures_util::poll!(delivery.as_mut()).is_pending());
        let command = active.receive().await.unwrap();
        assert!(events.try_recv().is_err());
        match command.input() {
            super::SteeringInput::TaskEntry {
                start: forwarded,
                text,
                submission,
                ..
            } => {
                assert_eq!(forwarded, start);
                assert_eq!(text, prompt);
                assert_eq!(
                    submission.record()?.input_message_id.as_deref(),
                    Some("task-entry")
                );
            }
            _ => panic!("forwarding discarded the task entry origin"),
        }
        command.complete(Ok(())).await;
        delivery.await?;
        let event = events.recv().await?.unwrap();
        let OwnerObservation::InputAcceptance {
            scope: accepted_scope,
            receipt,
            presentation:
                InputPresentation::TaskStarted {
                    start: accepted_start,
                    prompt: accepted_prompt,
                },
        } = event
        else {
            panic!("expected acknowledged task entry");
        };
        assert_eq!(accepted_scope, scope());
        assert_eq!(accepted_prompt, prompt);
        assert_eq!(accepted_start, start);
        assert_eq!(
            receipt.record()?.input_message_id.as_deref(),
            Some("task-entry")
        );
        assert!(events.try_recv().is_err());
        Ok(())
    }

    #[tokio::test]
    async fn emits_only_provider_acknowledged_input() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = std::sync::Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let store = crate::provider::submission::SubmissionStore::open(directory.path(), trace)?;
        let catalog = crate::provider::ProviderCatalogRequest {
            execution_owner: None,
            harness_session_id: "steering".into(),
            workspace: "workspace".into(),
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            provider_session_id: None,
        };
        let receipt = store.prepare_input(
            &catalog,
            crate::provider::ProviderInput::from_text("accepted"),
            forge_providers::registry::default_id(),
            None,
        )?;
        let lane = SteeringLane::default();
        let (event_sink, mut event_stream) = crate::provider::events::channel(scope());
        let mut active = lane.activate(Some(event_sink))?;
        let accepted_lane = lane.clone();
        let accepted = tokio::spawn(async move {
            accepted_lane
                .steer(super::SteeringInput::User {
                    text: "accepted".into(),
                    attachments: vec![],
                    submission: Some(receipt),
                })
                .await
        });
        let accepted_command = active.receive().await.unwrap();
        accepted_command.complete(Ok(())).await;
        accepted.await??;
        let event = event_stream
            .recv()
            .await?
            .expect("steering receipt owner observation");
        assert!(
            matches!(event, OwnerObservation::InputAcceptance { scope: accepted_scope,
            presentation: InputPresentation::Steering { text }, .. } if accepted_scope == scope() && text == "accepted")
        );
        let rejected_lane = lane.clone();
        let rejected = tokio::spawn(async move { rejected_lane.steer("rejected".into()).await });
        active
            .receive()
            .await
            .unwrap()
            .complete(Err(anyhow::anyhow!("provider rejected steering")))
            .await;
        assert!(rejected.await?.is_err());
        assert!(event_stream.try_recv().is_err());
        Ok(())
    }
}
