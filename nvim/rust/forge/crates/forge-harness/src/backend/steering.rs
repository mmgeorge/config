use anyhow::{Context, Result};
use std::sync::{Arc, Mutex};
use tokio::sync::{OwnedSemaphorePermit, Semaphore, mpsc, oneshot};

const MAX_STEERING_REQUESTS: usize = 32;

use super::{BackendEvent, BackendEventSink};

#[derive(Clone, Debug)]
/// Distinguishes user steering from child results delivered without user-prompt rendering.
pub enum SteeringInput {
    /// Records an explicit user addition in the parent timeline.
    User(String),
    /// Adds an attributed child result while its transcript remains under the child.
    AgentResult(String),
}

impl SteeringInput {
    /// Returns the provider-visible message without its presentation origin.
    pub fn text(&self) -> &str {
        match self { Self::User(text) | Self::AgentResult(text) => text }
    }
}

impl From<String> for SteeringInput {
    fn from(text: String) -> Self { Self::User(text) }
}

impl From<&str> for SteeringInput {
    fn from(text: &str) -> Self { Self::User(text.into()) }
}

/// Routes input into the provider turn that currently owns the backend transport.
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
    interrupt: Arc<Semaphore>,
}

/// Owns one active turn's steering receiver and clears it when that turn ends.
pub struct ActiveSteering {
    lane: SteeringLane,
    generation: u64,
    receiver: mpsc::Receiver<SteerCommand>,
    event_sink: Option<BackendEventSink>,
}

/// Carries one steering input and its provider acknowledgement.
pub struct SteerCommand {
    _admission: OwnedSemaphorePermit,
    pub text: String,
    pub target: Option<SteerTarget>,
    pub operation: ActiveTurnOperation,
    completion: Option<oneshot::Sender<std::result::Result<(), String>>>,
    event_sink: Option<BackendEventSink>,
    publish_input: bool,
}

/// Identifies the provider child receiving steering or interruption.
#[derive(Clone, Debug)]
pub struct SteerTarget {
    pub thread_id: String,
    pub turn_id: String,
}

/// Defines the active-turn operation carried over the shared provider process.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ActiveTurnOperation {
    Steer,
    Interrupt,
    CleanupExecution,
    PauseGoal,
    ClearGoal,
}

impl SteeringLane {
    /// Reports whether a provider turn or Harness continuation owns the input receiver.
    pub(crate) fn is_active(&self) -> bool {
        self.state.lock().expect("steering lane lock poisoned").sender.is_some()
    }

    /// Stop native goal continuation through the active provider reader.
    pub async fn stop_goal(&self, clear: bool) -> Result<()> {
        self.submit(
            if clear {
                ActiveTurnOperation::ClearGoal
            } else {
                ActiveTurnOperation::PauseGoal
            },
            String::new(),
            None,
            false,
        )
        .await
    }
    /// Attach a receiver to the active provider turn before accepting steering input.
    pub fn activate(&self, event_sink: Option<BackendEventSink>) -> Result<ActiveSteering> {
        let (sender, receiver) = mpsc::channel(MAX_STEERING_REQUESTS + 1);
        let mut state = self.state.lock().expect("steering lane lock poisoned");
        anyhow::ensure!(state.sender.is_none(), "backend already has an active turn");
        state.generation += 1;
        let generation = state.generation;
        state.sender = Some(SteeringSender {
            sender,
            ordinary: Arc::new(Semaphore::new(MAX_STEERING_REQUESTS)),
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
    pub async fn steer(&self, input: SteeringInput) -> Result<()> {
        let publish_input = matches!(input, SteeringInput::User(_));
        self.submit(ActiveTurnOperation::Steer, input.text().to_owned(), None, publish_input).await
    }

    /// Interrupt the parent and descendants and wait for terminal provider evidence.
    pub async fn cleanup_execution(&self) -> Result<()> {
        self.submit(ActiveTurnOperation::CleanupExecution, String::new(), None, false)
            .await
    }

    async fn submit(
        &self,
        operation: ActiveTurnOperation,
        text: String,
        target: Option<SteerTarget>,
        publish_input: bool,
    ) -> Result<()> {
        let sender = self
            .state
            .lock()
            .expect("steering lane lock poisoned")
            .sender
            .clone();
        let Some(sender) = sender else {
            anyhow::ensure!(operation == ActiveTurnOperation::CleanupExecution,
                "backend has no active turn to steer");
            return Ok(());
        };
        let admission = match operation {
            ActiveTurnOperation::Steer
            | ActiveTurnOperation::PauseGoal
            | ActiveTurnOperation::ClearGoal => sender.ordinary,
            ActiveTurnOperation::Interrupt | ActiveTurnOperation::CleanupExecution => {
                sender.interrupt
            }
        }
        .try_acquire_owned()
        .context("active turn control admission is full")?;
        let (completion, response) = oneshot::channel();
        sender
            .sender
            .try_send(SteerCommand {
                _admission: admission,
                text,
                target,
                operation,
                completion: Some(completion),
                event_sink: None,
                publish_input,
            })
            .map_err(|_| anyhow::anyhow!("active turn completed before steering input was sent"))?;
        response
            .await
            .context("active turn completed before steering input was acknowledged")?
            .map_err(anyhow::Error::msg)
    }
}

impl ActiveSteering {
    /// Drains input already admitted before a provider-turn boundary.
    pub(crate) fn try_receive(&mut self) -> Option<SteerCommand> {
        let mut command = self.receiver.try_recv().ok()?;
        command.event_sink.clone_from(&self.event_sink);
        Some(command)
    }

    /// Receive the next steering input in submission order.
    pub async fn receive(&mut self) -> Option<SteerCommand> {
        let mut command = self.receiver.recv().await?;
        command.event_sink.clone_from(&self.event_sink);
        Some(command)
    }
}

impl Drop for ActiveSteering {
    fn drop(&mut self) {
        let mut state = self.lane.state.lock().expect("steering lane lock poisoned");
        if state.generation == self.generation {
            state.sender = None;
        }
    }
}

impl SteerCommand {
    /// Resolve the originating Harness request with the provider result.
    pub async fn complete(mut self, result: Result<()>) {
        if result.is_ok()
            && self.publish_input
            && self.operation == ActiveTurnOperation::Steer
            && let Some(event_sink) = self.event_sink.take()
        {
            let _ = event_sink
                .send_wait(BackendEvent::steering_input(self.text.clone()))
                .await;
        }
        if let Some(completion) = self.completion.take() {
            let _ = completion.send(result.map_err(|error| format!("{error:#}")));
        }
    }
}

impl Drop for SteerCommand {
    fn drop(&mut self) {
        if let Some(completion) = self.completion.take() {
            let _ = completion.send(Err(
                "active turn completed before steering input was acknowledged".into(),
            ));
        }
    }
}

#[cfg(test)]
mod test {
    use super::SteeringLane;

    #[tokio::test]
    async fn cleanup_after_provider_settlement_does_not_require_a_live_steering_receiver() {
        let lane = SteeringLane::default();
        lane.cleanup_execution().await.unwrap();
        let mut active = lane.activate(None).unwrap();
        let mut cleanup = Box::pin(lane.cleanup_execution());
        assert!(futures_util::poll!(cleanup.as_mut()).is_pending());
        let command = active.receive().await.unwrap();
        assert_eq!(command.operation, super::ActiveTurnOperation::CleanupExecution);
        command.complete(Ok(())).await;
        cleanup.await.unwrap();
        drop(active);
        lane.cleanup_execution().await.unwrap();
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
        }
        assert!(
            lane.steer("overflow".into())
                .await
                .unwrap_err()
                .to_string()
                .contains("admission is full")
        );
        let first = active.receive().await.unwrap();
        assert!(lane.steer("still full".into()).await.is_err());
        let mut interrupt = Box::pin(lane.submit(super::ActiveTurnOperation::Interrupt,String::new(),None,false));
        assert!(futures_util::poll!(interrupt.as_mut()).is_pending());
        assert!(lane.submit(super::ActiveTurnOperation::Interrupt,String::new(),None,false).await.is_err());
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
        for request in pending {
            request.await.unwrap();
        }
        let mut request = Box::pin(lane.steer("after completion".into()));
        assert!(futures_util::poll!(request.as_mut()).is_pending());
        drop(active);
        assert!(request.await.is_err());
        assert!(lane.activate(None).is_ok());
    }

    #[tokio::test]
    async fn large_steering_preserves_complete_content() {
        let lane = SteeringLane::default();
        let mut active = lane.activate(None).unwrap();
        let text = "λ".repeat(65_537);
        let target = "target".repeat(4097);
        let mut request = Box::pin(lane.submit(
                super::ActiveTurnOperation::Steer,
                text.clone(),
                Some(super::SteerTarget {
                    thread_id: target.clone(),
                    turn_id: "turn".into(),
                }),
                true,
            ));
        assert!(futures_util::poll!(request.as_mut()).is_pending());
        let command = active.receive().await.unwrap();
        assert_eq!(command.text, text);
        assert_eq!(command.target.as_ref().unwrap().thread_id, target);
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
        assert!(format!("{error:#}").contains("no active turn"));
    }

    #[tokio::test]
    async fn child_results_reach_the_provider_without_user_steering_events() {
        let lane = SteeringLane::default();
        let (event_sink, mut event_stream) = crate::backend::events::channel();
        let mut active = lane.activate(Some(event_sink)).unwrap();
        let sender = lane.clone();
        let delivery = tokio::spawn(async move {
            sender.steer(super::SteeringInput::AgentResult("child report".into())).await
        });
        let command = active.receive().await.unwrap();
        assert_eq!(command.text,"child report");
        command.complete(Ok(())).await;
        delivery.await.unwrap().unwrap();
        assert!(event_stream.try_recv().is_err());
    }

    #[tokio::test]
    async fn emits_only_provider_acknowledged_input() {
        let lane = SteeringLane::default();
        let (event_sink, mut event_stream) = crate::backend::events::channel();
        let mut active = lane.activate(Some(event_sink)).unwrap();
        let accepted_lane = lane.clone();
        let accepted = tokio::spawn(async move { accepted_lane.steer("accepted".into()).await });
        let accepted_command = active.receive().await.unwrap();
        accepted_command.complete(Ok(())).await;
        accepted.await.unwrap().unwrap();
        let event = event_stream.recv().await.unwrap().expect("steering event");
        assert_eq!(event.kind, "steering_input");
        assert_eq!(event.text.as_deref(), Some("accepted"));

        let rejected_lane = lane.clone();
        let rejected = tokio::spawn(async move { rejected_lane.steer("rejected".into()).await });
        let rejected_command = active.receive().await.unwrap();
        rejected_command
            .complete(Err(anyhow::anyhow!("provider rejected steering")))
            .await;
        assert!(rejected.await.unwrap().is_err());
        assert!(event_stream.try_recv().is_err());
    }
}
