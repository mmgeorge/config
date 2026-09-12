use anyhow::{Context, Result};
use std::sync::{Arc, Mutex};
use tokio::sync::{OwnedSemaphorePermit, Semaphore, mpsc, oneshot};

const MAX_STEERING_REQUESTS: usize = 32;
const MAX_STEERING_BYTES: usize = 64 * 1024;
const MAX_TARGET_BYTES: usize = 4096;

use super::{BackendEvent, BackendEventSink};

/// Routes user input into the provider turn that currently owns the backend transport.
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
}

/// Identifies one provider thread and turn for direct child control.
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
}

impl SteeringLane {
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

    /// Deliver user input to the active turn and wait for the provider acknowledgement.
    pub async fn steer(&self, text: String) -> Result<()> {
        self.submit(ActiveTurnOperation::Steer, text, None).await
    }

    /// Deliver input to a specific child turn over the active provider process.
    pub async fn steer_target(&self, text: String, target: SteerTarget) -> Result<()> {
        self.submit(ActiveTurnOperation::Steer, text, Some(target))
            .await
    }

    /// Interrupt a specific child turn without cancelling its parent request.
    pub async fn interrupt_target(&self, target: SteerTarget) -> Result<()> {
        self.submit(ActiveTurnOperation::Interrupt, String::new(), Some(target))
            .await
    }

    async fn submit(
        &self,
        operation: ActiveTurnOperation,
        text: String,
        target: Option<SteerTarget>,
    ) -> Result<()> {
        let sender = self
            .state
            .lock()
            .expect("steering lane lock poisoned")
            .sender
            .clone()
            .context("backend has no active turn to steer")?;
        anyhow::ensure!(
            text.len() <= MAX_STEERING_BYTES,
            "steering input exceeds 64 KiB"
        );
        anyhow::ensure!(
            target
                .as_ref()
                .is_none_or(|target| target.thread_id.len() <= MAX_TARGET_BYTES
                    && target.turn_id.len() <= MAX_TARGET_BYTES),
            "steering target exceeds 4 KiB per identifier"
        );
        let admission = match operation {
            ActiveTurnOperation::Steer => sender.ordinary,
            ActiveTurnOperation::Interrupt => sender.interrupt,
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
            })
            .map_err(|_| anyhow::anyhow!("active turn completed before steering input was sent"))?;
        response
            .await
            .context("active turn completed before steering input was acknowledged")?
            .map_err(anyhow::Error::msg)
    }
}

impl ActiveSteering {
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
    pub fn complete(mut self, result: Result<()>) {
        if result.is_ok()
            && self.operation == ActiveTurnOperation::Steer
            && let Some(event_sink) = self.event_sink.take()
        {
            let _ = event_sink.send(BackendEvent::steering_input(self.text.clone()));
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
    async fn bounded_steering_preserves_interrupt_admission_and_inflight_accounting() {
        let lane = SteeringLane::default();
        let mut active = lane.activate(None).unwrap();
        let mut pending = Vec::new();
        for sequence in 0..super::MAX_STEERING_REQUESTS {
            let mut request = Box::pin(lane.steer(sequence.to_string()));
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
        let target = super::SteerTarget {
            thread_id: "child".into(),
            turn_id: "turn".into(),
        };
        let mut interrupt = Box::pin(lane.interrupt_target(target.clone()));
        assert!(futures_util::poll!(interrupt.as_mut()).is_pending());
        assert!(lane.interrupt_target(target).await.is_err());
        for sequence in 1..super::MAX_STEERING_REQUESTS {
            let command = active.receive().await.unwrap();
            assert_eq!(command.text, sequence.to_string());
            command.complete(Ok(()));
        }
        let command = active.receive().await.unwrap();
        assert_eq!(command.operation, super::ActiveTurnOperation::Interrupt);
        command.complete(Ok(()));
        interrupt.await.unwrap();
        first.complete(Ok(()));
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
    async fn oversized_steering_does_not_consume_admission() {
        let lane = SteeringLane::default();
        let mut active = lane.activate(None).unwrap();
        assert!(
            lane.steer("x".repeat(super::MAX_STEERING_BYTES + 1))
                .await
                .is_err()
        );
        assert!(
            lane.steer_target(
                "text".into(),
                super::SteerTarget {
                    thread_id: "x".repeat(super::MAX_TARGET_BYTES + 1),
                    turn_id: "turn".into(),
                }
            )
            .await
            .is_err()
        );
        assert!(active.receiver.try_recv().is_err());
        let mut request = Box::pin(lane.steer("valid".into()));
        assert!(futures_util::poll!(request.as_mut()).is_pending());
        active.receive().await.unwrap().complete(Ok(()));
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
        first_command.complete(Ok(()));
        let second_command = active.receive().await.unwrap();
        assert_eq!(second_command.text, "second");
        second_command.complete(Ok(()));

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
    async fn emits_only_provider_acknowledged_input() {
        let lane = SteeringLane::default();
        let (event_sink, mut event_stream) = crate::backend::events::channel();
        let mut active = lane.activate(Some(event_sink)).unwrap();
        let accepted_lane = lane.clone();
        let accepted = tokio::spawn(async move { accepted_lane.steer("accepted".into()).await });
        let accepted_command = active.receive().await.unwrap();
        accepted_command.complete(Ok(()));
        accepted.await.unwrap().unwrap();
        let event = event_stream.recv().await.unwrap().expect("steering event");
        assert_eq!(event.kind, "steering_input");
        assert_eq!(event.text.as_deref(), Some("accepted"));

        let rejected_lane = lane.clone();
        let rejected = tokio::spawn(async move { rejected_lane.steer("rejected".into()).await });
        let rejected_command = active.receive().await.unwrap();
        rejected_command.complete(Err(anyhow::anyhow!("provider rejected steering")));
        assert!(rejected.await.unwrap().is_err());
        assert!(event_stream.try_recv().is_err());
    }
}
