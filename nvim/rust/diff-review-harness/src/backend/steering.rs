use anyhow::{Context, Result};
use std::sync::{Arc, Mutex};
use tokio::sync::{mpsc, oneshot};

/// Routes user input into the provider turn that currently owns the backend transport.
#[derive(Clone, Default)]
pub struct SteeringLane {
    state: Arc<Mutex<SteeringState>>,
}

#[derive(Default)]
struct SteeringState {
    generation: u64,
    sender: Option<mpsc::UnboundedSender<SteerCommand>>,
}

/// Owns one active turn's steering receiver and clears it when that turn ends.
pub struct ActiveSteering {
    lane: SteeringLane,
    generation: u64,
    receiver: mpsc::UnboundedReceiver<SteerCommand>,
}

/// Carries one steering input and its provider acknowledgement.
pub struct SteerCommand {
    pub text: String,
    completion: Option<oneshot::Sender<std::result::Result<(), String>>>,
}

impl SteeringLane {
    /// Attach a receiver to the active provider turn before accepting steering input.
    pub fn activate(&self) -> Result<ActiveSteering> {
        let (sender, receiver) = mpsc::unbounded_channel();
        let mut state = self.state.lock().expect("steering lane lock poisoned");
        anyhow::ensure!(state.sender.is_none(), "backend already has an active turn");
        state.generation += 1;
        let generation = state.generation;
        state.sender = Some(sender);
        Ok(ActiveSteering {
            lane: self.clone(),
            generation,
            receiver,
        })
    }

    /// Deliver user input to the active turn and wait for the provider acknowledgement.
    pub async fn steer(&self, text: String) -> Result<()> {
        let sender = self
            .state
            .lock()
            .expect("steering lane lock poisoned")
            .sender
            .clone()
            .context("backend has no active turn to steer")?;
        let (completion, response) = oneshot::channel();
        sender
            .send(SteerCommand {
                text,
                completion: Some(completion),
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
        self.receiver.recv().await
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
    async fn preserves_submission_order_across_one_active_turn() {
        let lane = SteeringLane::default();
        let mut active = lane.activate().unwrap();
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
        drop(lane.activate().unwrap());
        let error = lane.steer("too late".into()).await.unwrap_err();
        assert!(format!("{error:#}").contains("no active turn"));
    }
}
