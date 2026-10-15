use super::SharedCopilotSession;
use anyhow::{Context, Result};
use forge_protocol::outbound::{self, MessageReceiver};
use github_copilot_sdk::SessionEvent;

/// Drains SDK notifications independently of provider processing and presentation.
pub(super) struct SessionEventStream {
    receiver: MessageReceiver,
    task: tokio::task::JoinHandle<()>,
}

impl SessionEventStream {
    /// Subscribe before starting work and retain only events needed by this consumer.
    pub(super) fn new(session: &SharedCopilotSession, accepts: fn(&SessionEvent) -> bool) -> Self {
        let mut subscription = session.subscribe();
        let (sender, receiver) = outbound::channel();
        let task = tokio::spawn(async move {
            loop {
                let event = match subscription.recv().await {
                    Ok(event) if accepts(&event) => event,
                    Ok(_) => continue,
                    Err(error) => {
                        sender.record_failure(&std::io::Error::other(error));
                        return;
                    }
                };
                if sender.send_wait(event).await.is_err() {
                    return;
                }
            }
        });
        Self { receiver, task }
    }

    /// Receive retained notifications in SDK order without doing work on the intake task.
    pub(super) async fn recv(&mut self) -> Result<SessionEvent> {
        let frame = self
            .receiver
            .recv()
            .await?
            .context("Copilot event intake stopped")?;
        Ok(serde_json::from_slice(frame.bytes())?)
    }
}

impl Drop for SessionEventStream {
    fn drop(&mut self) {
        self.task.abort();
    }
}
