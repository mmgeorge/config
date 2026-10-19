use super::SharedCopilotSession;
use anyhow::{Context, Result};
use forge_protocol::outbound::{self, MessageReceiver};
use github_copilot_sdk::subscription::RecvErrorKind;
use github_copilot_sdk::{Client, SessionEvent};
use std::sync::Arc;

/// Drains SDK notifications without owning the session or blocking on presentation.
pub(super) struct SessionEventStream {
    receiver: MessageReceiver,
    task: tokio::task::JoinHandle<()>,
}

impl SessionEventStream {
    /// Subscribe before starting work and retain only events needed by this consumer.
    pub(super) fn new(
        session: &SharedCopilotSession,
        accepts: fn(&SessionEvent) -> bool,
        trace: Arc<crate::trace::TraceStore>,
        harness_session_id: String,
    ) -> Self {
        let mut subscription = session.subscribe();
        let client = session.client().clone();
        let session_id = session.id().to_string();
        let (sender, receiver) = outbound::channel();
        let task = tokio::spawn(async move {
            let mut cursor = None;
            let mut lagged = false;
            loop {
                let event = match subscription.recv().await {
                    Ok(event) => event,
                    Err(error) if matches!(error.kind(), RecvErrorKind::Lagged(_)) => {
                        if let RecvErrorKind::Lagged(skipped) = error.kind() {
                            trace.record(&harness_session_id, "copilot.subscription.lagged",
                                serde_json::json!({"session_id": session_id, "skipped": skipped.skipped()}));
                        }
                        lagged = true;
                        continue;
                    }
                    Err(error) => {
                        sender.record_failure(&std::io::Error::other(error));
                        return;
                    }
                };
                if lagged {
                    if let Err(error) = Self::recover(
                        &client,
                        &session_id,
                        &sender,
                        accepts,
                        cursor.as_deref(),
                        event.parent_id.as_deref(),
                    )
                    .await
                    {
                        sender.record_failure(&std::io::Error::other(error));
                        return;
                    }
                    trace.record(
                        &harness_session_id,
                        "copilot.subscription.recovered",
                        serde_json::json!({"session_id": session_id, "parent_id": event.parent_id}),
                    );
                    cursor = event.parent_id.clone();
                    lagged = false;
                }
                if event.ephemeral != Some(true) {
                    cursor = Some(event.id.clone());
                } else if cursor.is_none() {
                    cursor = event.parent_id.clone();
                }
                if accepts(&event) && sender.send_wait(event).await.is_err() {
                    return;
                }
            }
        });
        Self { receiver, task }
    }

    async fn recover(
        client: &Client,
        session_id: &str,
        sender: &outbound::MessageSender,
        accepts: fn(&SessionEvent) -> bool,
        cursor: Option<&str>,
        parent: Option<&str>,
    ) -> Result<()> {
        if cursor == parent {
            return Ok(());
        }
        let cursor = cursor.context("Copilot event recovery has no prior durable event")?;
        let parent = parent.context("Copilot event recovery has no next durable parent")?;
        let response = tokio::time::timeout(
            std::time::Duration::from_secs(30),
            client.call(
                "session.getMessages",
                Some(serde_json::json!({"sessionId": session_id})),
            ),
        )
        .await
        .context("Copilot event recovery timed out")?
        .context("read Copilot events for subscription recovery")?;
        let history =
            serde_json::from_value::<github_copilot_sdk::types::GetMessagesResponse>(response)?
                .events;
        let start = history
            .iter()
            .position(|event| event.id == cursor)
            .context("Copilot event recovery cursor is absent from history")?;
        let end = history
            .iter()
            .position(|event| event.id == parent)
            .context("Copilot event recovery parent is absent from history")?;
        anyhow::ensure!(
            start <= end,
            "Copilot event recovery history is out of order"
        );
        // Replay only the gap before the queued event. Newer history still arrives live.
        for event in history.into_iter().take(end + 1).skip(start + 1) {
            if accepts(&event) {
                sender.send_wait(event).await?;
            }
        }
        Ok(())
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
