use super::SharedCopilotSession;
use anyhow::{Context, Result};
use github_copilot_sdk::subscription::RecvErrorKind;
use github_copilot_sdk::{Client, SessionEvent};
use std::sync::Arc;

/// Drains SDK notifications without owning the session or blocking on presentation.
pub(super) struct SessionEventStream {
    receiver: tokio::sync::mpsc::Receiver<SessionEvent>,
    task: tokio::task::JoinHandle<Result<()>>,
    session_id: String,
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
        let (sender, receiver) = tokio::sync::mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        let stream_session_id = session_id.clone();
        let task = tokio::spawn(async move {
            let result: Result<()> = async {
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
                        return Err(error).with_context(|| {
                            format!("receive Copilot SDK subscription for session {session_id}")
                        });
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
                        return Err(error).with_context(|| {
                            format!("recover Copilot SDK subscription for session {session_id}")
                        });
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
                if accepts(&event) {
                    sender.send(event).await.with_context(|| {
                        format!("deliver Copilot SDK event to intake receiver for session {session_id}")
                    })?;
                }
            }
            }.await;
            if let Err(error) = &result {
                trace.record(
                    &harness_session_id,
                    "copilot.subscription.failed",
                    serde_json::json!({"session_id": session_id, "error": format!("{error:#}")}),
                );
            }
            result
        });
        Self {
            receiver,
            task,
            session_id: stream_session_id,
        }
    }

    async fn recover(
        client: &Client,
        session_id: &str,
        sender: &tokio::sync::mpsc::Sender<SessionEvent>,
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
            serde_json::from_value::<github_copilot_sdk::types::GetMessagesResponse>(response)
                .context("decode Copilot session.getMessages recovery response")?
                .events;
        let start = history
            .iter()
            .position(|event| event.id == cursor)
            .with_context(|| {
                format!(
                    "Copilot recovery cursor {cursor} is absent from session {session_id} history"
                )
            })?;
        let end = history
            .iter()
            .position(|event| event.id == parent)
            .with_context(|| {
                format!(
                    "Copilot recovery parent {parent} is absent from session {session_id} history"
                )
            })?;
        anyhow::ensure!(
            start <= end,
            "Copilot session {session_id} recovery history is out of order: cursor {cursor} at {start}, parent {parent} at {end}"
        );
        // Replay only the gap before the queued event. Newer history still arrives live.
        for event in history.into_iter().take(end + 1).skip(start + 1) {
            if accepts(&event) {
                sender.send(event).await.with_context(|| {
                    format!("deliver recovered Copilot SDK event for session {session_id}")
                })?;
            }
        }
        Ok(())
    }

    /// Receive retained notifications in SDK order without doing work on the intake task.
    pub(super) async fn recv(&mut self) -> Result<SessionEvent> {
        if let Some(event) = self.receiver.recv().await {
            return Ok(event);
        }
        (&mut self.task)
            .await
            .with_context(|| {
                format!(
                    "Copilot session {} intake task did not complete normally",
                    self.session_id
                )
            })?
            .with_context(|| format!("Copilot session {} event intake failed", self.session_id))?;
        anyhow::bail!(
            "Copilot session {} intake task exited without a subscription error",
            self.session_id
        )
    }
}

impl Drop for SessionEventStream {
    fn drop(&mut self) {
        self.task.abort();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn intake_preserves_large_events_before_reporting_the_producer_error() {
        let (sender, receiver) = tokio::sync::mpsc::channel(1);
        let content = "patch".repeat(forge_protocol::MAX_FRAME_BYTES);
        let expected = content.clone();
        let task = tokio::spawn(async move {
            sender
                .send(SessionEvent {
                    id: "event".into(),
                    timestamp: "2026-10-19T00:00:00Z".into(),
                    parent_id: None,
                    ephemeral: None,
                    agent_id: None,
                    debug_cli_received_at_ms: None,
                    debug_ws_forwarded_at_ms: None,
                    event_type: "tool.execution_start".into(),
                    data: serde_json::json!({"patch": content}),
                })
                .await
                .unwrap();
            Err(anyhow::anyhow!("socket reset").context("SDK subscription disconnected"))
        });
        let mut stream = SessionEventStream {
            receiver,
            task,
            session_id: "native-session".into(),
        };
        let event = stream.recv().await.unwrap();
        assert_eq!(event.data["patch"].as_str(), Some(expected.as_str()));
        let error = format!("{:#}", stream.recv().await.unwrap_err());
        assert!(error.contains("native-session"), "{error}");
        assert!(
            error.contains("SDK subscription disconnected: socket reset"),
            "{error}"
        );
    }

    #[tokio::test]
    async fn intake_reports_task_panics_instead_of_clean_eof() {
        let (sender, receiver) = tokio::sync::mpsc::channel(1);
        let task = tokio::spawn(async move {
            let _sender = sender;
            panic!("intake fixture panic");
        });
        let mut stream = SessionEventStream {
            receiver,
            task,
            session_id: "native-session".into(),
        };
        let error = format!("{:#}", stream.recv().await.unwrap_err());
        assert!(error.contains("native-session"), "{error}");
        assert!(error.contains("intake fixture panic"), "{error}");
    }

    #[tokio::test]
    async fn intake_reports_unexpected_successful_task_exit() {
        let (sender, receiver) = tokio::sync::mpsc::channel(1);
        let task = tokio::spawn(async move {
            drop(sender);
            Ok(())
        });
        let mut stream = SessionEventStream {
            receiver,
            task,
            session_id: "native-session".into(),
        };
        let error = stream.recv().await.unwrap_err().to_string();
        assert!(error.contains("native-session"), "{error}");
        assert!(
            error.contains("exited without a subscription error"),
            "{error}"
        );
    }

    #[tokio::test]
    async fn intake_reports_task_cancellation() {
        let (sender, receiver) = tokio::sync::mpsc::channel(1);
        let task = tokio::spawn(async move {
            let _sender = sender;
            std::future::pending::<Result<()>>().await
        });
        task.abort();
        let mut stream = SessionEventStream {
            receiver,
            task,
            session_id: "native-session".into(),
        };
        let error = format!("{:#}", stream.recv().await.unwrap_err());
        assert!(error.contains("native-session"), "{error}");
        assert!(error.contains("cancelled"), "{error}");
    }
}
