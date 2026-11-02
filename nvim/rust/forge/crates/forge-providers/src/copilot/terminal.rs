use super::event_stream;
use anyhow::Context;
use std::sync::Arc;

/// Retains terminal observations after the prompt event subscription ends.
pub struct CopilotTerminalMonitor {
    session: std::sync::Weak<github_copilot_sdk::session::Session>,
    snapshot: tokio::sync::watch::Receiver<
        Option<std::result::Result<forge_provider_api::terminal::TerminalSnapshot, String>>,
    >,
    task: tokio::task::JoinHandle<()>,
}

impl Drop for CopilotTerminalMonitor {
    fn drop(&mut self) {
        self.task.abort();
    }
}

impl CopilotTerminalMonitor {
    /// Subscribe before the first query so terminal transitions cannot be lost at attachment.
    pub fn new(
        session: &std::sync::Arc<super::handle::CopilotSession>,
        trace: Arc<dyn forge_provider_api::trace::ProviderTrace>,
        harness_session_id: String,
    ) -> Self {
        let session = &session.inner;
        let mut event = event_stream::SessionEventStream::new(
            session,
            |event| event.event_type == "session.background_tasks_changed",
            trace,
            harness_session_id,
        );
        let session = Arc::downgrade(session);
        let monitored_session = session.clone();
        let (send, snapshot) = tokio::sync::watch::channel(None);
        let task = tokio::spawn(async move {
            loop {
                let Some(session) = session.upgrade() else {
                    return;
                };
                let result = tokio::time::timeout(std::time::Duration::from_secs(6), async {
                    session
                        .rpc()
                        .tasks()
                        .refresh()
                        .await
                        .context("refresh Copilot background terminals")?;
                    let result = session
                        .rpc()
                        .tasks()
                        .list()
                        .await
                        .context("list Copilot background terminals")?;
                    crate::terminal::parse_snapshot(&serde_json::to_value(result)?, true)
                })
                .await
                .context("Copilot terminal query timed out")
                .and_then(|result| result)
                .map_err(|error: anyhow::Error| format!("{error:#}"));
                let has_terminals = result
                    .as_ref()
                    .is_ok_and(|snapshot| !snapshot.terminal.is_empty());
                if send.send(Some(result)).is_err() {
                    return;
                }
                drop(session);
                // Bound refresh-triggered notifications to one query per two seconds.
                tokio::time::sleep(std::time::Duration::from_secs(2)).await;
                let delay = tokio::time::sleep(std::time::Duration::from_secs(if has_terminals {
                    2
                } else {
                    10
                }));
                tokio::pin!(delay);
                loop {
                    tokio::select! {
                        _ = &mut delay => break,
                        update = event.recv() => match update {
                            Ok(update) if update.event_type == "session.background_tasks_changed" => break,
                            Ok(_) => {},
                            Err(error) => {
                                let _ = send.send(Some(Err(format!("Copilot terminal event stream closed: {error:#}"))));
                                return;
                            },
                        }
                    }
                }
            }
        });
        Self {
            session: monitored_session,
            snapshot,
            task,
        }
    }
}

impl CopilotTerminalMonitor {
    /// Check whether this monitor still observes the requested native session.
    pub fn observes(&self, session: &Arc<super::handle::CopilotSession>) -> bool {
        let session = &session.inner;
        !self.task.is_finished() && self.session.ptr_eq(&Arc::downgrade(session))
    }

    /// Get the latest completed native inventory without waiting for another refresh.
    pub fn snapshot(
        &self,
    ) -> Option<std::result::Result<forge_provider_api::terminal::TerminalSnapshot, String>> {
        self.snapshot.borrow().clone()
    }

    /// Cancel and collect the observation task before releasing the session.
    pub async fn shutdown(mut self) {
        self.task.abort();
        let _ = (&mut self.task).await;
    }
}
