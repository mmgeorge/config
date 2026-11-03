use super::event_stream;
use anyhow::Context;
use std::sync::Arc;

/// Retains terminal observations after the prompt event subscription ends.
pub struct CopilotTerminalMonitor {
    session: std::sync::Weak<github_copilot_sdk::session::Session>,
    snapshot: tokio::sync::watch::Receiver<
        Option<std::result::Result<forge_provider_api::terminal::TerminalSnapshot, String>>,
    >,
    task: Option<tokio::task::JoinHandle<Result<(), forge_provider_api::ProviderError>>>,
    shutdown: Option<tokio::sync::oneshot::Sender<()>>,
}

impl Drop for CopilotTerminalMonitor {
    fn drop(&mut self) {
        if let Some(shutdown) = self.shutdown.take() {
            let _ = shutdown.send(());
        }
    }
}

impl CopilotTerminalMonitor {
    /// Subscribe before the first query so terminal transitions cannot be lost at attachment.
    pub fn new(
        session: &std::sync::Arc<super::handle::CopilotSession>,
        trace: Arc<dyn forge_provider_api::trace::ProviderTrace>,
        harness_session_id: String,
    ) -> Self {
        let event = event_stream::SessionEventStream::new(
            session,
            |event| event.event_type == "session.background_tasks_changed",
            trace,
            harness_session_id,
        );
        Self::from_stream(event, Arc::downgrade(&session.inner))
    }

    /// Retain the reader inside the cooperative monitor until explicit collection joins both.
    fn from_stream(
        mut event: event_stream::SessionEventStream,
        session: std::sync::Weak<github_copilot_sdk::session::Session>,
    ) -> Self {
        let monitored_session = session.clone();
        let (send, snapshot) = tokio::sync::watch::channel(None);
        let (shutdown, closing) = tokio::sync::oneshot::channel();
        let task = tokio::spawn(async move {
            let observe = async {
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
                    .map_err(|_: anyhow::Error| {
                        "Copilot terminal snapshot is unavailable".to_owned()
                    });
                    let has_terminals = result
                        .as_ref()
                        .is_ok_and(|snapshot| !snapshot.terminal.is_empty());
                    if send.send(Some(result)).is_err() {
                        return;
                    }
                    drop(session);
                    // Bound refresh-triggered notifications to one query per two seconds.
                    tokio::time::sleep(std::time::Duration::from_secs(2)).await;
                    let delay =
                        tokio::time::sleep(std::time::Duration::from_secs(if has_terminals {
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
                                Err(_) => {
                                    let _ = send.send(Some(Err("Copilot terminal observation is unavailable".to_owned())));
                                    return;
                                },
                            }
                        }
                    }
                }
            };
            tokio::select! { _ = closing => {}, _ = observe => {} }
            event.close().await.map_err(|_| {
                native_failure(
                    forge_provider_api::ProviderOperation::CollectEventReader,
                    forge_provider_api::ProviderFailureKind::Runtime,
                )
            })
        });
        Self {
            session: monitored_session,
            snapshot,
            task: Some(task),
            shutdown: Some(shutdown),
        }
    }
}

impl CopilotTerminalMonitor {
    /// Check whether this monitor still observes the requested native session.
    pub fn observes(&self, session: &Arc<super::handle::CopilotSession>) -> bool {
        let session = &session.inner;
        self.task.as_ref().is_some_and(|task| !task.is_finished())
            && self.session.ptr_eq(&Arc::downgrade(session))
    }

    /// Get the latest completed native inventory without waiting for another refresh.
    pub fn snapshot(
        &self,
    ) -> Option<std::result::Result<forge_provider_api::terminal::TerminalSnapshot, String>> {
        self.snapshot.borrow().clone()
    }

    /// Cancel and collect the observation task before releasing the session.
    pub async fn shutdown(&mut self) -> Result<(), forge_provider_api::ProviderError> {
        if let Some(shutdown) = self.shutdown.take() {
            let _ = shutdown.send(());
        }
        let Some(task) = self.task.as_mut() else {
            return Ok(());
        };
        let result = task.await.map_err(|_| {
            native_failure(
                forge_provider_api::ProviderOperation::CollectEventReader,
                forge_provider_api::ProviderFailureKind::Runtime,
            )
        });
        self.task.take();
        result?
    }
}

fn native_failure(
    operation: forge_provider_api::ProviderOperation,
    kind: forge_provider_api::ProviderFailureKind,
) -> forge_provider_api::ProviderError {
    forge_provider_api::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
        operation, kind,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    struct ReaderExit {
        entered: Option<tokio::sync::oneshot::Sender<()>>,
        release: std::sync::mpsc::Receiver<()>,
    }

    impl Drop for ReaderExit {
        fn drop(&mut self) {
            if let Some(entered) = self.entered.take() {
                let _ = entered.send(());
            }
            let _ = self.release.recv();
        }
    }

    /// Do not report parent collection while its owned SDK reader is still exiting.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn terminal_shutdown_joins_nested_subscription_reader() {
        let (_send, receive) = tokio::sync::mpsc::channel(1);
        let (started, starting) = tokio::sync::oneshot::channel();
        let (entered, exiting) = tokio::sync::oneshot::channel();
        let (release, released) = std::sync::mpsc::channel();
        let reader = tokio::spawn(async move {
            let _exit = ReaderExit {
                entered: Some(entered),
                release: released,
            };
            let _ = started.send(());
            std::future::pending::<()>().await;
            Ok(())
        });
        starting.await.unwrap();
        let stream = event_stream::SessionEventStream::fixture(receive, reader);
        let mut monitor = CopilotTerminalMonitor::from_stream(stream, std::sync::Weak::new());
        let mut collecting = Box::pin(monitor.shutdown());
        assert!(futures_util::poll!(collecting.as_mut()).is_pending());
        tokio::time::timeout(std::time::Duration::from_secs(2), exiting)
            .await
            .unwrap()
            .unwrap();
        assert!(
            futures_util::poll!(collecting.as_mut()).is_pending(),
            "parent collection released before its nested reader joined"
        );
        release.send(()).unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(2), collecting)
            .await
            .unwrap()
            .unwrap();
        monitor.shutdown().await.unwrap();
    }
}
