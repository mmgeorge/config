use anyhow::{Context, Result};
use forge_provider_api::lifecycle::ObservationFailure;
use forge_provider_api::trace::ProviderTrace;
use github_copilot_sdk::SessionEvent;
use github_copilot_sdk::subscription::RecvErrorKind;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

/// Identify the bounded native handoff that first could not retain an observation.
pub(crate) enum IntakeQueueStage {
    Subscription,
    Unbound,
    Attachment,
    Foreground,
    Release,
}

/// Retain diagnostic ownership independently of foreground decoder locks.
#[derive(Clone)]
pub(crate) struct QueueDiagnostics {
    reported: Arc<AtomicBool>,
}

impl QueueDiagnostics {
    /// Record one bounded first-loss witness without entering an ordinary observation queue.
    pub(crate) fn lost(
        &self,
        _stage: IntakeQueueStage,
        _queued: usize,
        _capacity: usize,
        _scope: Option<&forge_provider_api::InvocationScope>,
        _event: Option<&SessionEvent>,
    ) {
        self.reported.store(true, Ordering::Release);
    }

    /// Retain the SDK's public skipped-event count without inventing its private ring capacity.
    fn subscription_lagged(&self, skipped: u64) {
        let _ = skipped;
        self.reported.store(true, Ordering::Release);
    }
}

/// Distinguish pending initialization from its established native outcome.
#[derive(Clone, Copy, Eq, PartialEq)]
pub(crate) enum StartupState {
    /// Native creation or resumption has not returned.
    Pending,
    /// Initialization has established whether subscription closure is expected.
    Finished(StartupOutcome),
}

/// Establish whether the native session started or rejected initialization.
#[derive(Clone, Copy, Eq, PartialEq)]
pub(crate) enum StartupOutcome {
    /// Native initialization succeeded and observation must remain available.
    Started,
    /// Native initialization failed and the SDK closes its subscription.
    Failed,
}

/// Drains SDK notifications without owning the session or blocking on presentation.
pub struct SessionEventStream {
    receiver: tokio::sync::mpsc::Receiver<SessionEvent>,
    task: Option<tokio::task::JoinHandle<Result<()>>>,
    session_id: String,
    startup: Option<tokio::sync::watch::Sender<StartupState>>,
    diagnostics: QueueDiagnostics,
}

impl SessionEventStream {
    /// Subscribe before starting work and retain only events needed by this consumer.
    pub fn new(
        session: &Arc<super::handle::CopilotSession>,
        accepts: fn(&SessionEvent) -> bool,
        trace: Arc<dyn ProviderTrace>,
        harness_session_id: String,
    ) -> Self {
        Self::from_subscription(
            session.inner.subscribe(),
            session.id().to_string(),
            accepts,
            trace,
            harness_session_id,
            StartupState::Finished(StartupOutcome::Started),
            session.generation.clone(),
        )
    }

    /// Drain a subscription installed before the native session starts.
    pub(crate) fn from_subscription(
        mut subscription: github_copilot_sdk::subscription::EventSubscription,
        session_id: String,
        accepts: fn(&SessionEvent) -> bool,
        _trace: Arc<dyn ProviderTrace>,
        _harness_session_id: String,
        initial: StartupState,
        generation: super::client_generation::ClientGeneration,
    ) -> Self {
        let (sender, receiver) = tokio::sync::mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        let (startup, mut startup_state) = tokio::sync::watch::channel(initial);
        let stream_session_id = session_id.clone();
        let diagnostics = QueueDiagnostics {
            reported: Arc::new(AtomicBool::new(false)),
        };
        let queue_diagnostics = diagnostics.clone();
        let task = tokio::spawn(async move {
            let result: std::result::Result<(), ObservationFailure> = async {
                loop {
                    let event = match subscription.recv().await {
                        Ok(event) => event,
                        Err(error) => {
                            if matches!(error.kind(), RecvErrorKind::Closed) {
                                while *startup_state.borrow() == StartupState::Pending {
                                    if startup_state.changed().await.is_err() {
                                        break;
                                    }
                                }
                                if *startup_state.borrow()
                                    == StartupState::Finished(StartupOutcome::Failed)
                                {
                                    return Ok(());
                                }
                            }
                            let failure = match error.kind() {
                                RecvErrorKind::Lagged(lagged) => {
                                    queue_diagnostics.subscription_lagged(lagged.skipped());
                                    forge_provider_api::lifecycle::ObservationFailure::Overflow
                                }
                                RecvErrorKind::Closed => {
                                    forge_provider_api::lifecycle::ObservationFailure::Disconnected
                                }
                                _ => forge_provider_api::lifecycle::ObservationFailure::Protocol,
                            };
                            return Err(failure);
                        }
                    };
                    if accepts(&event) {
                        match sender.try_send(event) {
                            Ok(()) => {}
                            Err(tokio::sync::mpsc::error::TrySendError::Full(event)) => {
                                queue_diagnostics.lost(
                                    IntakeQueueStage::Subscription,
                                    sender.max_capacity(),
                                    sender.max_capacity(),
                                    None,
                                    Some(&event),
                                );
                                return Err(ObservationFailure::Overflow);
                            }
                            Err(tokio::sync::mpsc::error::TrySendError::Closed(_)) => return Ok(()),
                        }
                    }
                }
            }
            .await;
            result.map_err(|failure| {
                generation.fail(failure.clone());
                forge_provider_api::ProviderError::Observation(failure).into()
            })
        });
        Self {
            receiver,
            task: Some(task),
            session_id: stream_session_id,
            startup: (initial == StartupState::Pending).then_some(startup),
            diagnostics,
        }
    }

    #[cfg(test)]
    pub(crate) fn fixture(
        receiver: tokio::sync::mpsc::Receiver<SessionEvent>,
        task: tokio::task::JoinHandle<Result<()>>,
    ) -> Self {
        Self {
            receiver,
            task: Some(task),
            session_id: "fixture".into(),
            startup: None,
            diagnostics: QueueDiagnostics {
                reported: Arc::new(AtomicBool::new(false)),
            },
        }
    }

    /// Share first-loss diagnostics with handoffs below this subscription reader.
    pub(crate) fn queue_diagnostics(&self) -> QueueDiagnostics {
        self.diagnostics.clone()
    }

    /// Resolve whether closure during session initialization was expected.
    pub(crate) fn finish_startup(&mut self, outcome: StartupOutcome) {
        if let Some(startup) = self.startup.take() {
            startup.send_replace(StartupState::Finished(outcome));
        }
    }

    /// Stop and join the SDK subscription reader before releasing its session.
    pub(crate) async fn close(&mut self) -> Result<()> {
        let Some(task) = self.task.as_mut() else {
            return Ok(());
        };
        task.abort();
        let result = task.await;
        self.task.take();
        match result {
            // The producer has already published typed observation loss to the generation.
            // Joining that failed producer proves collection; it is not a second cleanup error.
            Ok(Err(error))
                if matches!(
                    error.downcast_ref::<forge_provider_api::ProviderError>(),
                    Some(forge_provider_api::ProviderError::Observation(_))
                ) =>
            {
                Ok(())
            }
            Ok(result) => result,
            Err(error) if error.is_cancelled() => Ok(()),
            Err(error) => Err(error).context("Copilot event intake failed during collection"),
        }
    }

    /// Receive retained notifications in SDK order without doing work on the intake task.
    pub async fn recv(&mut self) -> Result<SessionEvent> {
        if let Some(event) = self.receiver.recv().await {
            return Ok(event);
        }
        let result = self
            .task
            .as_mut()
            .context("Copilot event intake was already collected")?
            .await;
        self.task.take();
        result
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
        if let Some(task) = &self.task {
            task.abort();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Keep a producer's handle when another select branch interrupts error collection.
    #[tokio::test]
    async fn cancelled_receive_retains_producer_for_collection() {
        let (sender, receiver) = tokio::sync::mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        drop(sender);
        let (release, released) = tokio::sync::oneshot::channel::<()>();
        let task = tokio::spawn(async move {
            released.await?;
            Err(forge_provider_api::ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Disconnected,
            )
            .into())
        });
        let mut stream = SessionEventStream::fixture(receiver, task);
        assert!(
            tokio::time::timeout(std::time::Duration::from_millis(1), stream.recv())
                .await
                .is_err()
        );
        assert!(stream.task.is_some());
        release.send(()).unwrap();
        let error = stream.recv().await.unwrap_err();
        assert!(matches!(
            error.downcast_ref::<forge_provider_api::ProviderError>(),
            Some(forge_provider_api::ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Disconnected
            ))
        ));
        stream.close().await.unwrap();
    }

    /// Explicit close waits for the producer to drop its queue and can be repeated safely.
    #[tokio::test]
    async fn explicit_close_collects_the_subscription_task() {
        let (sender, receiver) = tokio::sync::mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        let probe = sender.clone();
        let (ready, started) = tokio::sync::oneshot::channel();
        let task = tokio::spawn(async move {
            let _sender = sender;
            let _ = ready.send(());
            std::future::pending::<Result<()>>().await
        });
        let mut stream = SessionEventStream::fixture(receiver, task);
        started.await.unwrap();
        stream.close().await.unwrap();
        assert_eq!(probe.strong_count(), 1);
        stream.close().await.unwrap();
    }

    /// Treat a joined observation failure as collected while retaining real worker failure.
    #[tokio::test]
    async fn close_separates_reported_observation_loss_from_worker_failure() {
        let (sender, receiver) = tokio::sync::mpsc::channel(1);
        let task = tokio::spawn(async move {
            drop(sender);
            Err(forge_provider_api::ProviderError::Observation(ObservationFailure::Overflow).into())
        });
        let mut stream = SessionEventStream::fixture(receiver, task);
        while !stream.task.as_ref().unwrap().is_finished() {
            tokio::task::yield_now().await;
        }
        stream.close().await.unwrap();

        let (sender, receiver) = tokio::sync::mpsc::channel(1);
        let task = tokio::spawn(async move {
            drop(sender);
            panic!("failed subscription worker");
        });
        let mut stream = SessionEventStream::fixture(receiver, task);
        while !stream.task.as_ref().unwrap().is_finished() {
            tokio::task::yield_now().await;
        }
        assert!(
            format!("{:#}", stream.close().await.unwrap_err())
                .contains("failed subscription worker")
        );
    }

    #[tokio::test]
    async fn intake_preserves_large_events_before_reporting_the_producer_error() {
        let (sender, receiver) = tokio::sync::mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
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
        let mut stream = SessionEventStream::fixture(receiver, task);
        stream.session_id = "native-session".into();
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
        let (sender, receiver) = tokio::sync::mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        let task = tokio::spawn(async move {
            let _sender = sender;
            panic!("intake fixture panic");
        });
        let mut stream = SessionEventStream::fixture(receiver, task);
        stream.session_id = "native-session".into();
        let error = format!("{:#}", stream.recv().await.unwrap_err());
        assert!(error.contains("native-session"), "{error}");
        assert!(error.contains("intake fixture panic"), "{error}");
    }

    #[tokio::test]
    async fn intake_reports_unexpected_successful_task_exit() {
        let (sender, receiver) = tokio::sync::mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        let task = tokio::spawn(async move {
            drop(sender);
            Ok(())
        });
        let mut stream = SessionEventStream::fixture(receiver, task);
        stream.session_id = "native-session".into();
        let error = stream.recv().await.unwrap_err().to_string();
        assert!(error.contains("native-session"), "{error}");
        assert!(
            error.contains("exited without a subscription error"),
            "{error}"
        );
    }

    #[tokio::test]
    async fn intake_reports_task_cancellation() {
        let (sender, receiver) = tokio::sync::mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        let task = tokio::spawn(async move {
            let _sender = sender;
            std::future::pending::<Result<()>>().await
        });
        task.abort();
        let mut stream = SessionEventStream::fixture(receiver, task);
        stream.session_id = "native-session".into();
        let error = format!("{:#}", stream.recv().await.unwrap_err());
        assert!(error.contains("native-session"), "{error}");
        assert!(error.contains("cancelled"), "{error}");
    }
}
