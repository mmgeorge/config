use anyhow::{Context, Result};
use forge_provider_api::lifecycle::ObservationFailure;
use forge_provider_api::trace::ProviderTrace;
use github_copilot_sdk::SessionEvent;
use github_copilot_sdk::subscription::RecvErrorKind;
use std::sync::Arc;

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

/// Retain first receipt times independently of delayed decoding and send acknowledgements.
#[derive(Clone, Default)]
pub(crate) struct ObservationTimes {
    times: Arc<std::sync::Mutex<std::collections::HashMap<String, i64>>>,
}
impl ObservationTimes {
    /// Freeze the first SDK receipt time for this native event identity.
    fn record(&self, event: &SessionEvent) {
        self.times
            .lock()
            .expect("native observation clock is not poisoned")
            .entry(event.id.clone())
            .or_insert_with(super::decode::timestamp_ms);
    }
    /// Get the original native receipt time without charging later queue or decoder waits.
    pub(crate) fn observed_at(&self, event: &SessionEvent) -> i64 {
        self.record(event);
        self.times
            .lock()
            .expect("native observation clock is not poisoned")[&event.id]
    }
}

/// Preserve native order without imposing an adapter-local event-count limit.
enum NativeReceiver {
    Live(tokio::sync::mpsc::UnboundedReceiver<SessionEvent>),
    #[cfg(test)]
    Fixture(tokio::sync::mpsc::Receiver<SessionEvent>),
}
impl NativeReceiver {
    /// Drain the retained native queue before observing its producer's failure.
    async fn recv(&mut self) -> Option<SessionEvent> {
        match self {
            Self::Live(receiver) => receiver.recv().await,
            #[cfg(test)]
            Self::Fixture(receiver) => receiver.recv().await,
        }
    }
}

/// Drains SDK notifications without owning the session or blocking on presentation.
pub struct SessionEventStream {
    receiver: NativeReceiver,
    task: Option<tokio::task::JoinHandle<Result<()>>>,
    session_id: String,
    startup: Option<tokio::sync::watch::Sender<StartupState>>,
    observation_times: ObservationTimes,
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
        let (sender, receiver) = tokio::sync::mpsc::unbounded_channel();
        let (startup, mut startup_state) = tokio::sync::watch::channel(initial);
        let stream_session_id = session_id.clone();
        let observation_times = ObservationTimes::default();
        let observations = observation_times.clone();
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
                                RecvErrorKind::Lagged(_) => {
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
                    observations.record(&event);
                    if accepts(&event) && sender.send(event).is_err() {
                        return Ok(());
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
            receiver: NativeReceiver::Live(receiver),
            task: Some(task),
            session_id: stream_session_id,
            startup: (initial == StartupState::Pending).then_some(startup),
            observation_times,
        }
    }

    #[cfg(test)]
    pub(crate) fn fixture(
        receiver: tokio::sync::mpsc::Receiver<SessionEvent>,
        task: tokio::task::JoinHandle<Result<()>>,
    ) -> Self {
        Self {
            receiver: NativeReceiver::Fixture(receiver),
            task: Some(task),
            session_id: "fixture".into(),
            startup: None,
            observation_times: ObservationTimes::default(),
        }
    }

    /// Share immutable native receipt times with retained foreground and background decoders.
    pub(crate) fn observation_times(&self) -> ObservationTimes {
        self.observation_times.clone()
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
            self.observation_times.record(&event);
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
}
