use super::turn::CopilotTurnEvent;
use forge_provider_api::{AdapterActivity, ProviderError, ProviderEventSink, SanitizedMessage};
use forge_provider_api::{Compaction, CompactionState};
use std::sync::Arc;
use tokio::sync::Mutex;

/// Correlates manual RPC identities with native session lifecycle notifications.
#[derive(Default)]
pub struct ManualCompactionMap {
    pending: Vec<String>,
    operation_by_native_id: std::collections::HashMap<String, String>,
}

impl ManualCompactionMap {
    /// Admit the Harness identity before issuing the native manual request.
    pub fn register(&mut self, id: String) {
        self.pending.push(id);
    }

    /// Release an unassigned RPC without losing identities used by delayed completions.
    pub fn finish(&mut self, id: &str) {
        self.pending.retain(|pending| pending != id);
    }

    fn correlate(&mut self, compaction: &mut Compaction) {
        if compaction.state == CompactionState::Running
            && !self.pending.is_empty()
            && !self.operation_by_native_id.contains_key(&compaction.id)
        {
            let operation = self.pending.remove(0);
            self.operation_by_native_id
                .insert(compaction.id.clone(), operation);
        }
        if let Some(operation) = self.operation_by_native_id.get(&compaction.id) {
            compaction.id = operation.clone();
            compaction.manual = true;
        }
    }
}

/// Retains a session subscription independently of prompt completion.
#[derive(Clone)]
pub struct CompactionSubscription {
    /// Receives normalized evidence independently of the active prompt reader.
    pub sink: ProviderEventSink,
    state: Arc<Mutex<std::collections::HashMap<String, Compaction>>>,
    manual: Arc<Mutex<ManualCompactionMap>>,
}

impl CompactionSubscription {
    /// Preserve normalization and manual identity across SDK session rebinding.
    pub fn new(sink: ProviderEventSink) -> Self {
        Self {
            sink,
            state: Arc::new(Mutex::new(Default::default())),
            manual: Arc::new(Mutex::new(ManualCompactionMap::default())),
        }
    }
}

/// Owns the SDK notification reader and its active and background delivery lanes.
pub struct CompactionMonitor {
    /// Maps manual RPC results and native notifications onto their owning row.
    pub manual: Arc<Mutex<ManualCompactionMap>>,
    task: Option<tokio::task::JoinHandle<Result<(), ProviderError>>>,
    shutdown: Option<tokio::sync::oneshot::Sender<()>>,
}

impl CompactionMonitor {
    /// Subscribe before the owning session begins processing prompts.
    pub fn new(
        session: &std::sync::Arc<super::handle::CopilotSession>,
        subscription_state: CompactionSubscription,
        turn_event: Arc<CopilotTurnEvent>,
        trace: Arc<dyn forge_provider_api::trace::ProviderTrace>,
        harness_session_id: String,
    ) -> Self {
        let subscription = super::event_stream::SessionEventStream::new(
            session,
            |event| {
                matches!(
                    event.event_type.as_str(),
                    "session.compaction_start" | "session.compaction_complete"
                )
            },
            trace.clone(),
            harness_session_id.clone(),
        );
        Self::from_stream(
            subscription,
            session.inner.id().to_string(),
            subscription_state,
            turn_event,
            trace,
            harness_session_id,
        )
    }

    /// Own subscription delivery and collection independently of the native session handle.
    fn from_stream(
        mut subscription: super::event_stream::SessionEventStream,
        thread_id: String,
        subscription_state: CompactionSubscription,
        _turn_event: Arc<CopilotTurnEvent>,
        _trace: Arc<dyn forge_provider_api::trace::ProviderTrace>,
        _harness_session_id: String,
    ) -> Self {
        let manual = Arc::clone(&subscription_state.manual);
        let correlation = Arc::clone(&manual);
        let (shutdown, closing) = tokio::sync::oneshot::channel();
        let task = tokio::spawn(async move {
            let observe = async {
                let sink = subscription_state.sink;
                loop {
                    let event = match subscription.recv().await {
                        Ok(event) => event,
                        Err(_) => {
                            sink.fail(forge_provider_api::events::EventDeliveryError::Observation(
                                forge_provider_api::ObservationFailure::Disconnected,
                            ));
                            return;
                        }
                    };
                    if !matches!(
                        event.event_type.as_str(),
                        "session.compaction_start" | "session.compaction_complete"
                    ) {
                        continue;
                    }
                    if event.agent_id.is_some() {
                        continue;
                    }
                    let event_thread = thread_id.as_str();
                    let mut state = subscription_state.state.lock().await;
                    let update = super::decode::decode_compaction(
                        &event,
                        sink.scope().thread_id().clone().as_str(),
                        state.get(event_thread),
                    );
                    let mut update = match update {
                        Ok(update) => update,
                        Err(_error) => {
                            drop(state);
                            let warning = AdapterActivity::Warning(forge_provider_api::ProviderWarning {
                                scope: forge_provider_api::WarningScope::Thread { thread_id: sink.scope().thread_id().clone() },
                                kind: forge_provider_api::ProviderWarningKind::IncompleteCompactionMetadata,
                                message: SanitizedMessage::new("Provider compaction event could not be normalized").expect("static warning is nonempty"),
                            });
                            if sink.activity_writer().send_wait(warning).await.is_err() {
                                return;
                            }
                            continue;
                        }
                    };
                    state.insert(event_thread.into(), update.clone());
                    drop(state);
                    if event.agent_id.is_none() {
                        correlation.lock().await.correlate(&mut update);
                    }
                    if let Err(error) = sink
                        .activity_writer()
                        .send_wait(AdapterActivity::Thread {
                            thread_id: sink.scope().thread_id().clone(),
                            event: forge_provider_api::ThreadEvent::CompactionUpdated(Box::new(
                                update,
                            )),
                        })
                        .await
                    {
                        let _ = error;
                        return;
                    }
                }
            };
            tokio::select! {
                biased;
                _ = closing => {},
                _ = observe => {},
            }
            subscription.close().await.map_err(|_| {
                native_failure(
                    forge_provider_api::ProviderOperation::ThreadClose,
                    forge_provider_api::ProviderFailureKind::Runtime,
                )
            })
        });
        Self {
            manual,
            task: Some(task),
            shutdown: Some(shutdown),
        }
    }

    /// Stop delivery and join both the monitor and its owned SDK subscription reader.
    pub async fn shutdown(&mut self) -> Result<(), ProviderError> {
        if let Some(shutdown) = self.shutdown.take() {
            let _ = shutdown.send(());
        }
        let Some(task) = self.task.as_mut() else {
            return Ok(());
        };
        let result = task.await.map_err(|_| {
            native_failure(
                forge_provider_api::ProviderOperation::ThreadClose,
                forge_provider_api::ProviderFailureKind::Runtime,
            )
        });
        self.task.take();
        result?
    }
}

impl Drop for CompactionMonitor {
    fn drop(&mut self) {
        if let Some(shutdown) = self.shutdown.take() {
            let _ = shutdown.send(());
        }
    }
}

fn native_failure(
    operation: forge_provider_api::ProviderOperation,
    kind: forge_provider_api::ProviderFailureKind,
) -> ProviderError {
    ProviderError::Failure(forge_provider_api::ProviderFailure::new(operation, kind))
}
