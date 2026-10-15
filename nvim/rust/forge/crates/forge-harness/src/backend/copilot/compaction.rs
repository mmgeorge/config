use super::{
    BackendEvent, BackendEventSink, BackendOutput, CopilotEventDecoder, CopilotTurnEvent,
    SharedCopilotSession,
};
use crate::backend::{Compaction, CompactionState};
use std::sync::Arc;
use tokio::sync::Mutex;

/// Correlates manual RPC identities with native session lifecycle notifications.
#[derive(Default)]
pub(super) struct ManualCompactionMap {
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
pub(super) struct CompactionSubscription {
    /// Receives normalized evidence independently of the active prompt reader.
    pub sink: BackendEventSink,
    output: Arc<Mutex<BackendOutput>>,
    manual: Arc<Mutex<ManualCompactionMap>>,
}

impl CompactionSubscription {
    /// Preserve normalization and manual identity across SDK session rebinding.
    pub fn new(sink: BackendEventSink) -> Self {
        Self {
            sink,
            output: Arc::new(Mutex::new(BackendOutput::default())),
            manual: Arc::new(Mutex::new(ManualCompactionMap::default())),
        }
    }
}

/// Owns the SDK notification reader and its active and background delivery lanes.
pub(super) struct CompactionMonitor {
    /// Maps manual RPC results and native notifications onto their owning row.
    pub manual: Arc<Mutex<ManualCompactionMap>>,
    task: tokio::task::JoinHandle<()>,
}

impl CompactionMonitor {
    /// Subscribe before the owning session begins processing prompts.
    pub fn new(
        session: &SharedCopilotSession,
        subscription_state: CompactionSubscription,
        turn_event: Arc<CopilotTurnEvent>,
        trace: Arc<crate::trace::TraceStore>,
        harness_session_id: String,
    ) -> Self {
        let mut subscription = super::event_stream::SessionEventStream::new(session, |event| {
            matches!(event.event_type.as_str(), "session.compaction_start" | "session.compaction_complete")
        }, trace, harness_session_id);
        let manual = Arc::clone(&subscription_state.manual);
        let correlation = Arc::clone(&manual);
        let thread_id = session.id().to_string();
        let task = tokio::spawn(async move {
            let decoder = CopilotEventDecoder;
            let sink = subscription_state.sink;
            subscription_state.output.lock().await.backend_session_id = Some(thread_id);
            loop {
                let event = match subscription.recv().await {
                    Ok(event) => event,
                    Err(error) => {
                        let _ = sink
                            .send_wait(BackendEvent {
                                received_at_ms: None,
                                address: None,
                                turn_boundary: None,
                                kind: "error".into(),
                                text: Some(format!(
                                    "Copilot compaction subscription failed: {error}"
                                )),
                                data: serde_json::Value::Null,
                                activity: None,
                                summary: None,
                                task_update: None,
                            })
                            .await;
                        return;
                    }
                };
                if !matches!(
                    event.event_type.as_str(),
                    "session.compaction_start" | "session.compaction_complete"
                ) {
                    continue;
                }
                let mut output = subscription_state.output.lock().await;
                decoder.decode(&event, &mut output, None).await;
                let normalized = output
                    .event
                    .iter()
                    .rev()
                    .find(|normalized| {
                        normalized
                            .data
                            .get("provider_event_id")
                            .or_else(|| normalized.data.get("id"))
                            .and_then(serde_json::Value::as_str)
                            == Some(event.id.as_str())
                    })
                    .cloned();
                let thread_id = output.backend_session_id.clone();
                drop(output);
                if let Some(mut normalized) = normalized {
                    if let Ok(Some(mut compaction)) = normalized.compaction() {
                        if thread_id.as_deref() == Some(compaction.thread_id.as_str()) {
                            correlation.lock().await.correlate(&mut compaction);
                        }
                        normalized.data = serde_json::to_value(compaction)
                            .expect("compaction is JSON serializable");
                    }
                    let active_sink = turn_event.event_sink();
                    if let Some(active_sink) = active_sink {
                        let _ = active_sink.send_wait(normalized.clone()).await;
                    }
                    if sink.send_wait(normalized).await.is_err() {
                        return;
                    }
                }
            }
        });
        Self { manual, task }
    }
}

impl Drop for CompactionMonitor {
    fn drop(&mut self) {
        self.task.abort();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn compaction_identity_survives_turn_and_subscription_rebinding() {
        let (sink, _stream) = crate::backend::events::channel();
        let subscription = CompactionSubscription::new(sink);
        let rebound = subscription.clone();
        subscription.output.lock().await.backend_session_id = Some("thread".into());
        let start = github_copilot_sdk::SessionEvent {
            id: "native-start".into(),
            timestamp: "2026-10-10T00:00:00Z".into(),
            parent_id: None,
            ephemeral: None,
            agent_id: None,
            debug_cli_received_at_ms: None,
            debug_ws_forwarded_at_ms: None,
            event_type: "session.compaction_start".into(),
            data: serde_json::json!({"model":"summary-model"}),
        };
        let complete = github_copilot_sdk::SessionEvent {
            id: "native-complete".into(),
            event_type: "session.compaction_complete".into(),
            data: serde_json::json!({"success":true,"summaryContent":"Keep the task constraints."}),
            ..start.clone()
        };
        CopilotEventDecoder
            .decode(&start, &mut *subscription.output.lock().await, None)
            .await;
        CopilotEventDecoder
            .decode(&complete, &mut *rebound.output.lock().await, None)
            .await;
        let output = rebound.output.lock().await;
        assert_eq!(output.event.len(), 1);
        let compaction = output.event[0].compaction().unwrap().unwrap();
        assert_eq!(compaction.id, start.id);
        assert_eq!(compaction.state, CompactionState::Completed);
        assert_eq!(compaction.result.model.as_deref(), Some("summary-model"));
        assert_eq!(
            compaction.result.summary_content.as_deref(),
            Some("Keep the task constraints.")
        );
        assert!(Arc::ptr_eq(&subscription.manual, &rebound.manual));
    }

    #[test]
    fn manual_compaction_keeps_native_replays_and_delayed_completions_on_one_row() {
        let mut correlation = ManualCompactionMap::default();
        let automatic = Compaction {
            id: "automatic".into(),
            thread_id: "thread".into(),
            state: CompactionState::Running,
            manual: false,
            started_at_ms: None,
            completed_at_ms: None,
            result: Default::default(),
        };
        let mut background_completion = automatic.clone();
        background_completion.state = CompactionState::Completed;
        correlation.register("manual-row".into());
        correlation.correlate(&mut background_completion);
        assert_eq!(background_completion.id, "automatic");
        assert!(!background_completion.manual);
        let mut start = automatic;
        start.id = "native-manual".into();
        let completion = Compaction {
            state: CompactionState::Completed,
            ..start.clone()
        };
        correlation.correlate(&mut start);
        assert_eq!(start.id, "manual-row");
        assert!(start.manual);
        correlation.finish("manual-row");
        correlation.register("next-manual-row".into());
        for mut replay in [completion.clone(), completion] {
            correlation.correlate(&mut replay);
            assert_eq!(replay.id, "manual-row");
            assert!(replay.manual);
        }
        assert_eq!(correlation.pending, vec!["next-manual-row"]);
        correlation.finish("next-manual-row");
        assert!(correlation.pending.is_empty());
    }
}
