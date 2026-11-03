use super::turn::CopilotTurnEvent;
use forge_provider_api::{Compaction, CompactionState};
use forge_provider_api::{ProviderEvent, ProviderEventSink};
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
    task: tokio::task::JoinHandle<()>,
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
        let session = &session.inner;
        let mut subscription = super::event_stream::SessionEventStream::new(
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
        let manual = Arc::clone(&subscription_state.manual);
        let correlation = Arc::clone(&manual);
        let thread_id = session.id().to_string();
        let task = tokio::spawn(async move {
            let sink = subscription_state.sink;
            loop {
                let event = match subscription.recv().await {
                    Ok(event) => event,
                    Err(error) => {
                        let diagnostic = ProviderEvent::Diagnostic {
                            scope: sink.scope().clone(),
                            diagnostic: forge_provider_api::ProviderDiagnostic {
                                code: Some("copilot_compaction_subscription_failed".into()),
                                message: format!(
                                    "Copilot compaction subscription failed: {error:#}"
                                ),
                                fatal: true,
                            },
                        };
                        if let Err(delivery) = sink.send_wait(diagnostic).await {
                            trace.record(
                                &harness_session_id,
                                "copilot.compaction.delivery_failed",
                                serde_json::json!({"error":delivery.to_string()}),
                            );
                        }
                        return;
                    }
                };
                if !matches!(
                    event.event_type.as_str(),
                    "session.compaction_start" | "session.compaction_complete"
                ) {
                    continue;
                }
                let event_thread = event.agent_id.as_deref().unwrap_or(&thread_id);
                let mut state = subscription_state.state.lock().await;
                let update =
                    super::decode::decode_compaction(&event, event_thread, state.get(event_thread));
                let mut update = match update {
                    Ok(update) => update,
                    Err(error) => {
                        drop(state);
                        let diagnostic = ProviderEvent::Diagnostic {
                            scope: sink.scope().clone(),
                            diagnostic: forge_provider_api::ProviderDiagnostic {
                                code: Some("invalid_copilot_compaction".into()),
                                message: error.to_string(),
                                fatal: false,
                            },
                        };
                        if let Err(delivery) = sink.send_wait(diagnostic).await {
                            trace.record(
                                &harness_session_id,
                                "copilot.compaction.delivery_failed",
                                serde_json::json!({"error":delivery.to_string()}),
                            );
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
                if let Some(active_sink) = turn_event.event_sink()
                    && let Err(error) = active_sink
                        .send_wait(ProviderEvent::Compaction {
                            scope: active_sink.scope().clone(),
                            update: update.clone().into(),
                        })
                        .await
                {
                    trace.record(
                        &harness_session_id,
                        "copilot.compaction.active_delivery_failed",
                        serde_json::json!({"error":error.to_string()}),
                    );
                }
                if let Err(error) = sink
                    .send_wait(ProviderEvent::Compaction {
                        scope: sink.scope().clone(),
                        update: update.into(),
                    })
                    .await
                {
                    trace.record(
                        &harness_session_id,
                        "copilot.compaction.delivery_failed",
                        serde_json::json!({"error":error.to_string()}),
                    );
                    return;
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

    /// Retain the native summary, failure, accounting, and start identity through replay.
    #[test]
    fn compaction_retains_summary_failure_and_identity_across_replays() -> anyhow::Result<()> {
        let started: github_copilot_sdk::SessionEvent =
            serde_json::from_value(serde_json::json!({
                "id":"compaction-start","type":"session.compaction_start",
                "timestamp":"2026-10-29T12:00:00Z",
                "data":{"model":"summary-model","conversationTokens":100}
            }))?;
        let completed: github_copilot_sdk::SessionEvent = serde_json::from_value(
            serde_json::json!({
                "id":"compaction-complete","type":"session.compaction_complete",
                "timestamp":"2026-10-29T12:00:00Z",
                "data":{
                    "success":false,"error":"summary request failed","summaryContent":"Retain the contract.",
                    "preCompactionTokens":100,"postCompactionTokens":40,"tokensRemoved":60,"messagesRemoved":0,
                    "compactionTokensUsed":{"inputTokens":100,"outputTokens":20,"cacheReadTokens":0,
                        "copilotUsage":{"totalNanoAiu":1.5,"tokenDetails":[{"tokenType":"input","tokenCount":100}]}}
                }
            }),
        )?;
        let first = super::super::decode::decode_compaction(&started, "root", None)?;
        let settled = super::super::decode::decode_compaction(&completed, "root", Some(&first))?;
        let replayed = super::super::decode::decode_compaction(&completed, "root", Some(&settled))?;
        assert_eq!(replayed.id, started.id);
        assert_eq!(replayed.state, CompactionState::Failed);
        assert_eq!(
            replayed.result.summary_content.as_deref(),
            Some("Retain the contract.")
        );
        assert_eq!(
            replayed.result.error.as_deref(),
            Some("summary request failed")
        );
        assert_eq!(replayed.result.messages_removed, Some(0));
        assert_eq!(replayed.result.model.as_deref(), Some("summary-model"));
        assert_eq!(
            replayed.result.before_context.unwrap().conversation_tokens,
            Some(100)
        );
        let usage = replayed.result.compaction_tokens_used.unwrap();
        assert_eq!(usage.cached_input, Some(0));
        assert_eq!(usage.billing.unwrap().measurements[0].value, "1.5");
        let orphan = super::super::decode::decode_compaction(
            &serde_json::from_value(serde_json::json!({
                "id":"orphan","type":"session.compaction_complete",
                "timestamp":"2026-10-29T12:00:00Z","data":{"success":true}
            }))?,
            "root",
            None,
        )?;
        assert_eq!(orphan.result.pre_compaction_tokens, None);
        Ok(())
    }

    #[tokio::test]
    async fn compaction_identity_survives_turn_and_subscription_rebinding() {
        let (sink, _stream) =
            forge_provider_api::events::channel(forge_provider_api::EventScope::SessionOperation {
                owner: forge_provider_api::ExecutionOwner::root("session"),
                operation_id: "compaction-observer".into(),
            });
        let subscription = CompactionSubscription::new(sink);
        let rebound = subscription.clone();
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
        let update = super::super::decode::decode_compaction(&start, "thread", None).unwrap();
        subscription
            .state
            .lock()
            .await
            .insert("thread".into(), update);
        let mut state = rebound.state.lock().await;
        let compaction =
            super::super::decode::decode_compaction(&complete, "thread", state.get("thread"))
                .unwrap();
        state.insert("thread".into(), compaction.clone());
        assert_eq!(state.len(), 1);
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
