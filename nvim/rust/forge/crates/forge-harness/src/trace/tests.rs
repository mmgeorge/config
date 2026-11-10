use super::{LogLevel, store::TraceStore};
use serde_json::json;
use tempfile::tempdir;

/// Retain normalized collection diagnostics through storage and inspection without native text.
#[test]
fn terminal_collection_diagnostics_survive_trace_policy() {
    let directory = tempdir().unwrap();
    let trace = TraceStore::open(directory.path()).unwrap();
    trace.configure(LogLevel::Trace).unwrap();
    let failure = forge_provider_api::ProviderFailure::terminal_inventory(
        forge_provider_api::ProviderOperation::ThreadClose,
        forge_provider_api::terminal::TerminalInventoryError::MissingIdentity,
    )
    .at_phase(
        forge_provider_api::ProviderPhase::Collection,
        forge_provider_api::FailureDisposition::Invalidated,
    );
    trace.record(
        "session",
        "provider.operation.completed",
        json!({
            "operation":failure.operation(), "kind":failure.kind(), "phase":failure.phase(),
            "disposition":failure.disposition(), "terminal_inventory_error":failure.terminal_inventory_error(),
            "reason":"private native response", "command":"private command",
        }),
    );
    trace.flush().unwrap();
    let contents = std::fs::read_to_string(trace.session_status("session").path).unwrap();
    let record: serde_json::Value = serde_json::from_str(contents.trim()).unwrap();
    let inspected = super::policy::inspection_record(&record);
    assert_eq!(inspected["payload"]["kind"], "protocol");
    assert_eq!(
        inspected["payload"]["terminal_inventory_error"],
        "missing_identity"
    );
    assert_eq!(inspected["payload"]["disposition"], "invalidated");
    assert!(!contents.contains("private"));
    assert_eq!(record["payload"], inspected["payload"]);
}

#[test]
fn detailed_logs_exclude_bodies_isolate_sessions_and_redact_credentials() {
    let directory = tempdir().unwrap();
    let trace = TraceStore::open(directory.path()).unwrap();
    trace.configure(LogLevel::Trace).unwrap();
    trace.record(
        "session-a",
        "model.sent",
        json!({"params":{"prompt":"full prompt","authorization":"secret"}}),
    );
    trace.record("session-a", "model.sent", json!({"params":{"config":{"mcp_servers":{
            "fixture":{"env":{"CUSTOM_NAME":"credential-value"},"http_headers":{"X-Custom":"header-value"}}
        }}}}));
    trace.record(
        "session-b",
        "model.received",
        json!({"text":"other conversation"}),
    );
    trace.flush().unwrap();
    let contents = std::fs::read_to_string(trace.session_status("session-a").path).unwrap();
    assert!(!contents.contains("full prompt"));
    assert!(!contents.contains("params"));
    assert!(!contents.contains("secret"));
    assert!(!contents.contains("credential-value") && !contents.contains("header-value"));
    assert!(!contents.contains("other conversation"));
    trace.configure(LogLevel::None).unwrap();
    trace.record("session-a", "model.sent", json!({"text":"disabled"}));
    assert_eq!(
        contents,
        std::fs::read_to_string(trace.session_status("session-a").path).unwrap()
    );
    drop(trace);
    let reopened = TraceStore::open(directory.path()).unwrap();
    assert_eq!(
        reopened.configure_default(LogLevel::Info).unwrap().level,
        LogLevel::None
    );
}

/// Filter the same callsites dynamically while retaining error correlation below Info.
#[test]
fn logging_levels_filter_events_and_preserve_redacted_span_context() {
    struct PrivatePayload;
    impl std::fmt::Debug for PrivatePayload {
        /// Reject accidental formatting of native request or response bodies.
        fn fmt(&self, _formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            panic!("native payload fields must not be formatted");
        }
    }
    let directory = tempdir().unwrap();
    let trace = TraceStore::open(directory.path()).unwrap();
    assert_eq!(trace.status().level, LogLevel::Info);
    let dispatch = trace.dispatch();
    for (level, expected) in [
        (LogLevel::None, vec![]),
        (LogLevel::Error, vec!["error"]),
        (LogLevel::Warn, vec!["warn", "error"]),
        (LogLevel::Info, vec!["info", "warn", "error"]),
        (LogLevel::Trace, vec!["trace", "info", "warn", "error"]),
    ] {
        trace.configure(level).unwrap();
        trace.clear_session("session").unwrap();
        tracing::dispatcher::with_default(&dispatch, || {
            let span = tracing::info_span!(
                "ipc",
                session_id = "session",
                request_id = 17,
                method = "session.prompt.submit"
            );
            let _entered = span.enter();
            tracing::trace!(target: "forge::test", event_name = "trace", prompt = "private prompt", payload = ?PrivatePayload);
            tracing::info!(target: "forge::ipc", event_name = "info", elapsed_ms = 1.5);
            tracing::warn!(target: "forge::test", event_name = "warn", token = "private token");
            tracing::error!(target: "forge::test", event_name = "error",
                error = "connection failed: Authorization=Bearer private-credential; github_pat_privatevalue");
        });
        trace.flush().unwrap();
        let contents = std::fs::read_to_string(trace.session_status("session").path).unwrap();
        let records = contents
            .lines()
            .map(|line| serde_json::from_str::<serde_json::Value>(line).unwrap())
            .collect::<Vec<_>>();
        assert_eq!(
            records
                .iter()
                .map(|record| record["event"].as_str().unwrap())
                .collect::<Vec<_>>(),
            expected
        );
        assert!(!contents.contains("private"), "{contents}");
        if let Some(record) = records.last() {
            assert_eq!(record["payload"]["request_id"], 17);
            assert_eq!(record["payload"]["method"], "session.prompt.submit");
            assert!(
                record["payload"]["error"]
                    .as_str()
                    .unwrap()
                    .contains("connection failed")
            );
            assert_eq!(record["payload"]["span_path"][0]["name"], "ipc");
            assert!(
                super::inspection_record(record)["payload"]
                    .get("error")
                    .is_none()
            );
        }
    }
}

/// Preserve explicit legacy choices and report invalid settings instead of resetting them.
#[test]
fn logging_configuration_migrates_choices_and_rejects_malformed_settings() {
    for (enabled, expected) in [(true, LogLevel::Trace), (false, LogLevel::None)] {
        let directory = tempdir().unwrap();
        let path = directory.path().join("harness-trace-config.json");
        std::fs::write(
            &path,
            serde_json::to_vec(&json!({"enabled":enabled})).unwrap(),
        )
        .unwrap();
        let trace = TraceStore::open(directory.path()).unwrap();
        assert_eq!(trace.status().level, expected);
        assert_eq!(
            trace.configure_default(LogLevel::Info).unwrap().level,
            expected
        );
        drop(trace);
        let contents: serde_json::Value =
            serde_json::from_slice(&std::fs::read(&path).unwrap()).unwrap();
        assert_eq!(contents["level"], serde_json::to_value(expected).unwrap());
        assert!(contents.get("enabled").is_none());
    }
    let directory = tempdir().unwrap();
    let path = directory.path().join("harness-trace-config.json");
    let invalid = br#"{"level":"unsupported"}"#;
    std::fs::write(&path, invalid).unwrap();
    let error = TraceStore::open(directory.path()).err().unwrap();
    assert!(
        error.to_string().contains("harness-trace-config.json"),
        "{error}"
    );
    assert_eq!(std::fs::read(&path).unwrap(), invalid);
}

/// Keep concurrent coarse events instead of treating ordinary producer contention as lost diagnostics.
#[test]
fn concurrent_info_events_survive_shared_writer_admission() {
    let directory = tempdir().unwrap();
    let trace = TraceStore::open(directory.path()).unwrap();
    let ready = std::sync::Barrier::new(4);
    std::thread::scope(|scope| {
        for worker_index in 0..4 {
            let dispatch = trace.dispatch();
            let ready = &ready;
            scope.spawn(move || {
                ready.wait();
                tracing::dispatcher::with_default(&dispatch, || {
                    for request_index in 0..8 {
                        tracing::info!(target: "forge::ipc", session_id = "session",
                            event_name = "ipc.completed", request_id = worker_index * 8 + request_index);
                    }
                });
            });
        }
    });
    trace.flush().unwrap();
    let contents = std::fs::read_to_string(trace.session_status("session").path).unwrap();
    assert_eq!(contents.lines().count(), 32);
    assert_eq!(trace.status().rejected_record_count, 0);
}

/// Keep logical operation identity across async suspension without entering a span across await.
#[tokio::test]
async fn logging_preserves_async_span_context() {
    use tracing::{Instrument, instrument::WithSubscriber};
    let directory = tempdir().unwrap();
    let trace = TraceStore::open(directory.path()).unwrap();
    let span = tracing::dispatcher::with_default(&trace.dispatch(), || {
        tracing::info_span!(
            "provider",
            session_id = "session",
            operation_id = "operation"
        )
    });
    async {
        tokio::task::yield_now().await;
        tracing::error!(target: "forge::test", event_name = "provider.failure", error = "socket closed");
    }.instrument(span).with_subscriber(trace.dispatch()).await;
    trace.flush().unwrap();
    let contents = std::fs::read_to_string(trace.session_status("session").path).unwrap();
    let record: serde_json::Value = serde_json::from_str(contents.trim()).unwrap();
    assert_eq!(record["payload"]["operation_id"], "operation");
    assert_eq!(record["payload"]["error"], "socket closed");
}
