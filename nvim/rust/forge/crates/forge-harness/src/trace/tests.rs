use super::store::TraceStore;
use crate::trace::writer::MAX_TRACE_BYTES;
use serde_json::json;
use tempfile::tempdir;

#[test]
fn detailed_logs_exclude_bodies_isolate_sessions_and_redact_credentials() {
    let directory = tempdir().unwrap();
    let trace = TraceStore::open(directory.path()).unwrap();
    trace.configure(true).unwrap();
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
    trace.configure(false).unwrap();
    trace.record("session-a", "model.sent", json!({"text":"disabled"}));
    assert_eq!(
        contents,
        std::fs::read_to_string(trace.session_status("session-a").path).unwrap()
    );
    drop(trace);
    let reopened = TraceStore::open(directory.path()).unwrap();
    assert!(!reopened.configure_default(true).unwrap().enabled);
}

#[test]
fn bounds_metadata_and_truncates_before_crossing_file_limit() {
    let directory = tempdir().unwrap();
    let trace = TraceStore::open(directory.path()).unwrap();
    trace.configure(true).unwrap();
    let path = crate::storage::state::directory(directory.path()).join("harness-trace.jsonl");
    std::fs::OpenOptions::new()
        .write(true)
        .open(&path)
        .unwrap()
        .set_len(MAX_TRACE_BYTES)
        .unwrap();
    trace.record(
            "session",
            "bounded",
            json!({ "method": "x".repeat(1_000_000), "body": "private body", "prompt": "private prompt", "authorization": "private credential", "nested": { "body": "ignored" } }),
        );
    trace.flush().unwrap();
    let contents = std::fs::read_to_string(&path).unwrap();
    assert!(contents.len() < 1024);
    let record: serde_json::Value = serde_json::from_str(contents.trim()).unwrap();
    assert_eq!(record["payload"]["method"].as_str().unwrap().len(), 256);
    assert!(record["payload"].get("body").is_none());
    assert!(record["payload"].get("prompt").is_none());
    assert!(record["payload"].get("authorization").is_none());
    assert!(record["payload"].get("nested").is_none());
}

/// Reject captured private input after disable, including a later enablement period.
#[test]
fn inflight_private_record_cannot_cross_disable_or_reenable_epoch() {
    use std::sync::Arc;
    for reenable in [false, true] {
        let directory = tempdir().unwrap();
        let trace = Arc::new(TraceStore::open(directory.path()).unwrap());
        trace.configure(true).unwrap();
        let hold = trace.hold_serialization().unwrap();
        let producer_trace = trace.clone();
        let producer = std::thread::spawn(move || {
            producer_trace.record(
                "session",
                "obsolete-private-event",
                json!({"text":"private-old-period"}),
            );
        });
        hold.wait_entered().unwrap();
        trace.configure(false).unwrap();
        if reenable {
            trace.configure(true).unwrap();
        }
        drop(hold);
        producer.join().unwrap();
        trace.flush().unwrap();
        let details =
            std::fs::read_to_string(trace.session_status("session").path).unwrap_or_default();
        assert!(
            !details.contains("private-old-period") && !details.contains("obsolete-private-event")
        );
        let metadata = std::fs::read_to_string(trace.status().path).unwrap();
        assert!(!metadata.contains("obsolete-private-event"));
        assert!(
            metadata
                .lines()
                .map(|line| serde_json::from_str::<serde_json::Value>(line).unwrap())
                .any(|record| record["event"] == "trace.loss"
                    && record["payload"]["obsolete_count"] == 1)
        );
        if reenable {
            trace.record("session", "new-period", json!({"text":"current-period"}));
            trace.flush().unwrap();
            assert!(
                std::fs::read_to_string(trace.session_status("session").path)
                    .unwrap()
                    .contains("new-period")
            );
        }
        trace.close().unwrap();
    }
}
