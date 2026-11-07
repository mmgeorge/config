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

/// Keep open reader handles on the old segment while the writer creates its successor.
#[test]
fn detailed_log_rotates_without_silently_erasing_the_previous_segment() {
    use std::io::{BufRead, BufReader};
    let directory = tempdir().unwrap();
    let trace = TraceStore::open(directory.path()).unwrap();
    trace.configure(true).unwrap();
    trace.record("session", "first", json!({}));
    trace.flush().unwrap();
    let path = trace.session_path("session");
    std::fs::OpenOptions::new()
        .write(true)
        .open(&path)
        .unwrap()
        .set_len(MAX_TRACE_BYTES)
        .unwrap();
    let reader = std::fs::File::open(&path).unwrap();
    trace.record("session", "second", json!({}));
    trace.flush().unwrap();
    assert_eq!(reader.metadata().unwrap().len(), MAX_TRACE_BYTES);
    let mut first = String::new();
    BufReader::new(reader).read_line(&mut first).unwrap();
    assert!(first.contains("first"));
    assert!(std::fs::read_to_string(&path).unwrap().contains("second"));
    assert!(path.with_extension("jsonl.1").exists());
    assert!(path.metadata().unwrap().len() < 1024);
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

/// Keep another lease writable after one host explicitly collects its own callbacks.
#[test]
fn closing_one_shared_lease_preserves_other_writer_and_last_close_collects() {
    let directory = tempdir().unwrap();
    let first = TraceStore::open(directory.path()).unwrap();
    first.configure(true).unwrap();
    let second = TraceStore::open(directory.path()).unwrap();
    first.record("session", "before-close", json!({}));
    first.close().unwrap();
    first.record("session", "late-closed-callback", json!({}));
    second.record("session", "after-other-close", json!({}));
    second.close().unwrap();
    let contents = std::fs::read_to_string(second.status().path).unwrap();
    assert!(contents.contains("before-close") && contents.contains("after-other-close"));
    assert!(!contents.contains("late-closed-callback"));
    let replacement = TraceStore::open(directory.path()).unwrap();
    replacement.record("session", "replacement", json!({}));
    replacement.close().unwrap();
    assert!(
        std::fs::read_to_string(replacement.status().path)
            .unwrap()
            .contains("replacement")
    );
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

/// Preserve accepted FIFO records while rejecting a closing lease's held producer.
#[test]
fn closed_shared_lease_rejects_inflight_record_without_stopping_live_lease() {
    use std::sync::Arc;
    let directory = tempdir().unwrap();
    let first = Arc::new(TraceStore::open(directory.path()).unwrap());
    first.configure(true).unwrap();
    let second = TraceStore::open(directory.path()).unwrap();
    first.record(
        "session",
        "accepted-before-close",
        json!({"text":"retained"}),
    );
    let hold = first.hold_serialization().unwrap();
    let producer_trace = first.clone();
    let producer = std::thread::spawn(move || {
        producer_trace.record(
            "session",
            "obsolete-after-close",
            json!({"text":"private-late-input"}),
        );
    });
    hold.wait_entered().unwrap();
    first.close().unwrap();
    second.record("session", "live-lease", json!({"text":"still-active"}));
    drop(hold);
    producer.join().unwrap();
    second.flush().unwrap();
    let details = std::fs::read_to_string(second.session_status("session").path).unwrap();
    assert!(details.contains("accepted-before-close") && details.contains("live-lease"));
    assert!(!details.contains("obsolete-after-close") && !details.contains("private-late-input"));
    let metadata = std::fs::read_to_string(second.status().path).unwrap();
    assert!(
        metadata
            .lines()
            .map(|line| serde_json::from_str::<serde_json::Value>(line).unwrap())
            .any(|record| record["event"] == "trace.loss"
                && record["payload"]["obsolete_count"] == 1)
    );
    second.close().unwrap();
}
