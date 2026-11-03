use super::store::TraceStore;
use crate::trace::policy::metadata_payload;
use crate::trace::writer::MAX_TRACE_BYTES;
use serde_json::json;
use tempfile::tempdir;

#[test]
fn detailed_logs_preserve_bodies_isolate_sessions_and_redact_credentials() {
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
    assert!(contents.contains("full prompt"));
    assert!(contents.contains("[redacted]"));
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
fn retry_error_larger_than_rotation_budget_is_retained_in_full() {
    let directory = tempdir().unwrap();
    let trace = TraceStore::open(directory.path()).unwrap();
    trace.configure(true).unwrap();
    let message = "network detail ".repeat(MAX_TRACE_BYTES as usize / 15 + 1);
    trace.record(
        "session",
        "provider.retry",
        json!({"status":"failed", "detail":{"error":message}}),
    );
    trace.flush().unwrap();
    let contents = std::fs::read_to_string(trace.session_path("session")).unwrap();
    let record: serde_json::Value = serde_json::from_str(contents.trim()).unwrap();
    assert_eq!(
        record["payload"]["detail"]["error"].as_str(),
        Some(message.as_str())
    );
    trace.record("session", "provider.retry", json!({"status":"exhausted"}));
    trace.flush().unwrap();
    let rotated = trace.session_path("session").with_extension("jsonl.1");
    assert_eq!(std::fs::read_to_string(rotated).unwrap(), contents);
}

#[test]
fn rejection_metadata_preserves_execution_identity_without_message_content() {
    let metadata = metadata_payload(&json!({
        "exchange_id":"exchange", "thread_id":"thread", "turn_id":"turn",
        "event_type":"assistant_message", "code":"execution_unknown_or_settled",
        "text":"private response"
    }));
    assert_eq!(metadata["exchange_id"], "exchange");
    assert_eq!(metadata["thread_id"], "thread");
    assert_eq!(metadata["turn_id"], "turn");
    assert_eq!(metadata["code"], "execution_unknown_or_settled");
    assert!(metadata.get("text").is_none());
}

#[test]
fn bounds_metadata_and_truncates_before_crossing_file_limit() {
    let directory = tempdir().unwrap();
    let trace = TraceStore::open(directory.path()).unwrap();
    trace.configure(true).unwrap();
    let path = directory.path().join("harness-trace.jsonl");
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

#[test]
fn appends_session_identified_records_only_when_enabled() {
    let directory = tempdir().unwrap();
    let trace = TraceStore::open(directory.path()).unwrap();
    trace.record("session-a", "ignored", json!({ "value": 0 }));
    assert!(!directory.path().join("harness-trace.jsonl").exists());

    trace.configure(true).unwrap();
    trace.record("session-a", "recorded", json!({ "secret": "retained" }));
    trace.flush().unwrap();
    let contents = std::fs::read_to_string(directory.path().join("harness-trace.jsonl")).unwrap();
    assert!(contents.contains("session-a"));
    assert!(!contents.contains("retained"));
}

#[test]
fn persists_enablement_and_clears_the_append_only_file() {
    let directory = tempdir().unwrap();
    let trace = TraceStore::open(directory.path()).unwrap();
    trace.configure(true).unwrap();
    trace.record("session-a", "first", json!({}));
    drop(trace);

    let trace = TraceStore::open(directory.path()).unwrap();
    assert!(trace.status().enabled);
    trace.clear().unwrap();
    trace.flush().unwrap();
    let contents = std::fs::read_to_string(directory.path().join("harness-trace.jsonl")).unwrap();
    assert!(contents.contains("trace.cleared"));
    assert!(!contents.contains("\"event\":\"first\""));
}

#[test]
fn independent_handles_append_after_another_owner_clears() {
    let directory = tempdir().unwrap();
    let first = TraceStore::open(directory.path()).unwrap();
    first.configure(true).unwrap();
    let second = TraceStore::open(directory.path()).unwrap();
    first.record("first", "before", json!({}));
    second.clear().unwrap();
    first.record("first", "after_first", json!({}));
    second.record("second", "after_second", json!({}));
    second.flush().unwrap();
    let contents = std::fs::read_to_string(directory.path().join("harness-trace.jsonl")).unwrap();
    let event = contents
        .lines()
        .map(|line| {
            serde_json::from_str::<serde_json::Value>(line).unwrap()["event"]
                .as_str()
                .unwrap()
                .to_owned()
        })
        .collect::<Vec<_>>();
    assert_eq!(event, vec!["trace.cleared", "after_first", "after_second"]);
}

/// Keep record producers independent of a held filesystem owner and bound record count.
#[test]
fn held_writer_bounds_records_without_blocking_producers_and_reports_loss() {
    use crate::trace::writer::{MAX_PENDING_BYTES, MAX_PENDING_RECORDS};
    use std::sync::{Arc, mpsc};
    use std::time::Duration;
    let directory = tempdir().unwrap();
    let trace = Arc::new(TraceStore::open(directory.path()).unwrap());
    trace.configure(true).unwrap();
    let hold = trace.hold_writer().unwrap();
    let producer_trace = trace.clone();
    let (finished, received) = mpsc::channel();
    let producer = std::thread::spawn(move || {
        for count in 0..MAX_PENDING_RECORDS + 17 {
            producer_trace.record("session", "queued", json!({"count":count}));
        }
        finished.send(()).unwrap();
    });
    let progressed_while_held = received.recv_timeout(Duration::from_secs(5));
    let (record_count, bytes) = trace.pending();
    drop(hold);
    producer.join().unwrap();
    progressed_while_held.expect("record production must finish before the writer is released");
    assert_eq!(record_count, MAX_PENDING_RECORDS);
    assert!(bytes <= MAX_PENDING_BYTES);
    trace.flush().unwrap();
    assert_eq!(trace.pending(), (0, 0));
    let contents = std::fs::read_to_string(trace.status().path).unwrap();
    let records: Vec<serde_json::Value> = contents
        .lines()
        .map(|line| serde_json::from_str(line).unwrap())
        .collect();
    assert_eq!(
        records
            .iter()
            .filter(|record| record["event"] == "queued")
            .count(),
        MAX_PENDING_RECORDS
    );
    let loss = records
        .iter()
        .find(|record| record["event"] == "trace.loss")
        .unwrap();
    assert_eq!(loss["payload"]["queue_full_count"], 17);
    assert_eq!(loss["payload"]["count"], 17);
    assert_eq!(
        crate::trace::inspection_record(loss)["payload"]["queue_full_count"],
        17
    );
    trace.close().unwrap();
}

/// Bound retained allocation as well as record count and report oversized diagnostics.
#[test]
fn held_writer_bounds_bytes_and_reports_oversize_before_any_following_record() {
    use crate::trace::writer::{MAX_PENDING_BYTES, MAX_PENDING_RECORDS, MAX_RECORD_BYTES};
    let directory = tempdir().unwrap();
    let trace = std::sync::Arc::new(TraceStore::open(directory.path()).unwrap());
    trace.configure(true).unwrap();
    let hold = trace.hold_writer().unwrap();
    trace.record(
        "session",
        "too-large",
        json!({"text":"x".repeat(MAX_RECORD_BYTES + 1)}),
    );
    let payload = json!({"text":"y".repeat(24 * 1024 * 1024)});
    for _ in 0..4 {
        trace.record_ref("session", "large", &payload);
    }
    let (record_count, bytes) = trace.pending();
    assert!(record_count < MAX_PENDING_RECORDS);
    assert!(bytes <= MAX_PENDING_BYTES);
    assert!(bytes >= 48 * 1024 * 1024);
    drop(hold);
    trace.flush().unwrap();
    let contents = std::fs::read_to_string(trace.status().path).unwrap();
    let records: Vec<serde_json::Value> = contents
        .lines()
        .map(|line| serde_json::from_str(line).unwrap())
        .collect();
    let loss = records
        .iter()
        .find(|record| record["event"] == "trace.loss")
        .unwrap();
    assert_eq!(loss["payload"]["oversize_count"], 1);
    assert!(loss["payload"]["queue_full_count"].as_u64().unwrap() >= 1);
    assert!(records.iter().all(|record| record["event"] != "too-large"));
    trace.close().unwrap();
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

/// Surface original filesystem causes through a barrier without affecting event execution.
#[test]
fn explicit_flush_preserves_typed_writer_io_failure() {
    let directory = tempdir().unwrap();
    let trace = TraceStore::open(directory.path()).unwrap();
    trace.configure(true).unwrap();
    std::fs::rename(
        directory.path().join("logs"),
        directory.path().join("original-logs"),
    )
    .unwrap();
    std::fs::write(directory.path().join("logs"), b"blocked directory").unwrap();
    trace.record("session", "accepted-diagnostic", json!({}));
    let failure = trace.flush().unwrap_err();
    assert!(matches!(failure, super::TraceError::WriterFailed(_)));
    assert!(std::error::Error::source(&failure).is_some());
    assert!(
        std::fs::read_to_string(trace.status().path)
            .unwrap()
            .contains("accepted-diagnostic")
    );
    trace
        .close()
        .expect("a diagnostic I/O failure does not make a joined writer uncollected");
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
                    .contains("current-period")
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
    assert!(details.contains("accepted-before-close") && details.contains("still-active"));
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
