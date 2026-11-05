use super::store::TraceStore;
use crate::trace::policy::metadata_payload;
use crate::trace::writer::MAX_TRACE_BYTES;
use serde_json::json;
use tempfile::tempdir;

/// Public session inspection must preserve owner correlation without exposing request or answer bodies.
#[test]
fn inspection_preserves_command_wait_and_publication_correlation() {
    let record = crate::trace::inspection_record(&json!({
        "session_id":"session","event":"exchange.question_wait.retirement_failed","timestamp_ms":1,
        "payload":{"command_id":"command","sequence":2,"update_step":3,"work_id":"work","wait_id":"wait",
            "question_id":"question","question_revision":4,"question_count":1,"input_id":"input","input_revision":5,
            "publication_sequence":6,"delivery":"pending","observed":false,"entry_accepted":false,"signal":"publication_completed","error_code":"question_wait_retirement_failed","outcome":"abandoned",
            "text":"private answer","reason":"private storage detail","question_ids":["private list"],"params":{"body":"private request"}}
    }));
    let payload = &record["payload"];
    assert_eq!(payload["command_id"], "command");
    assert_eq!(payload["sequence"], 2);
    assert_eq!(payload["update_step"], 3);
    assert_eq!(payload["wait_id"], "wait");
    assert_eq!(payload["question_id"], "question");
    assert_eq!(payload["publication_sequence"], 6);
    assert_eq!(payload["error_code"], "question_wait_retirement_failed");
    assert_eq!(payload["observed"], false);
    assert_eq!(payload["entry_accepted"], false);
    assert_eq!(payload["signal"], "publication_completed");
    assert!(payload.get("text").is_none());
    assert!(payload.get("reason").is_none());
    assert!(payload.get("question_ids").is_none());
    assert!(payload.get("params").is_none());
}

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

/// Preserve correlation while excluding arbitrarily large native error details.
#[test]
fn retry_diagnostic_excludes_raw_error_and_retains_typed_outcome() {
    let directory = tempdir().unwrap();
    let trace = TraceStore::open(directory.path()).unwrap();
    trace.configure(true).unwrap();
    trace.record(
        "session",
        "provider.retry",
        json!({"status":"failed", "operation_id":"operation",
        "detail":{"error":"network detail ".repeat(100_000)}}),
    );
    trace.flush().unwrap();
    let contents = std::fs::read_to_string(trace.session_path("session")).unwrap();
    let record: serde_json::Value = serde_json::from_str(contents.trim()).unwrap();
    assert_eq!(record["payload"]["operation_id"], "operation");
    assert_eq!(record["payload"]["status"], "failed");
    assert!(record["payload"].get("detail").is_none());
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
    use crate::trace::writer::MAX_PENDING_RECORDS;
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
    assert_eq!(trace.status().rejected_record_count, 17);
    assert!(bytes > 0);
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

/// Return saturation without blocking and keep the rejected count after loss publication.
#[test]
fn held_writer_exposes_saturation_without_losing_accepted_correlations() {
    use crate::trace::writer::{MAX_PENDING_RECORDS, TraceAdmission};
    let directory = tempdir().unwrap();
    let trace = std::sync::Arc::new(TraceStore::open(directory.path()).unwrap());
    trace.configure(true).unwrap();
    let hold = trace.hold_writer().unwrap();
    for sequence in 0..MAX_PENDING_RECORDS {
        assert_eq!(
            trace.record(
                "session",
                "event.consumed",
                json!({"consumed_sequence": sequence,
            "operation_id":"operation", "invocation_id":"invocation", "attempt_id":"attempt"})
            ),
            TraceAdmission::Queued
        );
    }
    assert_eq!(
        trace.record("session", "rejected", json!({})),
        TraceAdmission::Saturated
    );
    drop(hold);
    trace.flush().unwrap();
    assert_eq!(trace.status().rejected_record_count, 1);
    let contents = std::fs::read_to_string(trace.session_path("session")).unwrap();
    let records: Vec<serde_json::Value> = contents
        .lines()
        .map(|line| serde_json::from_str(line).unwrap())
        .collect();
    assert_eq!(records.len(), MAX_PENDING_RECORDS);
    assert_eq!(
        records.last().unwrap()["payload"]["consumed_sequence"],
        MAX_PENDING_RECORDS - 1
    );
    assert_eq!(records.last().unwrap()["payload"]["attempt_id"], "attempt");
}

/// Retain ten diagnostic generations and clear every retained segment explicitly.
#[test]
fn session_rotation_retains_ten_generations_and_clear_removes_them() {
    use crate::trace::writer::ROTATED_TRACE_FILES;
    let directory = tempdir().unwrap();
    let trace = TraceStore::open(directory.path()).unwrap();
    trace.configure(true).unwrap();
    let path = trace.session_path("session");
    for sequence in 0..ROTATED_TRACE_FILES + 2 {
        trace.record("session", "segment", json!({"sequence": sequence}));
        trace.flush().unwrap();
        if sequence < ROTATED_TRACE_FILES + 1 {
            std::fs::OpenOptions::new()
                .write(true)
                .open(&path)
                .unwrap()
                .set_len(MAX_TRACE_BYTES)
                .unwrap();
        }
    }
    assert!(path.with_extension("jsonl.10").exists());
    assert!(!path.with_extension("jsonl.11").exists());
    trace.clear_session("session").unwrap();
    assert_eq!(path.metadata().unwrap().len(), 0);
    for generation in 1..=ROTATED_TRACE_FILES {
        assert!(!path.with_extension(format!("jsonl.{generation}")).exists());
    }
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

/// Preserve identity fields even after many disallowed keys and without truncation.
#[test]
fn metadata_preserves_complete_owner_identity_after_disallowed_fields() {
    let mut payload = serde_json::Map::new();
    for index in 0..64 {
        payload.insert(format!("a_private_{index}"), json!("private"));
    }
    let operation_id = "operation".repeat(100);
    payload.insert("operation_id".into(), json!(operation_id));
    payload.insert("consumed_sequence".into(), json!(71));
    payload.insert("failure".into(), json!("raw provider diagnostic"));
    let metadata = metadata_payload(&serde_json::Value::Object(payload));
    assert_eq!(metadata["operation_id"], operation_id);
    assert_eq!(metadata["consumed_sequence"], 71);
    assert_eq!(metadata.as_object().unwrap().len(), 2);
}
