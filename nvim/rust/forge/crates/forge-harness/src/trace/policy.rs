use serde_json::{Value, json};

/// Bound public metadata text without splitting a UTF-8 code point.
pub(super) fn bounded_text(text: &str) -> &str {
    let mut end = text.len().min(256);
    while !text.is_char_boundary(end) {
        end -= 1;
    }
    &text[..end]
}

/// Retain only bounded allowlisted scalar diagnostic metadata.
pub(super) fn metadata_payload(payload: &Value) -> Value {
    let Some(object) = payload.as_object() else {
        return Value::Null;
    };
    let mut metadata = serde_json::Map::new();
    for (key, value) in object.iter() {
        if !diagnostic_field(key) {
            continue;
        }
        let value = match value {
            Value::String(text) => Value::String(if key.ends_with("_id") {
                text.clone()
            } else {
                bounded_text(text).to_owned()
            }),
            Value::Bool(_) | Value::Number(_) | Value::Null => value.clone(),
            _ => continue,
        };
        metadata.insert(key.clone(), value);
    }
    Value::Object(metadata)
}

/// Identify generic diagnostic metadata permitted in public inspection.
fn diagnostic_field(key: &str) -> bool {
    matches!(
        key,
        "method"
            | "command_id"
            | "sequence"
            | "update_step"
            | "work_id"
            | "wait_id"
            | "question_id"
            | "question_revision"
            | "question_count"
            | "input_id"
            | "input_revision"
            | "publication_sequence"
            | "delivery"
            | "observed"
            | "entry_accepted"
            | "error_code"
            | "outcome"
            | "operation"
            | "operation_id"
            | "message_id"
            | "submission_id"
            | "invocation_id"
            | "attempt_id"
            | "event_sequence"
            | "consumed_sequence"
            | "response_id"
            | "native_sequence"
            | "binding_id"
            | "kind"
            | "event"
            | "signal"
            | "busy_ms"
            | "poll_count"
            | "from_state"
            | "to_state"
            | "request_id"
            | "session_id"
            | "agent_id"
            | "exchange_id"
            | "thread_id"
            | "turn_id"
            | "tool_id"
            | "type"
            | "success"
            | "provider"
            | "status"
            | "code"
            | "enabled"
            | "cached"
            | "cancelled"
            | "generation"
            | "revision"
            | "phase"
            | "elapsed_ms"
            | "duration_ms"
            | "received_at_ms"
            | "count"
            | "retry_attempt"
            | "max_retries"
            | "rows"
            | "bytes"
            | "source_bytes"
            | "result_bytes"
            | "queue_bytes"
            | "queue_count"
            | "active_jobs"
            | "retained_bytes"
            | "oversize_count"
            | "queue_full_count"
            | "encoding_error_count"
            | "obsolete_count"
            | "contention_count"
    )
}

/// Filter a retained record to public diagnostic metadata.
pub(crate) fn inspection_record(record: &Value) -> Value {
    let mut payload = inspection_metadata(&record["payload"]);
    for (name, pointer) in [("data", "/payload/data"), ("params", "/payload/params")] {
        if let Some(value) = record.pointer(pointer) {
            let metadata = inspection_metadata(value);
            if !metadata.is_empty() {
                payload.insert(name.into(), Value::Object(metadata));
            }
        }
    }
    json!({
        "timestamp_ms":record["timestamp_ms"],
        "session_id":record["session_id"],
        "event":record["event"],
        "payload":payload,
    })
}

/// Exclude private bodies and nested values from an inspection payload.
fn inspection_metadata(value: &Value) -> serde_json::Map<String, Value> {
    value
        .as_object()
        .into_iter()
        .flatten()
        .filter(|(key, value)| diagnostic_field(key) && !value.is_object() && !value.is_array())
        .map(|(key, value)| (key.clone(), value.clone()))
        .collect()
}
