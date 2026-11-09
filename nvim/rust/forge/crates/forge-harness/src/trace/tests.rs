use super::store::TraceStore;
use serde_json::json;
use tempfile::tempdir;

/// Retain normalized collection diagnostics through storage and inspection without native text.
#[test]
fn terminal_collection_diagnostics_survive_trace_policy() {
    let directory = tempdir().unwrap();
    let trace = TraceStore::open(directory.path()).unwrap();
    trace.configure(true).unwrap();
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
