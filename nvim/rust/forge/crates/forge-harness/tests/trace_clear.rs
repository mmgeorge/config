use forge_harness::trace::TraceStore;
use serde_json::json;
use std::path::PathBuf;

#[test]
fn clearing_one_session_removes_rotations_without_affecting_other_sessions() {
    let directory = tempfile::tempdir().unwrap();
    let trace = TraceStore::open(directory.path()).unwrap();
    trace.configure(true).unwrap();
    trace.record("session-a", "before", json!({}));
    trace.record("session-b", "retained", json!({}));
    let path = PathBuf::from(trace.session_status("session-a").path);
    for generation in 1..=3 {
        std::fs::write(path.with_extension(format!("jsonl.{generation}")), b"older").unwrap();
    }

    let status = trace.clear_session("session-a").unwrap();
    assert!(status.enabled);
    assert_eq!(status.path, path.to_string_lossy());
    assert_eq!(std::fs::read_to_string(&path).unwrap(), "");
    for generation in 1..=3 {
        assert!(!path.with_extension(format!("jsonl.{generation}")).exists());
    }
    assert!(
        std::fs::read_to_string(trace.session_status("session-b").path)
            .unwrap()
            .contains("retained")
    );

    trace.record("session-a", "after", json!({}));
    assert!(std::fs::read_to_string(&path).unwrap().contains("after"));
    trace.configure(false).unwrap();
    assert!(!trace.clear_session("session-a").unwrap().enabled);
    assert_eq!(std::fs::read_to_string(&path).unwrap(), "");
}
