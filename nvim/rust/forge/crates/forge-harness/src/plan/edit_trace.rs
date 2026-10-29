use crate::trace::TraceStore;
use anyhow::Result;
use serde_json::{Value, json};
use std::sync::Arc;
use std::time::Instant;

#[derive(Clone)]
/// Correlates review-edit latency without recording plan or comment contents.
pub(crate) struct PlanEditTrace {
    store: Arc<TraceStore>,
    session: String,
    identity: Value,
    enabled: bool,
}

impl PlanEditTrace {
    /// Shares an operation identity across the UI, service, and persistence stages.
    pub(crate) fn new(store: Arc<TraceStore>, session: &str, method: &str, params: &Value) -> Self {
        let enabled = store.status().enabled;
        Self {
            store,
            session: session.into(),
            identity: json!({"method":method, "operation_id":params.get("operation_id"),
                "plan_id":params.get("plan_id"), "document":params.get("document"),
                "revision":params.get("expected_version")}),
            enabled,
        }
    }

    /// Records an unfinished stage even when its caller returns early or is cancelled.
    pub(crate) fn stage(&self, phase: &str) -> PlanEditStage {
        let mut payload = self.identity.clone();
        payload["phase"] = json!(phase);
        payload["status"] = json!("started");
        if self.enabled {
            self.store.record_ref(&self.session, "plan.edit", &payload);
        }
        PlanEditStage {
            trace: self.clone(),
            payload,
            started: Instant::now(),
            completed: false,
        }
    }
}

/// Measures enabled synchronous work without changing its success or failure result.
pub(crate) fn measure<T>(
    trace: Option<&PlanEditTrace>,
    phase: &str,
    action: impl FnOnce() -> Result<T>,
) -> Result<T> {
    let Some(trace) = trace.filter(|trace| trace.enabled) else {
        return action();
    };
    let stage = trace.stage(phase);
    let result = action();
    stage.complete(json!({"status":if result.is_ok() { "completed" } else { "failed" }}));
    result
}

/// Emits elapsed time when a synchronous or asynchronous review-edit stage ends.
pub(crate) struct PlanEditStage {
    trace: PlanEditTrace,
    payload: Value,
    started: Instant,
    completed: bool,
}

impl PlanEditStage {
    /// Adds bounded counts and marks the stage complete before its final record.
    pub(crate) fn complete(mut self, details: Value) {
        self.payload["status"] = json!("completed");
        if let Some(details) = details.as_object() {
            self.payload
                .as_object_mut()
                .unwrap()
                .extend(details.clone());
        }
        self.completed = true;
    }
}

impl Drop for PlanEditStage {
    fn drop(&mut self) {
        if !self.trace.enabled {
            return;
        }
        if !self.completed {
            self.payload["status"] = json!("interrupted");
        }
        self.payload["duration_ms"] = json!(self.started.elapsed().as_secs_f64() * 1000.0);
        self.trace
            .store
            .record_ref(&self.trace.session, "plan.edit", &self.payload);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn records_correlated_completed_failed_and_interrupted_stages() {
        let directory = tempfile::tempdir().unwrap();
        let store = Arc::new(TraceStore::open(directory.path()).unwrap());
        store.configure(true).unwrap();
        let trace = PlanEditTrace::new(
            Arc::clone(&store),
            "session",
            "session.plan.test.delete",
            &json!({"operation_id":"edit-1","plan_id":"plan","expected_version":3,
                "draft_annotation":[{"body":"must not be logged"}]}),
        );
        assert_eq!(measure(Some(&trace), "success", || Ok(42)).unwrap(), 42);
        let error = measure::<()>(Some(&trace), "failure", || {
            anyhow::bail!("original failure")
        })
        .unwrap_err();
        assert_eq!(error.to_string(), "original failure");
        drop(trace.stage("interrupted"));
        let content = std::fs::read_to_string(store.session_status("session").path).unwrap();
        assert!(!content.contains("must not be logged"));
        let records: Vec<Value> = content
            .lines()
            .map(|line| serde_json::from_str(line).unwrap())
            .collect();
        for (phase, status) in [
            ("success", "completed"),
            ("failure", "failed"),
            ("interrupted", "interrupted"),
        ] {
            let payload = &records
                .iter()
                .find(|record| {
                    record["payload"]["phase"] == phase && record["payload"]["status"] == status
                })
                .unwrap()["payload"];
            assert_eq!(payload["operation_id"], "edit-1");
            assert_eq!(payload["revision"], 3);
            assert!(payload["duration_ms"].as_f64().unwrap() >= 0.0);
        }
    }
}
