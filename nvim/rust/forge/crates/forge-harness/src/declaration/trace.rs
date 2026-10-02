use crate::trace::TraceStore;
use forge_buffer::input::DocumentInput;
use serde_json::{Value, json};
use std::sync::Arc;
use std::time::Instant;

#[derive(Clone)]
/// Correlates opt-in declaration jump timings with one review input.
pub(crate) struct DeclarationTrace {
    store: Arc<TraceStore>,
    session: String,
    identity: Value,
}

impl DeclarationTrace {
    /// Capture identity only when the existing Harness log is enabled.
    pub(crate) fn new(
        store: Arc<TraceStore>,
        session: &str,
        input: &DocumentInput,
    ) -> Option<Self> {
        store.status().enabled.then(|| Self {
            store,
            session: session.into(),
            identity: json!({"document":input.document,"view":input.view,
                "sequence":input.sequence,"revision":input.revision,"block":input.block,
                "row":input.position.row,"column":input.position.column}),
        })
    }

    /// Emit a start record so an unfinished lookup still identifies its active phase.
    pub(crate) fn stage(&self, phase: &str, baseline: Option<bool>) -> DeclarationStage {
        let mut payload = self.identity.clone();
        payload["phase"] = json!(phase);
        payload["status"] = json!("started");
        if let Some(baseline) = baseline {
            payload["side"] = json!(if baseline { "baseline" } else { "proposed" });
        }
        self.store
            .record_ref(&self.session, "declaration.jump", &payload);
        DeclarationStage {
            trace: self.clone(),
            payload,
            started: Instant::now(),
            completed: false,
        }
    }
}

/// Retains an interrupted phase's elapsed time on errors or cancellation.
pub(crate) struct DeclarationStage {
    trace: DeclarationTrace,
    payload: Value,
    started: Instant,
    completed: bool,
}

impl DeclarationStage {
    /// Append bounded work counts and the outcome without recording source contents.
    pub(crate) fn complete(mut self, details: Value) {
        if let Some(details) = details.as_object() {
            self.payload
                .as_object_mut()
                .unwrap()
                .extend(details.clone());
        }
        self.payload["status"] = json!("completed");
        self.completed = true;
    }
}

impl Drop for DeclarationStage {
    fn drop(&mut self) {
        if !self.completed {
            self.payload["status"] = json!("interrupted");
        }
        self.payload["duration_ms"] = json!(self.started.elapsed().as_secs_f64() * 1000.0);
        self.trace
            .store
            .record_ref(&self.trace.session, "declaration.jump", &self.payload);
    }
}
