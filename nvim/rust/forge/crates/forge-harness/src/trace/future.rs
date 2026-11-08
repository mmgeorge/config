use std::{
    future::Future,
    time::{Duration, Instant},
};

use anyhow::Result;
use serde_json::json;

use super::TraceStore;

struct FutureTrace<'trace> {
    trace: &'trace TraceStore,
    session_id: &'trace str,
    operation_id: &'trace str,
    phase: &'trace str,
    started: Instant,
    busy: Duration,
    poll_count: u64,
    status: &'static str,
}

impl TraceStore {
    /// Measures elapsed and poll time without keeping a thread-local span entered across an await.
    pub(crate) async fn instrument<Output>(
        &self,
        session_id: &str,
        operation_id: &str,
        phase: &str,
        future: impl Future<Output = Result<Output>>,
    ) -> Result<Output> {
        if !self.is_enabled() {
            return future.await;
        }
        self.record(
            session_id,
            "future.started",
            json!({"operation_id":operation_id,"phase":phase}),
        );
        let mut observation = FutureTrace {
            trace: self,
            session_id,
            operation_id,
            phase,
            started: Instant::now(),
            busy: Duration::ZERO,
            poll_count: 0,
            status: "cancelled",
        };
        let mut future = std::pin::pin!(future);
        let result = std::future::poll_fn(|context| {
            let started = Instant::now();
            let polled = future.as_mut().poll(context);
            observation.busy += started.elapsed();
            observation.poll_count += 1;
            polled
        })
        .await;
        observation.status = if result.is_ok() {
            "completed"
        } else {
            "failed"
        };
        result
    }
}

impl Drop for FutureTrace<'_> {
    fn drop(&mut self) {
        self.trace.record(
            self.session_id,
            "future.finished",
            json!({
                "operation_id":self.operation_id,"phase":self.phase,
                "status":if std::thread::panicking() { "panicked" } else { self.status },
                "elapsed_ms":self.started.elapsed().as_secs_f64() * 1000.0,
                "busy_ms":self.busy.as_secs_f64() * 1000.0,"poll_count":self.poll_count,
            }),
        );
    }
}
