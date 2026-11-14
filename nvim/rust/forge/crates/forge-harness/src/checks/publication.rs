use super::CheckRun;
use crate::broker::publication::{SessionUpdate, SessionUpdateSink};
use anyhow::{Context, Result};
use std::{path::PathBuf, sync::Arc};
use tokio::{
    sync::{mpsc, oneshot, watch},
    task::JoinHandle,
};

/// Persists the latest snapshot independently of command I/O and cancellation.
pub(super) struct CheckPublication {
    update: watch::Sender<Arc<CheckRun>>,
    barrier: mpsc::Sender<oneshot::Sender<Result<()>>>,
    worker: JoinHandle<Result<()>>,
}

impl CheckPublication {
    /// Starts persistence with the admitted run's first snapshot.
    ///
    /// Every write replaces the whole record, so a failed progress write is superseded by the
    /// next successful one. The worker therefore keeps writing after a failure. Explicit
    /// barriers report their own write errors; `finish` reports the final snapshot's error. A
    /// transient error, such as a Windows sharing violation on one rename, cannot turn a run
    /// whose terminal record was saved into an execution error.
    pub(super) fn new(path: PathBuf, run: &CheckRun) -> Self {
        let (update, mut pending) = watch::channel(Arc::new(run.clone()));
        let (barrier, mut barriers) = mpsc::channel::<oneshot::Sender<Result<()>>>(1);
        let worker = tokio::spawn(async move {
            loop {
                let run = Arc::clone(&pending.borrow_and_update());
                let path = path.clone();
                let written = tokio::task::spawn_blocking({
                    let path = path.clone();
                    move || super::supervisor::write_record(&path, &run)
                })
                .await
                .context("check persistence worker failed")
                .and_then(|written| written);
                // `changed` reports an unseen snapshot before it reports the closed channel, so
                // the terminal snapshot sent by `finish` is always written last.
                tokio::select! {
                    biased;
                    Some(reply) = barriers.recv() => {
                        // Consume pending progress before committing so an older snapshot
                        // cannot overwrite the barrier's snapshot after acknowledgment.
                        let current = Arc::clone(&pending.borrow_and_update());
                        let path = path.clone();
                        let committed = tokio::task::spawn_blocking(move || {
                            super::supervisor::write_record(&path, &current)
                        })
                        .await
                        .context("check persistence worker failed")
                        .and_then(|written| written);
                        let _ = reply.send(committed);
                    }
                    changed = pending.changed() => {
                        if changed.is_err() {
                            return written;
                        }
                    }
                }
                if let Err(error) = written {
                    tracing::warn!(
                        target: "forge::checks",
                        event_name = "checks.progress_write_failed",
                        path = %path.display(),
                        error = %format!("{error:#}"),
                        "check progress record write failed; retrying with the next snapshot"
                    );
                }
            }
        });
        Self {
            update,
            barrier,
            worker,
        }
    }

    /// Replaces pending progress while a prior snapshot is being written.
    pub(super) fn update(&self, run: &CheckRun) {
        self.update.send_replace(Arc::new(run.clone()));
    }

    /// Commit this snapshot before allowing an operation that can mutate source.
    pub(super) async fn persist(&self, run: &CheckRun) -> Result<()> {
        self.update(run);
        let (reply, received) = oneshot::channel();
        self.barrier
            .send(reply)
            .await
            .context("check persistence worker is unavailable")?;
        received
            .await
            .context("check persistence barrier was not acknowledged")?
    }

    /// Flushes the terminal snapshot and returns any persistence failure.
    pub(super) async fn finish(self, run: &CheckRun) -> Result<()> {
        self.update(run);
        drop(self.update);
        self.worker.await.context("check persistence task failed")?
    }
}

/// Coalesces view invalidations without retaining a completed run's transport.
pub(super) struct CheckNotification {
    pending: Arc<std::sync::Mutex<PendingNotice>>,
    wake: tokio::sync::mpsc::Sender<()>,
    _worker: JoinHandle<()>,
    trace: Arc<crate::trace::TraceStore>,
    session_id: String,
}

#[derive(Default)]
struct PendingNotice {
    latest: Option<(SessionUpdateSink, crate::timeline::stream::TimelinePatch)>,
    error: Vec<(SessionUpdateSink, SessionUpdate)>,
}

impl CheckNotification {
    /// Starts a delivery worker with one pending progress slot.
    pub(super) fn new(trace: Arc<crate::trace::TraceStore>, session_id: String) -> Self {
        let pending = Arc::new(std::sync::Mutex::new(PendingNotice::default()));
        let (wake, mut receiver) = tokio::sync::mpsc::channel(1);
        let queue = Arc::clone(&pending);
        let worker_trace = trace.clone();
        let worker_session = session_id.clone();
        let worker = tokio::spawn(async move {
            while receiver.recv().await.is_some() {
                let (latest, errors) = {
                    let mut pending = queue.lock().expect("check notification lock poisoned");
                    (pending.latest.take(), std::mem::take(&mut pending.error))
                };
                for (sink, event) in errors {
                    if let Err(error) = sink.send_wait(event).await {
                        worker_trace.record(
                            &worker_session,
                            "checks.error_delivery_failed",
                            serde_json::json!({"error":error.to_string()}),
                        );
                    }
                }
                if let Some((sink, patch)) = latest {
                    if let Err(error) = sink.send_wait(SessionUpdate::ChecksChanged).await {
                        worker_trace.record(
                            &worker_session,
                            "checks.catalog_delivery_failed",
                            serde_json::json!({"error":error.to_string()}),
                        );
                    }
                    let event = SessionUpdate::DocumentChanged { patch };
                    // Finish an admitted multipart transfer before replacing pending progress.
                    if let Err(error) = sink.send_wait(event).await {
                        worker_trace.record(
                            &worker_session,
                            "checks.presentation_delivery_failed",
                            serde_json::json!({"error":error.to_string()}),
                        );
                    }
                }
            }
        });
        Self {
            pending,
            wake,
            _worker: worker,
            trace,
            session_id,
        }
    }

    /// Retains the latest complete view invalidation without waiting for transport capacity.
    pub(super) fn publish(
        &self,
        sink: &SessionUpdateSink,
        patch: crate::timeline::stream::TimelinePatch,
    ) {
        self.pending
            .lock()
            .expect("check notification lock poisoned")
            .latest = Some((sink.clone(), patch));
        self.signal();
    }

    /// Queues a unique error separately from replaceable progress updates.
    pub(super) fn error(&self, sink: &SessionUpdateSink, event: SessionUpdate) {
        self.pending
            .lock()
            .expect("check notification lock poisoned")
            .error
            .push((sink.clone(), event));
        self.signal();
    }

    fn signal(&self) {
        if matches!(
            self.wake.try_send(()),
            Err(tokio::sync::mpsc::error::TrySendError::Closed(()))
        ) {
            self.trace.record(
                &self.session_id,
                "checks.notification_worker_closed",
                serde_json::json!({}),
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Build the ordinary serialized history contract without starting a check process.
    fn run() -> CheckRun {
        serde_json::from_value(serde_json::json!({
            "id":"run", "owner":{"kind":"session","session_id":"session"},
            "revision":0, "shell":"system", "executable":null, "workspace":".",
            "environment_digest":"test", "started_at_ms":1, "completed_at_ms":null,
            "state":"running", "progress":crate::plan::execution::SemanticProgress::default(),
            "workspace_identity":null, "workspace_digest":{}, "commands":[]
        }))
        .unwrap()
    }

    /// Acknowledgment means the requested state is on disk and older progress cannot replace it.
    #[tokio::test]
    async fn barrier_commits_latest_state_before_acknowledgment() -> Result<()> {
        let root = tempfile::tempdir()?;
        let path = root.path().join("run/record.json");
        let mut run = run();
        let publication = CheckPublication::new(path.clone(), &run);
        run.revision = 1;
        publication.update(&run);
        run.revision = 2;
        publication.persist(&run).await?;
        let recorded: serde_json::Value = serde_json::from_slice(&std::fs::read(&path)?)?;
        assert_eq!(recorded["run"]["revision"], 2);
        run.revision = 3;
        publication.finish(&run).await?;
        let recorded: serde_json::Value = serde_json::from_slice(&std::fs::read(&path)?)?;
        assert_eq!(recorded["run"]["revision"], 3);
        Ok(())
    }

    /// Failed barriers retain the I/O cause; repairing storage allows a later explicit commit.
    #[tokio::test]
    async fn barrier_reports_storage_failure_and_worker_can_recover() -> Result<()> {
        let root = tempfile::tempdir()?;
        let parent = root.path().join("blocked");
        std::fs::write(&parent, "not a directory")?;
        let path = parent.join("record.json");
        let run = run();
        let publication = CheckPublication::new(path.clone(), &run);
        let error = publication.persist(&run).await.unwrap_err();
        assert!(error.chain().any(|cause| cause.is::<std::io::Error>()));
        std::fs::remove_file(&parent)?;
        publication.persist(&run).await?;
        assert!(path.is_file());
        publication.finish(&run).await?;
        Ok(())
    }
}
