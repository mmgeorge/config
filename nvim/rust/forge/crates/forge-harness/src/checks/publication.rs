use super::CheckRun;
use crate::backend::{BackendEvent, BackendEventSink};
use anyhow::{Context, Result};
use std::{path::PathBuf, sync::Arc};
use tokio::{sync::watch, task::JoinHandle};

/// Persists the latest snapshot independently of command I/O and cancellation.
pub(super) struct CheckPublication {
    update: watch::Sender<Arc<CheckRun>>,
    worker: JoinHandle<Result<()>>,
}

impl CheckPublication {
    /// Starts persistence with the admitted run's first snapshot.
    pub(super) fn new(path: PathBuf, run: &CheckRun) -> Self {
        let (update, mut pending) = watch::channel(Arc::new(run.clone()));
        let worker = tokio::spawn(async move {
            loop {
                let run = Arc::clone(&pending.borrow_and_update());
                let path = path.clone();
                tokio::task::spawn_blocking(move || super::supervisor::write_record(&path, &run))
                    .await
                    .context("check persistence worker failed")??;
                if pending.changed().await.is_err() {
                    return Ok(());
                }
            }
        });
        Self { update, worker }
    }

    /// Replaces pending progress while a prior snapshot is being written.
    pub(super) fn update(&self, run: &CheckRun) {
        self.update.send_replace(Arc::new(run.clone()));
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
}

#[derive(Default)]
struct PendingNotice {
    latest: Option<(BackendEventSink, serde_json::Value)>,
    error: Vec<(BackendEventSink, BackendEvent)>,
}

impl CheckNotification {
    /// Starts a delivery worker with one pending progress slot.
    pub(super) fn new() -> Self {
        let pending = Arc::new(std::sync::Mutex::new(PendingNotice::default()));
        let (wake, mut receiver) = tokio::sync::mpsc::channel(1);
        let queue = Arc::clone(&pending);
        let worker = tokio::spawn(async move {
            while receiver.recv().await.is_some() {
                let (latest, errors) = {
                    let mut pending = queue.lock().expect("check notification lock poisoned");
                    (pending.latest.take(), std::mem::take(&mut pending.error))
                };
                for (sink, event) in errors {
                    if let Err(error) = sink.send_wait(event).await {
                        eprintln!("check error delivery failed: {error}");
                    }
                }
                if let Some((sink, data)) = latest {
                    let event = BackendEvent {
                        received_at_ms: None,
                        address: None,
                        turn_boundary: None,
                        kind: "document_changed".into(),
                        text: None,
                        data,
                        activity: None,
                        summary: None,
                        task_update: None,
                    };
                    // Finish an admitted multipart transfer before replacing pending progress.
                    if let Err(error) = sink.send_wait(event).await {
                        eprintln!("check presentation delivery failed: {error}");
                    }
                }
            }
        });
        Self {
            pending,
            wake,
            _worker: worker,
        }
    }

    /// Retains the latest complete view invalidation without waiting for transport capacity.
    pub(super) fn publish(&self, sink: &BackendEventSink, data: serde_json::Value) {
        self.pending
            .lock()
            .expect("check notification lock poisoned")
            .latest = Some((sink.clone(), data));
        self.signal();
    }

    /// Queues a unique error separately from replaceable progress updates.
    pub(super) fn error(&self, sink: &BackendEventSink, event: BackendEvent) {
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
            eprintln!("check notification worker closed");
        }
    }
}
