use std::collections::HashMap;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, OnceLock, Weak};

use serde::Serialize;
use serde_json::Value;

use super::writer::{Admission, TraceError, TraceResult, TraceWriter, session_path};
use super::{LogLevel, subscriber::LoggingLayer};

static WRITERS: OnceLock<Mutex<HashMap<PathBuf, Weak<TraceWriter>>>> = OnceLock::new();

/// Describe the cached preference and the selected private trace file.
#[derive(Clone, Debug, Serialize)]
pub struct TraceStatus {
    pub level: LogLevel,
    pub path: String,
    pub configured: bool,
    /// Count rejected optional diagnostics independently of execution outcomes.
    pub rejected_record_count: u64,
}

/// Own one tracing lease while a bounded shared writer retains diagnostic files.
pub struct TraceStore {
    root: PathBuf,
    writer: Arc<TraceWriter>,
    admission: Arc<Admission>,
    dispatch: tracing::Dispatch,
    collection: Mutex<Option<Result<(), Arc<TraceError>>>>,
}

impl TraceStore {
    /// Create a trace lease, sharing ordered writes with other leases for this root.
    pub fn open(data_root: &Path) -> TraceResult<Self> {
        let root = dunce::canonicalize(data_root)
            .or_else(|_| std::path::absolute(data_root))
            .map_err(|source| TraceError::Io {
                operation: "resolve Harness trace root",
                path: data_root.to_owned(),
                source,
            })?;
        let mut writers = WRITERS
            .get_or_init(Mutex::default)
            .lock()
            .map_err(|_| TraceError::Poisoned)?;
        let writer = match writers.get(&root).and_then(Weak::upgrade) {
            Some(writer) => writer,
            None => {
                let writer = Arc::new(TraceWriter::open(&root)?);
                writers.insert(root.clone(), Arc::downgrade(&writer));
                writer
            }
        };
        writer.acquire();
        let admission = Arc::new(Admission::new());
        let dispatch = LoggingLayer::dispatch(Arc::downgrade(&writer), admission.clone());
        Ok(Self {
            root,
            writer,
            admission,
            dispatch,
            collection: Mutex::new(None),
        })
    }

    /// Get the cached trace preference without filesystem access.
    pub fn status(&self) -> TraceStatus {
        TraceStatus {
            level: self.writer.level(),
            path: crate::storage::state::directory(&self.root)
                .join("harness-trace.jsonl")
                .to_string_lossy()
                .into_owned(),
            configured: self.writer.configured(),
            rejected_record_count: self.writer.rejected_record_count(),
        }
    }

    /// Get the private detailed log owned by one Harness session.
    pub fn session_status(&self, session_id: &str) -> TraceStatus {
        let mut status = self.status();
        status.path = self.session_path(session_id).to_string_lossy().into_owned();
        status
    }

    /// Apply the editor default only before an explicit preference has been saved.
    pub fn configure_default(&self, level: LogLevel) -> TraceResult<TraceStatus> {
        self.ensure_open()?;
        self.writer.configure(level, true)?;
        Ok(self.status())
    }

    /// Persist enablement after preceding accepted writes have completed.
    pub fn configure(&self, level: LogLevel) -> TraceResult<TraceStatus> {
        self.ensure_open()?;
        self.writer.configure(level, false)?;
        Ok(self.status())
    }

    /// Clear the global stream in writer order without replacing its inode.
    pub fn clear(&self) -> TraceResult<TraceStatus> {
        self.ensure_open()?;
        self.writer.clear()?;
        Ok(self.status())
    }

    /// Clear a session's detail stream and rotations after preceding accepted writes.
    pub fn clear_session(&self, session_id: &str) -> TraceResult<TraceStatus> {
        self.ensure_open()?;
        self.writer.clear_session(session_id)?;
        Ok(self.session_status(session_id))
    }

    /// Enqueue bounded owned diagnostics without waiting for filesystem operations.
    pub fn record(&self, session_id: &str, event: &str, payload: Value) {
        self.record_ref(session_id, event, &payload)
    }

    /// Capture bounded borrowed input and receipt time without filesystem operations.
    pub fn record_ref(&self, session_id: &str, event: &str, payload: &Value) {
        if !self.is_enabled() {
            return;
        }
        tracing::dispatcher::with_default(&self.dispatch, || {
            tracing::trace!(target: "forge::record", session_id, event_name = event,
                payload = %super::policy::metadata_payload(payload));
        });
    }

    /// Get a dispatcher for scoped instrumentation without changing the process subscriber.
    pub fn dispatch(&self) -> tracing::Dispatch {
        self.dispatch.clone()
    }

    /// Finish preceding accepted writes and loss summaries before reading a snapshot.
    ///
    /// This control blocks on filesystem work; asynchronous callers use a blocking task.
    pub fn flush(&self) -> TraceResult<()> {
        self.ensure_open()?;
        self.writer.flush()
    }

    /// Drain this lease and join the filesystem owner when its final lease closes.
    ///
    /// Other open stores for the same root remain usable. Late callbacks through this
    /// store are ignored after close. Asynchronous hosts collect on a blocking task.
    pub fn close(&self) -> TraceResult<()> {
        // Concurrent close callers must wait for the same final collection;
        // observing closed admission alone is not proof that its writer joined.
        let mut collection = self.collection.lock().map_err(|_| TraceError::Poisoned)?;
        if let Some(result) = collection.as_ref() {
            return result.clone().map_err(TraceError::WriterFailed);
        }
        self.admission.close()?;
        let mut writers = WRITERS
            .get_or_init(Mutex::default)
            .lock()
            .map_err(|_| TraceError::Poisoned)?;
        // Registry access is confined to open/close. Keeping it through the final
        // join prevents a replacement writer from overtaking the old owner's drain.
        let result = if self.writer.release() {
            writers.remove(&self.root);
            self.writer.close()
        } else {
            match self.writer.flush() {
                Ok(()) => Ok(()),
                Err(error) if error.is_diagnostic() => {
                    let _ = writeln!(
                        std::io::stderr(),
                        "Forge Harness diagnostic trace write failed: {error}"
                    );
                    Ok(())
                }
                Err(error) => Err(error),
            }
        };
        let result = result.map_err(Arc::new);
        *collection = Some(result.clone());
        result.map_err(TraceError::WriterFailed)
    }

    /// Get cached enablement for inline future instrumentation.
    pub(crate) fn is_enabled(&self) -> bool {
        !self.admission.is_closed() && self.writer.level() == LogLevel::Trace
    }

    /// Route process diagnostics only when one data root owns the host's logging destination.
    pub(super) fn active_writer() -> Option<Arc<TraceWriter>> {
        let registry = WRITERS.get()?;
        let writers = match registry.lock() {
            Ok(writers) => writers,
            Err(_) => {
                eprintln!("Forge logging destination registry is poisoned");
                return None;
            }
        };
        let mut active = writers.values().filter_map(Weak::upgrade);
        let writer = active.next()?;
        if active.next().is_some() {
            eprintln!(
                "Forge logging requires an explicit destination when multiple data roots are active"
            );
            return None;
        }
        Some(writer)
    }

    /// Resolve the private path using the same traversal protection as the writer.
    pub(super) fn session_path(&self, session_id: &str) -> PathBuf {
        session_path(&self.root, session_id)
    }

    /// Reject controls after this lease has been explicitly collected.
    fn ensure_open(&self) -> TraceResult<()> {
        if self.admission.is_closed() {
            return Err(TraceError::Closed);
        }
        Ok(())
    }
}

impl Drop for TraceStore {
    /// Collect this lease when explicit host shutdown was omitted.
    fn drop(&mut self) {
        let _ = self.close();
    }
}
