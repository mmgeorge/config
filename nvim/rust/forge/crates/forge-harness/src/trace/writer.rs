use std::fs::{self, File, OpenOptions};
use std::io::{self, ErrorKind, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, mpsc};
use std::thread::JoinHandle;
use std::time::{SystemTime, UNIX_EPOCH};

use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use super::policy::{bounded_text, metadata_payload};

pub(super) const MAX_TRACE_BYTES: u64 = 100 * 1024 * 1024;
pub(super) const MAX_PENDING_RECORDS: usize = 4096;
pub(super) const ROTATED_TRACE_FILES: usize = 10;

/// Describe optional diagnostic admission independently of command execution.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TraceAdmission {
    /// The owned writer accepted the diagnostic record.
    Queued,
    /// Optional diagnostics were disabled.
    Disabled,
    /// The producing lease or writer was closed.
    Closed,
    /// The approved record queue was full.
    Saturated,
    /// Another producer held this lease's admission boundary.
    Contended,
    /// The logging period changed before admission.
    Obsolete,
    /// Encoding or allocation failed before admission.
    EncodingFailed,
}

/// Preserve diagnostic writer failures independently of execution outcomes.
#[derive(Debug)]
pub enum TraceError {
    /// The disposable trace directory could not retain its state lease.
    State(anyhow::Error),
    /// An owned trace file or writer thread could not be accessed.
    Io {
        operation: &'static str,
        path: PathBuf,
        source: io::Error,
    },
    /// A retained diagnostic record could not be encoded or decoded.
    Encoding(serde_json::Error),
    /// The writer was closed before the requested control completed.
    Closed,
    /// An owned writer synchronization primitive was poisoned.
    Poisoned,
    /// The owned writer thread panicked before collection.
    Panicked,
    /// A preceding write failed; the original typed cause remains available.
    WriterFailed(Arc<TraceError>),
}

impl TraceError {
    /// Distinguish lost diagnostics from a writer that could not be collected.
    pub(super) fn is_diagnostic(&self) -> bool {
        match self {
            Self::Io { .. } | Self::Encoding(_) | Self::State(_) => true,
            Self::WriterFailed(source) => source.is_diagnostic(),
            Self::Closed | Self::Poisoned | Self::Panicked => false,
        }
    }
}

impl std::fmt::Display for TraceError {
    /// Render diagnostic failures at the caller's reporting boundary.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::State(source) => write!(formatter, "acquire Harness trace state: {source:#}"),
            Self::Io {
                operation,
                path,
                source,
            } => write!(formatter, "{operation} {}: {source}", path.display()),
            Self::Encoding(source) => write!(formatter, "encode Harness trace: {source}"),
            Self::Closed => formatter.write_str("Harness trace writer is closed"),
            Self::Poisoned => {
                formatter.write_str("Harness trace writer ownership lock is poisoned")
            }
            Self::Panicked => formatter.write_str("Harness trace writer panicked"),
            Self::WriterFailed(source) => {
                write!(formatter, "Harness trace writer failed: {source}")
            }
        }
    }
}

impl std::error::Error for TraceError {
    /// Retain the typed filesystem, encoding, or prior writer cause.
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::State(source) => Some(source.as_ref()),
            Self::Io { source, .. } => Some(source),
            Self::Encoding(source) => Some(source),
            Self::WriterFailed(source) => Some(source.as_ref()),
            Self::Closed | Self::Poisoned | Self::Panicked => None,
        }
    }
}

/// Retain typed diagnostic and collection failures at explicit trace controls.
pub type TraceResult<T> = Result<T, TraceError>;
type Reply = mpsc::Sender<TraceResult<()>>;

enum Command {
    Record(EncodedRecord),
    Configure {
        enabled: bool,
        default_only: bool,
        reply: Reply,
    },
    Clear(Reply),
    ClearSession {
        session_id: String,
        reply: Reply,
    },
    Flush(Reply),
    Close(Reply),
}

/// Linearize one store's accepted records with its explicit close operation.
pub(super) struct Admission {
    closed: AtomicBool,
    commit: Mutex<()>,
    #[cfg(test)]
    serialization: Mutex<Option<SerializationBarrier>>,
}

impl Admission {
    /// Create open admission without retaining any producer or payload registry.
    pub(super) fn new() -> Self {
        Self {
            closed: AtomicBool::new(false),
            commit: Mutex::new(()),
            #[cfg(test)]
            serialization: Mutex::new(None),
        }
    }

    /// Get whether this lease has closed record admission.
    pub(super) fn is_closed(&self) -> bool {
        self.closed.load(Ordering::Acquire)
    }

    /// Close admission after all previously committed enqueue operations.
    pub(super) fn close(&self) -> TraceResult<()> {
        let _commit = self.commit.lock().map_err(|_| TraceError::Poisoned)?;
        self.closed.store(true, Ordering::Release);
        Ok(())
    }

    /// Commit a record without waiting for another producer or a closing lease.
    fn enqueue(&self, sender: &mpsc::SyncSender<Command>, record: EncodedRecord) -> Commit {
        let Ok(_commit) = self.commit.try_lock() else {
            return Commit::Contended;
        };
        if self.is_closed() {
            return Commit::Obsolete;
        }
        match sender.try_send(Command::Record(record)) {
            Ok(()) => Commit::Accepted,
            Err(mpsc::TrySendError::Full(_)) => Commit::Full,
            Err(mpsc::TrySendError::Disconnected(_)) => Commit::Obsolete,
        }
    }

    /// Install a single owned barrier before the next record finishes serialization.
    #[cfg(test)]
    pub(super) fn hold_serialization(
        &self,
        owner: Arc<super::store::TraceStore>,
    ) -> TraceResult<TraceSerializationHold> {
        let (entered, received) = mpsc::channel();
        let (release, resumed) = mpsc::channel();
        let mut barrier = self
            .serialization
            .lock()
            .map_err(|_| TraceError::Poisoned)?;
        if barrier.is_some() {
            return Err(TraceError::Poisoned);
        }
        *barrier = Some(SerializationBarrier {
            entered,
            release: resumed,
        });
        Ok(TraceSerializationHold {
            entered: received,
            release,
            _owner: owner,
        })
    }

    /// Suspend one test producer after capturing its receipt epoch and timestamp.
    #[cfg(test)]
    fn serialization_barrier(&self) {
        let barrier = self
            .serialization
            .lock()
            .ok()
            .and_then(|mut barrier| barrier.take());
        if let Some(barrier) = barrier {
            let _ = barrier.entered.send(());
            let _ = barrier.release.recv();
        }
    }
}

enum Commit {
    Accepted,
    Obsolete,
    Contended,
    Full,
}

#[derive(Clone, Copy, Eq, PartialEq)]
struct TraceEpoch(u64);

/// Own one record-bounded diagnostic queue and its filesystem thread.
pub(super) struct TraceWriter {
    sender: mpsc::SyncSender<Command>,
    shared: Arc<Shared>,
    join: Mutex<Option<JoinHandle<()>>>,
    leases: AtomicUsize,
}

impl TraceWriter {
    /// Create the only filesystem owner for a canonical data root.
    pub(super) fn open(root: &Path) -> TraceResult<Self> {
        let files = Files::open(root)?;
        let shared = Arc::new(Shared::new(files.enabled, files.config_path.exists()));
        let (sender, receiver) = mpsc::sync_channel(MAX_PENDING_RECORDS);
        let worker_shared = shared.clone();
        let join = std::thread::Builder::new()
            .name("forge-trace".into())
            .spawn(move || files.run(receiver, worker_shared))
            .map_err(|source| io_error("start Harness trace writer", root, source))?;
        Ok(Self {
            sender,
            shared,
            join: Mutex::new(Some(join)),
            leases: AtomicUsize::new(0),
        })
    }

    /// Retain a store lease while the open registry serializes acquisition.
    pub(super) fn acquire(&self) {
        self.leases.fetch_add(1, Ordering::AcqRel);
    }

    /// Release a store lease and identify its final owning close.
    pub(super) fn release(&self) -> bool {
        self.leases.fetch_sub(1, Ordering::AcqRel) == 1
    }

    /// Get enablement without filesystem queries or writer locks.
    pub(super) fn enabled(&self) -> bool {
        self.shared.enabled.load(Ordering::Acquire)
    }

    /// Get whether an explicit persistent preference has been saved.
    pub(super) fn configured(&self) -> bool {
        self.shared.configured.load(Ordering::Acquire)
    }

    /// Accept safe owned metadata without waiting for the filesystem owner.
    pub(super) fn record(
        &self,
        admission: &Admission,
        session_id: &str,
        event: &str,
        payload: &Value,
    ) -> TraceAdmission {
        let result = self.admit(admission, session_id, event, payload);
        if matches!(
            result,
            TraceAdmission::Saturated
                | TraceAdmission::Contended
                | TraceAdmission::Obsolete
                | TraceAdmission::EncodingFailed
        ) {
            self.shared
                .rejected_record_count
                .fetch_add(1, Ordering::Relaxed);
        }
        result
    }

    /// Serialize optional metadata without blocking the lifecycle owner.
    fn admit(
        &self,
        admission: &Admission,
        session_id: &str,
        event: &str,
        payload: &Value,
    ) -> TraceAdmission {
        let epoch = TraceEpoch(self.shared.epoch.load(Ordering::Acquire));
        let received_at = timestamp_ms();
        if self.shared.closed.load(Ordering::Acquire) {
            return TraceAdmission::Closed;
        }
        if !self.enabled() {
            return TraceAdmission::Disabled;
        }
        let Some(mut buffer) = RecordBuffer::reserve(self.shared.clone()) else {
            self.shared
                .losses
                .queue_full
                .fetch_add(1, Ordering::Relaxed);
            return TraceAdmission::Saturated;
        };
        #[cfg(test)]
        admission.serialization_barrier();
        let record = BorrowedRecord {
            timestamp_ms: received_at,
            session_id,
            event,
            payload: &metadata_payload(payload),
        };
        if serde_json::to_writer(&mut buffer, &record).is_err() {
            self.shared.losses.encoding.fetch_add(1, Ordering::Relaxed);
            self.shared.losses.bytes.fetch_add(
                u64::try_from(buffer.bytes.len()).unwrap_or(u64::MAX),
                Ordering::Relaxed,
            );
            return TraceAdmission::EncodingFailed;
        }
        if epoch != TraceEpoch(self.shared.epoch.load(Ordering::Acquire)) {
            self.shared.losses.obsolete.fetch_add(1, Ordering::Relaxed);
            return TraceAdmission::Obsolete;
        }
        match admission.enqueue(&self.sender, EncodedRecord { epoch, buffer }) {
            Commit::Accepted => TraceAdmission::Queued,
            Commit::Obsolete => {
                self.shared.losses.obsolete.fetch_add(1, Ordering::Relaxed);
                TraceAdmission::Closed
            }
            Commit::Contended => {
                self.shared
                    .losses
                    .contention
                    .fetch_add(1, Ordering::Relaxed);
                TraceAdmission::Contended
            }
            Commit::Full => {
                self.shared
                    .losses
                    .queue_full
                    .fetch_add(1, Ordering::Relaxed);
                TraceAdmission::Saturated
            }
        }
    }

    /// Get cumulative rejected diagnostics without consuming loss summaries.
    pub(super) fn rejected_record_count(&self) -> u64 {
        self.shared.rejected_record_count.load(Ordering::Acquire)
    }

    /// Order a persisted enablement update after preceding accepted records.
    pub(super) fn configure(&self, enabled: bool, default_only: bool) -> TraceResult<()> {
        self.control(|reply| Command::Configure {
            enabled,
            default_only,
            reply,
        })
    }

    /// Order a global clear after preceding accepted records.
    pub(super) fn clear(&self) -> TraceResult<()> {
        self.control(Command::Clear)
    }

    /// Order a session clear after preceding accepted records.
    pub(super) fn clear_session(&self, session_id: &str) -> TraceResult<()> {
        self.control(|reply| Command::ClearSession {
            session_id: session_id.to_owned(),
            reply,
        })
    }

    /// Finish preceding accepted records and publish any diagnostic loss summary.
    pub(super) fn flush(&self) -> TraceResult<()> {
        self.control(Command::Flush)
    }

    /// Drain accepted records and collect the owned filesystem thread exactly once.
    pub(super) fn close(&self) -> TraceResult<()> {
        let mut join = self.join.lock().map_err(|_| TraceError::Poisoned)?;
        let Some(task) = join.take() else {
            return Ok(());
        };
        self.shared.closed.store(true, Ordering::Release);
        let result = self.control(Command::Close);
        task.join().map_err(|_| TraceError::Panicked)?;
        result
    }

    /// Wait for a control operation outside the record hot path.
    fn control(&self, command: impl FnOnce(Reply) -> Command) -> TraceResult<()> {
        let (reply, receiver) = mpsc::channel();
        self.sender
            .send(command(reply))
            .map_err(|_| TraceError::Closed)?;
        receiver.recv().map_err(|_| TraceError::Closed)?
    }
}

impl Drop for TraceWriter {
    /// Collect the filesystem owner when explicit shutdown was omitted.
    fn drop(&mut self) {
        let _ = self.close();
    }
}

/// Release a captured producer before releasing its owning trace lease.
#[cfg(test)]
#[must_use]
pub(crate) struct TraceSerializationHold {
    entered: mpsc::Receiver<()>,
    release: mpsc::Sender<()>,
    _owner: Arc<super::store::TraceStore>,
}

#[cfg(test)]
impl TraceSerializationHold {
    /// Wait until the actual producer captures its original logging epoch.
    pub(crate) fn wait_entered(&self) -> TraceResult<()> {
        self.entered.recv().map_err(|_| TraceError::Closed)
    }
}

#[cfg(test)]
impl Drop for TraceSerializationHold {
    /// Resume the captured producer on normal and unwinding test exits.
    fn drop(&mut self) {
        let _ = self.release.send(());
    }
}

#[cfg(test)]
struct SerializationBarrier {
    entered: mpsc::Sender<()>,
    release: mpsc::Receiver<()>,
}

struct Shared {
    enabled: AtomicBool,
    configured: AtomicBool,
    closed: AtomicBool,
    record_count: AtomicUsize,
    rejected_record_count: AtomicU64,
    bytes: AtomicUsize,
    losses: Losses,
    epoch: AtomicU64,
}

impl Shared {
    /// Create cached state and budgets independent of filesystem ownership.
    fn new(enabled: bool, configured: bool) -> Self {
        Self {
            enabled: AtomicBool::new(enabled),
            configured: AtomicBool::new(configured),
            closed: AtomicBool::new(false),
            record_count: AtomicUsize::new(0),
            rejected_record_count: AtomicU64::new(0),
            bytes: AtomicUsize::new(0),
            losses: Losses::default(),
            epoch: AtomicU64::new(0),
        }
    }
}

#[derive(Default)]
struct Losses {
    queue_full: AtomicU64,
    oversize: AtomicU64,
    encoding: AtomicU64,
    bytes: AtomicU64,
    obsolete: AtomicU64,
    contention: AtomicU64,
}

#[derive(Serialize)]
struct BorrowedRecord<'a> {
    timestamp_ms: i64,
    session_id: &'a str,
    event: &'a str,
    payload: &'a Value,
}

#[derive(Deserialize)]
struct OwnedRecord {
    timestamp_ms: i64,
    session_id: String,
    event: String,
    payload: Value,
}

struct RecordBuffer {
    shared: Arc<Shared>,
    bytes: Vec<u8>,
    charged_bytes: usize,
}

impl RecordBuffer {
    /// Reserve one record before any owned payload allocation.
    fn reserve(shared: Arc<Shared>) -> Option<Self> {
        shared
            .record_count
            .try_update(Ordering::AcqRel, Ordering::Acquire, |count| {
                (count < MAX_PENDING_RECORDS).then_some(count + 1)
            })
            .ok()?;
        Some(Self {
            shared,
            bytes: Vec::new(),
            charged_bytes: 0,
        })
    }
}

impl Write for RecordBuffer {
    /// Account retained encoded bytes without imposing an unapproved byte cap.
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        self.bytes
            .try_reserve(bytes.len())
            .map_err(io::Error::other)?;
        self.shared.bytes.fetch_add(bytes.len(), Ordering::AcqRel);
        self.charged_bytes += bytes.len();
        self.bytes.extend_from_slice(bytes);
        Ok(bytes.len())
    }

    /// Keep serialization flush free of filesystem effects.
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl Drop for RecordBuffer {
    /// Release queue reservations on writes, rejected enqueue, and early encoding failure.
    fn drop(&mut self) {
        self.shared
            .bytes
            .fetch_sub(self.charged_bytes, Ordering::AcqRel);
        self.shared.record_count.fetch_sub(1, Ordering::AcqRel);
    }
}

struct EncodedRecord {
    epoch: TraceEpoch,
    buffer: RecordBuffer,
}

#[derive(Deserialize, Serialize)]
struct TraceConfig {
    #[serde(default)]
    enabled: bool,
}

struct Files {
    root: PathBuf,
    path: PathBuf,
    config_path: PathBuf,
    file: Option<File>,
    enabled: bool,
    _lease: crate::storage::state::StateLease,
}

impl Files {
    /// Open persistent configuration before publishing the writer to producers.
    fn open(root: &Path) -> TraceResult<Self> {
        let lease = crate::storage::state::StateLease::open(root).map_err(TraceError::State)?;
        let config_path = root.join("harness-trace-config.json");
        let enabled = fs::read(&config_path)
            .ok()
            .and_then(|contents| serde_json::from_slice::<TraceConfig>(&contents).ok())
            .is_some_and(|config| config.enabled);
        let path = crate::storage::state::directory(root).join("harness-trace.jsonl");
        let file = enabled.then(|| open_global(&path)).transpose()?;
        Ok(Self {
            _lease: lease,
            root: root.to_owned(),
            path,
            config_path,
            file,
            enabled,
        })
    }

    /// Process ordered records and controls on the sole filesystem thread.
    fn run(mut self, receiver: mpsc::Receiver<Command>, shared: Arc<Shared>) {
        let mut failure = None;
        while let Ok(command) = receiver.recv() {
            if let Err(error) = self.report_losses(&shared) {
                failure.get_or_insert_with(|| Arc::new(error));
            }
            let result = match command {
                Command::Record(record) => {
                    if record.epoch != TraceEpoch(shared.epoch.load(Ordering::Acquire))
                        || !self.enabled
                    {
                        shared.losses.obsolete.fetch_add(1, Ordering::Relaxed);
                        continue;
                    }
                    self.write_encoded(&record.buffer.bytes)
                }
                Command::Configure {
                    enabled,
                    default_only,
                    reply,
                } => {
                    let result = self.configure(enabled, default_only, &shared);
                    let _ = reply.send(result);
                    continue;
                }
                Command::Clear(reply) => {
                    let result = self.clear();
                    let _ = reply.send(result);
                    continue;
                }
                Command::ClearSession { session_id, reply } => {
                    let result = self.clear_session(&session_id);
                    let _ = reply.send(result);
                    continue;
                }
                Command::Flush(reply) => {
                    let result = self.flush().and_then(|()| prior_failure(&failure));
                    let _ = reply.send(result);
                    continue;
                }
                Command::Close(reply) => {
                    let result = self.flush().and_then(|()| prior_failure(&failure));
                    if let Err(error) = result {
                        let _ = writeln!(
                            std::io::stderr(),
                            "Forge Harness diagnostic trace write failed: {error}"
                        );
                    }
                    // A joined writer with lost diagnostics is collected. Only
                    // thread/command collection failures may fail host shutdown.
                    let _ = reply.send(Ok(()));
                    break;
                }
            };
            if let Err(error) = result {
                failure.get_or_insert_with(|| Arc::new(error));
            }
        }
        shared.closed.store(true, Ordering::Release);
    }

    /// Persist a preference before publishing cached enablement.
    fn configure(&mut self, enabled: bool, default_only: bool, shared: &Shared) -> TraceResult<()> {
        if default_only && self.config_path.exists() {
            return Ok(());
        }
        let file = enabled.then(|| open_global(&self.path)).transpose()?;
        let contents =
            serde_json::to_vec_pretty(&TraceConfig { enabled }).map_err(TraceError::Encoding)?;
        fs::write(&self.config_path, contents)
            .map_err(|source| io_error("write trace configuration", &self.config_path, source))?;
        self.file = file;
        if self.enabled != enabled {
            shared.epoch.fetch_add(1, Ordering::AcqRel);
        }
        self.enabled = enabled;
        shared.configured.store(true, Ordering::Release);
        shared.enabled.store(enabled, Ordering::Release);
        if enabled {
            self.write_record(
                timestamp_ms(),
                "global",
                "trace.configured",
                &json!({"enabled":enabled}),
            )?;
        }
        Ok(())
    }

    /// Clear the same global inode so other processes retain valid append handles.
    fn clear(&mut self) -> TraceResult<()> {
        let file = open_global(&self.path)?;
        file.lock()
            .map_err(|source| io_error("lock trace clear", &self.path, source))?;
        let result = file.set_len(0);
        let unlocked = file.unlock();
        result
            .and(unlocked)
            .map_err(|source| io_error("clear trace", &self.path, source))?;
        self.file = self.enabled.then_some(file);
        if self.enabled {
            self.write_record(timestamp_ms(), "global", "trace.cleared", &Value::Null)?;
        }
        Ok(())
    }

    /// Clear one detail stream and its retained segments under its cross-process lock.
    fn clear_session(&self, session_id: &str) -> TraceResult<()> {
        let path = session_path(&self.root, session_id);
        let lock = detail_lock(&path)?;
        let result = (|| {
            for generation in 1..=ROTATED_TRACE_FILES {
                remove_if_present(&path.with_extension(format!("jsonl.{generation}")))?;
            }
            OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open(&path)
                .map_err(|source| io_error("clear session trace", &path, source))?;
            Ok(())
        })();
        let unlocked = lock
            .unlock()
            .map_err(|source| io_error("unlock session trace", &path, source));
        result.and(unlocked)
    }

    /// Decode owned metadata away from the provider event reducer.
    fn write_encoded(&mut self, bytes: &[u8]) -> TraceResult<()> {
        let mut decoder = serde_json::Deserializer::from_slice(bytes);
        decoder.disable_recursion_limit();
        let OwnedRecord {
            timestamp_ms,
            session_id,
            event,
            payload,
        } = OwnedRecord::deserialize(serde_stacker::Deserializer::new(&mut decoder))
            .map_err(TraceError::Encoding)?;
        self.write_record(timestamp_ms, &session_id, &event, &payload)
    }

    /// Preserve safe diagnostic metadata with one receipt timestamp.
    fn write_record(
        &mut self,
        timestamp_ms: i64,
        session_id: &str,
        event: &str,
        payload: &Value,
    ) -> TraceResult<()> {
        let detail_result = self.write_detail(timestamp_ms, session_id, event, payload);
        let record = json!({"timestamp_ms":timestamp_ms,"session_id":bounded_text(session_id),
            "event":bounded_text(event),"payload":metadata_payload(payload)});
        let line = serde_json::to_vec(&record).map_err(TraceError::Encoding)?;
        let global_result = if let Some(file) = self.file.as_mut() {
            append_global(file, &self.path, &line)
        } else {
            Ok(())
        };
        detail_result.and(global_result)
    }

    /// Rotate by rename so active readers retain their original detail segment.
    fn write_detail(
        &self,
        timestamp_ms: i64,
        session_id: &str,
        event: &str,
        payload: &Value,
    ) -> TraceResult<()> {
        let path = session_path(&self.root, session_id);
        let lock = detail_lock(&path)?;
        let result = (|| {
            let line =
                serde_json::to_vec(&json!({"timestamp_ms":timestamp_ms,"session_id":session_id,
                "event":event,"payload":metadata_payload(payload)}))
                .map_err(TraceError::Encoding)?;
            physical_records(&line, MAX_TRACE_BYTES, |part| {
                append_detail(&path, part, MAX_TRACE_BYTES)
            })
        })();
        let unlocked = lock
            .unlock()
            .map_err(|source| io_error("unlock session trace", &path, source));
        result.and(unlocked)
    }

    /// Publish diagnostic-only losses even when no subsequent record is accepted.
    fn report_losses(&mut self, shared: &Shared) -> TraceResult<()> {
        let queue_full_count = shared.losses.queue_full.swap(0, Ordering::AcqRel);
        let oversize_count = shared.losses.oversize.swap(0, Ordering::AcqRel);
        let encoding_error_count = shared.losses.encoding.swap(0, Ordering::AcqRel);
        let obsolete_count = shared.losses.obsolete.swap(0, Ordering::AcqRel);
        let contention_count = shared.losses.contention.swap(0, Ordering::AcqRel);
        let bytes = shared.losses.bytes.swap(0, Ordering::AcqRel);
        let count = queue_full_count
            .saturating_add(oversize_count)
            .saturating_add(encoding_error_count)
            .saturating_add(obsolete_count)
            .saturating_add(contention_count);
        if count == 0 {
            return Ok(());
        }
        // Obsolete in-flight input can finish after disable. Publish only its
        // scalar loss summary, never that input's private body or event metadata.
        let temporary_file = self.file.is_none();
        if temporary_file {
            self.file = Some(open_global(&self.path)?);
        }
        let result = self.write_record(timestamp_ms(), "global", "trace.loss", &json!({"count":count,"bytes":bytes,
            "queue_full_count":queue_full_count,"oversize_count":oversize_count,"encoding_error_count":encoding_error_count,
            "obsolete_count":obsolete_count,"contention_count":contention_count,
            "queue_count":shared.record_count.load(Ordering::Acquire),"queue_bytes":shared.bytes.load(Ordering::Acquire)}));
        if temporary_file {
            self.file = None;
        }
        result
    }

    /// Finish buffered file writes before acknowledging a snapshot barrier.
    fn flush(&mut self) -> TraceResult<()> {
        if let Some(file) = self.file.as_mut() {
            file.flush()
                .map_err(|source| io_error("flush trace", &self.path, source))?;
        }
        Ok(())
    }
}

/// Get the receipt time before a record enters the bounded writer queue.
fn timestamp_ms() -> i64 {
    i64::try_from(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis(),
    )
    .unwrap_or(i64::MAX)
}

/// Preserve the first actual writer failure for explicit barriers and shutdown.
fn prior_failure(failure: &Option<Arc<TraceError>>) -> TraceResult<()> {
    match failure {
        Some(error) => Err(TraceError::WriterFailed(error.clone())),
        None => Ok(()),
    }
}

/// Attach the failing trace path and operation to its original I/O cause.
fn io_error(operation: &'static str, path: &Path, source: io::Error) -> TraceError {
    TraceError::Io {
        operation,
        path: path.to_owned(),
        source,
    }
}

/// Open a stable global inode for lock-protected append and clear operations.
fn open_global(path: &Path) -> TraceResult<File> {
    OpenOptions::new()
        .create(true)
        .truncate(false)
        .read(true)
        .write(true)
        .open(path)
        .map_err(|source| io_error("open Harness trace", path, source))
}

/// Acquire the session lock before detail rotation, append, or clear.
fn detail_lock(path: &Path) -> TraceResult<File> {
    fs::create_dir_all(path.parent().expect("session traces have a logs directory"))
        .map_err(|source| io_error("create session trace directory", path, source))?;
    let lock = open_global(&path.with_extension("lock"))?;
    lock.lock()
        .map_err(|source| io_error("lock session trace", path, source))?;
    Ok(lock)
}

/// Remove a retained segment while tolerating a previously absent generation.
fn remove_if_present(path: &Path) -> TraceResult<()> {
    match fs::remove_file(path) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == ErrorKind::NotFound => Ok(()),
        Err(source) => Err(io_error("remove session trace segment", path, source)),
    }
}

/// Append bounded public metadata under the existing cross-process file lock.
fn append_global(file: &mut File, path: &Path, line: &[u8]) -> TraceResult<()> {
    file.lock()
        .map_err(|source| io_error("lock Harness trace", path, source))?;
    let result = physical_records(line, MAX_TRACE_BYTES, |part| {
        let result = (|| -> io::Result<()> {
            if file
                .metadata()?
                .len()
                .saturating_add(u64::try_from(part.len()).unwrap_or(u64::MAX))
                .saturating_add(1)
                > MAX_TRACE_BYTES
            {
                file.set_len(0)?;
            }
            file.seek(SeekFrom::End(0))?;
            file.write_all(part)?;
            file.write_all(b"\n")
        })();
        result.map_err(|source| io_error("append Harness trace", path, source))
    });
    let unlocked = file.unlock();
    let unlocked = unlocked.map_err(|source| io_error("unlock Harness trace", path, source));
    result.and(unlocked)
}

/// Emit bounded physical records while preserving an oversized logical JSON record losslessly.
fn physical_records(
    line: &[u8],
    file_limit: u64,
    mut append: impl FnMut(&[u8]) -> TraceResult<()>,
) -> TraceResult<()> {
    if u64::try_from(line.len())
        .unwrap_or(u64::MAX)
        .saturating_add(1)
        <= file_limit
    {
        return append(line);
    }
    let id = uuid::Uuid::new_v4().to_string();
    let envelope = |sequence: usize, part_count: usize, payload_hex: String| {
        json!({"trace_chunk":{
            "id":id,"sequence":sequence,"part_count":part_count,"payload_hex":payload_hex,
        }})
    };
    let overhead = serde_json::to_vec(&envelope(usize::MAX, usize::MAX, String::new()))
        .map_err(TraceError::Encoding)?
        .len()
        + 1;
    let available = usize::try_from(file_limit)
        .unwrap_or(usize::MAX)
        .checked_sub(overhead)
        .filter(|available| *available >= 2)
        .ok_or_else(|| {
            io_error(
                "split Harness trace",
                Path::new("trace_chunk"),
                io::Error::new(
                    ErrorKind::InvalidInput,
                    "trace file budget cannot hold a chunk envelope",
                ),
            )
        })?;
    let part_bytes = (available / 2).min(256 * 1024);
    let part_count = line.len().div_ceil(part_bytes);
    for (sequence, payload) in line.chunks(part_bytes).enumerate() {
        let part = serde_json::to_vec(&envelope(sequence, part_count, hex::encode(payload)))
            .map_err(TraceError::Encoding)?;
        append(&part)?;
    }
    Ok(())
}

/// Rotate one physical record without exceeding the selected file budget.
fn append_detail(path: &Path, line: &[u8], file_limit: u64) -> TraceResult<()> {
    if path.metadata().is_ok_and(|metadata| {
        metadata
            .len()
            .saturating_add(u64::try_from(line.len()).unwrap_or(u64::MAX))
            .saturating_add(1)
            > file_limit
    }) {
        remove_if_present(&path.with_extension(format!("jsonl.{ROTATED_TRACE_FILES}")))?;
        for generation in (1..=ROTATED_TRACE_FILES).rev() {
            let source = if generation == 1 {
                path.to_owned()
            } else {
                path.with_extension(format!("jsonl.{}", generation - 1))
            };
            let destination = path.with_extension(format!("jsonl.{generation}"));
            match fs::rename(&source, destination) {
                Ok(()) => {}
                Err(error) if error.kind() == ErrorKind::NotFound => {}
                Err(source_error) => {
                    return Err(io_error("rotate session trace", &source, source_error));
                }
            }
        }
    }
    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
        .map_err(|source| io_error("open session trace", path, source))?;
    file.write_all(line)
        .and_then(|()| file.write_all(b"\n"))
        .map_err(|source| io_error("append session trace", path, source))
}

/// Resolve private session detail paths without permitting traversal.
pub(super) fn session_path(root: &Path, session_id: &str) -> PathBuf {
    let name = if !session_id.is_empty()
        && session_id
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-')
    {
        session_id.to_owned()
    } else {
        format!("_{}", hex::encode(session_id.as_bytes()))
    };
    crate::storage::state::directory(root)
        .join("logs")
        .join(format!("{name}.jsonl"))
}

#[cfg(test)]
mod chunk_tests {
    use super::*;

    /// Reassemble every chunk across rotations without allocating a production-sized fixture.
    #[test]
    fn oversized_record_chunks_fit_each_rotated_file_and_preserve_utf8() -> TraceResult<()> {
        let root = tempfile::tempdir().expect("fixture directory opens");
        let path = root.path().join("chunked.jsonl");
        let original = serde_json::to_vec(&json!({"operation_id":"\u{754c}\u{1f642}".repeat(800)}))
            .map_err(TraceError::Encoding)?;
        physical_records(&original, 2048, |part| append_detail(&path, part, 2048))?;
        let mut reconstructed = Vec::new();
        let mut expected_sequence = 0_u64;
        let mut record_id = None;
        let mut expected_count = 0;
        for generation in (0..=ROTATED_TRACE_FILES).rev() {
            let segment = if generation == 0 {
                path.clone()
            } else {
                path.with_extension(format!("jsonl.{generation}"))
            };
            let Ok(contents) = fs::read_to_string(&segment) else {
                continue;
            };
            assert!(u64::try_from(contents.len()).unwrap() <= 2048);
            for line in contents.lines() {
                let value: Value = serde_json::from_str(line).map_err(TraceError::Encoding)?;
                let chunk = &value["trace_chunk"];
                assert_eq!(chunk["sequence"], expected_sequence);
                expected_sequence += 1;
                expected_count = chunk["part_count"].as_u64().unwrap();
                let id = chunk["id"].as_str().unwrap();
                assert_eq!(record_id.get_or_insert_with(|| id.to_owned()).as_str(), id);
                reconstructed.extend(hex::decode(chunk["payload_hex"].as_str().unwrap()).unwrap());
            }
        }
        assert_eq!(expected_sequence, expected_count);
        assert_eq!(reconstructed, original);
        Ok(())
    }
}
