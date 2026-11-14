use super::RepositoryAccess;
use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, HashSet},
    fs::{File, OpenOptions},
    io::{Read, Write},
    num::NonZeroU32,
    path::{Path, PathBuf},
    sync::Mutex,
};

const VERSION: u32 = 2;
static QUARANTINED: Mutex<Vec<QuarantinedOwner>> = Mutex::new(Vec::new());

/// Preserve the cause that prevents safe repository admission or journal publication.
#[derive(Debug, thiserror::Error)]
pub(crate) enum JournalError {
    #[error("repository_recovery_required: journal I/O failed for {}: {source}", path.display())]
    Io {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },
    #[error("repository_recovery_required: invalid journal {}: {source}", path.display())]
    Json {
        path: PathBuf,
        #[source]
        source: serde_json::Error,
    },
    #[error("repository_recovery_required: invalid journal {}: {reason}", path.display())]
    Invalid { path: PathBuf, reason: &'static str },
    #[error("repository_recovery_required: unsupported journal version {version} at {} (expected {VERSION})", path.display())]
    Version { path: PathBuf, version: u32 },
    #[error("repository_recovery_required: operation {operation} from process {pid} may have uncollected children; inspect {} before admitting more work", path.display())]
    Uncollected {
        path: PathBuf,
        operation: uuid::Uuid,
        pid: NonZeroU32,
    },
    #[error("repository_busy: active repository access prevents recovery for {}", .0.display())]
    Busy(PathBuf),
    #[error("repository_ownership_unavailable: {0}")]
    Unavailable(&'static str),
}

/// Retain one operation's journal and kernel-held owner identity outside session state.
#[derive(Debug)]
pub(super) struct RepositoryJournal {
    directory: PathBuf,
    path: PathBuf,
    _lease: File,
    record: Mutex<OperationRecord>,
    containment: std::sync::Arc<forge_shell::containment::ProcessContainment>,
    admission: PathBuf,
}

impl RepositoryJournal {
    /// Validate all existing records under the admission gate before granting source access.
    pub(super) fn recover(
        directory: &Path,
        key: &str,
        workspace: &Path,
    ) -> Result<(), JournalError> {
        let directory = directory.join(format!("{key}.operations"));
        Self::retry_quarantined(&directory)?;
        if !directory
            .try_exists()
            .map_err(|source| io(&directory, source))?
        {
            return Ok(());
        }
        let mut paths = std::fs::read_dir(&directory)
            .map_err(|source| io(&directory, source))?
            .map(|entry| {
                entry
                    .map(|entry| entry.path())
                    .map_err(|source| io(&directory, source))
            })
            .collect::<Result<Vec<_>, _>>()?;
        paths.sort();
        let mut recovery_lock = None;
        // Validate records before using their owner identities or changing retained files.
        for path in paths {
            let metadata = std::fs::symlink_metadata(&path).map_err(|source| io(&path, source))?;
            if metadata.is_dir() && path.file_name().is_some_and(|name| name == "owners") {
                continue;
            }
            if metadata.is_file()
                && path
                    .file_name()
                    .is_some_and(|name| name.to_string_lossy().starts_with(".pending-"))
            {
                continue;
            }
            if !metadata.is_file() || path.extension().is_none_or(|extension| extension != "json") {
                return Err(invalid(&path, "unexpected journal entry"));
            }
            let mut record = read_record(&path, workspace)?;
            let lease_path = lease_path(&directory, record.owner.id);
            let mut lease = OpenOptions::new()
                .read(true)
                .write(true)
                .open(&lease_path)
                .map_err(|source| io(&lease_path, source))?;
            match lease.try_lock() {
                Ok(()) => {}
                Err(std::fs::TryLockError::WouldBlock) => continue,
                Err(std::fs::TryLockError::Error(source)) => return Err(io(&lease_path, source)),
            }
            // Windows range locks forbid reading another live owner's locked handle.
            // Inspect the metadata through this handle only after acquiring its lease.
            let mut bytes = Vec::new();
            lease
                .read_to_end(&mut bytes)
                .map_err(|source| io(&lease_path, source))?;
            let identity: OwnerIdentity =
                serde_json::from_slice(&bytes).map_err(|source| json(&lease_path, source))?;
            if identity != record.owner {
                return Err(invalid(
                    &lease_path,
                    "owner lease identity does not match its journal",
                ));
            }
            if record.phase != OperationPhase::Collected && recovery_lock.is_none() {
                recovery_lock = Some(recovery_access(
                    directory
                        .parent()
                        .ok_or_else(|| invalid(&directory, "journal has no parent"))?,
                    key,
                    workspace,
                )?);
            }
            // Collect the registered tree independently of the dead owner's process identity.
            if matches!(
                record.phase,
                OperationPhase::Running | OperationPhase::Collecting
            ) {
                match forge_shell::containment::ProcessContainment::recover(
                    &record.containment.to_string(),
                )
                .map_err(|source| io(&path, source))?
                {
                    forge_shell::containment::CollectionStatus::Collected => {
                        record.phase = OperationPhase::Collected;
                        record.borrowers.clear();
                        for outcome in record.outcomes.values_mut() {
                            if matches!(outcome, PublicationState::Unpublished) {
                                *outcome = PublicationState::Unresolved {
                                    reason: "owner exited before durable result publication".into(),
                                };
                            }
                        }
                        write_record(&path, &record)?;
                    }
                    forge_shell::containment::CollectionStatus::Pending
                    | forge_shell::containment::CollectionStatus::Unsupported => {
                        return Err(JournalError::Uncollected {
                            path,
                            operation: record.operation,
                            pid: record.owner.pid,
                        });
                    }
                }
            }
            // Retain unresolved results after collection; they no longer block source admission.
            match record.phase {
                OperationPhase::Prepared | OperationPhase::Collected => {
                    if record
                        .outcomes
                        .values()
                        .any(|outcome| matches!(outcome, PublicationState::Unresolved { .. }))
                    {
                        continue;
                    }
                    // Remove the record first: an interrupted cleanup may leave only an inert lease.
                    std::fs::remove_file(&path).map_err(|source| io(&path, source))?;
                    drop(lease);
                    std::fs::remove_file(&lease_path).map_err(|source| io(&lease_path, source))?;
                }
                OperationPhase::Running | OperationPhase::Collecting => {
                    return Err(JournalError::Uncollected {
                        path,
                        operation: record.operation,
                        pid: record.owner.pid,
                    });
                }
            }
        }
        Ok(())
    }

    /// Persist prepared ownership before any operation can execute or borrow it.
    pub(super) fn begin(
        directory: &Path,
        key: &str,
        workspace: &Path,
        operation: uuid::Uuid,
        access: RepositoryAccess,
    ) -> Result<Self, JournalError> {
        let admission = directory.join(format!("{key}.admission.lock"));
        let directory = directory.join(format!("{key}.operations"));
        let owners = directory.join("owners");
        std::fs::create_dir_all(&owners).map_err(|source| io(&owners, source))?;
        // Establish a unique locked identity before publishing any operation that refers to it.
        let owner = OwnerIdentity {
            version: VERSION,
            id: uuid::Uuid::new_v4(),
            pid: NonZeroU32::new(std::process::id()).expect("OS process IDs are nonzero"),
        };
        let lease_path = lease_path(&directory, owner.id);
        let mut lease = OpenOptions::new()
            .read(true)
            .write(true)
            .create_new(true)
            .open(&lease_path)
            .map_err(|source| io(&lease_path, source))?;
        lease.try_lock().map_err(|error| match error {
            std::fs::TryLockError::WouldBlock => {
                invalid(&lease_path, "new owner identity is already locked")
            }
            std::fs::TryLockError::Error(source) => io(&lease_path, source),
        })?;
        let encoded = serde_json::to_vec(&owner).map_err(|source| json(&lease_path, source))?;
        lease
            .write_all(&encoded)
            .map_err(|source| io(&lease_path, source))?;
        lease.sync_all().map_err(|source| io(&lease_path, source))?;
        // Publish preparation before returning authority to execute.
        let path = directory.join(format!("{operation}.json"));
        let containment_id = uuid::Uuid::new_v4();
        let containment = std::sync::Arc::new(
            forge_shell::containment::ProcessContainment::create(&containment_id.to_string())
                .map_err(|source| io(&path, source))?,
        );
        let record = OperationRecord {
            version: VERSION,
            repository: workspace.to_owned(),
            operation,
            owner,
            access,
            phase: OperationPhase::Prepared,
            borrowers: Vec::new(),
            containment: containment_id,
            outcomes: BTreeMap::new(),
        };
        write_record(&path, &record)?;
        Ok(Self {
            directory,
            path,
            _lease: lease,
            record: Mutex::new(record),
            containment,
            admission,
        })
    }

    /// Get the container whose identity is already durable before launch.
    pub(super) fn containment(&self) -> &forge_shell::containment::ProcessContainment {
        &self.containment
    }

    /// Record result publication separately from physical process collection.
    #[cfg(test)]
    pub(super) fn published(&self) -> Result<(), JournalError> {
        let operation = self
            .record
            .lock()
            .map_err(|_| JournalError::Unavailable("repository journal lock poisoned"))?
            .operation;
        self.published_for(operation)
    }

    /// Publish only the named owner's result; a child cannot discharge its parent's obligation.
    pub(super) fn published_for(&self, operation: uuid::Uuid) -> Result<(), JournalError> {
        self.publication(operation, PublicationState::Published)
    }

    /// Retain the actual storage failure before allowing collected work to release access.
    pub(super) fn unresolved(
        &self,
        operation: uuid::Uuid,
        reason: String,
    ) -> Result<(), JournalError> {
        self.publication(operation, PublicationState::Unresolved { reason })
    }

    /// Persist a complete publication transition before changing the accepted record.
    fn publication(
        &self,
        operation: uuid::Uuid,
        outcome: PublicationState,
    ) -> Result<(), JournalError> {
        let _gate = self.admission_gate()?;
        let mut record = self
            .record
            .lock()
            .map_err(|_| JournalError::Unavailable("repository journal lock poisoned"))?;
        let mut next = record.clone();
        next.outcomes.insert(operation, outcome);
        write_record(&self.path, &next)?;
        *record = next;
        Ok(())
    }

    /// Record a new descendant before publishing its runtime capability.
    pub(super) fn borrowed(&self, borrowers: Vec<BorrowedOwner>) -> Result<(), JournalError> {
        self.update(None, Some(borrowers))
    }

    /// Record possible execution before a caller can perform its first side effect.
    pub(super) fn started(&self, borrowers: Vec<BorrowedOwner>) -> Result<(), JournalError> {
        self.update(Some(OperationPhase::Running), Some(borrowers))
    }

    /// Persist owner revocation without implying that surviving descendants have exited.
    pub(super) fn collecting(&self) -> Result<(), JournalError> {
        if self
            .record
            .lock()
            .map_err(|_| JournalError::Unavailable("repository journal lock poisoned"))?
            .phase
            == OperationPhase::Prepared
        {
            return Ok(());
        }
        self.update(Some(OperationPhase::Collecting), None)
    }

    /// Publish collection while retaining both kernel locks if publication fails.
    pub(super) fn finish_or_retain(journal: Self, source: File) {
        if let Err(error) = journal.finish() {
            tracing::error!(error = %error, "repository ownership retained after journal failure");
            match QUARANTINED.lock() {
                Ok(mut owners) => owners.push(QuarantinedOwner {
                    journal,
                    _source: source,
                }),
                Err(_) => {
                    // A poisoned quarantine must never release uncertain ownership during unwinding.
                    std::mem::forget((journal, source));
                }
            }
        }
    }

    /// Publish a replacement only after its complete contents are durable.
    fn update(
        &self,
        phase: Option<OperationPhase>,
        borrowers: Option<Vec<BorrowedOwner>>,
    ) -> Result<(), JournalError> {
        let _gate = self.admission_gate()?;
        self.update_under_gate(phase, borrowers)
    }

    /// Publish a transition while the caller already retains the repository admission gate.
    fn update_under_gate(
        &self,
        phase: Option<OperationPhase>,
        borrowers: Option<Vec<BorrowedOwner>>,
    ) -> Result<(), JournalError> {
        let mut record = self
            .record
            .lock()
            .map_err(|_| JournalError::Unavailable("repository journal lock poisoned"))?;
        let mut next = record.clone();
        if let Some(phase) = phase {
            next.phase = phase;
            if phase == OperationPhase::Running {
                let operation = borrowers
                    .as_ref()
                    .and_then(|owners| owners.last())
                    .map_or(next.operation, |owner| owner.operation);
                next.outcomes
                    .insert(operation, PublicationState::Unpublished);
            }
        }
        if let Some(borrowers) = borrowers {
            next.borrowers = borrowers;
        }
        next.validate(&self.path, &next.repository)?;
        write_record(&self.path, &next)?;
        *record = next;
        Ok(())
    }

    /// Confirm collection after the last runtime owner and execution guard have gone.
    fn finish(&self) -> Result<(), JournalError> {
        let _gate = self.admission_gate()?;
        self.finish_under_gate()
    }

    /// Complete quarantine recovery without reacquiring its caller's admission gate.
    fn finish_under_gate(&self) -> Result<(), JournalError> {
        if !self
            .containment
            .collected()
            .map_err(|source| io(&self.path, source))?
        {
            return Err(JournalError::Unavailable(
                "repository process collection is incomplete",
            ));
        }
        {
            let mut record = self
                .record
                .lock()
                .map_err(|_| JournalError::Unavailable("repository journal lock poisoned"))?;
            if record
                .outcomes
                .values()
                .any(|outcome| matches!(outcome, PublicationState::Unpublished))
            {
                let mut next = record.clone();
                for outcome in next.outcomes.values_mut() {
                    if matches!(outcome, PublicationState::Unpublished) {
                        *outcome = PublicationState::Unresolved {
                            reason: "operation ended without durable result publication".into(),
                        };
                    }
                }
                write_record(&self.path, &next)?;
                *record = next;
            }
        }
        self.update_under_gate(Some(OperationPhase::Collected), Some(Vec::new()))
    }

    /// Serialize journal writes with admission; admission itself never waits for source access.
    fn admission_gate(&self) -> Result<File, JournalError> {
        let gate = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(&self.admission)
            .map_err(|source| io(&self.admission, source))?;
        gate.lock().map_err(|source| io(&self.admission, source))?;
        Ok(gate)
    }

    /// Retry only this repository's retained publication failures before normal recovery.
    fn retry_quarantined(directory: &Path) -> Result<(), JournalError> {
        let mut owners = QUARANTINED
            .lock()
            .map_err(|_| JournalError::Unavailable("repository quarantine lock poisoned"))?;
        let mut index = 0;
        while index < owners.len() {
            if owners[index].journal.directory == directory {
                owners[index].journal.finish_under_gate()?;
                owners.swap_remove(index);
            } else {
                index += 1;
            }
        }
        Ok(())
    }
}

/// Persist an admitted descendant's identity and the parent whose slot it borrows.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct BorrowedOwner {
    operation: uuid::Uuid,
    parent: uuid::Uuid,
    access: RepositoryAccess,
}

impl BorrowedOwner {
    /// Create one link in the host-authenticated ownership chain.
    pub(super) fn new(operation: uuid::Uuid, parent: uuid::Uuid, access: RepositoryAccess) -> Self {
        Self {
            operation,
            parent,
            access,
        }
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
enum OperationPhase {
    Prepared,
    Running,
    Collecting,
    Collected,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "state", rename_all = "snake_case", deny_unknown_fields)]
enum PublicationState {
    Unpublished,
    Published,
    Unresolved { reason: String },
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct OwnerIdentity {
    version: u32,
    id: uuid::Uuid,
    pid: NonZeroU32,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct OperationRecord {
    version: u32,
    repository: PathBuf,
    operation: uuid::Uuid,
    owner: OwnerIdentity,
    access: RepositoryAccess,
    phase: OperationPhase,
    borrowers: Vec<BorrowedOwner>,
    containment: uuid::Uuid,
    outcomes: BTreeMap<uuid::Uuid, PublicationState>,
}

impl OperationRecord {
    /// Reject missing or contradictory ownership before it can influence admission.
    fn validate(&self, path: &Path, workspace: &Path) -> Result<(), JournalError> {
        if self.version != VERSION {
            return Err(JournalError::Version {
                path: path.to_owned(),
                version: self.version,
            });
        }
        if self.owner.version != VERSION
            || self.owner.id.is_nil()
            || self.operation.is_nil()
            || self.containment.is_nil()
        {
            return Err(invalid(path, "invalid owner identity"));
        }
        if self.repository != workspace {
            return Err(invalid(path, "record belongs to another repository"));
        }
        if path
            .file_stem()
            .is_none_or(|name| name.to_string_lossy() != self.operation.to_string())
        {
            return Err(invalid(
                path,
                "operation identity does not match its filename",
            ));
        }
        let mut parent = self.operation;
        let mut access = self.access;
        let mut seen = HashSet::from([parent]);
        for borrower in &self.borrowers {
            if borrower.operation.is_nil()
                || borrower.parent != parent
                || !seen.insert(borrower.operation)
            {
                return Err(invalid(path, "invalid borrowed ownership chain"));
            }
            if access == RepositoryAccess::Read && borrower.access == RepositoryAccess::Write {
                return Err(invalid(path, "borrowed ownership upgrades shared access"));
            }
            parent = borrower.operation;
            access = borrower.access;
        }
        if self.phase == OperationPhase::Collected && !self.borrowers.is_empty() {
            return Err(invalid(path, "collected operation retains borrowers"));
        }
        Ok(())
    }
}

#[derive(Debug)]
struct QuarantinedOwner {
    journal: RepositoryJournal,
    _source: File,
}

/// Require exclusive source access before reconciling an abandoned operation.
fn recovery_access(directory: &Path, key: &str, workspace: &Path) -> Result<File, JournalError> {
    let path = directory.join(format!("{key}.lock"));
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(&path)
        .map_err(|source| io(&path, source))?;
    match file.try_lock() {
        Ok(()) => Ok(file),
        Err(std::fs::TryLockError::WouldBlock) => Err(JournalError::Busy(workspace.to_owned())),
        Err(std::fs::TryLockError::Error(source)) => Err(io(&path, source)),
    }
}

/// Read a version before interpreting fields from that version's ownership contract.
fn read_record(path: &Path, workspace: &Path) -> Result<OperationRecord, JournalError> {
    #[derive(Deserialize)]
    struct Header {
        version: u32,
    }
    let bytes = read_bytes(path)?;
    let header: Header = serde_json::from_slice(&bytes).map_err(|source| json(path, source))?;
    if header.version != VERSION {
        return Err(JournalError::Version {
            path: path.to_owned(),
            version: header.version,
        });
    }
    let record: OperationRecord =
        serde_json::from_slice(&bytes).map_err(|source| json(path, source))?;
    record.validate(path, workspace)?;
    Ok(record)
}

/// Read the complete durable record without truncating valid ownership data.
fn read_bytes(path: &Path) -> Result<Vec<u8>, JournalError> {
    let mut file = File::open(path).map_err(|source| io(path, source))?;
    let mut bytes = Vec::new();
    file.read_to_end(&mut bytes)
        .map_err(|source| io(path, source))?;
    Ok(bytes)
}

/// Atomically replace one operation record after flushing its complete encoded form.
fn write_record(path: &Path, record: &OperationRecord) -> Result<(), JournalError> {
    let bytes = serde_json::to_vec(record).map_err(|source| json(path, source))?;
    let directory = path
        .parent()
        .ok_or_else(|| invalid(path, "journal has no parent"))?;
    let mut pending = tempfile::Builder::new()
        .prefix(".pending-")
        .tempfile_in(directory)
        .map_err(|source| io(directory, source))?;
    pending
        .write_all(&bytes)
        .map_err(|source| io(path, source))?;
    pending
        .as_file()
        .sync_all()
        .map_err(|source| io(path, source))?;
    pending
        .persist(path)
        .map_err(|error| io(path, error.error))?;
    Ok(())
}

/// Derive the unique lease path without accepting paths from serialized input.
fn lease_path(directory: &Path, owner: uuid::Uuid) -> PathBuf {
    directory.join("owners").join(format!("{owner}.lock"))
}

/// Preserve a filesystem error and the exact object needed for recovery.
fn io(path: &Path, source: std::io::Error) -> JournalError {
    JournalError::Io {
        path: path.to_owned(),
        source,
    }
}

/// Preserve malformed JSON as the cause of rejected admission.
fn json(path: &Path, source: serde_json::Error) -> JournalError {
    JournalError::Json {
        path: path.to_owned(),
        source,
    }
}

/// Identify a structural ownership error without modifying the rejected record.
fn invalid(path: &Path, reason: &'static str) -> JournalError {
    JournalError::Invalid {
        path: path.to_owned(),
        reason,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A completed borrowed operation cannot erase its enclosing writer's missing result.
    #[test]
    fn child_publication_preserves_unresolved_parent() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let journal = fixture(directory.path(), workspace.path(), RepositoryAccess::Write);
        let parent = journal.record.lock().unwrap().operation;
        let child = uuid::Uuid::new_v4();
        journal.started(Vec::new())?;
        journal.started(vec![BorrowedOwner::new(
            child,
            parent,
            RepositoryAccess::Read,
        )])?;
        journal.published_for(child)?;
        journal.finish()?;
        let record = read_record(&journal.path, workspace.path())?;
        assert!(matches!(
            record.outcomes.get(&parent),
            Some(PublicationState::Unresolved { .. })
        ));
        assert!(matches!(
            record.outcomes.get(&child),
            Some(PublicationState::Published)
        ));
        Ok(())
    }

    /// Terminal publication waits behind admission rather than exposing a partial transition.
    #[test]
    fn terminal_publication_obeys_admission_gate() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let journal = fixture(directory.path(), workspace.path(), RepositoryAccess::Read);
        journal.started(Vec::new())?;
        let gate = journal.admission_gate()?;
        let (started, waiting) = std::sync::mpsc::channel();
        std::thread::scope(|threads| -> anyhow::Result<()> {
            let worker = threads.spawn(|| {
                started.send(()).unwrap();
                journal.published()
            });
            waiting.recv()?;
            let record = read_record(&journal.path, workspace.path())?;
            assert!(
                record
                    .outcomes
                    .values()
                    .all(|outcome| matches!(outcome, PublicationState::Unpublished))
            );
            drop(gate);
            worker.join().unwrap()?;
            let record = read_record(&journal.path, workspace.path())?;
            assert!(
                record
                    .outcomes
                    .values()
                    .all(|outcome| matches!(outcome, PublicationState::Published))
            );
            Ok(())
        })?;
        Ok(())
    }

    /// Construct a real journal and lease without launching any external process.
    fn fixture(directory: &Path, workspace: &Path, access: RepositoryAccess) -> RepositoryJournal {
        RepositoryJournal::begin(
            directory,
            "repository",
            workspace,
            uuid::Uuid::new_v4(),
            access,
        )
        .unwrap()
    }

    /// A held nonce lease proves liveness independently of a reused or still-current PID.
    #[test]
    fn owner_liveness_uses_the_lease_and_preserves_uncollected_work() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let journal = fixture(directory.path(), workspace.path(), RepositoryAccess::Read);
        journal.started(Vec::new())?;
        let path = journal.path.clone();
        #[cfg(not(windows))]
        let before = std::fs::read(&path)?;
        RepositoryJournal::recover(directory.path(), "repository", workspace.path())?;
        drop(journal);
        #[cfg(windows)]
        {
            RepositoryJournal::recover(directory.path(), "repository", workspace.path())?;
            let recovered = read_record(&path, workspace.path())?;
            assert_eq!(recovered.phase, OperationPhase::Collected);
            assert!(
                recovered
                    .outcomes
                    .values()
                    .any(|outcome| matches!(outcome, PublicationState::Unresolved { .. }))
            );
            RepositoryJournal::recover(directory.path(), "repository", workspace.path())?;
            assert!(path.exists());
        }
        // The recorded PID is this still-running test process, but the unique lease is gone.
        #[cfg(not(windows))]
        for _ in 0..2 {
            let error =
                RepositoryJournal::recover(directory.path(), "repository", workspace.path())
                    .unwrap_err();
            assert!(matches!(error, JournalError::Uncollected { .. }));
            assert!(error.to_string().contains(&path.display().to_string()));
            assert_eq!(std::fs::read(&path)?, before);
        }
        Ok(())
    }

    /// Abandoned preparation can be retired only after obtaining exclusive source access.
    #[test]
    fn prepared_recovery_waits_for_readers_and_is_idempotent() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let journal = fixture(directory.path(), workspace.path(), RepositoryAccess::Read);
        let path = journal.path.clone();
        let lease = lease_path(&journal.directory, journal.record.lock().unwrap().owner.id);
        drop(journal);
        let source = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(directory.path().join("repository.lock"))?;
        source.try_lock_shared()?;
        assert!(matches!(
            RepositoryJournal::recover(directory.path(), "repository", workspace.path()),
            Err(JournalError::Busy(_))
        ));
        assert!(path.exists());
        drop(source);
        RepositoryJournal::recover(directory.path(), "repository", workspace.path())?;
        assert!(!path.exists());
        assert!(!lease.exists());
        RepositoryJournal::recover(directory.path(), "repository", workspace.path())?;
        Ok(())
    }

    /// A terminal record does not permit cleanup of a lease that is still held by its owner.
    #[test]
    fn collected_records_are_pruned_after_the_owner_releases_its_lease() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let journal = fixture(directory.path(), workspace.path(), RepositoryAccess::Write);
        journal.started(Vec::new())?;
        journal.published()?;
        journal.finish()?;
        let path = journal.path.clone();
        RepositoryJournal::recover(directory.path(), "repository", workspace.path())?;
        assert!(path.exists());
        drop(journal);
        RepositoryJournal::recover(directory.path(), "repository", workspace.path())?;
        assert!(!path.exists());
        Ok(())
    }

    /// Unknown versions and incomplete current records fail before even a live lease is accepted.
    #[test]
    fn invalid_records_preserve_their_bytes_and_causes() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let journal = fixture(directory.path(), workspace.path(), RepositoryAccess::Write);
        let original = std::fs::read(&journal.path)?;
        let mut value: serde_json::Value = serde_json::from_slice(&original)?;
        value["version"] = serde_json::json!(VERSION + 1);
        std::fs::write(&journal.path, serde_json::to_vec(&value)?)?;
        assert!(matches!(
            RepositoryJournal::recover(directory.path(), "repository", workspace.path()),
            Err(JournalError::Version { .. })
        ));
        assert_eq!(std::fs::read(&journal.path)?, serde_json::to_vec(&value)?);
        value = serde_json::from_slice(&original)?;
        value.as_object_mut().unwrap().remove("owner");
        std::fs::write(&journal.path, serde_json::to_vec(&value)?)?;
        assert!(matches!(
            RepositoryJournal::recover(directory.path(), "repository", workspace.path()),
            Err(JournalError::Json { .. })
        ));
        assert_eq!(std::fs::read(&journal.path)?, serde_json::to_vec(&value)?);
        std::fs::write(&journal.path, b"{broken")?;
        assert!(matches!(
            RepositoryJournal::recover(directory.path(), "repository", workspace.path()),
            Err(JournalError::Json { .. })
        ));
        assert_eq!(std::fs::read(&journal.path)?, b"{broken");
        Ok(())
    }

    /// Validate ancestry and access restrictions before replacing a previously valid record.
    #[test]
    fn borrowed_ownership_rejects_upgrades_and_foreign_parents() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let journal = fixture(directory.path(), workspace.path(), RepositoryAccess::Read);
        let operation = journal.record.lock().unwrap().operation;
        let original = std::fs::read(&journal.path)?;
        let child = uuid::Uuid::new_v4();
        assert!(
            journal
                .borrowed(vec![BorrowedOwner::new(
                    child,
                    operation,
                    RepositoryAccess::Write
                )])
                .is_err()
        );
        assert!(
            journal
                .borrowed(vec![BorrowedOwner::new(
                    child,
                    uuid::Uuid::new_v4(),
                    RepositoryAccess::Read
                )])
                .is_err()
        );
        assert_eq!(std::fs::read(&journal.path)?, original);
        journal.borrowed(vec![BorrowedOwner::new(
            child,
            operation,
            RepositoryAccess::Read,
        )])?;
        let record = read_record(&journal.path, workspace.path())?;
        assert_eq!(record.borrowers.len(), 1);
        assert_eq!(record.borrowers[0].operation, child);
        Ok(())
    }

    /// A missing or changed lease cannot be interpreted as evidence that a record is safe to erase.
    #[test]
    fn missing_or_mismatched_owner_identity_blocks_recovery() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let journal = fixture(directory.path(), workspace.path(), RepositoryAccess::Write);
        let lease = lease_path(&journal.directory, journal.record.lock().unwrap().owner.id);
        let path = journal.path.clone();
        drop(journal);
        let original = std::fs::read(&lease)?;
        let mut identity: OwnerIdentity = serde_json::from_slice(&original)?;
        identity.id = uuid::Uuid::new_v4();
        std::fs::write(&lease, serde_json::to_vec(&identity)?)?;
        assert!(matches!(
            RepositoryJournal::recover(directory.path(), "repository", workspace.path()),
            Err(JournalError::Invalid { .. })
        ));
        std::fs::remove_file(&lease)?;
        assert!(matches!(
            RepositoryJournal::recover(directory.path(), "repository", workspace.path()),
            Err(JournalError::Io { .. })
        ));
        assert!(path.exists());
        Ok(())
    }

    /// Preserve collection and publication as independent facts across repeated recovery.
    #[test]
    fn collected_storage_failure_remains_inspectable_after_owner_exit() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let journal = fixture(directory.path(), workspace.path(), RepositoryAccess::Write);
        journal.started(Vec::new())?;
        let operation = journal.record.lock().unwrap().operation;
        journal.unresolved(operation, "write result: access denied".into())?;
        journal.finish()?;
        let path = journal.path.clone();
        drop(journal);
        for _ in 0..2 {
            RepositoryJournal::recover(directory.path(), "repository", workspace.path())?;
            let retained = read_record(&path, workspace.path())?;
            assert_eq!(retained.phase, OperationPhase::Collected);
            assert!(retained.outcomes.values().any(|outcome| matches!(outcome, PublicationState::Unresolved { reason } if reason == "write result: access denied")));
        }
        Ok(())
    }

    /// Reject a matching version whose required process registration is missing.
    #[test]
    fn missing_containment_is_rejected_without_resetting_the_journal() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let journal = fixture(directory.path(), workspace.path(), RepositoryAccess::Write);
        let mut record: serde_json::Value = serde_json::from_slice(&std::fs::read(&journal.path)?)?;
        record.as_object_mut().unwrap().remove("containment");
        let bytes = serde_json::to_vec(&record)?;
        std::fs::write(&journal.path, &bytes)?;
        assert!(matches!(
            RepositoryJournal::recover(directory.path(), "repository", workspace.path()),
            Err(JournalError::Json { .. })
        ));
        assert_eq!(std::fs::read(&journal.path)?, bytes);
        Ok(())
    }
}
