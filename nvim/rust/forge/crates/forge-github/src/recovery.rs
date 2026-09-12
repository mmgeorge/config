//! Durable remote outcomes remain separate from deletable repository caches and executable retries.

use std::fs::{self, File, TryLockError};
use std::io::Read;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail, ensure};
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::model::GithubRepositoryId;
use crate::publication::publish_json;

pub const MAX_RECOVERY_RECORDS: usize = 64;
pub const MAX_RECOVERY_RECORD_BYTES: usize = 16 * 1024 * 1024;
pub const MAX_RECOVERY_BYTES: usize = 64 * 1024 * 1024;
const DIAGNOSTIC_RESERVATION: usize = 64 * 1024;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum RecoveryResourceKind {
    Repository,
    PullRequest,
    Issue,
    Notification,
}

#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecoveryResource {
    pub repository: GithubRepositoryId,
    pub kind: RecoveryResourceKind,
    pub number: u64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum RecoveryOperation {
    PullRequestCreate,
    ConversationCreate,
    InlineCreate,
    ReplyCreate,
    PendingReviewCreate,
    ReviewSubmit,
    CommentEdit,
    CommentDelete,
    PullRequestEdit,
    PullRequestTransition,
    ReviewerSet,
    MilestoneSet,
    IssueEdit,
    NotificationRead,
    NotificationDone,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecoveryCapture {
    pub operation_id: String,
    pub actor: String,
    pub operation: RecoveryOperation,
    pub edit_sequence: Option<u64>,
    pub submitted: Value,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "phase", rename_all = "snake_case", deny_unknown_fields)]
pub enum RecoveryPhase {
    Prepared,
    DispatchPossible,
    Confirmed { result: Value },
    Rejected { diagnostic: String },
    OutcomeUnknown { diagnostic: String },
    UserLinked { result: Value, diagnostic: String },
    UserClosedUnknown { diagnostic: String },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecoveryRecord {
    pub version: u32,
    pub resource: RecoveryResource,
    pub capture: RecoveryCapture,
    pub reserved_bytes: usize,
    pub state: RecoveryPhase,
    pub confirmed_steps: Vec<Value>,
}

#[derive(Clone, Debug)]
pub struct RecoveryStore {
    directory: PathBuf,
}

pub struct RecoveryLease {
    store: RecoveryStore,
    path: PathBuf,
    record: RecoveryRecord,
    _ownership: File,
}

impl RecoveryStore {
    /// Records an absolute recovery root without filesystem IO or accepting repository cache roots.
    pub fn new(directory: PathBuf) -> Result<Self> {
        ensure!(
            directory.is_absolute(),
            "GitHub recovery directory must be absolute"
        );
        ensure!(
            directory.ends_with(Path::new("forge/recovery/github/v1")),
            "GitHub recovery directory must be forge/recovery/github/v1 outside repository caches"
        );
        ensure!(
            !directory
                .components()
                .any(|component| matches!(component, std::path::Component::ParentDir)),
            "GitHub recovery directory cannot traverse parents"
        );
        let part: Vec<_> = directory
            .components()
            .map(|component| component.as_os_str().to_string_lossy().to_ascii_lowercase())
            .collect();
        ensure!(
            !part
                .windows(4)
                .any(|part| part[0] == "forge" && part[1] == "github" && part[3] == "repos"),
            "GitHub recovery directory cannot be nested inside a deletable repository cache"
        );
        Ok(Self { directory })
    }

    pub fn directory(&self) -> &Path {
        &self.directory
    }

    /// Persists a captured intent before write admission and retains resource ownership until drop.
    ///
    /// Existing records reject new attempts regardless of host lifetime. Reservation covers the
    /// captured submission, one equally bounded response, and terminal diagnostics. No unresolved
    /// record is removed to recover admission capacity.
    pub fn prepare(
        &self,
        resource: RecoveryResource,
        capture: RecoveryCapture,
    ) -> Result<RecoveryLease> {
        resource.validate()?;
        capture.validate()?;
        let path = self.path(&resource);
        fs::create_dir_all(&self.directory)?;
        ensure!(
            !fs::symlink_metadata(&self.directory)?
                .file_type()
                .is_symlink(),
            "recovery directory cannot be a symlink"
        );
        let ownership = acquire(&self.lease_path(&resource))?;
        ensure!(
            !path.try_exists()?,
            "OutcomeUnknown: resource has an unsettled durable operation"
        );
        let _admission = acquire(&self.directory.join(".admission.lock"))?;
        let mut record = RecoveryRecord {
            version: 1,
            resource,
            capture,
            reserved_bytes: 0,
            state: RecoveryPhase::Prepared,
            confirmed_steps: Vec::new(),
        };
        let encoded = serde_json::to_vec(&record)?.len();
        record.reserved_bytes = encoded
            .checked_mul(2)
            .and_then(|bytes| bytes.checked_add(DIAGNOSTIC_RESERVATION))
            .context("recovery reservation overflow")?;
        ensure!(
            record.reserved_bytes <= MAX_RECOVERY_RECORD_BYTES,
            "recovery record exceeds 16 MiB reservation"
        );
        let (count, bytes) = self.usage()?;
        ensure!(
            count < MAX_RECOVERY_RECORDS,
            "recovery admission has 64 unsettled resources"
        );
        ensure!(
            bytes <= MAX_RECOVERY_BYTES - record.reserved_bytes,
            "recovery admission exceeds 64 MiB"
        );
        self.create_parent(&path)?;
        publish_json(&path, &record)?;
        Ok(RecoveryLease {
            store: self.clone(),
            path,
            record,
            _ownership: ownership,
        })
    }

    /// Reads the current record without dispatching, rewriting, or treating uncertain work as retryable.
    pub fn inspect(&self, resource: &RecoveryResource) -> Result<Option<RecoveryRecord>> {
        resource.validate()?;
        let path = self.path(resource);
        if !path.try_exists()? {
            return Ok(None);
        }
        let record = read_record(&path)?;
        ensure!(
            record.resource == *resource,
            "recovery resource does not match its path"
        );
        Ok(Some(record))
    }

    /// Acquires an existing attempt for verified read-only resolution or settlement acknowledgement.
    pub fn resume(&self, resource: &RecoveryResource, operation_id: &str) -> Result<RecoveryLease> {
        resource.validate()?;
        let path = self.path(resource);
        let ownership = acquire(&self.lease_path(resource))?;
        let record = read_record(&path)?;
        ensure!(
            record.resource == *resource && record.capture.operation_id == operation_id,
            "stale recovery operation identity"
        );
        Ok(RecoveryLease {
            store: self.clone(),
            path,
            record,
            _ownership: ownership,
        })
    }

    fn path(&self, resource: &RecoveryResource) -> PathBuf {
        let repository = resource.repository.repository_name();
        let (owner, name) = repository.split_once('/').expect("validated repository");
        let kind = resource.kind.label();
        self.directory
            .join(component(resource.repository.hostname()))
            .join(component(owner))
            .join(component(name))
            .join(format!("{kind}-{}.json", resource.number))
    }

    fn lease_path(&self, resource: &RecoveryResource) -> PathBuf {
        let identity = format!(
            "{}:{}:{}:{}",
            resource.repository.hostname(),
            resource.repository.repository_name(),
            resource.kind.label(),
            resource.number
        );
        let stripe = identity.bytes().fold(0xcbf29ce484222325u64, |hash, byte| {
            (hash ^ u64::from(byte)).wrapping_mul(0x100000001b3)
        }) % 64;
        self.directory.join(format!(".resource-{stripe:02}.lock"))
    }

    fn create_parent(&self, path: &Path) -> Result<()> {
        let relative = path
            .parent()
            .context("recovery path needs parent")?
            .strip_prefix(&self.directory)?;
        fs::create_dir_all(&self.directory)?;
        ensure!(
            !fs::symlink_metadata(&self.directory)?
                .file_type()
                .is_symlink(),
            "recovery directory cannot be a symlink"
        );
        let mut parent = self.directory.clone();
        for part in relative.components() {
            parent.push(part);
            match fs::create_dir(&parent) {
                Ok(()) => {}
                Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {}
                Err(error) => return Err(error.into()),
            }
            let metadata = fs::symlink_metadata(&parent)?;
            ensure!(
                metadata.is_dir() && !metadata.file_type().is_symlink(),
                "recovery parent must be an owned directory"
            );
        }
        Ok(())
    }

    fn usage(&self) -> Result<(usize, usize)> {
        let mut pending = vec![(self.directory.clone(), 0)];
        let mut count = 0;
        let mut bytes = 0usize;
        let mut visited = 0usize;
        while let Some((directory, depth)) = pending.pop() {
            for entry in fs::read_dir(directory)? {
                let entry = entry?;
                visited += 1;
                ensure!(
                    visited <= 4096,
                    "recovery directory inventory exceeds its bound"
                );
                let metadata = entry.file_type()?;
                ensure!(
                    !metadata.is_symlink(),
                    "recovery inventory contains a symlink"
                );
                if metadata.is_dir() {
                    ensure!(depth < 3, "unexpected recovery directory depth");
                    pending.push((entry.path(), depth + 1));
                } else if entry
                    .path()
                    .extension()
                    .is_some_and(|extension| extension == "json")
                {
                    count += 1;
                    ensure!(
                        count <= MAX_RECOVERY_RECORDS,
                        "recovery inventory exceeds 64 records"
                    );
                    let reservation = read_record(&entry.path())
                        .map(|record| record.reserved_bytes)
                        .unwrap_or(MAX_RECOVERY_RECORD_BYTES);
                    bytes = bytes
                        .checked_add(reservation)
                        .context("recovery inventory size overflow")?;
                }
            }
        }
        Ok((count, bytes))
    }
}

impl RecoveryLease {
    pub(crate) fn confirmed_step(&mut self, result: Value) -> Result<()> {
        ensure!(
            matches!(self.record.state, RecoveryPhase::DispatchPossible)
                && self.record.confirmed_steps.len() < 4,
            "operation cannot record another confirmed step"
        );
        let mut record = self.record.clone();
        record.confirmed_steps.push(result);
        ensure!(
            serde_json::to_vec(&record)?.len() <= record.reserved_bytes,
            "confirmed intermediate step exceeds recovery reservation"
        );
        publish_json(&self.path, &record)?;
        self.record = record;
        Ok(())
    }

    pub fn record(&self) -> &RecoveryRecord {
        &self.record
    }

    /// Persists possible dispatch before the caller invokes the remote mutation.
    pub fn dispatch_possible(&mut self) -> Result<()> {
        ensure!(
            matches!(self.record.state, RecoveryPhase::Prepared),
            "durable operation has already entered dispatch"
        );
        self.publish(RecoveryPhase::DispatchPossible)
    }

    /// Persists a validated remote settlement while preserving captured text and operation identity.
    pub fn settle(&mut self, outcome: RecoveryPhase) -> Result<()> {
        ensure!(
            matches!(
                outcome,
                RecoveryPhase::Confirmed { .. }
                    | RecoveryPhase::Rejected { .. }
                    | RecoveryPhase::OutcomeUnknown { .. }
            ),
            "invalid API settlement phase"
        );
        ensure!(
            matches!(self.record.state, RecoveryPhase::DispatchPossible)
                || (matches!(self.record.state, RecoveryPhase::Prepared)
                    && matches!(outcome, RecoveryPhase::Rejected { .. })),
            "operation is not awaiting API settlement"
        );
        self.publish(outcome)
    }

    /// Records explicit user attribution after the remote owner verifies an exact result identity.
    pub(crate) fn user_linked(&mut self, result: Value) -> Result<()> {
        let diagnostic = self.unresolved_diagnostic()?;
        self.publish(RecoveryPhase::UserLinked { result, diagnostic })
    }

    /// Retires executable intent only after explicit acknowledgement of an unproven remote outcome.
    pub(crate) fn user_closed_unknown(&mut self) -> Result<()> {
        let diagnostic = self.unresolved_diagnostic()?;
        self.publish(RecoveryPhase::UserClosedUnknown { diagnostic })
    }

    /// Removes only a terminal record whose matching draft settlement has been durably acknowledged.
    pub fn acknowledge(self, operation_id: &str) -> Result<()> {
        ensure!(
            self.record.capture.operation_id == operation_id,
            "stale recovery acknowledgement"
        );
        ensure!(
            matches!(
                self.record.state,
                RecoveryPhase::Confirmed { .. }
                    | RecoveryPhase::Rejected { .. }
                    | RecoveryPhase::UserLinked { .. }
                    | RecoveryPhase::UserClosedUnknown { .. }
            ),
            "uncertain remote outcome cannot be acknowledged as saved"
        );
        let _admission = acquire(&self.store.directory.join(".admission.lock"))?;
        fs::remove_file(&self.path)?;
        let mut parent = self.path.parent();
        while let Some(directory) = parent.filter(|directory| *directory != self.store.directory) {
            ensure!(
                directory.starts_with(&self.store.directory),
                "recovery cleanup escaped its root"
            );
            match fs::remove_dir(directory) {
                Ok(()) => parent = directory.parent(),
                Err(error)
                    if matches!(
                        error.kind(),
                        std::io::ErrorKind::DirectoryNotEmpty | std::io::ErrorKind::NotFound
                    ) =>
                {
                    break;
                }
                Err(error) => return Err(error.into()),
            }
        }
        Ok(())
    }

    fn unresolved_diagnostic(&self) -> Result<String> {
        match &self.record.state {
            RecoveryPhase::DispatchPossible => {
                Ok("host ended after dispatch became possible".into())
            }
            RecoveryPhase::OutcomeUnknown { diagnostic } => Ok(diagnostic.clone()),
            _ => bail!("operation does not have an uncertain outcome"),
        }
    }

    fn publish(&mut self, phase: RecoveryPhase) -> Result<()> {
        let mut record = self.record.clone();
        record.state = phase;
        ensure!(
            serde_json::to_vec(&record)?.len() <= record.reserved_bytes,
            "recovery settlement exceeds reserved durable bytes"
        );
        publish_json(&self.path, &record)?;
        self.record = record;
        Ok(())
    }
}

impl RecoveryResource {
    pub fn validate(&self) -> Result<()> {
        ensure!(
            if self.kind == RecoveryResourceKind::Repository {
                self.number == 0
            } else {
                self.number > 0
            },
            "invalid recovery resource number"
        );
        Ok(())
    }
}

impl RecoveryResourceKind {
    fn label(self) -> &'static str {
        match self {
            Self::Repository => "repository",
            Self::PullRequest => "pull_request",
            Self::Issue => "issue",
            Self::Notification => "notification",
        }
    }
}

impl RecoveryCapture {
    fn validate(&self) -> Result<()> {
        ensure!(
            !self.operation_id.is_empty()
                && self.operation_id.len() <= 256
                && self
                    .operation_id
                    .bytes()
                    .all(|byte| byte.is_ascii_alphanumeric()
                        || matches!(byte, b'-' | b'_' | b'.' | b':')),
            "invalid recovery operation identity"
        );
        ensure!(
            !self.actor.trim().is_empty()
                && self.actor.len() <= 256
                && !self.actor.contains(['\n', '\r', '\0']),
            "invalid captured GitHub actor"
        );
        Ok(())
    }
}

pub(crate) fn component(value: &str) -> String {
    let trailing = value.len() - value.trim_end_matches('.').len();
    format!(
        "r-{}{}",
        &value[..value.len() - trailing],
        "~2e".repeat(trailing)
    )
}

fn acquire(path: &Path) -> Result<File> {
    if let Ok(metadata) = fs::symlink_metadata(path) {
        ensure!(
            !metadata.file_type().is_symlink(),
            "recovery lease cannot be a symlink"
        );
    }
    let file = File::options()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(path)?;
    match file.try_lock() {
        Ok(()) => Ok(file),
        Err(TryLockError::WouldBlock) => bail!("Busy: another process owns the remote resource"),
        Err(TryLockError::Error(error)) => Err(error).context("acquire recovery lease"),
    }
}

fn read_record(path: &Path) -> Result<RecoveryRecord> {
    ensure!(
        !fs::symlink_metadata(path)?.file_type().is_symlink(),
        "recovery record cannot be a symlink"
    );
    let mut bytes = Vec::new();
    File::open(path)?
        .take(MAX_RECOVERY_RECORD_BYTES as u64 + 1)
        .read_to_end(&mut bytes)?;
    ensure!(
        bytes.len() <= MAX_RECOVERY_RECORD_BYTES,
        "recovery record exceeds 16 MiB"
    );
    let record: RecoveryRecord = serde_json::from_slice(&bytes)
        .context("malformed recovery record, preserved without replay")?;
    ensure!(
        record.version == 1,
        "unsupported recovery record version, preserved without replay"
    );
    record.resource.validate()?;
    record.capture.validate()?;
    ensure!(
        record.reserved_bytes >= bytes.len() && record.reserved_bytes <= MAX_RECOVERY_RECORD_BYTES,
        "invalid recovery reservation"
    );
    Ok(record)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    #[ignore]
    fn cross_process_lock_child() {
        let directory = std::env::var_os("FORGE_TEST_RECOVERY_ROOT").unwrap();
        let expected_busy = std::env::var_os("FORGE_TEST_RECOVERY_BUSY").is_some();
        let store = RecoveryStore::new(directory.into()).unwrap();
        let outcome = store.resume(&resource(1), "operation-1");
        assert_eq!(outcome.is_err(), expected_busy);
    }

    #[test]
    fn resource_lock_survives_process_boundaries_and_releases_after_owner_exit() {
        let directory = tempfile::tempdir().unwrap();
        let store = store(&directory);
        let ownership = store.prepare(resource(1), capture("operation-1")).unwrap();
        let mut child = std::process::Command::new(std::env::current_exe().unwrap());
        child
            .args([
                "--exact",
                "recovery::tests::cross_process_lock_child",
                "--ignored",
            ])
            .env("FORGE_TEST_RECOVERY_ROOT", store.directory())
            .env("FORGE_TEST_RECOVERY_BUSY", "1");
        assert!(child.status().unwrap().success());
        drop(ownership);
        child.env_remove("FORGE_TEST_RECOVERY_BUSY");
        assert!(child.status().unwrap().success());
    }

    fn store(directory: &tempfile::TempDir) -> RecoveryStore {
        RecoveryStore::new(directory.path().join("forge/recovery/github/v1")).unwrap()
    }

    fn resource(number: u64) -> RecoveryResource {
        RecoveryResource {
            repository: GithubRepositoryId::new("github.com", "owner", "repo").unwrap(),
            kind: RecoveryResourceKind::PullRequest,
            number,
        }
    }

    fn capture(id: &str) -> RecoveryCapture {
        RecoveryCapture {
            operation_id: id.into(),
            actor: "viewer".into(),
            operation: RecoveryOperation::ConversationCreate,
            edit_sequence: Some(42),
            submitted: json!({"body":"captured raw text\r\n", "parent":"PR_1"}),
        }
    }

    #[test]
    fn configuration_rejects_relative_cache_or_parent_traversal_roots_without_io() {
        assert!(RecoveryStore::new(PathBuf::from("forge/recovery/github/v1")).is_err());
        let directory = tempfile::tempdir().unwrap();
        assert!(
            RecoveryStore::new(directory.path().join("forge/github/host/repos/owner/repo"))
                .is_err()
        );
        assert!(RecoveryStore::new(directory.path().join("../forge/recovery/github/v1")).is_err());
        let configured = store(&directory);
        assert!(!configured.directory.exists());
    }

    #[test]
    fn prepared_and_possible_dispatch_survive_owner_drop_and_block_new_attempts() {
        let directory = tempfile::tempdir().unwrap();
        let store = store(&directory);
        let mut lease = store.prepare(resource(1), capture("operation-1")).unwrap();
        assert!(matches!(
            store.inspect(&resource(1)).unwrap().unwrap().state,
            RecoveryPhase::Prepared
        ));
        assert!(store.prepare(resource(1), capture("operation-2")).is_err());
        lease.dispatch_possible().unwrap();
        drop(lease);
        let restarted = RecoveryStore::new(store.directory.clone()).unwrap();
        assert!(matches!(
            restarted.inspect(&resource(1)).unwrap().unwrap().state,
            RecoveryPhase::DispatchPossible
        ));
        assert!(
            restarted
                .prepare(resource(1), capture("operation-2"))
                .is_err()
        );
        assert!(
            restarted
                .resume(&resource(1), "operation-1")
                .unwrap()
                .acknowledge("operation-1")
                .is_err()
        );
    }

    #[test]
    fn confirmation_remains_until_matching_durable_draft_acknowledgement() {
        let directory = tempfile::tempdir().unwrap();
        let store = store(&directory);
        let mut lease = store.prepare(resource(1), capture("operation-1")).unwrap();
        lease.dispatch_possible().unwrap();
        lease
            .settle(RecoveryPhase::Confirmed {
                result: json!({"id":"comment-1"}),
            })
            .unwrap();
        assert_eq!(lease.record().capture.edit_sequence, Some(42));
        drop(lease);
        assert!(store.prepare(resource(1), capture("operation-2")).is_err());
        assert!(store.resume(&resource(1), "wrong-operation").is_err());
        store
            .resume(&resource(1), "operation-1")
            .unwrap()
            .acknowledge("operation-1")
            .unwrap();
        assert!(store.inspect(&resource(1)).unwrap().is_none());
        assert!(store.prepare(resource(1), capture("operation-2")).is_ok());
    }

    #[test]
    fn cache_deletion_cannot_remove_uncertain_captured_text() {
        let directory = tempfile::tempdir().unwrap();
        let store = store(&directory);
        let cache = directory
            .path()
            .join("forge/github/github.com/repos/owner/repo");
        fs::create_dir_all(&cache).unwrap();
        fs::write(cache.join("review.json"), "draft").unwrap();
        let mut lease = store.prepare(resource(1), capture("operation-1")).unwrap();
        lease.dispatch_possible().unwrap();
        lease
            .settle(RecoveryPhase::OutcomeUnknown {
                diagnostic: "connection ended after dispatch".into(),
            })
            .unwrap();
        drop(lease);
        fs::remove_file(cache.join("review.json")).unwrap();
        fs::remove_dir(cache).unwrap();
        let restored = store.inspect(&resource(1)).unwrap().unwrap();
        assert_eq!(restored.capture.submitted["body"], "captured raw text\r\n");
        assert!(matches!(
            restored.state,
            RecoveryPhase::OutcomeUnknown { .. }
        ));
    }

    #[test]
    fn malformed_and_future_version_records_are_preserved_and_block_resource_writes() {
        let directory = tempfile::tempdir().unwrap();
        let store = store(&directory);
        drop(store.prepare(resource(1), capture("operation-1")).unwrap());
        let path = store.path(&resource(1));
        fs::write(&path, b"{broken").unwrap();
        assert!(store.inspect(&resource(1)).is_err());
        assert!(store.prepare(resource(1), capture("operation-2")).is_err());
        assert_eq!(fs::read(&path).unwrap(), b"{broken");
        let mut future = store
            .prepare(resource(2), capture("operation-2"))
            .unwrap()
            .record()
            .clone();
        future.version = 2;
        let future_bytes = serde_json::to_vec(&future).unwrap();
        let future_path = store.path(&resource(2));
        fs::write(&future_path, &future_bytes).unwrap();
        assert!(store.inspect(&resource(2)).is_err());
        assert!(store.prepare(resource(2), capture("operation-3")).is_err());
        assert_eq!(fs::read(&future_path).unwrap(), future_bytes);
    }

    #[test]
    fn explicit_user_resolution_retains_original_diagnostic_and_capture_until_ack() {
        let directory = tempfile::tempdir().unwrap();
        let store = store(&directory);
        let mut lease = store.prepare(resource(1), capture("operation-1")).unwrap();
        lease.dispatch_possible().unwrap();
        lease
            .settle(RecoveryPhase::OutcomeUnknown {
                diagnostic: "original remote failure".into(),
            })
            .unwrap();
        lease
            .user_linked(json!({"id":"user-selected-comment"}))
            .unwrap();
        assert!(
            matches!(&lease.record().state, RecoveryPhase::UserLinked { diagnostic, .. } if diagnostic == "original remote failure")
        );
        lease.acknowledge("operation-1").unwrap();
        let mut next = store.prepare(resource(1), capture("operation-2")).unwrap();
        next.dispatch_possible().unwrap();
        next.user_closed_unknown().unwrap();
        assert_eq!(
            next.record().capture.submitted["body"],
            "captured raw text\r\n"
        );
        assert!(matches!(
            next.record().state,
            RecoveryPhase::UserClosedUnknown { .. }
        ));
    }

    #[test]
    fn resource_record_limit_rejects_before_creating_another_record() {
        let directory = tempfile::tempdir().unwrap();
        let store = store(&directory);
        for number in 1..=64 {
            drop(
                store
                    .prepare(resource(number), capture(&format!("operation-{number}")))
                    .unwrap(),
            );
        }
        assert!(
            store
                .prepare(resource(65), capture("operation-65"))
                .is_err()
        );
        assert!(store.inspect(&resource(65)).unwrap().is_none());
        assert_eq!(store.usage().unwrap().0, 64);
    }

    #[test]
    fn windows_reserved_names_and_trailing_dots_do_not_alias_recovery_paths() {
        let directory = tempfile::tempdir().unwrap();
        let store = store(&directory);
        let mut first = resource(1);
        first.repository = GithubRepositoryId::new("github.com", "con", "repo.").unwrap();
        let mut second = first.clone();
        second.repository = GithubRepositoryId::new("github.com", "con", "repo").unwrap();
        assert_ne!(store.path(&first), store.path(&second));
        drop(store.prepare(first, capture("first")).unwrap());
        assert!(store.prepare(second, capture("second")).is_ok());
    }
}
