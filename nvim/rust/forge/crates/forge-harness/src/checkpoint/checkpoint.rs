use crate::storage::objects::ObjectStore;
use anyhow::{Context, Result};
use std::collections::BTreeMap;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
#[cfg(test)]
use std::path::Path;
use std::sync::Arc;

use forge_diff::engine::{DiffEngine, DiffRequest};
use forge_diff::source::{
    MAX_SOURCE_BYTES, Representation, SourceError, SourcePair, SourceVersion,
};
use forge_diff::unified::{write_file_header, write_unified};
use forge_diff::workers::WorkPriority;
use forge_git::coordinator::AdmissionGuard;
use forge_git::mutation::{MutationScope, OperationCompletion, OperationId};
use forge_git::read_pool::{BlockingReadPool, ReadCancellation};
use forge_git::repository::{RepositoryGeneration, RepositoryState};
use forge_git::store::RepositoryStore;

use super::manifest::ResolvedFile;
use super::manifest::{CheckpointFile, CheckpointRecord};
use super::restore::{RestoreJournal, RestorePreview};
/// Owns Git-backed worktree checkpoint capture and safe rollback.
pub struct GitCheckpoint {
    workspace: PathBuf,
}

struct CheckpointCapture {
    checkpoint: GitCheckpoint,
    session_id: String,
    now_ms: i64,
    input_bytes: usize,
    repository: Arc<RepositoryState>,
    generation: RepositoryGeneration,
    // Dispose copied inputs before acknowledging their retained-byte receipt.
    admission: CaptureAdmission,
}

struct CaptureAdmission {
    repositories: Arc<RepositoryStore>,
    operation: OperationId,
    guard: Option<AdmissionGuard>,
}

impl CheckpointCapture {
    fn run(
        mut self,
        objects: &ObjectStore,
        cancellation: ReadCancellation,
    ) -> Result<(CheckpointRecord, Self)> {
        let result = self
            .checkpoint
            .capture_native(objects, &self.session_id, self.now_ms, || {
                self.check(&cancellation)
            })
            .and_then(|record| {
                self.check(&cancellation)?;
                Ok(record)
            });
        self.admission
            .guard
            .take()
            .context("checkpoint capture lost admission")?
            .finish(if result.is_ok() {
                OperationCompletion::Completed
            } else {
                OperationCompletion::Failed
            })?;
        Ok((result?, self))
    }

    fn check(&self, cancellation: &ReadCancellation) -> Result<()> {
        cancellation.check()?;
        anyhow::ensure!(
            !self
                .admission
                .guard
                .as_ref()
                .context("checkpoint capture has no admission")?
                .cancellation_requested(),
            "checkpoint capture cancelled"
        );
        anyhow::ensure!(
            self.generation == self.repository.generation(),
            "checkpoint capture was superseded by repository invalidation"
        );
        Ok(())
    }
}

impl Drop for CaptureAdmission {
    fn drop(&mut self) {
        // Capture only publishes immutable objects, so abandonment cannot leave a worktree mutation.
        if let Some(guard) = self.guard.take() {
            let _ = guard.finish(OperationCompletion::Failed);
        }
        let _ = self.repositories.writes.cancel(self.operation);
        self.repositories.writes.take_receipt(self.operation);
    }
}

#[cfg(test)]
pub(crate) struct CheckpointRestore {
    checkpoint: GitCheckpoint,
    expected: CheckpointRecord,
    target: CheckpointRecord,
    repository: Arc<RepositoryState>,
    repositories: Arc<RepositoryStore>,
    operation: OperationId,
    guard: Option<AdmissionGuard>,
    started: bool,
    acknowledge: bool,
}

#[cfg(test)]
impl CheckpointRestore {
    /// Runs existing source checks and filesystem restoration while retaining every mutation scope.
    pub(crate) async fn restore(mut self, objects: ObjectStore) -> Result<()> {
        let waiter = RestoreWait {
            repositories: Arc::clone(&self.repositories),
            operation: self.operation,
        };
        self.acknowledge = false;
        let (mut owner, result) = tokio::task::spawn_blocking(move || {
            let result = self.restore_owned(&objects);
            (self, result)
        })
        .await
        .context("checkpoint restore worker failed")?;
        owner.acknowledge = true;
        drop(waiter);
        result
    }

    fn restore_owned(&mut self, objects: &ObjectStore) -> Result<()> {
        anyhow::ensure!(
            !self
                .guard
                .as_ref()
                .context("checkpoint restore has no admission")?
                .cancellation_requested(),
            "checkpoint restore cancelled before execution"
        );
        self.started = true;
        let result = self
            .checkpoint
            .restore(objects, &self.expected, &self.target);
        let invalidation = self.repository.invalidate();
        let completion = self
            .guard
            .take()
            .context("checkpoint restore has no admission")?
            .finish(if result.is_ok() {
                OperationCompletion::Completed
            } else {
                OperationCompletion::Uncertain
            });
        result?;
        invalidation?;
        completion?;
        Ok(())
    }
}

#[cfg(test)]
impl Drop for CheckpointRestore {
    fn drop(&mut self) {
        if !self.started
            && let Some(guard) = self.guard.take()
        {
            let _ = guard.finish(OperationCompletion::Failed);
        }
        let _ = self.repositories.writes.cancel(self.operation);
        if self.acknowledge || !self.started {
            self.repositories.writes.take_receipt(self.operation);
        }
    }
}

impl GitCheckpoint {
    /// Applies a confirmed preview under repository mutation ownership.
    pub(crate) async fn apply_preview(
        self,
        repositories: Arc<RepositoryStore>,
        objects: ObjectStore,
        preview: RestorePreview,
        resume: bool,
    ) -> Result<RestoreJournal> {
        let repository = repositories
            .open(self.workspace.clone())
            .await?
            .context("restore repository is missing")?;
        let operation = repositories
            .writes
            .admit(checkpoint_scopes(&repository)?, preview.retained_bytes()?)?;
        let mut admission = CaptureAdmission {
            repositories: Arc::clone(&repositories),
            operation,
            guard: None,
        };
        admission.guard = Some(repositories.writes.wait_start(operation)?.await?);
        let waiter = RestoreWait {
            repositories: Arc::clone(&repositories),
            operation,
        };
        let result = tokio::task::spawn_blocking(move || {
            let result = (|| {
                let mut journal = if resume {
                    let journal = RestoreJournal::load(&objects, &preview.session_id)?
                        .context("restore journal is missing")?;
                    anyhow::ensure!(journal.preview.id == preview.id, "restore journal changed");
                    journal
                } else {
                    if let Some(journal) = RestoreJournal::load(&objects, &preview.session_id)? {
                        anyhow::ensure!(
                            journal.state == super::restore::RestoreState::Complete
                                || preview.recovery_of.as_deref()
                                    == Some(journal.preview.id.as_str()),
                            "an interrupted restore requires recovery"
                        );
                        anyhow::ensure!(
                            journal.preview.id != preview.id,
                            "restore preview was already applied"
                        );
                    }
                    RestoreJournal::begin(&objects, preview)?
                };
                journal.apply(&objects, &mut || {
                    anyhow::ensure!(
                        !admission.guard.as_ref().unwrap().cancellation_requested(),
                        "restore cancelled; recovery required"
                    );
                    Ok(())
                })?;
                Ok(journal)
            })();
            let _ = repository.invalidate();
            let completion = admission.guard.take().unwrap().finish(if result.is_ok() {
                OperationCompletion::Completed
            } else {
                OperationCompletion::Uncertain
            });
            completion?;
            result
        })
        .await
        .context("restore worker failed")?;
        drop(waiter);
        repositories.writes.take_receipt(operation);
        result
    }

    /// Acquires all scopes observed by rollback before validating or changing workspace files.
    #[cfg(test)]
    pub(crate) async fn admit_restore(
        self,
        repositories: Arc<RepositoryStore>,
        expected: CheckpointRecord,
        target: CheckpointRecord,
    ) -> Result<CheckpointRestore> {
        let input_bytes = checkpoint_retained_bytes(&expected)?
            .checked_add(checkpoint_retained_bytes(&target)?)
            .context("checkpoint input size overflow")?;
        let repository = repositories
            .open(self.workspace.clone())
            .await?
            .context("checkpoint repository is missing")?;
        let operation = repositories
            .writes
            .admit(checkpoint_scopes(&repository)?, input_bytes)?;
        let mut restore = CheckpointRestore {
            checkpoint: self,
            expected,
            target,
            repository,
            repositories: Arc::clone(&repositories),
            operation,
            guard: None,
            started: false,
            acknowledge: true,
        };
        restore.guard = Some(repositories.writes.wait_start(operation)?.await?);
        Ok(restore)
    }

    /// Build a checkpoint adapter for one exact worktree root.
    pub fn new(workspace: impl Into<PathBuf>) -> Self {
        Self {
            workspace: workspace.into(),
        }
    }

    fn file_path(&self, relative: &str) -> Result<PathBuf> {
        super::restore::checked_path(&self.workspace, relative)
    }
}

impl GitCheckpoint {
    /// Captures tracked and nonignored files under shared repository scopes and blocking admission.
    ///
    /// Discovery resolves the canonical worktree before admitting its index, file, and shared-ref
    /// scopes. Capacity and closure errors reject before checkpoint file acquisition. Observed
    /// repository invalidation rejects the result. Dropping the waiter requests cancellation and
    /// retains native ownership until acquisition exits. Cancellation can leave unreferenced
    /// immutable objects, but returns no checkpoint for publication. File I/O already in progress
    /// is not interrupted, while active Git children receive a termination request.
    pub async fn capture(
        &self,
        objects: &ObjectStore,
        repositories: &Arc<RepositoryStore>,
        session_id: &str,
        now_ms: i64,
    ) -> Result<CheckpointRecord> {
        let capture = self
            .admit_capture(Arc::clone(repositories), session_id.to_owned(), now_ms)
            .await?;
        let objects = objects.clone();
        let (record, _capture) = repositories
            .reads
            .submit(capture.input_bytes, move |cancellation| {
                capture.run(&objects, cancellation)
            })?
            .finish()
            .await?;
        Ok(record)
    }

    async fn admit_capture(
        &self,
        repositories: Arc<RepositoryStore>,
        session_id: String,
        now_ms: i64,
    ) -> Result<CheckpointCapture> {
        let repository = repositories
            .open(self.workspace.clone())
            .await?
            .context("checkpoint repository is missing")?;
        let checkpoint = Self::new(
            repository
                .identity
                .worktree_root
                .clone()
                .context("checkpoint requires a worktree")?,
        );
        let input_bytes = checkpoint
            .workspace
            .capacity()
            .checked_add(session_id.capacity())
            .context("checkpoint capture input size overflow")?;
        let operation = repositories
            .writes
            .admit(checkpoint_scopes(&repository)?, input_bytes)?;
        let mut capture = CheckpointCapture {
            checkpoint,
            session_id,
            now_ms,
            input_bytes,
            generation: repository.generation(),
            repository,
            admission: CaptureAdmission {
                repositories: Arc::clone(&repositories),
                operation,
                guard: None,
            },
        };
        capture.admission.guard = Some(repositories.writes.wait_start(operation)?.await?);
        capture.generation = capture.repository.generation();
        Ok(capture)
    }

    fn capture_native(
        &self,
        objects: &ObjectStore,
        session_id: &str,
        now_ms: i64,
        mut check: impl FnMut() -> Result<()>,
    ) -> Result<CheckpointRecord> {
        check()?;
        let started = std::time::Instant::now();
        let inventory = forge_git::checkpoint::inventory(&self.workspace, &mut check)?;
        let mut file = Vec::new();
        let mut deleted = Vec::new();
        let mut hits = 0usize;
        let mut bytes = 0u64;
        for (relative, clean) in &inventory.candidate {
            check()?;
            if inventory.missing.contains(relative) {
                deleted.push(relative.clone());
                continue;
            }
            if *clean {
                continue;
            }
            let path = self.file_path(relative)?;
            let metadata = match fs::symlink_metadata(&path) {
                Ok(metadata) => metadata,
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                    if inventory.base.contains_key(relative) {
                        deleted.push(relative.clone());
                    }
                    continue;
                }
                Err(error) => return Err(error.into()),
            };
            if metadata.is_dir() {
                if inventory
                    .base
                    .get(relative)
                    .is_some_and(|base| base.mode == 0o160000)
                {
                    continue;
                }
                if inventory.base.contains_key(relative) {
                    deleted.push(relative.clone());
                }
                continue;
            }
            anyhow::ensure!(
                metadata.is_file() || metadata.is_symlink(),
                "unsupported checkpoint file kind: {relative}"
            );
            #[allow(unused_mut)]
            let mut mode = super::restore::file_mode(&metadata);
            #[cfg(windows)]
            if mode == 0o100644
                && inventory
                    .base
                    .get(relative)
                    .is_some_and(|base| base.mode == 0o100755)
            {
                mode = 0o100755;
            }
            let base = inventory
                .base
                .get(relative)
                .filter(|base| base.mode == mode && base.checkout.unsupported.is_none());
            let (identity, reused) = if metadata.is_symlink() {
                let target = fs::read_link(&path)?;
                let content = target.as_os_str().as_encoded_bytes();
                let digest = crate::plan::digest(content);
                let unchanged = base
                    .map(|base| objects.matches_baseline(&self.workspace, base, &digest))
                    .transpose()?
                    .unwrap_or(false);
                (
                    if unchanged {
                        None
                    } else {
                        Some(objects.put(content)?)
                    },
                    false,
                )
            } else {
                objects
                    .capture_file(&path, &self.workspace, base, &mut check)
                    .with_context(|| format!("capture checkpoint source {}", path.display()))?
            };
            if reused {
                hits += 1;
            } else {
                bytes += metadata.len();
            }
            let Some(identity) = identity else {
                continue;
            };
            file.push(CheckpointFile {
                path: relative.clone(),
                object_id: identity,
                mode,
            });
        }
        check()?;
        let verified = forge_git::checkpoint::inventory(&self.workspace, &mut check)?;
        anyhow::ensure!(
            inventory == verified,
            "workspace changed during checkpoint capture"
        );
        let head = inventory.head.unwrap_or_else(|| "UNBORN".into());
        let checkout = inventory
            .base
            .into_iter()
            .filter_map(|(path, base)| {
                (base.checkout != Default::default()).then_some((path, base.checkout))
            })
            .collect::<BTreeMap<_, _>>();
        let manifest =
            serde_json::to_vec(&(session_id, &inventory.tree, &file, &deleted, &checkout))?;
        eprintln!(
            "checkpoint.capture candidates={} overrides={} cache_hits={} bytes_read={} duration_ms={}",
            inventory.candidate.len(),
            file.len(),
            hits,
            bytes,
            started.elapsed().as_millis()
        );
        Ok(CheckpointRecord {
            id: crate::plan::digest(&manifest),
            session_id: session_id.into(),
            workspace: self.workspace.to_string_lossy().into_owned(),
            head,
            tree: inventory.tree,
            file,
            deleted,
            checkout,
            created_at_ms: now_ms,
        })
    }

    /// Write a target checkpoint after validating the expected current checkpoint.
    #[cfg(test)]
    fn restore(
        &self,
        objects: &ObjectStore,
        expected_current: &CheckpointRecord,
        target: &CheckpointRecord,
    ) -> Result<()> {
        let preview = RestorePreview::prepare(objects, &self.workspace, expected_current, target)?;
        anyhow::ensure!(
            preview.warning.is_empty(),
            "workspace diverged after the interaction completed"
        );
        let mut journal = RestoreJournal::begin(objects, preview)?;
        journal.apply(objects, &mut || Ok(()))
    }
}

fn checkpoint_scopes(repository: &RepositoryState) -> Result<Vec<MutationScope>> {
    let worktree = repository
        .identity
        .worktree
        .clone()
        .context("checkpoint requires a worktree")?;
    Ok(vec![
        MutationScope::Index(worktree.clone()),
        MutationScope::WorktreeFiles(worktree),
        MutationScope::SharedRefs(repository.identity.storage.clone()),
    ])
}

#[cfg(test)]
fn checkpoint_retained_bytes(record: &CheckpointRecord) -> Result<usize> {
    let mut bytes = record
        .file
        .capacity()
        .checked_mul(std::mem::size_of::<CheckpointFile>())
        .and_then(|bytes| bytes.checked_add(std::mem::size_of::<CheckpointRecord>()))
        .context("checkpoint input size overflow")?;
    for value in [
        &record.id,
        &record.session_id,
        &record.workspace,
        &record.head,
    ]
    .into_iter()
    .chain(
        record
            .file
            .iter()
            .flat_map(|file| [&file.path, &file.object_id]),
    ) {
        bytes = bytes
            .checked_add(value.capacity())
            .context("checkpoint input size overflow")?;
    }
    Ok(bytes)
}

/// Build the exact per-interaction diff between two durable checkpoints.
pub async fn checkpoint_diff(
    objects: &ObjectStore,
    reads: &BlockingReadPool,
    engine: &Arc<DiffEngine>,
    before: &CheckpointRecord,
    after: &CheckpointRecord,
) -> Result<String> {
    checkpoint_diff_matching(objects, reads, engine, before, after, None).await
}

/// Build the exact per-interaction diff for one selected set of normalized paths.
pub async fn checkpoint_diff_for_paths(
    objects: &ObjectStore,
    reads: &BlockingReadPool,
    engine: &Arc<DiffEngine>,
    before: &CheckpointRecord,
    after: &CheckpointRecord,
    path_set: &std::collections::BTreeSet<String>,
) -> Result<String> {
    checkpoint_diff_matching(objects, reads, engine, before, after, Some(path_set)).await
}

async fn checkpoint_diff_matching(
    objects: &ObjectStore,
    reads: &BlockingReadPool,
    engine: &Arc<DiffEngine>,
    before: &CheckpointRecord,
    after: &CheckpointRecord,
    selected_path_set: Option<&std::collections::BTreeSet<String>>,
) -> Result<String> {
    let before_file = before.resolved()?;
    let after_file = after.resolved()?;
    let mut path_set = std::collections::BTreeSet::new();
    path_set.extend(before_file.keys().cloned());
    path_set.extend(after_file.keys().cloned());
    let mut output = Vec::new();
    for path in path_set {
        if selected_path_set.is_some_and(|selected| !selected.contains(&path)) {
            continue;
        }
        let before_id = before_file.get(&path);
        let after_id = after_file.get(&path);
        if before_id == after_id {
            continue;
        }
        let before_name = format!("a/{path}");
        let after_name = format!("b/{path}");
        write_file_header(&before_name, &after_name, &mut output)?;
        let pair = match checkpoint_source(objects, reads, &before.workspace, before_id, after_id)
            .await?
        {
            CheckpointSource::Text(pair) => pair,
            CheckpointSource::TooLarge => {
                writeln!(
                    output,
                    "Diff unavailable: checkpoint source exceeds {MAX_SOURCE_BYTES} bytes"
                )?;
                continue;
            }
            CheckpointSource::Binary => {
                writeln!(output, "Binary files a/{path} and b/{path} differ")?;
                continue;
            }
            CheckpointSource::UnsupportedEncoding => {
                output.write_all(b"Diff unavailable: checkpoint source is not valid UTF-8\n")?;
                continue;
            }
        };
        let diff = engine
            .compare(DiffRequest {
                source: pair,
                priority: WorkPriority::Foreground,
            })
            .await
            .map_err(|error| anyhow::anyhow!("checkpoint comparison failed: {error:?}"))?;
        let before_header = if before_id.is_some() {
            before_name.as_str()
        } else {
            "/dev/null"
        };
        let after_header = if after_id.is_some() {
            after_name.as_str()
        } else {
            "/dev/null"
        };
        write_unified(&diff, before_header, after_header, 3, &mut output)?;
    }
    String::from_utf8(output).context("checkpoint diff output is not UTF-8")
}

#[derive(Debug)]
enum CheckpointSource {
    Text(SourcePair),
    TooLarge,
    Binary,
    UnsupportedEncoding,
}

async fn checkpoint_source(
    objects: &ObjectStore,
    reads: &BlockingReadPool,
    workspace: &str,
    before_id: Option<&ResolvedFile>,
    after_id: Option<&ResolvedFile>,
) -> Result<CheckpointSource> {
    let before_id = before_id.cloned();
    let after_id = after_id.cloned();
    let workspace = PathBuf::from(workspace);
    let input_bytes = workspace.capacity() + 1024;
    let objects = objects.clone();
    reads
        .submit(input_bytes, move |cancellation| {
            let before_content = match before_id.as_ref() {
                Some(identity) => identity.read(&objects, &workspace, MAX_SOURCE_BYTES)?,
                None => Some(Vec::new()),
            };
            cancellation.check()?;
            let after_content = match after_id.as_ref() {
                Some(identity) => identity.read(&objects, &workspace, MAX_SOURCE_BYTES)?,
                None => Some(Vec::new()),
            };
            cancellation.check()?;
            let (Some(before_content), Some(after_content)) = (before_content, after_content)
            else {
                return Ok(CheckpointSource::TooLarge);
            };
            let old = SourceVersion::new(before_content, Representation::Raw);
            cancellation.check()?;
            let new = SourceVersion::new(after_content, Representation::Raw);
            match (old, new) {
                (Ok(old), Ok(new)) => Ok(CheckpointSource::Text(SourcePair { old, new })),
                (Err(SourceError::Binary), _) | (_, Err(SourceError::Binary)) => {
                    Ok(CheckpointSource::Binary)
                }
                (Err(SourceError::UnsupportedEncoding), _)
                | (_, Err(SourceError::UnsupportedEncoding)) => {
                    Ok(CheckpointSource::UnsupportedEncoding)
                }
                (Err(error), _) | (_, Err(error)) => Err(error.into()),
            }
        })?
        .finish()
        .await
}

struct RestoreWait {
    repositories: Arc<RepositoryStore>,
    operation: OperationId,
}

impl Drop for RestoreWait {
    fn drop(&mut self) {
        let _ = self.repositories.writes.cancel(self.operation);
    }
}

#[cfg(test)]
#[path = "test.rs"]
mod test;
