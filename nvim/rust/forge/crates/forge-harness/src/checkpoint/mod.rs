use crate::storage::objects::ObjectStore;
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use forge_diff::engine::{DiffEngine, DiffRequest};
use forge_diff::source::{
    MAX_SOURCE_BYTES, Representation, SourceError, SourcePair, SourceVersion,
};
use forge_diff::unified::{write_file_header, write_unified};
use forge_diff::workers::WorkPriority;
use forge_git::command::{CommandLimits, read_command};
use forge_git::coordinator::AdmissionGuard;
use forge_git::mutation::{MutationScope, OperationCompletion, OperationId};
use forge_git::read_pool::{BlockingReadPool, ReadCancellation};
use forge_git::repository::{RepositoryGeneration, RepositoryState};
use forge_git::store::RepositoryStore;

const CHECKPOINT_COMMAND_LIMITS: CommandLimits = CommandLimits {
    stdout_bytes: 16 * 1024 * 1024,
    stderr_bytes: 64 * 1024,
    timeout: std::time::Duration::from_secs(30),
};

/// Represents one nonignored workspace file stored by content digest.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CheckpointFile {
    pub path: String,
    pub object_id: String,
}

/// Represents a complete worktree checkpoint without mutating Git history or index state.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CheckpointRecord {
    pub id: String,
    pub session_id: String,
    pub workspace: String,
    pub head: String,
    pub index_digest: String,
    pub file: Vec<CheckpointFile>,
    pub created_at_ms: i64,
}

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
    /// Acquires all scopes observed by rollback before validating or changing workspace files.
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

    fn git_output(&self, args: &[&str], check: impl FnMut() -> Result<()>) -> Result<Vec<u8>> {
        let output = self.git_command(args, check)?;
        anyhow::ensure!(
            output.status.success(),
            "git {} failed: {}",
            args.join(" "),
            String::from_utf8_lossy(&output.stderr).trim()
        );
        Ok(output.stdout)
    }

    fn git_command(
        &self,
        args: &[&str],
        check: impl FnMut() -> Result<()>,
    ) -> Result<std::process::Output> {
        let mut command = std::process::Command::new("git");
        command
            .args([
                "--no-pager",
                "--no-optional-locks",
                "-c",
                "core.fsmonitor=false",
            ])
            .args(args)
            .current_dir(&self.workspace);
        read_command(&mut command, CHECKPOINT_COMMAND_LIMITS, check)
            .with_context(|| format!("run git {}", args.join(" ")))
    }

    fn file_path(&self, relative: &str) -> Result<PathBuf> {
        anyhow::ensure!(
            Path::new(relative).components().all(|component| matches!(
                component,
                std::path::Component::Normal(_) | std::path::Component::CurDir
            )),
            "checkpoint path escaped workspace: {relative}"
        );
        Ok(self.workspace.join(relative))
    }

    fn head(&self, mut check: impl FnMut() -> Result<()>) -> Result<String> {
        let output = self.git_command(&["rev-parse", "--verify", "HEAD"], &mut check)?;
        if output.status.success() {
            return Ok(String::from_utf8(output.stdout)?.trim().to_owned());
        }
        self.git_output(&["rev-parse", "--is-inside-work-tree"], check)?;
        Ok("UNBORN".into())
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
        let head = self.head(&mut check)?;
        check()?;
        let index_digest =
            crate::plan::digest(&self.git_output(&["ls-files", "-z", "--stage"], &mut check)?);
        check()?;
        let listed = self.git_output(
            &[
                "ls-files",
                "-z",
                "--cached",
                "--others",
                "--exclude-standard",
            ],
            &mut check,
        )?;
        let mut file = Vec::new();
        for entry in listed
            .split(|byte| *byte == 0)
            .filter(|entry| !entry.is_empty())
        {
            check()?;
            let relative =
                String::from_utf8(entry.to_vec()).context("decode Git workspace path")?;
            let path = self.file_path(&relative)?;
            if !path.is_file() {
                continue;
            }
            file.push(CheckpointFile {
                path: relative,
                object_id: objects.put_file(&path)?,
            });
        }
        check()?;
        file.sort_by(|left, right| left.path.cmp(&right.path));
        let manifest = serde_json::to_vec(&(session_id, &head, &index_digest, &file))?;
        Ok(CheckpointRecord {
            id: crate::plan::digest(&manifest),
            session_id: session_id.into(),
            workspace: self.workspace.to_string_lossy().into_owned(),
            head,
            index_digest,
            file,
            created_at_ms: now_ms,
        })
    }

    /// Write a target checkpoint after validating the expected current checkpoint.
    fn restore(
        &self,
        objects: &ObjectStore,
        expected_current: &CheckpointRecord,
        target: &CheckpointRecord,
    ) -> Result<()> {
        anyhow::ensure!(
            expected_current.workspace == target.workspace,
            "rollback crossed a worktree boundary"
        );
        anyhow::ensure!(
            fs::canonicalize(&self.workspace)? == fs::canonicalize(&expected_current.workspace)?,
            "rollback checkpoint does not belong to the admitted worktree"
        );
        let actual = self.capture_native(
            objects,
            &expected_current.session_id,
            expected_current.created_at_ms,
            || Ok(()),
        )?;
        anyhow::ensure!(
            actual.head == expected_current.head,
            "HEAD changed after the interaction completed"
        );
        anyhow::ensure!(
            actual.index_digest == expected_current.index_digest,
            "Git index changed after the interaction completed"
        );
        anyhow::ensure!(
            actual.id == expected_current.id,
            "workspace diverged after the interaction completed"
        );
        anyhow::ensure!(
            target.head == expected_current.head,
            "rollback would cross a HEAD boundary"
        );
        anyhow::ensure!(
            target.index_digest == expected_current.index_digest,
            "rollback would cross an index boundary"
        );

        let actual_file: BTreeMap<_, _> = actual
            .file
            .iter()
            .map(|item| (&item.path, &item.object_id))
            .collect();
        let target_file: BTreeMap<_, _> = target
            .file
            .iter()
            .map(|item| (&item.path, &item.object_id))
            .collect();
        for relative in actual_file
            .keys()
            .filter(|path| !target_file.contains_key(*path))
        {
            self.file_path(relative)?;
        }
        for item in &target.file {
            if actual_file.get(&item.path) != Some(&&item.object_id) {
                self.file_path(&item.path)?;
                objects.verify(&item.object_id)?;
            }
        }
        for relative in actual_file
            .keys()
            .filter(|path| !target_file.contains_key(*path))
        {
            let path = self.file_path(relative)?;
            if path.exists() {
                fs::remove_file(&path)
                    .with_context(|| format!("remove rollback file {}", path.display()))?;
            }
        }
        for item in &target.file {
            if actual_file.get(&item.path) == Some(&&item.object_id) {
                continue;
            }
            let path = self.file_path(&item.path)?;
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent)?;
            }
            objects
                .restore_file(&item.object_id, &path)
                .with_context(|| format!("restore rollback file {}", path.display()))?;
        }
        Ok(())
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
        &record.index_digest,
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
    let before_file: BTreeMap<_, _> = before
        .file
        .iter()
        .map(|item| (item.path.as_str(), item.object_id.as_str()))
        .collect();
    let after_file: BTreeMap<_, _> = after
        .file
        .iter()
        .map(|item| (item.path.as_str(), item.object_id.as_str()))
        .collect();
    let mut path_set = std::collections::BTreeSet::new();
    path_set.extend(before_file.keys().copied());
    path_set.extend(after_file.keys().copied());
    let mut output = Vec::new();
    for path in path_set {
        if selected_path_set.is_some_and(|selected| !selected.contains(path)) {
            continue;
        }
        let before_id = before_file.get(path).copied();
        let after_id = after_file.get(path).copied();
        if before_id == after_id {
            continue;
        }
        let before_name = format!("a/{path}");
        let after_name = format!("b/{path}");
        write_file_header(&before_name, &after_name, &mut output)?;
        let pair = match checkpoint_source(objects, reads, before_id, after_id).await? {
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
    before_id: Option<&str>,
    after_id: Option<&str>,
) -> Result<CheckpointSource> {
    let before_id = before_id.map(str::to_owned);
    let after_id = after_id.map(str::to_owned);
    let input_bytes = before_id.as_ref().map_or(0, String::capacity)
        + after_id.as_ref().map_or(0, String::capacity);
    let objects = objects.clone();
    reads
        .submit(input_bytes, move |cancellation| {
            let before_content = match before_id.as_deref() {
                Some(identity) => objects.get(identity, MAX_SOURCE_BYTES)?,
                None => Some(Vec::new()),
            };
            cancellation.check()?;
            let after_content = match after_id.as_deref() {
                Some(identity) => objects.get(identity, MAX_SOURCE_BYTES)?,
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
mod test {
    use super::*;
    use crate::storage::SqliteStore;
    use std::collections::BTreeSet;

    #[tokio::test]
    async fn capture_waits_for_each_repository_scope_before_reading() {
        use std::task::Poll;
        use std::time::Duration;
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let repositories = Arc::new(RepositoryStore::default());
        let handle = repositories
            .open(repository.path().to_owned())
            .await
            .unwrap()
            .unwrap();
        let checkpoint = GitCheckpoint::new(repository.path());
        for (sequence, scope) in checkpoint_scopes(&handle).unwrap().into_iter().enumerate() {
            let predecessor = repositories.writes.admit(vec![scope], 0).unwrap();
            let running = repositories.writes.start(predecessor).unwrap().unwrap();
            let mut capture =
                Box::pin(checkpoint.capture(&store.objects, &repositories, "session", 1));
            tokio::time::timeout(
                Duration::from_secs(2),
                std::future::poll_fn(|context| {
                    assert!(capture.as_mut().poll(context).is_pending());
                    if repositories.writes.usage().operations == 2 {
                        Poll::Ready(())
                    } else {
                        context.waker().wake_by_ref();
                        Poll::Pending
                    }
                }),
            )
            .await
            .unwrap();
            assert_eq!(repositories.reads.status().active_jobs, 0);
            let content = format!("scope predecessor {sequence}\n");
            fs::write(repository.path().join("tracked.txt"), &content).unwrap();
            running.finish(OperationCompletion::Completed).unwrap();
            repositories.writes.take_receipt(predecessor).unwrap();
            let captured = capture.await.unwrap();
            let tracked = captured
                .file
                .iter()
                .find(|file| file.path == "tracked.txt")
                .unwrap();
            assert_eq!(
                store
                    .objects
                    .get(&tracked.object_id, 4096)
                    .unwrap()
                    .unwrap(),
                content.as_bytes()
            );
            assert_eq!(repositories.writes.usage().operations, 0);
            assert_eq!(repositories.writes.usage().input_bytes, 0);
        }
    }

    #[tokio::test]
    async fn dropping_capture_queued_on_a_scope_removes_its_admission() {
        use std::task::Poll;
        use std::time::Duration;
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let repositories = Arc::new(RepositoryStore::default());
        let handle = repositories
            .open(repository.path().to_owned())
            .await
            .unwrap()
            .unwrap();
        let predecessor = repositories
            .writes
            .admit(checkpoint_scopes(&handle).unwrap(), 0)
            .unwrap();
        let running = repositories.writes.start(predecessor).unwrap().unwrap();
        let checkpoint = GitCheckpoint::new(repository.path());
        let mut capture = Box::pin(checkpoint.capture(&store.objects, &repositories, "session", 1));
        tokio::time::timeout(
            Duration::from_secs(2),
            std::future::poll_fn(|context| {
                assert!(capture.as_mut().poll(context).is_pending());
                if repositories.writes.usage().operations == 2 {
                    Poll::Ready(())
                } else {
                    context.waker().wake_by_ref();
                    Poll::Pending
                }
            }),
        )
        .await
        .unwrap();
        drop(capture);
        assert_eq!(repositories.writes.usage().operations, 1);
        assert_eq!(repositories.writes.usage().input_bytes, 0);
        assert!(
            store
                .objects
                .get(&crate::plan::digest(b"before\n"), 4096)
                .is_err()
        );
        running.finish(OperationCompletion::Completed).unwrap();
        repositories.writes.take_receipt(predecessor).unwrap();
        assert_eq!(repositories.writes.usage().operations, 0);
    }

    #[tokio::test]
    async fn cancelled_capture_worker_retains_scopes_until_native_exit() {
        use std::time::Duration;
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let repositories = Arc::new(RepositoryStore::default());
        let checkpoint = GitCheckpoint::new(repository.path());
        let capture = checkpoint
            .admit_capture(Arc::clone(&repositories), "session".into(), 1)
            .await
            .unwrap();
        let operation = capture.admission.operation;
        let scope = checkpoint_scopes(&capture.repository).unwrap();
        let objects = store.objects.clone();
        let (started, ready) = tokio::sync::oneshot::channel();
        let (release, wait) = std::sync::mpsc::channel();
        let task = repositories
            .reads
            .submit(capture.input_bytes, move |cancellation| {
                started.send(()).unwrap();
                wait.recv_timeout(Duration::from_secs(5)).unwrap();
                capture.run(&objects, cancellation)
            })
            .unwrap();
        ready.await.unwrap();
        drop(task);
        let successor = repositories.writes.admit(scope, 0).unwrap();
        assert!(repositories.writes.start(successor).unwrap().is_none());
        assert_eq!(repositories.reads.status().active_jobs, 1);
        assert!(repositories.writes.usage().input_bytes > 0);
        release.send(()).unwrap();
        let running = tokio::time::timeout(
            Duration::from_secs(2),
            repositories.writes.wait_start(successor).unwrap(),
        )
        .await
        .unwrap()
        .unwrap();
        running.finish(OperationCompletion::Completed).unwrap();
        repositories.writes.take_receipt(successor).unwrap();
        assert!(
            repositories
                .reads
                .shutdown(Duration::from_secs(2))
                .await
                .unfinished
                .is_empty()
        );
        assert!(repositories.writes.state(operation).is_none());
        assert_eq!(repositories.writes.usage().input_bytes, 0);
        assert!(
            store
                .objects
                .get(&crate::plan::digest(b"before\n"), 4096)
                .is_err()
        );
    }

    #[tokio::test]
    async fn capture_from_nested_path_uses_the_canonical_worktree() {
        let repository = repository();
        fs::create_dir(repository.path().join("nested")).unwrap();
        fs::write(repository.path().join("nested/file.txt"), b"nested\n").unwrap();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let repositories = Arc::new(RepositoryStore::default());
        let handle = repositories
            .open(repository.path().to_owned())
            .await
            .unwrap()
            .unwrap();
        let captured = GitCheckpoint::new(repository.path().join("nested"))
            .capture(&store.objects, &repositories, "session", 1)
            .await
            .unwrap();
        assert_eq!(
            captured.workspace,
            handle
                .identity
                .worktree_root
                .as_ref()
                .unwrap()
                .to_string_lossy()
        );
        assert!(captured.file.iter().any(|file| file.path == "tracked.txt"));
        assert!(
            captured
                .file
                .iter()
                .any(|file| file.path == "nested/file.txt")
        );
    }

    #[tokio::test]
    async fn admitted_capture_rejects_cancellation_and_invalidation_before_native_work() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let repositories = Arc::new(RepositoryStore::default());
        let checkpoint = GitCheckpoint::new(repository.path());
        for invalidate in [false, true] {
            let capture = checkpoint
                .admit_capture(Arc::clone(&repositories), "session".into(), 1)
                .await
                .unwrap();
            if invalidate {
                capture.repository.invalidate().unwrap();
            } else {
                repositories
                    .writes
                    .cancel(capture.admission.operation)
                    .unwrap();
            }
            let objects = store.objects.clone();
            let error = repositories
                .reads
                .submit(capture.input_bytes, move |cancellation| {
                    capture
                        .run(&objects, cancellation)
                        .map(|(record, _capture)| record)
                })
                .unwrap()
                .finish()
                .await
                .unwrap_err();
            assert!(error.to_string().contains(if invalidate {
                "invalidation"
            } else {
                "cancelled"
            }));
            assert_eq!(repositories.writes.usage().operations, 0);
            assert_eq!(repositories.writes.usage().input_bytes, 0);
            assert!(
                store
                    .objects
                    .get(&crate::plan::digest(b"before\n"), 4096)
                    .is_err()
            );
        }
    }

    #[tokio::test]
    async fn completed_capture_releases_scopes_but_retains_its_receipt_until_collection() {
        use forge_git::coordinator::OperationState;
        use std::time::Duration;
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let repositories = Arc::new(RepositoryStore::default());
        let capture = GitCheckpoint::new(repository.path())
            .admit_capture(Arc::clone(&repositories), "session".into(), 1)
            .await
            .unwrap();
        let operation = capture.admission.operation;
        let scope = checkpoint_scopes(&capture.repository).unwrap();
        let objects = store.objects.clone();
        let task = repositories
            .reads
            .submit(capture.input_bytes, move |cancellation| {
                capture.run(&objects, cancellation)
            })
            .unwrap();
        tokio::time::timeout(Duration::from_secs(2), async {
            while repositories.writes.state(operation)
                != Some(OperationState::Finished(OperationCompletion::Completed))
            {
                tokio::task::yield_now().await;
            }
        })
        .await
        .unwrap();
        assert_eq!(repositories.writes.usage().operations, 1);
        assert!(repositories.writes.usage().input_bytes > 0);
        let successor = repositories.writes.admit(scope, 0).unwrap();
        let running = repositories.writes.start(successor).unwrap().unwrap();
        running.finish(OperationCompletion::Completed).unwrap();
        repositories.writes.take_receipt(successor).unwrap();
        let (record, capture) = task.finish().await.unwrap();
        assert!(!record.file.is_empty());
        assert_eq!(repositories.writes.usage().operations, 1);
        drop(capture);
        assert_eq!(repositories.writes.usage().operations, 0);
        assert_eq!(repositories.writes.usage().input_bytes, 0);
    }

    #[tokio::test]
    async fn capture_uses_shared_admission_before_acquisition() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let repositories = Arc::new(RepositoryStore::new(1, 1, 4096).unwrap());
        let reads = &repositories.reads;
        let checkpoint = GitCheckpoint::new(repository.path());
        let occupied = reads.submit(0, |_| Ok(())).unwrap();
        let error = checkpoint
            .capture(&store.objects, &repositories, "session", 1)
            .await
            .unwrap_err();
        assert!(error.to_string().contains("capacity is full"));
        occupied.finish().await.unwrap();
        let captured = checkpoint
            .capture(&store.objects, &repositories, "session", 1)
            .await
            .unwrap();
        assert_eq!(captured.file.len(), 2);
        assert_eq!(captured.file[0].path, ".gitignore");
        assert_eq!(captured.file[1].path, "tracked.txt");
        assert_eq!(
            store
                .objects
                .get(&captured.file[1].object_id, 4096)
                .unwrap()
                .unwrap(),
            b"before\n"
        );
        assert_eq!(reads.status().active_jobs, 0);
        assert!(
            reads
                .shutdown(std::time::Duration::ZERO)
                .await
                .unfinished
                .is_empty()
        );
        let error = checkpoint
            .capture(&store.objects, &repositories, "session", 2)
            .await
            .unwrap_err();
        assert!(error.to_string().contains("closed"));
    }

    #[test]
    fn cancelled_queued_capture_keeps_admission_and_does_not_acquire_objects() {
        use std::task::Poll;
        use std::time::Duration;
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .max_blocking_threads(1)
            .build()
            .unwrap();
        runtime.block_on(async {
            let repository = repository();
            let data = tempfile::tempdir().unwrap();
            let store = SqliteStore::open(data.path()).unwrap();
            let repositories = Arc::new(RepositoryStore::new(1, 1, 4096).unwrap());
            let reads = &repositories.reads;
            let checkpoint = GitCheckpoint::new(repository.path());
            let (started, ready) = tokio::sync::oneshot::channel();
            let (release, wait) = std::sync::mpsc::channel();
            let blocker = tokio::task::spawn_blocking(move || {
                started.send(()).unwrap();
                wait.recv_timeout(Duration::from_secs(5)).unwrap();
            });
            ready.await.unwrap();
            let mut capture =
                Box::pin(checkpoint.capture(&store.objects, &repositories, "session", 1));
            std::future::poll_fn(|context| {
                assert!(capture.as_mut().poll(context).is_pending());
                Poll::Ready(())
            })
            .await;
            drop(capture);
            assert_eq!(reads.status().active_jobs, 1);
            assert!(reads.status().reserved_input_bytes > 0);
            release.send(()).unwrap();
            blocker.await.unwrap();
            assert!(
                reads
                    .shutdown(Duration::from_secs(2))
                    .await
                    .unfinished
                    .is_empty()
            );
            assert_eq!(reads.status().reserved_input_bytes, 0);
            assert!(
                store
                    .objects
                    .get(&crate::plan::digest(b"before\n"), 4096)
                    .is_err()
            );
        });
    }

    #[test]
    fn capture_cancellation_after_object_publication_returns_no_record() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let checkpoint = GitCheckpoint::new(repository.path());
        let identity = crate::plan::digest(b"before\n");
        let result = checkpoint.capture_native(&store.objects, "session", 1, || {
            anyhow::ensure!(
                store.objects.get(&identity, 4096).is_err(),
                "capture cancelled"
            );
            Ok(())
        });
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("capture cancelled")
        );
        assert_eq!(
            fs::read(repository.path().join("tracked.txt")).unwrap(),
            b"before\n"
        );
        assert_eq!(
            store.objects.get(&identity, 4096).unwrap().unwrap(),
            b"before\n"
        );
        assert!(
            checkpoint
                .capture_native(&store.objects, "session", 2, || Ok(()))
                .is_ok()
        );
    }

    fn git(workspace: &Path, args: &[&str]) {
        let status = std::process::Command::new("git")
            .args(args)
            .current_dir(workspace)
            .status()
            .unwrap();
        assert!(status.success(), "git {} failed", args.join(" "));
    }

    fn repository() -> tempfile::TempDir {
        let temporary = tempfile::tempdir().unwrap();
        git(temporary.path(), &["init", "-q"]);
        git(
            temporary.path(),
            &["config", "user.email", "harness@example.invalid"],
        );
        git(temporary.path(), &["config", "user.name", "Harness Test"]);
        fs::write(
            temporary.path().join(".gitignore"),
            "ignored.tmp\ntarget/\n",
        )
        .unwrap();
        fs::write(temporary.path().join("tracked.txt"), "before\n").unwrap();
        git(temporary.path(), &["add", "."]);
        git(temporary.path(), &["commit", "-qm", "seed"]);
        temporary
    }

    #[tokio::test]
    async fn diffs_and_restores_tracked_and_nonignored_untracked_files() {
        let engine = DiffEngine::new(forge_diff::cache::CacheLimits::default(), 4);
        let reads = BlockingReadPool::new(4, 1024 * 1024).unwrap();
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let snapshot = GitCheckpoint::new(repository.path());
        let before = snapshot
            .capture_native(&store.objects, "session", 1, || Ok(()))
            .unwrap();
        fs::write(repository.path().join("tracked.txt"), "after\n").unwrap();
        fs::write(repository.path().join("new.txt"), "new\n").unwrap();
        fs::write(repository.path().join("ignored.tmp"), "ignored\n").unwrap();
        fs::create_dir_all(repository.path().join("target/generated/deep")).unwrap();
        fs::write(
            repository.path().join("target/generated/deep/artifact.txt"),
            "ignored nested artifact\n",
        )
        .unwrap();
        let after = snapshot
            .capture_native(&store.objects, "session", 2, || Ok(()))
            .unwrap();
        let diff = checkpoint_diff(&store.objects, &reads, &engine, &before, &after)
            .await
            .unwrap();
        assert!(diff.contains("tracked.txt"));
        assert!(diff.contains("new.txt"));
        assert!(!diff.contains("ignored.tmp"));
        assert!(!diff.contains("target/generated/deep/artifact.txt"));

        let repositories = Arc::new(RepositoryStore::default());
        snapshot
            .admit_restore(Arc::clone(&repositories), after.clone(), before.clone())
            .await
            .unwrap()
            .restore(store.objects.clone())
            .await
            .unwrap();
        assert_eq!(repositories.writes.usage().operations, 0);
        assert_eq!(repositories.writes.usage().input_bytes, 0);
        assert_eq!(
            fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
            "before\n"
        );
        assert!(!repository.path().join("new.txt").exists());
        assert!(repository.path().join("ignored.tmp").exists());
        assert!(
            repository
                .path()
                .join("target/generated/deep/artifact.txt")
                .exists()
        );
    }

    #[tokio::test]
    async fn queued_checkpoint_restore_rechecks_sources_after_scope_handoff() {
        use std::future::Future;
        use std::task::Poll;
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let checkpoint = GitCheckpoint::new(repository.path());
        let before = checkpoint
            .capture_native(&store.objects, "session", 1, || Ok(()))
            .unwrap();
        fs::write(repository.path().join("tracked.txt"), "after\n").unwrap();
        let after = checkpoint
            .capture_native(&store.objects, "session", 2, || Ok(()))
            .unwrap();
        let repositories = Arc::new(RepositoryStore::default());
        let handle = repositories
            .open(repository.path().to_owned())
            .await
            .unwrap()
            .unwrap();
        let first = repositories
            .writes
            .admit(
                vec![MutationScope::WorktreeFiles(
                    handle.identity.worktree.clone().unwrap(),
                )],
                0,
            )
            .unwrap();
        let running = repositories.writes.start(first).unwrap().unwrap();
        let mut waiting = Box::pin(checkpoint.admit_restore(
            Arc::clone(&repositories),
            after.clone(),
            before.clone(),
        ));
        tokio::time::timeout(
            std::time::Duration::from_secs(2),
            std::future::poll_fn(|context| {
                assert!(waiting.as_mut().poll(context).is_pending());
                if repositories.writes.usage().operations == 2 {
                    Poll::Ready(())
                } else {
                    context.waker().wake_by_ref();
                    Poll::Pending
                }
            }),
        )
        .await
        .unwrap();
        fs::write(repository.path().join("tracked.txt"), "external change\n").unwrap();
        running.finish(OperationCompletion::Completed).unwrap();
        let admitted = waiting.await.unwrap();
        assert!(admitted.restore(store.objects.clone()).await.is_err());
        assert_eq!(
            fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
            "external change\n"
        );
        repositories.writes.take_receipt(first).unwrap();
        assert_eq!(repositories.writes.usage().operations, 0);
        assert_eq!(repositories.writes.usage().input_bytes, 0);
    }

    #[tokio::test]
    async fn dropping_queued_checkpoint_restore_releases_its_receipt_and_input_charge() {
        use std::future::Future;
        use std::task::Poll;
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let checkpoint = GitCheckpoint::new(repository.path());
        let before = checkpoint
            .capture_native(&store.objects, "session", 1, || Ok(()))
            .unwrap();
        let repositories = Arc::new(RepositoryStore::default());
        let handle = repositories
            .open(repository.path().to_owned())
            .await
            .unwrap()
            .unwrap();
        let first = repositories
            .writes
            .admit(
                vec![MutationScope::Index(
                    handle.identity.worktree.clone().unwrap(),
                )],
                0,
            )
            .unwrap();
        let running = repositories.writes.start(first).unwrap().unwrap();
        let mut waiting = Box::pin(checkpoint.admit_restore(
            Arc::clone(&repositories),
            before.clone(),
            before.clone(),
        ));
        tokio::time::timeout(
            std::time::Duration::from_secs(2),
            std::future::poll_fn(|context| {
                assert!(waiting.as_mut().poll(context).is_pending());
                if repositories.writes.usage().operations == 2 {
                    Poll::Ready(())
                } else {
                    context.waker().wake_by_ref();
                    Poll::Pending
                }
            }),
        )
        .await
        .unwrap();
        assert!(repositories.writes.usage().input_bytes > 0);
        drop(waiting);
        assert_eq!(repositories.writes.usage().operations, 1);
        assert_eq!(repositories.writes.usage().input_bytes, 0);
        running.finish(OperationCompletion::Completed).unwrap();
        repositories.writes.take_receipt(first).unwrap();
        let admitted = GitCheckpoint::new(repository.path())
            .admit_restore(Arc::clone(&repositories), before.clone(), before.clone())
            .await
            .unwrap();
        drop(admitted);
        assert_eq!(repositories.writes.usage().operations, 0);
        assert_eq!(repositories.writes.usage().input_bytes, 0);
    }

    #[test]
    fn rejects_checkpoint_records_from_a_different_worktree() {
        let repository = repository();
        let foreign = tempfile::tempdir().unwrap();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let checkpoint = GitCheckpoint::new(repository.path());
        let mut record = checkpoint
            .capture_native(&store.objects, "session", 1, || Ok(()))
            .unwrap();
        record.workspace = foreign.path().to_string_lossy().into_owned();
        let error = checkpoint
            .restore(&store.objects, &record, &record)
            .unwrap_err();
        assert!(error.to_string().contains("admitted worktree"));
        assert_eq!(
            fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
            "before\n"
        );
    }

    #[test]
    fn corrupt_restore_object_is_rejected_before_any_worktree_deletion() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let objects = ObjectStore::open(data.path()).unwrap();
        let checkpoint = GitCheckpoint::new(repository.path());
        let before = checkpoint
            .capture_native(&objects, "session", 1, || Ok(()))
            .unwrap();
        fs::write(repository.path().join("tracked.txt"), "after\n").unwrap();
        fs::write(repository.path().join("new.txt"), "keep until verified\n").unwrap();
        let after = checkpoint
            .capture_native(&objects, "session", 2, || Ok(()))
            .unwrap();
        let identity = &before
            .file
            .iter()
            .find(|file| file.path == "tracked.txt")
            .unwrap()
            .object_id;
        let object = data
            .path()
            .join("objects/sha256")
            .join(&identity[..2])
            .join(&identity[2..]);
        fs::write(object, "corrupt checkpoint").unwrap();
        assert!(checkpoint.restore(&objects, &after, &before).is_err());
        assert_eq!(
            fs::read_to_string(repository.path().join("new.txt")).unwrap(),
            "keep until verified\n"
        );
        assert_eq!(
            fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
            "after\n"
        );
    }

    #[test]
    fn invalid_later_restore_path_is_rejected_before_any_worktree_deletion() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let objects = ObjectStore::open(data.path()).unwrap();
        let checkpoint = GitCheckpoint::new(repository.path());
        let mut before = checkpoint
            .capture_native(&objects, "session", 1, || Ok(()))
            .unwrap();
        fs::write(repository.path().join("new.txt"), "keep until verified\n").unwrap();
        let after = checkpoint
            .capture_native(&objects, "session", 2, || Ok(()))
            .unwrap();
        before.file.push(CheckpointFile {
            path: "../outside".into(),
            object_id: objects.put(b"invalid target").unwrap(),
        });
        assert!(checkpoint.restore(&objects, &after, &before).is_err());
        assert_eq!(
            fs::read_to_string(repository.path().join("new.txt")).unwrap(),
            "keep until verified\n"
        );
    }

    #[test]
    fn cancelled_blocking_restore_keeps_admission_until_its_worker_exits() {
        use std::future::Future;
        use std::task::Poll;
        let executor = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .max_blocking_threads(1)
            .build()
            .unwrap();
        executor.block_on(async {
            let repository = repository();
            let data = tempfile::tempdir().unwrap();
            let objects = ObjectStore::open(data.path()).unwrap();
            let checkpoint = GitCheckpoint::new(repository.path());
            let before = checkpoint
                .capture_native(&objects, "session", 1, || Ok(()))
                .unwrap();
            fs::write(repository.path().join("tracked.txt"), "after\n").unwrap();
            let after = checkpoint
                .capture_native(&objects, "session", 2, || Ok(()))
                .unwrap();
            let repositories = Arc::new(RepositoryStore::default());
            let admitted = checkpoint
                .admit_restore(Arc::clone(&repositories), after, before)
                .await
                .unwrap();
            let (started, started_receiver) = tokio::sync::oneshot::channel();
            let (release, release_receiver) = std::sync::mpsc::channel();
            let blocker = tokio::task::spawn_blocking(move || {
                started.send(()).unwrap();
                release_receiver
                    .recv_timeout(std::time::Duration::from_secs(5))
                    .unwrap();
            });
            started_receiver.await.unwrap();
            let mut restoring = Box::pin(admitted.restore(objects));
            std::future::poll_fn(|context| {
                assert!(restoring.as_mut().poll(context).is_pending());
                Poll::Ready(())
            })
            .await;
            drop(restoring);
            assert_eq!(repositories.writes.usage().operations, 1);
            assert!(repositories.writes.usage().input_bytes > 0);
            release.send(()).unwrap();
            blocker.await.unwrap();
            tokio::time::timeout(std::time::Duration::from_secs(2), async {
                while repositories.writes.usage().operations != 0 {
                    tokio::task::yield_now().await;
                }
            })
            .await
            .unwrap();
            assert_eq!(
                fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
                "after\n"
            );
        });
    }

    #[test]
    fn uncollected_completed_restore_keeps_its_receipt_for_reconciliation() {
        use forge_git::coordinator::OperationState;
        use std::future::Future;
        use std::task::Poll;
        let executor = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .max_blocking_threads(1)
            .build()
            .unwrap();
        executor.block_on(async {
            let repository = repository();
            let data = tempfile::tempdir().unwrap();
            let objects = ObjectStore::open(data.path()).unwrap();
            let checkpoint = GitCheckpoint::new(repository.path());
            let before = checkpoint
                .capture_native(&objects, "session", 1, || Ok(()))
                .unwrap();
            fs::write(repository.path().join("tracked.txt"), "after\n").unwrap();
            let after = checkpoint
                .capture_native(&objects, "session", 2, || Ok(()))
                .unwrap();
            let repositories = Arc::new(RepositoryStore::default());
            let admitted = checkpoint
                .admit_restore(Arc::clone(&repositories), after, before)
                .await
                .unwrap();
            let operation = admitted.operation;
            let (release, release_receiver) = std::sync::mpsc::channel();
            let blocker = tokio::task::spawn_blocking(move || {
                release_receiver
                    .recv_timeout(std::time::Duration::from_secs(5))
                    .unwrap();
            });
            let mut restoring = Box::pin(admitted.restore(objects));
            std::future::poll_fn(|context| {
                assert!(restoring.as_mut().poll(context).is_pending());
                Poll::Ready(())
            })
            .await;
            release.send(()).unwrap();
            blocker.await.unwrap();
            tokio::time::timeout(std::time::Duration::from_secs(2), async {
                while !matches!(
                    repositories.writes.state(operation),
                    Some(OperationState::Finished(_))
                ) {
                    tokio::task::yield_now().await;
                }
            })
            .await
            .unwrap();
            drop(restoring);
            assert_eq!(repositories.writes.usage().operations, 1);
            assert!(repositories.writes.usage().input_bytes > 0);
            assert_eq!(
                repositories.writes.take_receipt(operation),
                Some(OperationCompletion::Completed)
            );
            assert_eq!(
                fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
                "before\n"
            );
        });
    }

    #[tokio::test]
    async fn diffs_only_selected_checkpoint_paths() {
        let engine = DiffEngine::new(forge_diff::cache::CacheLimits::default(), 4);
        let reads = BlockingReadPool::new(4, 1024 * 1024).unwrap();
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let snapshot = GitCheckpoint::new(repository.path());
        let before = snapshot
            .capture_native(&store.objects, "session", 1, || Ok(()))
            .unwrap();
        fs::write(repository.path().join("tracked.txt"), "after\n").unwrap();
        fs::write(repository.path().join("external.txt"), "external\n").unwrap();
        let after = snapshot
            .capture_native(&store.objects, "session", 2, || Ok(()))
            .unwrap();
        let selected = BTreeSet::from(["tracked.txt".to_owned()]);

        let diff =
            checkpoint_diff_for_paths(&store.objects, &reads, &engine, &before, &after, &selected)
                .await
                .unwrap();

        assert!(diff.contains("tracked.txt"));
        assert!(!diff.contains("external.txt"));
        assert_eq!(engine.usage().cache.cached_entries, 1);
        assert_eq!(
            checkpoint_diff_for_paths(&store.objects, &reads, &engine, &before, &after, &selected)
                .await
                .unwrap(),
            diff
        );
        assert_eq!(engine.usage().cache.cached_entries, 1);
        assert!(
            checkpoint_diff_for_paths(
                &store.objects,
                &reads,
                &engine,
                &before,
                &after,
                &BTreeSet::new()
            )
            .await
            .unwrap()
            .is_empty()
        );
        engine.close();
        let error =
            checkpoint_diff_for_paths(&store.objects, &reads, &engine, &before, &after, &selected)
                .await
                .unwrap_err();
        assert!(error.to_string().contains("Closed"));
    }

    #[tokio::test]
    async fn oversized_checkpoint_sources_report_unavailable_without_hiding_other_changes() {
        let engine = DiffEngine::new(forge_diff::cache::CacheLimits::default(), 4);
        let reads = BlockingReadPool::new(4, 1024 * 1024).unwrap();
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let snapshot = GitCheckpoint::new(repository.path());
        let before = snapshot
            .capture_native(&store.objects, "session", 1, || Ok(()))
            .unwrap();
        fs::write(repository.path().join("tracked.txt"), b"after\n").unwrap();
        let mut after = snapshot
            .capture_native(&store.objects, "session", 2, || Ok(()))
            .unwrap();
        let large = store
            .objects
            .put(&vec![b'x'; MAX_SOURCE_BYTES + 1])
            .unwrap();
        after.file.push(CheckpointFile {
            path: "large.txt".into(),
            object_id: large.clone(),
        });
        for (source, target) in [(&before, &after), (&after, &before)] {
            let diff = checkpoint_diff(&store.objects, &reads, &engine, source, target)
                .await
                .unwrap();
            assert!(diff.contains("diff --git a/large.txt b/large.txt\nDiff unavailable:"));
            assert!(diff.contains("8388608 bytes"));
            assert!(diff.contains("diff --git a/tracked.txt b/tracked.txt"));
            assert!(diff.contains("after"));
        }
        let destination = repository.path().join("restored-large.txt");
        store.objects.restore_file(&large, &destination).unwrap();
        assert_eq!(
            fs::metadata(destination).unwrap().len(),
            (MAX_SOURCE_BYTES + 1) as u64
        );
    }

    #[tokio::test]
    async fn checkpoint_diff_distinguishes_binary_content_from_unsupported_encoding() {
        let engine = DiffEngine::new(forge_diff::cache::CacheLimits::default(), 4);
        let reads = BlockingReadPool::new(4, 1024 * 1024).unwrap();
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let snapshot = GitCheckpoint::new(repository.path());
        let before = snapshot
            .capture_native(&store.objects, "session", 1, || Ok(()))
            .unwrap();
        fs::write(repository.path().join("binary.dat"), b"text\0data").unwrap();
        fs::write(repository.path().join("encoded.txt"), [0xff, 0xfe]).unwrap();
        let after = snapshot
            .capture_native(&store.objects, "session", 2, || Ok(()))
            .unwrap();
        let diff = checkpoint_diff(&store.objects, &reads, &engine, &before, &after)
            .await
            .unwrap();
        assert!(diff.contains("Binary files a/binary.dat and b/binary.dat differ"));
        assert!(diff.contains("diff --git a/encoded.txt b/encoded.txt\nDiff unavailable: checkpoint source is not valid UTF-8"));
        assert!(!diff.contains('\0'));
    }

    #[tokio::test]
    async fn checkpoint_source_uses_shared_read_admission_and_preserves_exact_bytes() {
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let reads = BlockingReadPool::new(1, 128).unwrap();
        let old = store.objects.put(b"old\r\n").unwrap();
        let new = store.objects.put(b"new without newline").unwrap();
        let occupied = reads.submit(0, |_| Ok(())).unwrap();
        let error = checkpoint_source(&store.objects, &reads, Some(&old), Some(&new))
            .await
            .unwrap_err();
        assert!(error.to_string().contains("capacity is full"));
        occupied.finish().await.unwrap();
        let CheckpointSource::Text(pair) =
            checkpoint_source(&store.objects, &reads, Some(&old), Some(&new))
                .await
                .unwrap()
        else {
            panic!("text source must remain available");
        };
        assert_eq!(pair.old.bytes(), b"old\r\n");
        assert_eq!(pair.new.bytes(), b"new without newline");
        assert_eq!(reads.status().active_jobs, 0);
        assert_eq!(reads.status().reserved_input_bytes, 0);
        assert!(
            reads
                .shutdown(std::time::Duration::ZERO)
                .await
                .unfinished
                .is_empty()
        );
        let error = checkpoint_source(&store.objects, &reads, Some(&old), Some(&new))
            .await
            .unwrap_err();
        assert!(error.to_string().contains("closed"));
    }

    #[test]
    fn refuses_rollback_after_workspace_divergence() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let snapshot = GitCheckpoint::new(repository.path());
        let before = snapshot
            .capture_native(&store.objects, "session", 1, || Ok(()))
            .unwrap();
        fs::write(repository.path().join("tracked.txt"), "after\n").unwrap();
        let after = snapshot
            .capture_native(&store.objects, "session", 2, || Ok(()))
            .unwrap();
        fs::write(
            repository.path().join("tracked.txt"),
            "unrelated later edit\n",
        )
        .unwrap();
        assert!(snapshot.restore(&store.objects, &after, &before).is_err());
    }

    #[test]
    fn rejects_checkpoint_paths_outside_the_worktree() {
        let repository = repository();
        let snapshot = GitCheckpoint::new(repository.path());
        assert!(snapshot.file_path("../escape.txt").is_err());
        assert!(snapshot.file_path("/absolute.txt").is_err());
    }

    #[test]
    fn scopes_identical_checkpoint_content_to_its_session() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let snapshot = GitCheckpoint::new(repository.path());
        let first = snapshot
            .capture_native(&store.objects, "session-one", 1, || Ok(()))
            .unwrap();
        let second = snapshot
            .capture_native(&store.objects, "session-two", 1, || Ok(()))
            .unwrap();
        assert_ne!(first.id, second.id);
    }

    #[test]
    fn captures_an_unborn_git_worktree() {
        let repository = tempfile::tempdir().unwrap();
        git(repository.path(), &["init", "-q"]);
        fs::write(repository.path().join("first.txt"), "first\n").unwrap();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let checkpoint = GitCheckpoint::new(repository.path())
            .capture_native(&store.objects, "session", 1, || Ok(()))
            .unwrap();
        assert_eq!(checkpoint.head, "UNBORN");
        assert_eq!(checkpoint.file.len(), 1);
    }
}
