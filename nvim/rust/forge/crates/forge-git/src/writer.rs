use std::{
    collections::{BTreeMap, HashSet},
    io::Write,
    process::Command,
    sync::{
        Arc, Mutex,
        atomic::{AtomicU64, Ordering},
    },
    time::{Duration, Instant},
};

use anyhow::{Context, Result, ensure};
use forge_diff::raw::{RawDiff, RawHunkId, patch_body};
use tokio::sync::oneshot;

use crate::{
    RepositoryPath,
    command::{CommandLimits, CommandProgressSink, CommandStream, progress_command},
    coordinator::{AdmissionGuard, OperationState},
    mutation::{MutationScope, OperationCompletion, OperationId},
    repository::RepositoryState,
    resolve_argument,
    store::RepositoryStore,
};

mod compound;
mod config;
mod precondition;
use precondition::WritePrecondition;

pub const MAX_TARGETS: usize = 10_000;
pub const MAX_INPUT: usize = 16 * 1024 * 1024;
pub const MAX_PATH_BYTES: usize = 1024 * 1024;
const RECEIPT_RESERVATION: usize = 1024 * 1024;

#[derive(Clone, Copy, Eq, PartialEq)]
enum BulkAction {
    Stage,
    Unstage,
    DiscardIndex,
    DiscardHead,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PatchDirection {
    Index { before_exists: bool, after_exists: bool },
    Stage,
    Unstage,
    Discard,
    DiscardStaged,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DiscardSource {
    Index,
    Head,
}

/// Retains an exact canonical analysis and selected raw identities, never display row ranges.
#[derive(Clone, Debug)]
pub struct PatchTarget {
    pub path: RepositoryPath,
    pub analysis: RawDiff,
    pub selected: Vec<RawHunkId>,
}

#[derive(Clone, Debug)]
pub enum GitWriteAction {
    Batch {
        action: Vec<GitWriteAction>,
    },
    Stage {
        path: Vec<RepositoryPath>,
    },
    Unstage {
        path: Vec<RepositoryPath>,
    },
    /// Restores selected source state and deletes only explicit captured untracked file targets.
    Discard {
        path: Vec<RepositoryPath>,
        source: DiscardSource,
    },
    Patch {
        direction: PatchDirection,
        target: PatchTarget,
    },
    DiscardCombined {
        staged: PatchTarget,
        unstaged: PatchTarget,
    },
    CreateBranch {
        name: String,
    },
    /// Uses captured text without starting an interactive editor or an automatic retry.
    Commit {
        message: String,
    },
    CommitWithEditor {
        command: String,
        nvim_server: String,
    },
    UpdateRepositoryConfig {
        expected: Option<Vec<u8>>,
        replacement: Vec<u8>,
    },
    Push,
    PublishBranch {
        name: String,
        expected_head: Option<String>,
    },
    /// Preserves configured tracking and pull reconciliation policy.
    Pull,
}

/// An admitted immutable intent. Dropping an unsubmitted intent cancels and releases its receipt.
pub type SettlementHandler = Arc<dyn Fn(&WriteOutcome) -> Result<()> + Send + Sync>;

pub struct GitWriteIntent {
    store: Arc<RepositoryStore>,
    repository: Arc<RepositoryState>,
    action: GitWriteAction,
    precondition: Option<WritePrecondition>,
    expected: Mutex<Option<(crate::snapshot::HeadState, Vec<crate::snapshot::ObservedPath>)>>,
    reservation: IntentReservation,
    progress: Option<CommandProgressSink>,
    settlement: Option<SettlementHandler>,
}

struct IntentReservation {
    store: Arc<RepositoryStore>,
    operation: OperationId,
    submitted: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TargetCompletion {
    Completed,
    Rejected,
    OutcomeUnknown,
    NotStarted,
}

#[derive(Clone, Debug)]
pub struct TargetOutcome {
    pub path: Option<RepositoryPath>,
    pub completion: TargetCompletion,
    pub diagnostic: Option<String>,
    pub exit_code: Option<i32>,
}

#[derive(Clone, Debug)]
pub struct WriteOutcome {
    pub operation: OperationId,
    pub target: Vec<TargetOutcome>,
    pub affected: Vec<RepositoryPath>,
    /// Affected-path status collected after execution. None requires a full refresh.
    pub settled: Option<WriteSettlement>,
    pub settlement_diagnostic: Option<String>,
}

#[derive(Clone, Debug)]
pub struct WriteSettlement {
    pub head: crate::snapshot::HeadState,
    pub path: Vec<crate::snapshot::ObservedPath>,
}

/// The consumer may cancel or drop this waiter without abandoning native ownership.
pub struct GitWriteTicket {
    pub operation: OperationId,
    store: Arc<RepositoryStore>,
    completion: oneshot::Receiver<Arc<WriteOutcome>>,
    collected: bool,
}

/// Shares the repository store's mutation coordinator with checkpoint and other Git producers.
pub struct GitWriteService {
    store: Arc<RepositoryStore>,
    outcome: Arc<Mutex<BTreeMap<OperationId, Arc<WriteOutcome>>>>,
}

impl GitWriteService {
    pub async fn reconcile(&self, repository: &Arc<RepositoryState>) -> Result<()> {
        let Some(worktree) = &repository.identity.worktree else { return Ok(()); };
        let scope = vec![MutationScope::Index(worktree.clone()), MutationScope::WorktreeFiles(worktree.clone())];
        let operation = self.store.writes.quarantined(&scope);
        if operation.is_empty() { return Ok(()); }
        repository.invalidate()?;
        repository.observe(&self.store).await?;
        self.store.writes.reconciled(&operation);
        for operation in operation { self.acknowledge(operation); }
        Ok(())
    }

    pub fn new(store: Arc<RepositoryStore>) -> Self {
        Self {
            store,
            outcome: Arc::new(Mutex::new(BTreeMap::new())),
        }
    }

    /// Reserves queue order without disk access. Preparation runs after preceding writes settle.
    /// The caller owns the reservation until submission and may attach displayed source checks.
    pub fn reserve(&self, repository: Arc<RepositoryState>, action: GitWriteAction) -> Result<GitWriteIntent> {
        action.validate()?;
        let operation = self.store.writes.admit(action.scopes(&repository)?, action.retained_bytes()?)?;
        Ok(GitWriteIntent {
            store: Arc::clone(&self.store), repository, action, precondition: None,
            expected: Mutex::new(None),
            reservation: IntentReservation { store: Arc::clone(&self.store), operation, submitted: false },
            progress: None, settlement: None,
        })
    }

    /// Reserves retained input before collecting exact preconditions on the shared read pool.
    /// Queue order is established here. Submission never substitutes a newer source capture.
    pub async fn prepare(
        &self,
        repository: Arc<RepositoryState>,
        action: GitWriteAction,
    ) -> Result<GitWriteIntent> {
        action.validate()?;
        let retained = action.retained_bytes()?;
        let operation = self
            .store
            .writes
            .admit(action.scopes(&repository)?, retained)?;
        let reservation = IntentReservation {
            store: Arc::clone(&self.store),
            operation,
            submitted: false,
        };
        let worker = Arc::clone(&repository);
        self.store.writes.wait_ready(operation).await?;
        let prepared = self
            .store
            .read(
                Arc::clone(&repository),
                retained,
                move |mut local, cancellation| {
                    let started = Instant::now();
                    let mut check = || {
                        cancellation.check()?;
                        ensure!(
                            started.elapsed() < Duration::from_secs(30),
                            "write preparation exceeded 30 seconds"
                        );
                        Ok(())
                    };
                    let precondition =
                        WritePrecondition::capture(&mut local, &worker, &action, &mut check)?;
                    Ok((action, precondition))
                },
            )
            .await?
            .value;
        Ok(GitWriteIntent {
            store: Arc::clone(&self.store),
            repository,
            action: prepared.0,
            precondition: Some(prepared.1),
            expected: Mutex::new(None),
            reservation,
            progress: None,
            settlement: None,
        })
    }

    /// Starts an independent owner that retains admission through process reaping and settlement.
    /// Terminal receipts remain charged until the consumer explicitly acknowledges adoption.
    pub fn submit(&self, mut intent: GitWriteIntent) -> Result<GitWriteTicket> {
        ensure!(
            Arc::ptr_eq(&self.store, &intent.store),
            "intent belongs to another writer store"
        );
        let runtime =
            tokio::runtime::Handle::try_current().context("writer requires Tokio runtime")?;
        let operation = intent.reservation.operation;
        let (sender, completion) = oneshot::channel();
        let retained_outcome = Arc::clone(&self.outcome);
        intent.reservation.submitted = true;
        runtime.spawn(async move {
            let started = match intent.store.writes.wait_start(operation) {
                Ok(waiter) => waiter.await,
                Err(error) => Err(error),
            };
            let outcome = match started {
                Ok(guard) => {
                    match tokio::task::spawn_blocking(move || execute(intent, guard)).await {
                        Ok(outcome) => outcome,
                        Err(error) => WriteOutcome {
                            operation,
                            target: vec![TargetOutcome {
                                path: None,
                                completion: TargetCompletion::OutcomeUnknown,
                                exit_code: None,
                                diagnostic: Some(format!("Git owner panicked: {error}")),
                            }],
                            affected: Vec::new(),
                            settled: None,
                            settlement_diagnostic: None,
                        },
                    }
                }
                Err(error) => intent.failure(TargetCompletion::NotStarted, error.to_string()),
            };
            let outcome = Arc::new(outcome);
            retained_outcome
                .lock()
                .expect("Git outcome lock")
                .insert(operation, Arc::clone(&outcome));
            let _ = sender.send(outcome);
        });
        Ok(GitWriteTicket {
            operation,
            store: Arc::clone(&self.store),
            completion,
            collected: false,
        })
    }

    pub fn acknowledge(&self, operation: OperationId) -> Option<OperationCompletion> {
        let mut outcome = self.outcome.lock().expect("Git outcome lock");
        if !outcome.contains_key(&operation) {
            return None;
        }
        let completion = self.store.writes.take_receipt(operation)?;
        outcome.remove(&operation);
        Some(completion)
    }

    pub fn outcome(&self, operation: OperationId) -> Option<Arc<WriteOutcome>> {
        self.outcome
            .lock()
            .expect("Git outcome lock")
            .get(&operation)
            .cloned()
    }

    pub fn cancel(&self, operation: OperationId) -> Result<OperationState> {
        self.store.writes.cancel(operation)
    }
}

impl GitWriteTicket {
    pub fn cancel(&self) -> Result<OperationState> {
        self.store.writes.cancel(self.operation)
    }

    pub async fn finish(mut self) -> Result<Arc<WriteOutcome>> {
        let result = (&mut self.completion)
            .await
            .context("Git owner closed without an outcome");
        self.collected = true;
        result
    }
}

impl Drop for GitWriteTicket {
    fn drop(&mut self) {
        if !self.collected {
            let _ = self.store.writes.cancel(self.operation);
        }
    }
}

impl Drop for IntentReservation {
    fn drop(&mut self) {
        if !self.submitted {
            let _ = self.store.writes.cancel(self.operation);
            self.store.writes.take_receipt(self.operation);
        }
    }
}

impl GitWriteIntent {
    /// Rejects displayed source observations that differ from this prepared write.
    /// Checks selected index entries and worktree stamps without reading unrelated paths.
    /// Failure leaves the intent unsubmitted and performs no Git mutation.
    pub fn validate_observed_sources(
        &self,
        head: &crate::snapshot::HeadState,
        observed: &[crate::snapshot::ObservedPath],
    ) -> Result<()> {
        ensure!(
            observed.len() <= MAX_TARGETS,
            "observed write sources exceed target limit"
        );
        let selected: HashSet<_> = self.action.paths().into_iter().collect();
        ensure!(
            observed
                .iter()
                .all(|source| selected.contains(&source.change.path)),
            "observation contains an unselected write source"
        );
        if let Some(precondition) = &self.precondition {
            precondition.validate_observed(head, observed)
        } else {
            *self.expected.lock().expect("write expected-source lock") = Some((head.clone(), observed.to_vec()));
            Ok(())
        }
    }

    /// Replaces an unprepared queued action after exact local replay, retaining its original scopes.
    pub fn replace_action(&mut self, action: GitWriteAction) -> Result<()> {
        ensure!(self.precondition.is_none(), "prepared write cannot replace its captured action");
        action.validate()?;
        ensure!(action.paths().iter().all(|path| self.action.paths().contains(path)), "replayed write broadened its selected paths");
        self.store.writes.resize(self.operation(), action.retained_bytes()?)?;
        self.action = action;
        Ok(())
    }

    fn precondition(&self) -> &WritePrecondition {
        self.precondition.as_ref().expect("write execution requires prepared sources")
    }

    pub fn set_settlement_handler(
        &mut self,
        handler: SettlementHandler,
        retained_bytes: usize,
    ) -> Result<()> {
        ensure!(
            retained_bytes <= 1024 * 1024,
            "write settlement capture exceeds 1 MiB reservation"
        );
        ensure!(
            self.settlement.is_none(),
            "write settlement handler is already attached"
        );
        self.settlement = Some(handler);
        Ok(())
    }

    pub fn retained_bytes(&self) -> Result<usize> {
        self.action.retained_bytes()
    }
    /// Attaches one bounded progress consumer before native execution starts.
    pub fn set_progress(&mut self, progress: CommandProgressSink) {
        let stdout = AtomicU64::new(0);
        let stderr = AtomicU64::new(0);
        self.progress = Some(Arc::new(move |mut chunk| {
            let counter = match chunk.stream {
                CommandStream::Stdout => &stdout,
                CommandStream::Stderr => &stderr,
            };
            chunk.sequence = counter
                .fetch_update(Ordering::AcqRel, Ordering::Acquire, |sequence| {
                    sequence.checked_add(1)
                })
                .map_err(|_| anyhow::anyhow!("operation progress sequence exhausted"))?;
            progress(chunk)
        }));
    }

    pub fn operation(&self) -> OperationId {
        self.reservation.operation
    }

    fn failure(&self, completion: TargetCompletion, diagnostic: String) -> WriteOutcome {
        WriteOutcome {
            operation: self.operation(),
            target: self
                .action
                .targets()
                .into_iter()
                .enumerate()
                .map(|(index, path)| TargetOutcome {
                    path,
                    completion,
                    diagnostic: (index == 0).then(|| bounded_diagnostic(&diagnostic)),
                    exit_code: None,
                })
                .collect(),
            affected: self.action.paths(),
            settled: None,
            settlement_diagnostic: None,
        }
    }
}

impl GitWriteAction {
    fn bulk_action(&self) -> Option<BulkAction> {
        match self {
            Self::Stage { .. } => Some(BulkAction::Stage),
            Self::Unstage { .. } => Some(BulkAction::Unstage),
            Self::Discard {
                source: DiscardSource::Index,
                ..
            } => Some(BulkAction::DiscardIndex),
            Self::Discard {
                source: DiscardSource::Head,
                ..
            } => Some(BulkAction::DiscardHead),
            Self::Batch { action } => {
                let first = action.first()?.bulk_action()?;
                action
                    .iter()
                    .all(|action| action.bulk_action() == Some(first))
                    .then_some(first)
            }
            _ => None,
        }
    }
    fn paths(&self) -> Vec<RepositoryPath> {
        match self {
            Self::Batch { action } => action.iter().flat_map(Self::paths).collect(),
            Self::Stage { path } | Self::Unstage { path } | Self::Discard { path, .. } => {
                path.clone()
            }
            Self::Patch { target, .. } | Self::DiscardCombined { staged: target, .. } => {
                vec![target.path.clone()]
            }
            Self::UpdateRepositoryConfig { .. } => {
                vec![RepositoryPath::new(b".forge.json".to_vec()).expect("fixed config path")]
            }
            _ => Vec::new(),
        }
    }

    fn targets(&self) -> Vec<Option<RepositoryPath>> {
        let path = self.paths();
        if path.is_empty() {
            vec![None]
        } else {
            path.into_iter().map(Some).collect()
        }
    }

    fn validate(&self) -> Result<()> {
        if let Self::UpdateRepositoryConfig {
            expected,
            replacement,
        } = self
        {
            ensure!(
                expected
                    .as_ref()
                    .is_none_or(|bytes| bytes.len() <= 1024 * 1024)
                    && replacement.len() <= 1024 * 1024,
                "repository config exceeds 1 MiB"
            );
            std::str::from_utf8(replacement).context("repository config is not UTF-8")?;
            ensure!(!replacement.contains(&0), "repository config contains NUL");
        }
        if let Self::Batch { action } = self {
            ensure!(
                !action.is_empty() && action.len() <= MAX_TARGETS,
                "batch requires 1 to 10000 path operations"
            );
            for action in action {
                ensure!(
                    matches!(
                        action,
                        Self::Stage { .. }
                            | Self::Unstage { .. }
                            | Self::Discard { .. }
                            | Self::Patch { .. }
                            | Self::DiscardCombined { .. }
                    ),
                    "batch accepts only non-nested path operations"
                );
                action.validate()?;
            }
        }
        let paths = self.paths();
        ensure!(paths.len() <= MAX_TARGETS, "write exceeds 10000 targets");
        if matches!(
            self,
            Self::Stage { .. } | Self::Unstage { .. } | Self::Discard { .. }
        ) {
            ensure!(!paths.is_empty(), "write requires explicit targets");
        }
        let mut unique = HashSet::new();
        for path in &paths {
            resolve_argument(path)?;
            ensure!(unique.insert(path), "duplicate write target");
        }
        ensure!(
            paths.iter().map(|path| path.raw().len()).sum::<usize>() <= MAX_PATH_BYTES,
            "write paths exceed 1 MiB"
        );
        if let Self::CreateBranch { name } | Self::PublishBranch { name, .. } = self {
            ensure!(
                !name.is_empty() && !name.starts_with('-') && !name.contains('\0'),
                "invalid branch name"
            );
        }
        if let Self::CommitWithEditor {
            command,
            nvim_server,
        } = self
        {
            ensure!(
                !command.is_empty()
                    && !command.contains('\0')
                    && !nvim_server.is_empty()
                    && !nvim_server.contains('\0'),
                "invalid commit editor bridge"
            );
        }
        if let Self::Commit { message } = self {
            ensure!(
                !message.trim().is_empty() && !message.contains('\0'),
                "commit message is empty or contains NUL"
            );
        }
        if let Self::DiscardCombined { staged, unstaged } = self {
            ensure!(
                staged.path == unstaged.path,
                "compound discard paths differ"
            );
            ensure!(
                staged.analysis.source().new.identity()
                    == unstaged.analysis.source().old.identity(),
                "compound discard index sources differ"
            );
            validate_patch_selection(staged)?;
            validate_patch_selection(unstaged)?;
        }
        if let Self::Patch { target, .. } = self {
            validate_patch_selection(target)?;
        }
        Ok(())
    }

    fn retained_bytes(&self) -> Result<usize> {
        let path = self.paths();
        let path_bytes: usize = path.iter().map(RepositoryPath::retained_bytes).sum();
        let retained = self
            .source_bytes()?
            .checked_add(path_bytes.saturating_mul(6))
            .and_then(|bytes| bytes.checked_add(path.len().saturating_mul(768)))
            .and_then(|bytes| bytes.checked_add(RECEIPT_RESERVATION + 3 * 1024 * 1024))
            .context("write input accounting overflow")?;
        ensure!(retained <= MAX_INPUT, "write input exceeds 16 MiB");
        Ok(retained)
    }

    fn source_bytes(&self) -> Result<usize> {
        Ok(match self {
            Self::Batch { action } => action.iter().try_fold(0usize, |retained, action| {
                retained
                    .checked_add(action.source_bytes()?)
                    .context("batch input accounting overflow")
            })?,
            Self::Patch { target, .. } => {
                target.analysis.retained_source_bytes()
                    + target.analysis.retained_result_bytes()
                    + target.selected.capacity() * size_of::<RawHunkId>()
            }
            Self::DiscardCombined { staged, unstaged } => [staged, unstaged]
                .iter()
                .map(|target| {
                    target.analysis.retained_source_bytes()
                        + target.analysis.retained_result_bytes()
                        + target.selected.capacity() * size_of::<RawHunkId>()
                })
                .sum(),
            Self::CreateBranch { name } => name.capacity(),
            Self::PublishBranch {
                name,
                expected_head,
            } => name.capacity() + expected_head.as_ref().map_or(0, String::capacity),
            Self::UpdateRepositoryConfig {
                expected,
                replacement,
            } => expected.as_ref().map_or(0, Vec::capacity) + replacement.capacity(),
            Self::Commit { message } => message.capacity(),
            Self::CommitWithEditor {
                command,
                nvim_server,
            } => command.capacity() + nvim_server.capacity(),
            _ => 0,
        })
    }

    fn scopes(&self, repository: &RepositoryState) -> Result<Vec<MutationScope>> {
        let worktree = repository
            .identity
            .worktree
            .clone()
            .context("write requires worktree")?;
        let mut scopes = vec![MutationScope::Index(worktree.clone())];
        if self.bulk_action() != Some(BulkAction::Unstage)
            && !matches!(
                self,
                Self::Unstage { .. }
                    | Self::Patch {
                        direction: PatchDirection::Unstage,
                        ..
                    }
                    | Self::Push
                    | Self::PublishBranch { .. }
            )
        {
            scopes.push(MutationScope::WorktreeFiles(worktree));
        }
        if self.shared_refs() {
            scopes.push(MutationScope::SharedRefs(
                repository.identity.storage.clone(),
            ));
        }
        Ok(scopes)
    }

    fn shared_refs(&self) -> bool {
        matches!(
            self,
            Self::CreateBranch { .. }
                | Self::Commit { .. }
                | Self::CommitWithEditor { .. }
                | Self::Push
                | Self::PublishBranch { .. }
                | Self::Pull
        )
    }
}

fn execute(mut intent: GitWriteIntent, guard: AdmissionGuard) -> WriteOutcome {
    let mut local = intent.repository.repository.to_thread_local();
    let started = Instant::now();
    let interactive = matches!(intent.action, GitWriteAction::CommitWithEditor { .. });
    let mut check = || {
        ensure!(!guard.cancellation_requested(), "Git write cancelled");
        ensure!(
            interactive || started.elapsed() < Duration::from_secs(120),
            "Git write exceeded 120 seconds"
        );
        Ok(())
    };
    let prepared = (|| -> Result<()> {
        if intent.precondition.is_none() {
            intent.precondition = Some(WritePrecondition::capture(&mut local, &intent.repository, &intent.action, &mut check)?);
        }
        if let Some((head, observed)) = intent.expected.lock().expect("write expected-source lock").as_ref() {
            intent.precondition().validate_observed(head, observed)?;
        }
        intent.precondition().validate(&mut local, &intent.repository, &intent.action, &mut check)
    })();
    if let Err(error) = prepared {
        let mut outcome = intent.failure(
            TargetCompletion::Rejected,
            format!("write precondition rejected: {error:#}"),
        );
        if !outcome.affected.is_empty() {
            match settle_paths(&intent.repository, &outcome.affected) {
                Ok(path) => outcome.settled = Some(path),
                Err(error) => outcome.settlement_diagnostic = Some(format!("rejected write reconciliation failed: {error:#}")),
            }
            if let Err(error) = intent.repository.invalidate() { outcome.settlement_diagnostic = Some(format!("rejected write invalidation failed: {error:#}")); }
        }
        if !outcome.affected.is_empty() && outcome.settled.is_none() { let _ = guard.finish_quarantined(OperationCompletion::Failed); }
        else { let _ = guard.finish(OperationCompletion::Failed); }
        return outcome;
    }

    let mut outcome = WriteOutcome {
        operation: intent.operation(),
        target: Vec::new(),
        affected: intent.action.paths(),
        settled: None,
        settlement_diagnostic: None,
    };
    let mut stopped = false;
    let targets = intent.action.targets();
    let mut bulk = intent.action.bulk_action();
    if matches!(
        bulk,
        Some(BulkAction::DiscardIndex | BulkAction::DiscardHead)
    ) {
        let tracked = targets
            .iter()
            .flatten()
            .filter(|path| intent.precondition().tracked(path))
            .count();
        if tracked != 0 && tracked != targets.len() {
            bulk = None;
        }
    }
    let chunk_size = if bulk.is_some() && targets.len() > 256 {
        256
    } else {
        1
    };
    for chunk in targets.chunks(chunk_size) {
        if stopped {
            outcome
                .target
                .extend(chunk.iter().cloned().map(|path| TargetOutcome {
                    path,
                    completion: TargetCompletion::NotStarted,
                    exit_code: None,
                    diagnostic: None,
                }));
            continue;
        }
        let path: Vec<_> = chunk.iter().flatten().cloned().collect();
        let validated = if chunk_size > 1 {
            intent
                .precondition()
                .validate_targets(&intent.repository, &path, &mut check)
        } else if let Some(path) = path.first() {
            intent
                .precondition()
                .validate_target(&intent.repository, path, &mut check)
        } else {
            Ok(())
        };
        if let Err(error) = validated {
            outcome
                .target
                .extend(
                    chunk
                        .iter()
                        .cloned()
                        .enumerate()
                        .map(|(index, path)| TargetOutcome {
                            path,
                            completion: TargetCompletion::Rejected,
                            exit_code: None,
                            diagnostic: (index == 0)
                                .then(|| bounded_diagnostic(&format!("{error:#}"))),
                        }),
                );
            stopped = true;
            continue;
        }
        let result = if chunk_size > 1 {
            run_path_chunk(
                &intent,
                bulk.expect("validated bulk action"),
                &path,
                &mut check,
            )
        } else {
            run_target(&intent, chunk[0].as_ref(), &mut check)
        };
        let exit_code = result.as_ref().ok().and_then(|output| output.status.code());
        let (completion, diagnostic) = match result {
            Ok(output) if output.status.success() => (TargetCompletion::Completed, None),
            Ok(output) => (
                TargetCompletion::OutcomeUnknown,
                Some(format!(
                    "Git exited {}: {}",
                    output.status,
                    String::from_utf8_lossy(&output.stderr)
                )),
            ),
            Err(error) => (TargetCompletion::OutcomeUnknown, Some(format!("{error:#}"))),
        };
        stopped = completion != TargetCompletion::Completed;
        outcome
            .target
            .extend(chunk.iter().cloned().enumerate().map(|(index, path)| {
                TargetOutcome {
                    path,
                    completion,
                    diagnostic: diagnostic
                        .as_ref()
                        .filter(|_| index == 0)
                        .map(|text| bounded_diagnostic(text)),
                    exit_code,
                }
            }));
    }

    // Invalidate every possibly changed repository before releasing cooperative writer scopes.
    let invalidated = if intent.action.shared_refs() {
        intent
            .store
            .invalidate_storage(&intent.repository.identity.storage)
            .map(|_| ())
    } else {
        intent.repository.invalidate().map(|_| ())
    };
    if let Err(error) = invalidated {
        outcome.settlement_diagnostic = Some(format!("invalidation failed: {error:#}"));
    } else if !outcome.affected.is_empty() {
        match settle_paths(&intent.repository, &outcome.affected) {
            Ok(path) => outcome.settled = Some(path),
            Err(error) => {
                outcome.settlement_diagnostic =
                    Some(format!("affected-path settlement failed: {error:#}"))
            }
        }
    }
    if let Some(handler) = &intent.settlement {
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| handler(&outcome)));
        let failure = match result {
            Ok(Ok(())) => None,
            Ok(Err(error)) => Some(format!("native consumer settlement failed: {error:#}")),
            Err(_) => Some("native consumer settlement panicked".into()),
        };
        if let Some(failure) = failure {
            outcome.settlement_diagnostic = Some(bounded_diagnostic(&match outcome
                .settlement_diagnostic
                .take()
            {
                Some(previous) => format!("{previous}\n{failure}"),
                None => failure,
            }));
        }
    }
    let completion = if outcome
        .target
        .iter()
        .any(|target| target.completion == TargetCompletion::OutcomeUnknown)
    {
        OperationCompletion::Uncertain
    } else if stopped {
        OperationCompletion::Failed
    } else {
        OperationCompletion::Completed
    };
    let completion = if !outcome.affected.is_empty() && outcome.settled.is_none() { guard.finish_quarantined(completion) } else { guard.finish(completion) };
    if let Err(error) = completion {
        outcome.settlement_diagnostic = Some(format!("receipt collection failed: {error:#}"));
    }
    outcome
}

fn git_command(repository: &RepositoryState) -> Result<Command> {
    let mut command = Command::new("git");
    command
        .args([
            "--no-pager",
            "--no-optional-locks",
            "--literal-pathspecs",
            "-C",
        ])
        .arg(
            repository
                .identity
                .worktree_root
                .as_ref()
                .context("write requires worktree root")?,
        )
        .args(["-c", "core.fsmonitor=false"]);
    Ok(command)
}

fn bounded_diagnostic(text: &str) -> String {
    let mut end = text.len().min(2048);
    while !text.is_char_boundary(end) {
        end -= 1;
    }
    text[..end].to_owned()
}

fn run_path_chunk(
    intent: &GitWriteIntent,
    action: BulkAction,
    path: &[RepositoryPath],
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<std::process::Output> {
    ensure!(
        !path.is_empty() && path.len() <= 256,
        "invalid write chunk size"
    );
    let mut command = git_command(&intent.repository)?;
    if matches!(action, BulkAction::DiscardIndex | BulkAction::DiscardHead)
        && !intent.precondition().tracked(&path[0])
    {
        for path in path {
            check()?;
            let destination = crate::validate_path(
                intent
                    .repository
                    .identity
                    .worktree_root
                    .as_ref()
                    .context("discard root missing")?,
                path,
            )?;
            std::fs::remove_file(destination).context("delete captured untracked file")?;
        }
        #[cfg(unix)]
        use std::os::unix::process::ExitStatusExt;
        #[cfg(windows)]
        use std::os::windows::process::ExitStatusExt;
        return Ok(std::process::Output {
            status: std::process::ExitStatus::from_raw(0),
            stdout: Vec::new(),
            stderr: Vec::new(),
        });
    }
    match action {
        BulkAction::Stage => {
            command.arg("add");
        }
        BulkAction::Unstage if intent.precondition().unborn() => {
            command.args(["rm", "--cached", "-f"]);
        }
        BulkAction::Unstage => {
            command.args(["reset", "--quiet", "HEAD"]);
        }
        BulkAction::DiscardIndex => {
            command.args(["restore", "--worktree"]);
        }
        BulkAction::DiscardHead if intent.precondition().unborn() => {
            command.args(["rm", "-f"]);
        }
        BulkAction::DiscardHead => {
            command.args(["restore", "--source=HEAD", "--staged", "--worktree"]);
        }
    }
    command.args(["--pathspec-from-file=-", "--pathspec-file-nul"]);
    let mut input = Vec::new();
    for path in path {
        input.extend_from_slice(path.raw());
        input.push(0);
    }
    progress_command(
        &mut command,
        CommandLimits {
            stdout_bytes: 64 * 1024,
            stderr_bytes: 64 * 1024,
            timeout: Duration::from_secs(120),
        },
        Some(&input),
        intent.progress.as_ref(),
        check,
    )
}

fn run_target(
    intent: &GitWriteIntent,
    path: Option<&RepositoryPath>,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<std::process::Output> {
    check()?;
    let mut command = git_command(&intent.repository)?;
    let mut input = None;
    let action = match &intent.action {
        GitWriteAction::Batch { action } => action
            .iter()
            .find(|action| path.is_some_and(|path| action.paths().contains(path)))
            .context("batch target has no action")?,
        action => action,
    };
    if let GitWriteAction::Patch {
        direction: PatchDirection::DiscardStaged,
        target,
    } = action
    {
        return discard_staged_patch(intent, target, check);
    }
    match action {
        GitWriteAction::Batch { .. } => anyhow::bail!("nested batch is invalid"),
        GitWriteAction::DiscardCombined { staged, unstaged } => {
            return compound::discard(intent, staged, unstaged, check);
        }
        GitWriteAction::UpdateRepositoryConfig {
            expected,
            replacement,
        } => {
            return config::replace(
                &intent.repository,
                expected,
                replacement,
                intent.operation(),
                check,
            );
        }
        GitWriteAction::Stage { .. } => {
            command.args(["add", "--"]);
        }
        GitWriteAction::Unstage { .. } => {
            if intent.precondition().unborn() {
                command.args(["rm", "--cached", "-f", "--"]);
            } else {
                command.args(["reset", "--quiet", "HEAD", "--"]);
            }
        }
        GitWriteAction::Discard { source, .. } => {
            let path = path.context("discard requires path")?;
            if intent.precondition().tracked(path) {
                if *source == DiscardSource::Head {
                    if intent.precondition().unborn() {
                        command.args(["rm", "-f", "--"]);
                    } else {
                        command.args(["restore", "--source=HEAD", "--staged", "--worktree", "--"]);
                    }
                } else {
                    command.args(["restore", "--worktree", "--"]);
                }
            } else {
                let destination = crate::validate_path(
                    intent
                        .repository
                        .identity
                        .worktree_root
                        .as_ref()
                        .context("discard root missing")?,
                    path,
                )?;
                std::fs::remove_file(destination).context("delete captured untracked file")?;
                command.args(["diff", "--quiet", "--"]);
            }
        }
        GitWriteAction::Patch { direction, target } => {
            command.args(["apply", "--unidiff-zero", "--whitespace=nowarn"]);
            match direction {
                PatchDirection::Stage | PatchDirection::Index { .. } => {
                    command.arg("--cached");
                }
                PatchDirection::Unstage => {
                    command.args(["--cached", "--reverse"]);
                }
                PatchDirection::Discard => {
                    command.arg("--reverse");
                }
                PatchDirection::DiscardStaged => {
                    command.args(["--index", "--reverse"]);
                }
            }
            command.arg("-");
            let (before_exists, after_exists) =
                intent.precondition().patch_presence(&target.path, *direction);
            input = Some(encode_patch(target, before_exists, after_exists)?);
        }
        GitWriteAction::CreateBranch { name } => {
            command.args(["switch", "-c", name]);
        }
        GitWriteAction::Commit { message } => {
            command.args(["commit", "--file=-"]);
            input = Some(message.as_bytes().to_vec());
        }
        GitWriteAction::CommitWithEditor {
            command: editor,
            nvim_server,
        } => {
            command
                .arg("commit")
                .env("GIT_EDITOR", editor)
                .env("GIT_SEQUENCE_EDITOR", editor)
                .env("NVIM", nvim_server);
        }
        GitWriteAction::Push => {
            command.args(["push", "--progress"]);
        }
        GitWriteAction::PublishBranch { name, .. } => {
            command.args(["push", "--progress"]);
            if !intent.precondition().has_publish_upstream() {
                command.args(["-u", "origin", name]);
            }
        }
        GitWriteAction::Pull => {
            command.args(["pull", "--progress"]);
        }
    }
    if let Some(path) = path.filter(|_| !matches!(action, GitWriteAction::Patch { .. })) {
        command.arg(resolve_argument(path)?);
    }
    let limits = CommandLimits {
        stdout_bytes: 64 * 1024,
        stderr_bytes: 64 * 1024,
        timeout: if matches!(intent.action, GitWriteAction::CommitWithEditor { .. }) {
            Duration::MAX
        } else {
            Duration::from_secs(120)
        },
    };
    progress_command(
        &mut command,
        limits,
        input.as_deref(),
        intent.progress.as_ref(),
        check,
    )
}

fn discard_staged_patch(
    intent: &GitWriteIntent,
    target: &PatchTarget,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<std::process::Output> {
    let (before_exists, after_exists) = intent
        .precondition()
        .patch_presence(&target.path, PatchDirection::DiscardStaged);
    let patch = encode_patch(target, before_exists, after_exists)?;
    let mut stdout = Vec::new();
    let mut stderr = Vec::new();
    for (cached, verify) in [(true, true), (false, true), (true, false), (false, false)] {
        check()?;
        let mut command = git_command(&intent.repository)?;
        command.args([
            "apply",
            "--reverse",
            "--unidiff-zero",
            "--whitespace=nowarn",
        ]);
        if cached {
            command.arg("--cached");
        }
        if verify {
            command.arg("--check");
        }
        command.arg("-");
        let output = progress_command(
            &mut command,
            CommandLimits {
                stdout_bytes: 64 * 1024 - stdout.len(),
                stderr_bytes: 64 * 1024 - stderr.len(),
                timeout: Duration::from_secs(120),
            },
            Some(&patch),
            intent.progress.as_ref(),
            &mut *check,
        )?;
        stdout.extend(output.stdout);
        stderr.extend(output.stderr);
        if !output.status.success() || (!cached && !verify) {
            return Ok(std::process::Output {
                status: output.status,
                stdout,
                stderr,
            });
        }
    }
    unreachable!("final worktree application returns its collected output")
}

fn validate_patch_selection(target: &PatchTarget) -> Result<()> {
    ensure!(
        !target.selected.is_empty(),
        "patch requires selected raw hunks"
    );
    for (index, identity) in target.selected.iter().enumerate() {
        ensure!(
            !target.selected[..index].contains(identity),
            "duplicate selected hunk"
        );
        patch_body(&target.analysis, *identity)?;
    }
    Ok(())
}

fn encode_patch(target: &PatchTarget, before_exists: bool, after_exists: bool) -> Result<Vec<u8>> {
    let mut output = Vec::new();
    output.extend_from_slice(b"--- ");
    if before_exists {
        write_path(b"a/", target.path.raw(), &mut output)?;
    } else {
        output.extend_from_slice(b"/dev/null");
    }
    output.extend_from_slice(b"\n+++ ");
    if after_exists {
        write_path(b"b/", target.path.raw(), &mut output)?;
    } else {
        output.extend_from_slice(b"/dev/null");
    }
    output.push(b'\n');
    for hunk in target
        .analysis
        .hunks()
        .iter()
        .filter(|hunk| target.selected.contains(&hunk.id))
    {
        writeln!(
            output,
            "@@ -{},{} +{},{} @@",
            hunk.old_lines.start + usize::from(!hunk.old_lines.is_empty()),
            hunk.old_lines.len(),
            hunk.new_lines.start + usize::from(!hunk.new_lines.is_empty()),
            hunk.new_lines.len()
        )?;
        output.extend_from_slice(&patch_body(&target.analysis, hunk.id)?.bytes);
        ensure!(
            output.len() <= 8 * 1024 * 1024,
            "serialized patch exceeds 8 MiB"
        );
    }
    Ok(output)
}

fn write_path(prefix: &[u8], path: &[u8], output: &mut Vec<u8>) -> Result<()> {
    output.push(b'"');
    for byte in prefix.iter().chain(path) {
        match byte {
            b'"' | b'\\' => {
                output.push(b'\\');
                output.push(*byte);
            }
            32..=126 => output.push(*byte),
            _ => write!(output, "\\{byte:03o}")?,
        }
    }
    output.push(b'"');
    Ok(())
}

fn settle_paths(repository: &RepositoryState, paths: &[RepositoryPath]) -> Result<WriteSettlement> {
    let started = Instant::now();
    let mut check = || {
        ensure!(
            started.elapsed() < Duration::from_secs(30),
            "settlement exceeded 30 seconds"
        );
        Ok(())
    };
    let local = repository.repository.to_thread_local();
    let head = crate::snapshot::read_head(&local)?;
    let index = crate::snapshot::read_index_stamp(&repository.identity.index, &mut check)?;
    let mut settled = Vec::new();
    for chunk in paths.chunks(256) {
        let collected =
            crate::snapshot::collect_affected(&local, &repository.identity, chunk, &mut check)
                .or_else(|_| {
                    crate::snapshot::collect_affected(
                        &local,
                        &repository.identity,
                        chunk,
                        &mut check,
                    )
                })?;
        ensure!(
            collected.head == head && collected.index == index,
            "repository changed between settlement chunks"
        );
        settled.extend(collected.path);
    }
    Ok(WriteSettlement { head, path: settled })
}

#[cfg(test)]
mod tests;
