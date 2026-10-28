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
    command::{
        CommandLimits, CommandProgressSink, CommandStream, diagnostic_command, progress_command,
    },
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

const RECEIPT_RESERVATION: usize = 1024 * 1024;

#[derive(Clone, Copy, Eq, PartialEq)]
enum BulkAction {
    Git(PathCommand),
    DeleteUntracked,
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum PathCommand {
    Stage,
    Unstage,
    DiscardIndex,
    DiscardHead,
}

#[derive(Clone, Copy)]
enum ReversePatchStep {
    CheckIndex,
    CheckWorktree,
    ApplyIndex,
    ApplyWorktree,
}

struct PathBatch {
    first: RepositoryPath,
    rest: Vec<RepositoryPath>,
}

impl PathBatch {
    fn iter(&self) -> impl Iterator<Item = &RepositoryPath> {
        std::iter::once(&self.first).chain(&self.rest)
    }
}

struct PatchCapture<'action> {
    target: &'action PatchTarget,
    before_exists: bool,
    after_exists: bool,
}

struct PatchBatch<'action> {
    first: PatchCapture<'action>,
    rest: Vec<PatchCapture<'action>>,
}

impl<'action> PatchBatch<'action> {
    fn iter(&self) -> impl Iterator<Item = &PatchCapture<'action>> {
        std::iter::once(&self.first).chain(&self.rest)
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum GroupPatchDirection {
    Stage,
    Unstage,
    Discard,
}

enum SingleWrite<'action> {
    DiscardStaged(&'action PatchTarget),
    DiscardCombined {
        staged: &'action PatchTarget,
        unstaged: &'action PatchTarget,
    },
    CreateBranch {
        name: &'action str,
    },
    Commit {
        message: &'action str,
    },
    CommitWithEditor {
        command: &'action str,
        nvim_server: &'action str,
        amend: bool,
    },
    UpdateRepositoryConfig {
        expected: &'action Option<Vec<u8>>,
        replacement: &'action [u8],
    },
    Push,
    PublishBranch {
        name: &'action str,
    },
    Pull,
}

impl SingleWrite<'_> {
    fn target(&self) -> Option<RepositoryPath> {
        match self {
            Self::DiscardStaged(target) | Self::DiscardCombined { staged: target, .. } => {
                Some(target.path.clone())
            }
            Self::UpdateRepositoryConfig { .. } => Some(RepositoryPath::project_config()),
            Self::CreateBranch { .. }
            | Self::Commit { .. }
            | Self::CommitWithEditor { .. }
            | Self::Push
            | Self::PublishBranch { .. }
            | Self::Pull => None,
        }
    }
}

enum WriteGroup<'action> {
    Bulk {
        action: BulkAction,
        target: PathBatch,
    },
    Patch {
        direction: GroupPatchDirection,
        target: PatchBatch<'action>,
    },
    Single(SingleWrite<'action>),
}

fn append_bulk_group(groups: &mut Vec<WriteGroup<'_>>, action: BulkAction, path: &RepositoryPath) {
    if let Some(WriteGroup::Bulk {
        action: previous,
        target,
    }) = groups.last_mut()
        && *previous == action
        && target.rest.len() < 255
    {
        target.rest.push(path.clone());
    } else {
        groups.push(WriteGroup::Bulk {
            action,
            target: PathBatch {
                first: path.clone(),
                rest: Vec::new(),
            },
        });
    }
}

fn append_patch_group<'action>(
    groups: &mut Vec<WriteGroup<'action>>,
    direction: GroupPatchDirection,
    patch: PatchCapture<'action>,
) {
    if let Some(WriteGroup::Patch {
        direction: previous,
        target,
    }) = groups.last_mut()
        && *previous == direction
        && target.rest.len() < 255
    {
        target.rest.push(patch);
    } else {
        groups.push(WriteGroup::Patch {
            direction,
            target: PatchBatch {
                first: patch,
                rest: Vec::new(),
            },
        });
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PatchDirection {
    Index {
        before_exists: bool,
        after_exists: bool,
    },
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
        amend: bool,
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
    preparation_timing: BTreeMap<&'static str, u128>,
    store: Arc<RepositoryStore>,
    repository: Arc<RepositoryState>,
    action: GitWriteAction,
    precondition: Option<WritePrecondition>,
    expected: Mutex<
        Option<(
            crate::snapshot::HeadState,
            Vec<crate::snapshot::ObservedPath>,
        )>,
    >,
    reservation: IntentReservation,
    progress: Option<CommandProgressSink>,
    settlement: Option<SettlementHandler>,
}

struct WriteExecution<'intent> {
    intent: &'intent GitWriteIntent,
    precondition: WritePrecondition,
}

impl std::ops::Deref for WriteExecution<'_> {
    type Target = GitWriteIntent;
    fn deref(&self) -> &Self::Target {
        self.intent
    }
}

impl WriteExecution<'_> {
    fn precondition(&self) -> &WritePrecondition {
        &self.precondition
    }
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
        let Some(worktree) = &repository.identity.worktree else {
            return Ok(());
        };
        let scope = vec![
            MutationScope::Index(worktree.clone()),
            MutationScope::WorktreeFiles(worktree.clone()),
        ];
        let operation = self.store.writes.quarantined(&scope);
        if operation.is_empty() {
            return Ok(());
        }
        repository.invalidate()?;
        repository.observe(&self.store).await?;
        self.store.writes.reconciled(&operation);
        for operation in operation {
            self.acknowledge(operation);
        }
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
    pub fn reserve(
        &self,
        repository: Arc<RepositoryState>,
        action: GitWriteAction,
    ) -> Result<GitWriteIntent> {
        action.validate()?;
        let operation = self
            .store
            .writes
            .admit(action.scopes(&repository)?, action.retained_bytes()?)?;
        self.store
            .writes
            .describe(operation, action.trace_kind(), action.paths().len());
        Ok(GitWriteIntent {
            store: Arc::clone(&self.store),
            repository,
            action,
            precondition: None,
            preparation_timing: BTreeMap::new(),
            expected: Mutex::new(None),
            reservation: IntentReservation {
                store: Arc::clone(&self.store),
                operation,
                submitted: false,
            },
            progress: None,
            settlement: None,
        })
    }

    /// Reserves retained input before collecting exact preconditions on the shared read pool.
    /// Queue order is established here. Submission never substitutes a newer source capture.
    pub async fn prepare(
        &self,
        repository: Arc<RepositoryState>,
        action: GitWriteAction,
    ) -> Result<GitWriteIntent> {
        let started = Instant::now();
        let mut timing = BTreeMap::new();
        action.validate()?;
        let retained = action.retained_bytes()?;
        let operation = self
            .store
            .writes
            .admit(action.scopes(&repository)?, retained)?;
        self.store
            .writes
            .describe(operation, action.trace_kind(), action.paths().len());
        let reservation = IntentReservation {
            store: Arc::clone(&self.store),
            operation,
            submitted: false,
        };
        let worker = Arc::clone(&repository);
        timing.insert("admission", started.elapsed().as_micros());
        let waiting = Instant::now();
        self.store.writes.phase(operation, "preparation_wait");
        self.store.writes.wait_ready(operation).await?;
        timing.insert("write_queue", waiting.elapsed().as_micros());
        self.store.writes.phase(operation, "preparing");
        let dispatching = Instant::now();
        let prepared = self
            .store
            .read(
                Arc::clone(&repository),
                retained,
                move |mut local, cancellation| {
                    timing.insert("read_dispatch", dispatching.elapsed().as_micros());
                    let started = Instant::now();
                    let mut check = || {
                        cancellation.check()?;
                        ensure!(
                            started.elapsed() < Duration::from_secs(30),
                            "write preparation exceeded 30 seconds"
                        );
                        Ok(())
                    };
                    let precondition = WritePrecondition::capture(
                        &mut local,
                        &worker,
                        &action,
                        &mut check,
                        &mut timing,
                    )?;
                    timing.insert("capture_total", started.elapsed().as_micros());
                    Ok((action, precondition, timing))
                },
            )
            .await?
            .value;
        self.store.writes.phase(operation, "prepared");
        Ok(GitWriteIntent {
            store: Arc::clone(&self.store),
            repository,
            action: prepared.0,
            precondition: Some(prepared.1),
            preparation_timing: prepared.2,
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
        self.store.writes.phase(operation, "execution_wait");
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
    /// Reports completed preparation stages in microseconds, including admission and worker dispatch.
    pub fn preparation_timing(&self) -> &BTreeMap<&'static str, u128> {
        &self.preparation_timing
    }

    /// Rejects displayed source observations that differ from this prepared write.
    /// Checks selected index entries and worktree stamps without reading unrelated paths.
    /// Failure leaves the intent unsubmitted and performs no Git mutation.
    pub fn validate_observed_sources(
        &self,
        head: &crate::snapshot::HeadState,
        observed: &[crate::snapshot::ObservedPath],
    ) -> Result<()> {
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
            *self.expected.lock().expect("write expected-source lock") =
                Some((head.clone(), observed.to_vec()));
            Ok(())
        }
    }

    /// Replaces an unprepared queued action after exact local replay, retaining its original scopes.
    pub fn replace_action(&mut self, action: GitWriteAction) -> Result<()> {
        ensure!(
            self.precondition.is_none(),
            "prepared write cannot replace its captured action"
        );
        action.validate()?;
        ensure!(
            action
                .paths()
                .iter()
                .all(|path| self.action.paths().contains(path)),
            "replayed write broadened its selected paths"
        );
        self.store
            .writes
            .resize(self.operation(), action.retained_bytes()?)?;
        self.action = action;
        Ok(())
    }

    pub fn set_settlement_handler(&mut self, handler: SettlementHandler) -> Result<()> {
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
    fn trace_kind(&self) -> &'static str {
        match self {
            Self::Batch { action } => {
                let kind = action.first().map(Self::trace_kind).unwrap_or("batch");
                if action.iter().all(|action| action.trace_kind() == kind) {
                    kind
                } else {
                    "batch"
                }
            }
            Self::Stage { .. } => "stage",
            Self::Unstage { .. } => "unstage",
            Self::Discard { .. } => "discard",
            Self::Patch { .. } => "patch",
            Self::DiscardCombined { .. } => "discard_combined",
            Self::CreateBranch { .. } => "create_branch",
            Self::Commit { .. } => "commit",
            Self::CommitWithEditor { amend: true, .. } => "amend",
            Self::CommitWithEditor { .. } => "commit_editor",
            Self::UpdateRepositoryConfig { .. } => "repository_config",
            Self::Push => "push",
            Self::PublishBranch { .. } => "publish_branch",
            Self::Pull => "pull",
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
                vec![RepositoryPath::project_config()]
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

    fn write_groups(&self, precondition: &WritePrecondition) -> Vec<WriteGroup<'_>> {
        let mut pending = vec![self];
        let mut groups = Vec::new();
        while let Some(action) = pending.pop() {
            let single = match action {
                Self::Batch { action } => {
                    pending.extend(action.iter().rev());
                    continue;
                }
                Self::Stage { path } => {
                    for path in path {
                        append_bulk_group(&mut groups, BulkAction::Git(PathCommand::Stage), path);
                    }
                    continue;
                }
                Self::Unstage { path } => {
                    for path in path {
                        append_bulk_group(&mut groups, BulkAction::Git(PathCommand::Unstage), path);
                    }
                    continue;
                }
                Self::Discard { path, source } => {
                    for path in path {
                        let command = match source {
                            DiscardSource::Index => PathCommand::DiscardIndex,
                            DiscardSource::Head => PathCommand::DiscardHead,
                        };
                        let action = if precondition.tracked(path) {
                            BulkAction::Git(command)
                        } else {
                            BulkAction::DeleteUntracked
                        };
                        append_bulk_group(&mut groups, action, path);
                    }
                    continue;
                }
                Self::Patch { direction, target } => {
                    let (before_exists, after_exists) =
                        precondition.patch_presence(&target.path, *direction);
                    let direction = match direction {
                        PatchDirection::Index { .. } | PatchDirection::Stage => {
                            GroupPatchDirection::Stage
                        }
                        PatchDirection::Unstage => GroupPatchDirection::Unstage,
                        PatchDirection::Discard => GroupPatchDirection::Discard,
                        PatchDirection::DiscardStaged => {
                            groups.push(WriteGroup::Single(SingleWrite::DiscardStaged(target)));
                            continue;
                        }
                    };
                    append_patch_group(
                        &mut groups,
                        direction,
                        PatchCapture {
                            target,
                            before_exists,
                            after_exists,
                        },
                    );
                    continue;
                }
                Self::DiscardCombined { staged, unstaged } => {
                    SingleWrite::DiscardCombined { staged, unstaged }
                }
                Self::CreateBranch { name } => SingleWrite::CreateBranch { name },
                Self::Commit { message } => SingleWrite::Commit { message },
                Self::CommitWithEditor {
                    command,
                    nvim_server,
                    amend,
                } => SingleWrite::CommitWithEditor {
                    command,
                    nvim_server,
                    amend: *amend,
                },
                Self::UpdateRepositoryConfig {
                    expected,
                    replacement,
                } => SingleWrite::UpdateRepositoryConfig {
                    expected,
                    replacement,
                },
                Self::Push => SingleWrite::Push,
                Self::PublishBranch { name, .. } => SingleWrite::PublishBranch { name },
                Self::Pull => SingleWrite::Pull,
            };
            groups.push(WriteGroup::Single(single));
        }
        groups
    }

    fn validate(&self) -> Result<()> {
        if let Self::UpdateRepositoryConfig { replacement, .. } = self {
            std::str::from_utf8(replacement).context("repository config is not UTF-8")?;
            ensure!(!replacement.contains(&0), "repository config contains NUL");
        }
        if let Self::Batch { action } = self {
            ensure!(
                !action.is_empty(),
                "batch requires at least one path operation"
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
        if let Self::CreateBranch { name } | Self::PublishBranch { name, .. } = self {
            ensure!(
                !name.is_empty() && !name.starts_with('-') && !name.contains('\0'),
                "invalid branch name"
            );
        }
        if let Self::CommitWithEditor {
            command,
            nvim_server,
            ..
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
                ..
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
        if !matches!(self, Self::Unstage { .. })
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
    intent.store.writes.phase(intent.operation(), "validating");
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
    let prepared = (|| -> Result<WritePrecondition> {
        let mut local = intent.repository.local_repository()?;
        let precondition = match intent.precondition.take() {
            Some(precondition) => precondition,
            None => WritePrecondition::capture(
                &mut local,
                &intent.repository,
                &intent.action,
                &mut check,
                &mut intent.preparation_timing,
            )?,
        };
        if let Some((head, observed)) = intent
            .expected
            .lock()
            .expect("write expected-source lock")
            .as_ref()
        {
            precondition.validate_observed(head, observed)?;
        }
        precondition.validate(&mut local, &intent.repository, &intent.action, &mut check)?;
        Ok(precondition)
    })();
    let precondition = match prepared {
        Ok(precondition) => precondition,
        Err(error) => {
            intent
                .store
                .writes
                .phase(intent.operation(), "rejected_settlement");
            let mut outcome = intent.failure(
                TargetCompletion::Rejected,
                format!("write precondition rejected: {error:#}"),
            );
            if !outcome.affected.is_empty() {
                match settle_paths(&intent.repository, &outcome.affected) {
                    Ok(path) => outcome.settled = Some(path),
                    Err(error) => {
                        outcome.settlement_diagnostic =
                            Some(format!("rejected write reconciliation failed: {error:#}"))
                    }
                }
                if let Err(error) = intent.repository.invalidate() {
                    outcome.settlement_diagnostic =
                        Some(format!("rejected write invalidation failed: {error:#}"));
                }
            }
            if !outcome.affected.is_empty() && outcome.settled.is_none() {
                let _ = guard.finish_quarantined(OperationCompletion::Failed);
            } else {
                let _ = guard.finish(OperationCompletion::Failed);
            }
            return outcome;
        }
    };
    let intent = WriteExecution {
        intent: &intent,
        precondition,
    };

    let mut outcome = WriteOutcome {
        operation: intent.operation(),
        target: Vec::new(),
        affected: intent.action.paths(),
        settled: None,
        settlement_diagnostic: None,
    };
    intent.store.writes.phase(intent.operation(), "executing");
    let mut stopped = false;
    for group in intent.action.write_groups(intent.precondition()) {
        let chunk: Vec<_> = match &group {
            WriteGroup::Bulk { target, .. } => target.iter().cloned().map(Some).collect(),
            WriteGroup::Patch { target, .. } => target
                .iter()
                .map(|target| Some(target.target.path.clone()))
                .collect(),
            WriteGroup::Single(action) => vec![action.target()],
        };
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
        let validated = match &group {
            WriteGroup::Bulk { .. } | WriteGroup::Patch { .. } => intent
                .precondition()
                .validate_targets(&intent.repository, &path, &mut check),
            WriteGroup::Single(action) => match action.target() {
                Some(path) => {
                    intent
                        .precondition()
                        .validate_target(&intent.repository, &path, &mut check)
                }
                None => Ok(()),
            },
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
        let result = match &group {
            WriteGroup::Bulk { action, target } => {
                run_path_chunk(&intent, *action, target, &mut check)
            }
            WriteGroup::Patch { direction, target } => {
                run_patch_group(&intent, *direction, target, &mut check)
            }
            WriteGroup::Single(action) => run_single_write(&intent, action, &mut check),
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

    intent
        .store
        .writes
        .phase(intent.operation(), "settling_paths");
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
        intent
            .store
            .writes
            .phase(intent.operation(), "settling_consumer");
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
    let completion = if !outcome.affected.is_empty() && outcome.settled.is_none() {
        guard.finish_quarantined(completion)
    } else {
        guard.finish(completion)
    };
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
    if text.len() <= 2048 {
        return text.to_owned();
    }
    let notice = "\n[Diagnostic truncated]\n";
    let mut end = 1024;
    while !text.is_char_boundary(end) {
        end -= 1;
    }
    let mut start = text.len() - (2048 - end - notice.len());
    while !text.is_char_boundary(start) {
        start += 1;
    }
    format!("{}{notice}{}", &text[..end], &text[start..])
}

fn run_path_chunk(
    intent: &WriteExecution<'_>,
    action: BulkAction,
    path: &PathBatch,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<std::process::Output> {
    let action = match action {
        BulkAction::DeleteUntracked => {
            for path in path.iter() {
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
        BulkAction::Git(action) => action,
    };
    let mut command = git_command(&intent.repository)?;
    match action {
        PathCommand::Stage => {
            command.arg("add");
        }
        PathCommand::Unstage if intent.precondition().unborn() => {
            command.args(["rm", "--cached", "-f"]);
        }
        PathCommand::Unstage => {
            command.args(["reset", "--quiet", "HEAD"]);
        }
        PathCommand::DiscardIndex => {
            command.args(["restore", "--worktree"]);
        }
        PathCommand::DiscardHead if intent.precondition().unborn() => {
            command.args(["rm", "-f"]);
        }
        PathCommand::DiscardHead => {
            command.args(["restore", "--source=HEAD", "--staged", "--worktree"]);
        }
    }
    command.args(["--pathspec-from-file=-", "--pathspec-file-nul"]);
    let mut input = Vec::new();
    for path in path.iter() {
        input.extend_from_slice(path.raw());
        input.push(0);
    }
    intent.store.writes.command_started(intent.operation());
    progress_command(
        &mut command,
        CommandLimits {
            stderr_bytes: 64 * 1024,
            timeout: Duration::from_secs(120),
        },
        Some(&input),
        intent.progress.as_ref(),
        check,
    )
}

fn run_patch_group(
    intent: &WriteExecution<'_>,
    direction: GroupPatchDirection,
    target: &PatchBatch<'_>,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<std::process::Output> {
    let mut input = Vec::new();
    for capture in target.iter() {
        check()?;
        let patch = encode_patch(capture.target, capture.before_exists, capture.after_exists)?;
        input.extend_from_slice(&patch);
    }
    let mut command = git_command(&intent.repository)?;
    command.args(["apply", "--unidiff-zero", "--whitespace=nowarn"]);
    match direction {
        GroupPatchDirection::Stage => {
            command.arg("--cached");
        }
        GroupPatchDirection::Unstage => {
            command.args(["--cached", "--reverse"]);
        }
        GroupPatchDirection::Discard => {
            command.arg("--reverse");
        }
    }
    command.arg("-");
    intent.store.writes.command_started(intent.operation());
    progress_command(
        &mut command,
        CommandLimits {
            stderr_bytes: 64 * 1024,
            timeout: Duration::from_secs(120),
        },
        Some(&input),
        intent.progress.as_ref(),
        check,
    )
}

fn run_single_write(
    intent: &WriteExecution<'_>,
    action: &SingleWrite<'_>,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<std::process::Output> {
    check()?;
    let mut command = git_command(&intent.repository)?;
    let mut input = None;
    match action {
        SingleWrite::DiscardStaged(target) => return discard_staged_patch(intent, target, check),
        SingleWrite::DiscardCombined { staged, unstaged } => {
            return compound::discard(intent, staged, unstaged, check);
        }
        SingleWrite::UpdateRepositoryConfig {
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
        SingleWrite::CreateBranch { name } => {
            command.args(["switch", "-c", name]);
        }
        SingleWrite::Commit { message } => {
            command.args(["commit", "--file=-"]);
            input = Some(message.as_bytes().to_vec());
        }
        SingleWrite::CommitWithEditor {
            command: editor,
            nvim_server,
            amend,
        } => {
            command.arg("commit");
            if *amend {
                command.args(["--amend", "--only"]);
            }
            command
                .env("GIT_EDITOR", editor)
                .env("GIT_SEQUENCE_EDITOR", editor)
                .env("NVIM", nvim_server);
        }
        SingleWrite::Push => {
            command.args(["push", "--progress"]);
        }
        SingleWrite::PublishBranch { name, .. } => {
            command.args(["push", "--progress"]);
            if !intent.precondition().has_publish_upstream() {
                command.args(["-u", "origin", name]);
            }
        }
        SingleWrite::Pull => {
            command.args(["pull", "--progress"]);
        }
    }
    let limits = CommandLimits {
        stderr_bytes: 64 * 1024,
        timeout: if matches!(action, SingleWrite::CommitWithEditor { .. }) {
            Duration::MAX
        } else {
            Duration::from_secs(120)
        },
    };
    let run = if matches!(
        action,
        SingleWrite::Commit { .. } | SingleWrite::CommitWithEditor { .. }
    ) {
        diagnostic_command
    } else {
        progress_command
    };
    intent.store.writes.command_started(intent.operation());
    run(
        &mut command,
        limits,
        input.as_deref(),
        intent.progress.as_ref(),
        check,
    )
}

fn discard_staged_patch(
    intent: &WriteExecution<'_>,
    target: &PatchTarget,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<std::process::Output> {
    let (before_exists, after_exists) = intent
        .precondition()
        .patch_presence(&target.path, PatchDirection::DiscardStaged);
    let patch = encode_patch(target, before_exists, after_exists)?;
    let mut collected = run_reverse_patch(
        intent,
        &patch,
        ReversePatchStep::CheckIndex,
        64 * 1024,
        check,
    )?;
    for step in [
        ReversePatchStep::CheckWorktree,
        ReversePatchStep::ApplyIndex,
        ReversePatchStep::ApplyWorktree,
    ] {
        if !collected.status.success() {
            return Ok(collected);
        }
        let output = run_reverse_patch(
            intent,
            &patch,
            step,
            64 * 1024 - collected.stderr.len(),
            check,
        )?;
        collected.status = output.status;
        collected.stdout.extend(output.stdout);
        collected.stderr.extend(output.stderr);
    }
    Ok(collected)
}

fn run_reverse_patch(
    intent: &WriteExecution<'_>,
    patch: &[u8],
    step: ReversePatchStep,
    stderr_bytes: usize,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<std::process::Output> {
    check()?;
    let mut command = git_command(&intent.repository)?;
    command.args([
        "apply",
        "--reverse",
        "--unidiff-zero",
        "--whitespace=nowarn",
    ]);
    match step {
        ReversePatchStep::CheckIndex => {
            command.args(["--cached", "--check"]);
        }
        ReversePatchStep::CheckWorktree => {
            command.arg("--check");
        }
        ReversePatchStep::ApplyIndex => {
            command.arg("--cached");
        }
        ReversePatchStep::ApplyWorktree => {}
    }
    command.arg("-");
    intent.store.writes.command_started(intent.operation());
    progress_command(
        &mut command,
        CommandLimits {
            stderr_bytes,
            timeout: Duration::from_secs(120),
        },
        Some(patch),
        intent.progress.as_ref(),
        check,
    )
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
    let local = repository.local_repository()?;
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
    Ok(WriteSettlement {
        head,
        path: settled,
    })
}

#[cfg(test)]
mod tests;
