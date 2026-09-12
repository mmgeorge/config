use std::{
    collections::{HashMap, HashSet, VecDeque},
    sync::{Arc, Mutex},
    time::Instant,
};

use anyhow::{Context, Result, ensure};
use forge_buffer::{
    block::{TargetRange, TextPosition},
    identity::{BlockId, DocumentId, DocumentRevision, TargetId},
};
use forge_diff::{
    body::BodyKind,
    cache::AnalysisHandle,
    display::DisplayCursor,
    edit::{AppliedEdit, EditDirection, EditSelection},
    engine::DiffRequest,
    raw::{RawHunkId, compute_hunks},
    source::{Representation, SourcePair, SourceVersion},
    syntax::{SyntaxHandle, SyntaxRequest},
    workers::WorkPriority,
};
use forge_git::{
    RepositoryPath, WorktreeId,
    content::{ContentSource, WorktreeConversion},
    mutation::OperationId,
    snapshot::{
        ChangeKind, HeadState, LineStats, ObjectState, ObservedPath, PathRecord, PathState,
    },
    writer::{
        GitWriteAction, GitWriteIntent, GitWriteTicket, PatchDirection, PatchTarget,
        TargetCompletion, TargetOutcome, WriteOutcome,
    },
};
use serde::Serialize;
use tokio::sync::{Mutex as AsyncMutex, broadcast, watch};

use crate::{
    BodyDelivery, BodyState, StatusInput, StatusLocation, StatusSection, StatusSelection,
    StatusService,
    document::{FileTarget, StatusDocument},
    protocol::file_target,
};

#[derive(Clone, Debug, Serialize)]
pub struct StatusUpdate {
    pub document: DocumentId,
    pub delta: crate::StatusDelta,
    pub body: Vec<BodyDelivery>,
    pub operation_id: u64,
    pub phase: &'static str,
    pub diagnostic: Vec<String>,
    pub elapsed_us: u128,
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    pub resync: bool,
}

#[derive(Clone, Debug, Serialize)]
pub struct StatusActionAccepted {
    pub operation_id: u64,
    pub update: StatusUpdate,
}

pub enum StatusOperationTicket {
    Write(GitWriteTicket),
    Pending(watch::Receiver<Option<Arc<WriteOutcome>>>),
}

impl StatusOperationTicket {
    pub async fn finish(self) -> Result<Arc<WriteOutcome>> {
        match self {
            Self::Write(ticket) => ticket.finish().await,
            Self::Pending(mut receiver) => loop {
                if let Some(outcome) = receiver.borrow_and_update().clone() {
                    return Ok(outcome);
                }
                receiver
                    .changed()
                    .await
                    .context("status operation owner stopped before settlement")?;
            },
        }
    }
}

pub(crate) struct MutationStore {
    worktree: Mutex<HashMap<WorktreeId, Arc<AsyncMutex<WorktreeJournal>>>>,
    event: broadcast::Sender<Arc<StatusUpdate>>,
}

impl Default for MutationStore {
    fn default() -> Self {
        Self {
            worktree: Mutex::new(HashMap::new()),
            event: broadcast::channel(8).0,
        }
    }
}

#[derive(Default)]
pub(crate) struct WorktreeJournal {
    confirmed: HashMap<RepositoryPath, ObservedPath>,
    confirmed_source: HashMap<RepositoryPath, SourceVersion>,
    head_source: HashMap<RepositoryPath, SourceVersion>,
    worktree_source: HashMap<RepositoryPath, SourceVersion>,
    projected: HashMap<RepositoryPath, ProjectedIndex>,
    pending: VecDeque<PendingMutation>,
    transition: VecDeque<(RepositoryPath, AppliedEdit)>,
    running: bool,
    repository: Option<Arc<forge_git::repository::RepositoryState>>,
    head: Option<HeadState>,
}

impl WorktreeJournal {
    pub(crate) fn has_pending(&self) -> bool {
        !self.pending.is_empty()
    }
}

struct PendingMutation {
    operation: OperationId,
    intent: Option<GitWriteIntent>,
    target: Vec<MutationTarget>,
    completion: watch::Sender<Option<Arc<WriteOutcome>>>,
    started: Instant,
    document: DocumentId,
    omitted: Vec<TargetOutcome>,
}

#[derive(Clone)]
struct MutationTarget {
    observed: ObservedPath,
    section: StatusSection,
    stage: bool,
    edit: Option<EditSelection>,
    after_exists: bool,
    source: Option<SourcePair>,
}

#[derive(Clone)]
enum IndexSource {
    Captured(SourceVersion),
    Worktree,
    Head,
}

#[derive(Clone)]
pub(crate) struct ProjectedIndex {
    original: ObservedPath,
    index: IndexSource,
    mode: u32,
    worktree: Option<SourceVersion>,
    head: Option<SourceVersion>,
}

impl ProjectedIndex {
    fn captured_sources(&self, section: StatusSection) -> Option<SourcePair> {
        let index = match &self.index {
            IndexSource::Captured(source) => source.clone(),
            IndexSource::Head => self.head.clone()?,
            IndexSource::Worktree => self.worktree.clone()?,
        };
        if section == StatusSection::Staged {
            Some(SourcePair {
                old: self.head.clone()?,
                new: index,
            })
        } else {
            Some(SourcePair {
                old: index,
                new: self.worktree.clone()?,
            })
        }
    }

    fn record(&self, hash: gix::hash::Kind) -> Result<ObservedPath> {
        let mut observed = self.original.clone();
        observed.analysis = Default::default();
        let (mut head, mut index) = object_state(&observed.change);
        if head.mode == 0 {
            head.object = gix::ObjectId::null(hash);
        }
        index.mode = self.mode;
        index.object = match &self.index {
            _ if self.mode == 0 => gix::ObjectId::null(hash),
            IndexSource::Captured(source) => {
                gix::objs::compute_hash(hash, gix::objs::Kind::Blob, source.bytes())?
            }
            IndexSource::Head => head.object,
            IndexSource::Worktree => match &self.worktree {
                Some(source) => {
                    gix::objs::compute_hash(hash, gix::objs::Kind::Blob, source.bytes())?
                }
                None => gix::ObjectId::null(hash),
            },
        };
        observed.change.staged = if index == head && !matches!(self.index, IndexSource::Worktree) {
            ChangeKind::Unchanged
        } else if head.mode == 0 {
            ChangeKind::Added
        } else if index.mode == 0 {
            ChangeKind::Deleted
        } else {
            ChangeKind::Modified
        };
        observed.change.unstaged = if matches!(self.index, IndexSource::Worktree)
            || self
                .captured_sources(StatusSection::Unstaged)
                .is_some_and(|source| source.old.identity() == source.new.identity())
        {
            ChangeKind::Unchanged
        } else if observed.change.worktree_mode.is_none_or(|mode| mode == 0) {
            ChangeKind::Deleted
        } else if index.mode == 0 {
            ChangeKind::Added
        } else {
            ChangeKind::Modified
        };
        if index.mode == 0 && head.mode == 0 {
            observed.change.staged = ChangeKind::Unchanged;
            observed.change.state = PathState::Untracked;
        } else {
            let relocation = match &observed.change.state {
                PathState::Tracked { relocation, .. } => relocation.clone(),
                _ => None,
            };
            observed.change.state = PathState::Tracked {
                head,
                index,
                relocation,
            };
        }
        match self.index {
            IndexSource::Worktree => {
                observed.change.staged_stats = self.original.change.unstaged_stats;
            }
            IndexSource::Head => {
                observed.change.unstaged_stats = self.original.change.staged_stats;
            }
            IndexSource::Captured(_) => {}
        }
        Ok(observed)
    }

    pub(crate) async fn sources(
        &self,
        service: &StatusService,
        repository: &Arc<forge_git::repository::RepositoryState>,
        section: StatusSection,
    ) -> Result<(SourcePair, BodyKind)> {
        let head = match &self.original.change.state {
            PathState::Tracked { head, .. } if head.mode != 0 && !head.object.is_null() => {
                Some(ContentSource::Object(head.object))
            }
            _ => None,
        };
        let index = match &self.index {
            IndexSource::Captured(source) => source.clone(),
            IndexSource::Head => match &self.head {
                Some(source) => source.clone(),
                None => crate::source::acquire(&service.store, repository, head.clone()).await?,
            },
            IndexSource::Worktree => match &self.worktree {
                Some(source) => source.clone(),
                None => {
                    crate::source::acquire(
                        &service.store,
                        repository,
                        Some(ContentSource::Worktree {
                            path: self.original.change.path.clone(),
                            conversion: WorktreeConversion::GitCanonical,
                        }),
                    )
                    .await?
                }
            },
        };
        let (source, old_exists, new_exists) = if section == StatusSection::Staged {
            let old = match &self.head {
                Some(source) => source.clone(),
                None => crate::source::acquire(&service.store, repository, head.clone()).await?,
            };
            (
                SourcePair { old, new: index },
                head.is_some(),
                self.mode != 0,
            )
        } else {
            let new = match &self.worktree {
                Some(source) => source.clone(),
                None => {
                    crate::source::acquire(
                        &service.store,
                        repository,
                        Some(ContentSource::Worktree {
                            path: self.original.change.path.clone(),
                            conversion: WorktreeConversion::GitCanonical,
                        }),
                    )
                    .await?
                }
            };
            (
                SourcePair { old: index, new },
                self.mode != 0,
                self.original.change.worktree_mode.is_some(),
            )
        };
        Ok((
            source,
            match (old_exists, new_exists) {
                (false, true) => BodyKind::Added,
                (true, false) => BodyKind::Deleted,
                _ => BodyKind::Modified,
            },
        ))
    }
}

#[derive(Clone)]
pub(crate) struct SelectionVersion {
    revision: DocumentRevision,
    file: HashMap<u64, SelectedSource>,
    target: HashMap<TargetId, (u64, Vec<RawHunkId>)>,
}

#[derive(Clone)]
struct SelectedSource {
    observed: ObservedPath,
    section: StatusSection,
    analysis: Option<AnalysisHandle>,
    generation: u64,
    body_revision: Option<DocumentRevision>,
    block: HashMap<BlockId, Vec<TargetRange>>,
    hunk: HashMap<RawHunkId, TargetId>,
    group: Vec<(crate::document::DisplayGroupTarget, Vec<RawHunkId>)>,
}

impl SelectionVersion {
    fn retained_bytes(&self) -> usize {
        self.file
            .values()
            .map(|file| {
                512 + file.observed.change.path.retained_bytes()
                    + file.analysis.as_ref().map_or(0, |analysis| {
                        analysis.retained_source_bytes() + analysis.retained_result_bytes()
                    })
                    + file
                        .block
                        .values()
                        .map(|target| target.len() * 192)
                        .sum::<usize>()
                    + file.hunk.len() * 128
                    + file
                        .group
                        .iter()
                        .map(|(_, raw)| 128 + raw.len() * 32)
                        .sum::<usize>()
            })
            .sum::<usize>()
            + self
                .target
                .values()
                .map(|(_, raw)| 128 + raw.len() * 32)
                .sum::<usize>()
    }
    pub(crate) fn capture(document: &StatusDocument) -> Self {
        Self {
            revision: document.revision,
            file: document
                .file
                .values()
                .map(|file| {
                    (
                        file.id,
                        SelectedSource {
                            observed: observed(file),
                            section: file.section,
                            analysis: file.analysis.clone(),
                            generation: file.generation,
                            body_revision: file.body.as_ref().map(|body| body.revision()),
                            block: file
                                .body
                                .as_ref()
                                .map(|body| {
                                    body.snapshot()
                                        .block
                                        .into_iter()
                                        .map(|block| (block.id, block.metadata.target))
                                        .collect()
                                })
                                .unwrap_or_default(),
                            hunk: file.hunk.clone(),
                            group: file
                                .group
                                .values()
                                .filter_map(|group| {
                                    document
                                        .hunk
                                        .get(&group.target)
                                        .map(|hunk| (group.clone(), hunk.raw.clone()))
                                })
                                .collect(),
                        },
                    )
                })
                .collect(),
            target: document
                .hunk
                .iter()
                .filter_map(|(id, hunk)| {
                    document
                        .file
                        .get(&hunk.file)
                        .map(|file| (id.clone(), (file.id, hunk.raw.clone())))
                })
                .collect(),
        }
    }

    fn select(
        &self,
        location: &StatusLocation,
        selected: &mut HashMap<u64, Option<HashSet<RawHunkId>>>,
    ) -> Result<()> {
        match location {
            StatusLocation::File { id } => {
                ensure!(self.file.contains_key(id), "selected file expired");
                selected.insert(*id, None);
            }
            StatusLocation::Section { section } => {
                for (id, file) in &self.file {
                    if file.section == *section
                        || (*section == StatusSection::Unstaged
                            && file.section == StatusSection::Untracked)
                    {
                        selected.insert(*id, None);
                    }
                }
            }
            StatusLocation::Body {
                file,
                generation,
                revision,
                block,
                position,
                target,
            } => {
                let source = self.file.get(file).context("selected hunk file expired")?;
                ensure!(
                    source.generation == *generation
                        && source
                            .body_revision
                            .is_some_and(|current| *revision <= current),
                    "selected hunk body expired"
                );
                let actual = source
                    .block
                    .get(block)
                    .context("selected hunk block expired")?
                    .iter()
                    .find(|target| contains(target, *position))
                    .context("selected row has no action target")?;
                ensure!(
                    target.as_ref().is_none_or(|target| target == &actual.id),
                    "selected hunk target differs from its row"
                );
                let (owner, raw) = self
                    .target
                    .get(&actual.id)
                    .context("selected row is not a hunk")?;
                ensure!(*owner == *file, "selected hunk belongs to another file");
                if !selected.contains_key(file) {
                    selected.insert(*file, Some(HashSet::new()));
                }
                if let Some(Some(selected)) = selected.get_mut(file) {
                    selected.extend(raw);
                }
            }
            _ => anyhow::bail!("selected status row has no changes"),
        }
        Ok(())
    }
}

fn contains(target: &TargetRange, position: TextPosition) -> bool {
    let position = (position.row, position.column);
    (target.range.start.row, target.range.start.column) <= position
        && position < (target.range.end.row, target.range.end.column)
}

fn observed(file: &FileTarget) -> ObservedPath {
    ObservedPath {
        change: file.record.clone(),
        worktree: file.stamp.clone(),
        origin: file.origin_stamp.clone(),
        analysis: Default::default(),
    }
}

fn empty() -> SourceVersion {
    SourceVersion::new(Vec::new(), Representation::GitCanonical).expect("empty canonical source")
}

fn object_state(record: &PathRecord) -> (ObjectState, ObjectState) {
    match &record.state {
        PathState::Tracked { head, index, .. } => (head.clone(), index.clone()),
        PathState::Conflict { ours, .. } => (ours.clone(), ours.clone()),
        _ => {
            let absent = ObjectState {
                mode: 0,
                object: gix::ObjectId::null(gix::hash::Kind::Sha1),
            };
            (absent.clone(), absent)
        }
    }
}

impl StatusService {
    pub(crate) async fn lock_mutations(
        &self,
        repository: &forge_git::repository::RepositoryState,
    ) -> Result<tokio::sync::OwnedMutexGuard<WorktreeJournal>> {
        let worktree = repository
            .identity
            .worktree
            .clone()
            .context("status requires a worktree")?;
        let owner = self
            .mutation
            .worktree
            .lock()
            .expect("status journal lock")
            .entry(worktree)
            .or_insert_with(|| Arc::new(AsyncMutex::new(WorktreeJournal::default())))
            .clone();
        Ok(owner.lock_owned().await)
    }

    pub(crate) async fn register_document(
        &self,
        mut document: StatusDocument,
    ) -> Result<crate::StatusSnapshot> {
        self.writer.reconcile(&document.repository).await?;
        let journal = if document.comparison.is_none() {
            Some(self.lock_mutations(&document.repository).await?)
        } else {
            None
        };
        if let Some(journal) = journal.as_ref().filter(|journal| journal.has_pending()) {
            let mut observed: HashMap<_, _> = document
                .file
                .values()
                .map(|file| (file.record.path.clone(), observed(file)))
                .collect();
            let mut ignored = document.ignored.clone();
            for (path, projection) in &journal.projected {
                if document
                    .local_path
                    .as_ref()
                    .is_none_or(|local| local == path)
                {
                    observed.insert(
                        path.clone(),
                        projection.record(document.repository.object_hash())?,
                    );
                }
            }
            for target in journal.pending.iter().flat_map(|pending| &pending.target) {
                if target.section == StatusSection::Ignored {
                    ignored.remove(&target.observed.change.path);
                }
            }
            let context = document.context.clone();
            crate::reconcile::reconcile(
                &mut document,
                journal
                    .head
                    .as_ref()
                    .context("pending status HEAD missing")?,
                &observed.into_values().collect::<Vec<_>>(),
                context,
                ignored,
            )?;
            document.pending = journal
                .pending
                .iter()
                .map(|pending| pending.operation.value())
                .collect();
            for file in document.file.values_mut() {
                file.projected = journal.projected.get(&file.record.path).cloned();
            }
        }
        let snapshot = document.snapshot();
        let mut documents = self.document.lock().expect("status document lock");
        ensure!(
            !self.closed.load(std::sync::atomic::Ordering::Acquire),
            "status service closed during open"
        );
        ensure!(
            !documents.contains_key(&document.id),
            "status document identity is already open"
        );
        document._admission.check()?;
        documents.insert(document.id.clone(), Arc::new(AsyncMutex::new(document)));
        Ok(snapshot)
    }

    pub(crate) async fn has_pending_mutation(
        &self,
        repository: &forge_git::repository::RepositoryState,
    ) -> bool {
        let owner = repository.identity.worktree.as_ref().and_then(|worktree| {
            self.mutation
                .worktree
                .lock()
                .expect("status journal lock")
                .get(worktree)
                .cloned()
        });
        if let Some(owner) = owner {
            !owner.lock().await.pending.is_empty()
        } else {
            false
        }
    }

    pub fn subscribe_updates(&self) -> broadcast::Receiver<Arc<StatusUpdate>> {
        self.mutation.event.subscribe()
    }

    pub(crate) async fn admit_mutation(
        &self,
        input: StatusInput,
        selection: Option<StatusSelection>,
    ) -> Result<(StatusActionAccepted, StatusOperationTicket)> {
        let started = Instant::now();
        let owner = self.get(&input.document)?;
        let (repository, head, version) = {
            let mut document = owner.lock().await;
            document._admission.check()?;
            input.validate()?;
            ensure!(
                document.comparison.is_none(),
                "comparison documents are read-only"
            );
            ensure!(
                document
                    .input
                    .get(&input.view)
                    .is_none_or(|sequence| *sequence < input.sequence),
                "status input sequence was already accepted"
            );
            let version = if input.revision == document.revision {
                SelectionVersion::capture(&document)
            } else {
                document
                    .history
                    .iter()
                    .rev()
                    .find(|version| version.revision == input.revision)
                    .context("selected status revision expired")?
                    .clone()
            };
            document.input.insert(input.view.clone(), input.sequence);
            (
                Arc::clone(&document.repository),
                document.observed_head.clone(),
                version,
            )
        };
        let mut selected = HashMap::new();
        if let Some(selection) = selection {
            ensure!(
                !selection.target.is_empty() && selection.target.len() <= 65536,
                "invalid status selection size"
            );
            for location in selection.target {
                version.select(&location, &mut selected)?;
            }
        } else {
            version.select(&input.location, &mut selected)?;
        }
        let worktree = repository
            .identity
            .worktree
            .clone()
            .context("status mutation requires a worktree")?;
        let journal = self
            .mutation
            .worktree
            .lock()
            .expect("status journal lock")
            .entry(worktree)
            .or_insert_with(|| Arc::new(AsyncMutex::new(WorktreeJournal::default())))
            .clone();
        let mut state = journal.lock().await;
        ensure!(
            state.pending.len() < 64,
            "status operation admission is full"
        );
        if state.pending.is_empty() {
            state.confirmed.clear();
            state.confirmed_source.clear();
            state.head_source.clear();
            state.worktree_source.clear();
            state.projected.clear();
            state.transition.clear();
        }
        state.repository = Some(Arc::clone(&repository));
        state.head = Some(head.clone());
        let mut target = Vec::new();
        for (id, raw) in selected {
            let source = &version.file[&id];
            let stage = input.action == "stage";
            if (stage && source.section == StatusSection::Staged)
                || (!stage && source.section != StatusSection::Staged)
            {
                continue;
            }
            let mut edit = raw
                .map(|selected| {
                    EditSelection::capture(
                        source
                            .analysis
                            .as_ref()
                            .context("selected hunk analysis expired")?,
                        &selected.into_iter().collect::<Vec<_>>(),
                        if stage {
                            EditDirection::Forward
                        } else {
                            EditDirection::Reverse
                        },
                    )
                    .map_err(anyhow::Error::from)
                })
                .transpose()?;
            if let Some(selected) = &mut edit {
                if let Some(projected) = state.projected.get(&source.observed.change.path)
                    && let Some(pair) =
                        projected
                            .captured_sources(StatusSection::Unstaged)
                            .or_else(|| {
                                projected
                                    .captured_sources(StatusSection::Staged)
                                    .map(|pair| SourcePair {
                                        old: pair.new.clone(),
                                        new: pair.new,
                                    })
                            })
                {
                    *selected = rebase(
                        selected,
                        &pair.old,
                        &source.observed.change.path,
                        &state.transition,
                    )?;
                }
            }
            let (head, _) = object_state(&source.observed.change);
            let after_exists = if stage {
                source.observed.change.worktree_mode.is_some()
                    || edit.as_ref().is_some_and(|edit| {
                        edit.apply()
                            .is_ok_and(|applied| !applied.after().bytes().is_empty())
                    })
            } else {
                head.mode != 0
                    || edit.as_ref().is_some_and(|edit| {
                        edit.apply()
                            .is_ok_and(|applied| !applied.after().bytes().is_empty())
                    })
            };
            target.push(MutationTarget {
                observed: source.observed.clone(),
                section: source.section,
                stage,
                edit,
                after_exists,
                source: source
                    .analysis
                    .as_ref()
                    .map(|analysis| analysis.source().clone()),
            });
        }
        ensure!(
            !target.is_empty(),
            "selection contains no applicable changes"
        );
        target.retain(|target| target.edit.as_ref().is_none_or(|edit| !edit.is_empty()));
        if target.is_empty() {
            let snapshot = self.snapshot(&input.document).await?;
            let operation = OperationId::from_value(0)?;
            let outcome = Arc::new(WriteOutcome {
                operation,
                target: Vec::new(),
                affected: Vec::new(),
                settled: Some(forge_git::writer::WriteSettlement {
                    head: head.clone(),
                    path: Vec::new(),
                }),
                settlement_diagnostic: None,
            });
            let (_, receiver) = watch::channel(Some(outcome));
            let update = StatusUpdate {
                document: input.document,
                delta: crate::reconcile::delta(&snapshot, &snapshot),
                body: Vec::new(),
                operation_id: 0,
                phase: "noop",
                diagnostic: Vec::new(),
                elapsed_us: started.elapsed().as_micros(),
                resync: false,
            };
            return Ok((
                StatusActionAccepted {
                    operation_id: 0,
                    update,
                },
                StatusOperationTicket::Pending(receiver),
            ));
        }
        target.sort_by(|left, right| {
            left.observed
                .change
                .path
                .raw()
                .cmp(right.observed.change.path.raw())
        });
        let action = write_action(&target)?;
        let mut intent = self.writer.reserve(Arc::clone(&repository), action)?;
        intent.validate_observed_sources(
            &head,
            &target
                .iter()
                .map(|target| target.observed.clone())
                .collect::<Vec<_>>(),
        )?;
        let private_path: Vec<_> = target
            .iter()
            .filter(|target| target.section == StatusSection::Ignored)
            .map(|target| target.observed.change.path.clone())
            .collect();
        if !private_path.is_empty() {
            let directory = self
                .ignored_directory
                .lock()
                .expect("ignored directory lock")
                .clone()
                .context("Forge ignored-path persistence is unavailable")?;
            let root = repository
                .identity
                .worktree_root
                .clone()
                .context("ignored paths require worktree")?;
            let retained = private_path
                .iter()
                .map(|path| path.retained_bytes() * 2 + 64)
                .sum();
            intent.set_settlement_handler(
                Arc::new(move |outcome| {
                    let completed: Vec<_> = private_path
                        .iter()
                        .filter(|path| {
                            outcome.target.iter().any(|target| {
                                target.path.as_ref() == Some(path)
                                    && target.completion == TargetCompletion::Completed
                            })
                        })
                        .cloned()
                        .collect();
                    let expected = completed.iter().cloned().collect();
                    crate::ignored::update(&directory, &root, &expected, &completed, false)
                        .map(|_| ())
                }),
                retained,
            )?;
        }
        let operation = intent.operation();
        let (completion, receiver) = watch::channel(None);
        let mut prediction = prediction_copy(&state);
        for source in version.file.values() {
            if !target
                .iter()
                .any(|target| target.observed.change.path == source.observed.change.path)
            {
                continue;
            }
            if let Some(analysis) = &source.analysis {
                let path = source.observed.change.path.clone();
                if source.section == StatusSection::Staged {
                    prediction
                        .head_source
                        .entry(path)
                        .or_insert_with(|| analysis.source().old.clone());
                } else {
                    prediction
                        .worktree_source
                        .entry(path.clone())
                        .or_insert_with(|| analysis.source().new.clone());
                    let (head, index) = object_state(&source.observed.change);
                    if head == index {
                        prediction
                            .head_source
                            .entry(path)
                            .or_insert_with(|| analysis.source().old.clone());
                    }
                }
            }
        }
        for selected in &target {
            prediction
                .confirmed
                .entry(selected.observed.change.path.clone())
                .or_insert_with(|| selected.observed.clone());
            if let Some(source) = &selected.source {
                let candidate = if selected.section == StatusSection::Staged {
                    &source.new
                } else {
                    &source.old
                };
                let (_, index) =
                    object_state(&prediction.confirmed[&selected.observed.change.path].change);
                if index.mode == 0 {
                    prediction
                        .confirmed_source
                        .entry(selected.observed.change.path.clone())
                        .or_insert_with(empty);
                } else if source_matches(candidate, &index, repository.object_hash()) {
                    prediction
                        .confirmed_source
                        .entry(selected.observed.change.path.clone())
                        .or_insert_with(|| candidate.clone());
                }
            }
            apply_prediction(&mut prediction, selected)?;
        }
        ensure!(
            prediction_cost(&prediction)
                + state
                    .pending
                    .iter()
                    .flat_map(|pending| &pending.target)
                    .chain(&target)
                    .map(target_cost)
                    .sum::<usize>()
                <= 64 * 1024 * 1024,
            "status mutation journal exceeds 64 MiB"
        );
        state.confirmed = prediction.confirmed;
        state.confirmed_source = prediction.confirmed_source;
        state.head_source = prediction.head_source;
        state.worktree_source = prediction.worktree_source;
        state.projected = prediction.projected;
        state.transition = prediction.transition;
        let affected: HashSet<_> = target
            .iter()
            .map(|target| target.observed.change.path.clone())
            .collect();
        state.pending.push_back(PendingMutation {
            operation,
            intent: Some(intent),
            target,
            completion,
            started,
            document: input.document.clone(),
            omitted: Vec::new(),
        });
        let updates = self
            .project_mutations(
                &mut state,
                &affected,
                operation.value(),
                "accepted",
                Vec::new(),
                started,
            )
            .await;
        let update = match updates {
            Ok(updates) => updates
                .into_iter()
                .find(|update| update.document == input.document)
                .unwrap_or_else(|| {
                    recovery_update(
                        &input.document,
                        operation.value(),
                        "accepted",
                        Vec::new(),
                        started,
                    )
                }),
            Err(error) => recovery_update(
                &input.document,
                operation.value(),
                "accepted",
                vec![format!(
                    "Action accepted, status projection requires recovery: {error:#}"
                )],
                started,
            ),
        };
        let launch = !state.running;
        state.running = true;
        drop(state);
        if launch {
            let service = self.clone();
            tokio::spawn(async move {
                service.drain_mutations(journal).await;
            });
        }
        Ok((
            StatusActionAccepted {
                operation_id: operation.value(),
                update,
            },
            StatusOperationTicket::Pending(receiver),
        ))
    }

    async fn project_mutations(
        &self,
        journal: &mut WorktreeJournal,
        affected: &HashSet<RepositoryPath>,
        operation: u64,
        phase: &'static str,
        diagnostic: Vec<String>,
        started: Instant,
    ) -> Result<Vec<StatusUpdate>> {
        let repository = journal
            .repository
            .as_ref()
            .context("mutation repository missing")?;
        let hash = repository.object_hash();
        let owner: Vec<_> = self
            .document
            .lock()
            .expect("status document lock")
            .values()
            .cloned()
            .collect();
        let mut updates = Vec::new();
        for owner in owner {
            let mut document = owner.lock().await;
            if document.repository.identity.worktree != repository.identity.worktree
                || document.comparison.is_some()
            {
                continue;
            }
            if document
                .local_path
                .as_ref()
                .is_some_and(|path| !affected.contains(path))
            {
                continue;
            }
            let before = SelectionVersion::capture(&document);
            let before_snapshot = document.snapshot();
            let revision = document.revision;
            let loaded: HashMap<_, _> = document
                .file
                .values()
                .filter(|file| file.body.is_some())
                .map(|file| {
                    (
                        file.record.path.clone(),
                        file.body.as_ref().map_or(128, |body| {
                            body.snapshot()
                                .block
                                .iter()
                                .map(|block| block.text.row_count())
                                .sum::<usize>()
                                .max(128)
                        }),
                    )
                })
                .collect();
            let syntax: Vec<_> = document
                .file
                .values()
                .flat_map(|file| [file.old_syntax.clone(), file.new_syntax.clone()])
                .flatten()
                .collect();
            let mut observation: HashMap<_, _> = document
                .file
                .values()
                .map(|file| (file.record.path.clone(), observed(file)))
                .collect();
            for path in affected {
                if document
                    .local_path
                    .as_ref()
                    .is_some_and(|local| local != path)
                {
                    continue;
                }
                if let Some(projected) = journal.projected.get(path) {
                    observation.insert(path.clone(), projected.record(hash)?);
                } else if let Some(confirmed) = journal.confirmed.get(path) {
                    let mut confirmed = confirmed.clone();
                    if let Some(previous) = observation.get(path) {
                        if confirmed.change.staged_stats == LineStats::Unknown {
                            confirmed.change.staged_stats = previous.change.staged_stats;
                        }
                        if confirmed.change.unstaged_stats == LineStats::Unknown {
                            confirmed.change.unstaged_stats = previous.change.unstaged_stats;
                        }
                    }
                    observation.insert(path.clone(), confirmed);
                } else {
                    observation.remove(path);
                }
            }
            let context = document.context.clone();
            let mut ignored = document.ignored.clone();
            for pending in &journal.pending {
                for target in &pending.target {
                    if target.section == StatusSection::Ignored {
                        ignored.remove(&target.observed.change.path);
                    }
                }
            }
            let head = journal.head.as_ref().context("mutation HEAD missing")?;
            crate::reconcile::reconcile(
                &mut document,
                head,
                &observation.into_values().collect::<Vec<_>>(),
                context,
                ignored,
            )?;
            document.pending = journal
                .pending
                .iter()
                .map(|pending| pending.operation.value())
                .collect();
            let mut delivery = Vec::new();
            let selected: Vec<_> = document
                .file
                .iter()
                .filter(|(_, file)| affected.contains(&file.record.path))
                .map(|(id, _)| id.clone())
                .collect();
            for id in selected {
                let file = document.file.get_mut(&id).expect("projected file exists");
                file.projected = journal.projected.get(&file.record.path).cloned();
                let Some(projection) = &file.projected else {
                    continue;
                };
                if !loaded.contains_key(&file.record.path) {
                    continue;
                }
                let Some(source) = projection.captured_sources(file.section) else {
                    continue;
                };
                if file.analysis.as_ref().is_some_and(|analysis| {
                    analysis.source().old.identity() == source.old.identity()
                        && analysis.source().new.identity() == source.new.identity()
                }) && file.body.is_some()
                {
                    continue;
                }
                let analysis = self
                    .diff
                    .compare(DiffRequest {
                        source,
                        priority: WorkPriority::Foreground,
                    })
                    .await
                    .map_err(|error| anyhow::anyhow!("optimistic diff unavailable: {error:?}"))?;
                let mut file = document.file.remove(&id).expect("selected projected file");
                let result = (|| -> Result<BodyDelivery> {
                    reset_body(&mut document, &mut file)?;
                    file.old_syntax = syntax
                        .iter()
                        .find(|syntax| syntax.source_identity() == analysis.source().old.identity())
                        .cloned();
                    file.new_syntax = syntax
                        .iter()
                        .find(|syntax| syntax.source_identity() == analysis.source().new.identity())
                        .cloned();
                    let counts = analysis.line_counts();
                    let stats = LineStats::Exact {
                        added: counts.added,
                        deleted: counts.deleted,
                    };
                    if file.section == StatusSection::Staged {
                        file.record.staged_stats = stats;
                    } else {
                        file.record.unstaged_stats = stats;
                    }
                    file.cursor = Some(
                        DisplayCursor::compact(analysis.clone(), 3, body_kind(&file))
                            .with_syntax_context(
                                file.old_syntax.as_ref(),
                                file.new_syntax.as_ref(),
                            ),
                    );
                    file.analysis = Some(analysis);
                    if let Some(previous) = before.file.get(&file.id) {
                        preserve_identity(&mut document, &mut file, previous, &journal.transition);
                    }
                    file.state = BodyState::Partial;
                    let limit = loaded[&file.record.path].min(2048);
                    let mut rows = 0;
                    while file.state == BodyState::Partial && rows < limit {
                        self.deliver(&mut document, &id, &mut file)?;
                        rows += 128;
                    }
                    Ok(full_delivery(&document, &file))
                })();
                document.file.insert(id, file);
                delivery.push(result?);
            }
            if document.revision == revision {
                document.revision = document.revision.next()?;
            }
            remember(&mut document, before);
            let update = StatusUpdate {
                document: document.id.clone(),
                delta: crate::reconcile::delta(&before_snapshot, &document.snapshot()),
                body: delivery,
                operation_id: operation,
                phase,
                diagnostic: diagnostic.clone(),
                elapsed_us: started.elapsed().as_micros(),
                resync: false,
            };
            let _ = self.mutation.event.send(Arc::new(update.clone()));
            updates.push(update);
            self.queue_context(&mut document);
        }
        Ok(updates)
    }

    pub(crate) fn queue_context(&self, document: &mut StatusDocument) {
        for file in document.file.values_mut() {
            if file.context_requested
                || file.body.is_none()
                || (file.old_syntax.is_some() && file.new_syntax.is_some())
            {
                continue;
            }
            let (Some(language), Some(analysis)) = (
                crate::source::language(&file.record.path),
                file.analysis.clone(),
            ) else {
                continue;
            };
            file.context_requested = true;
            let service = self.clone();
            let document = document.id.clone();
            let file_id = file.id;
            let generation = file.generation;
            let old = file.old_syntax.clone();
            let new = file.new_syntax.clone();
            tokio::spawn(async move {
                let started = Instant::now();
                let parse = |source, cached| async {
                    if let Some(cached) = cached {
                        (Some(cached), None)
                    } else {
                        crate::service::optional_syntax(
                            &service.syntax,
                            SyntaxRequest {
                                source,
                                language,
                                priority: WorkPriority::Visible,
                                deadline: None,
                            },
                        )
                        .await
                    }
                };
                let ((old, old_diagnostic), (new, new_diagnostic)) = tokio::join!(
                    parse(analysis.source().old.clone(), old),
                    parse(analysis.source().new.clone(), new)
                );
                let diagnostic = old_diagnostic.into_iter().chain(new_diagnostic).collect();
                if let Err(error) = service
                    .adopt_context(
                        &document, file_id, generation, &analysis, old, new, diagnostic, started,
                    )
                    .await
                {
                    if let Ok(snapshot) = service.snapshot(&document).await {
                        let _ = service.mutation.event.send(Arc::new(StatusUpdate {
                            document,
                            delta: crate::reconcile::delta(&snapshot, &snapshot),
                            body: Vec::new(),
                            operation_id: 0,
                            phase: "context_failed",
                            diagnostic: vec![format!("Hunk context update failed: {error:#}")],
                            elapsed_us: started.elapsed().as_micros(),
                            resync: true,
                        }));
                    }
                }
            });
        }
    }

    async fn adopt_context(
        &self,
        id: &DocumentId,
        file_id: u64,
        generation: u64,
        analysis: &AnalysisHandle,
        old: Option<SyntaxHandle>,
        new: Option<SyntaxHandle>,
        diagnostic: Vec<String>,
        started: Instant,
    ) -> Result<()> {
        let Ok(owner) = self.get(id) else {
            return Ok(());
        };
        let mut document = owner.lock().await;
        if document._admission.check().is_err() {
            return Ok(());
        }
        let target = file_target(file_id);
        let Some(file) = document.file.get(&target) else {
            return Ok(());
        };
        if file.generation != generation
            || file.analysis.as_ref().is_none_or(|current| {
                current.source().old.identity() != analysis.source().old.identity()
                    || current.source().new.identity() != analysis.source().new.identity()
            })
        {
            return Ok(());
        }
        let snapshot = document.snapshot();
        let before = SelectionVersion::capture(&document);
        let mut file = document
            .file
            .remove(&target)
            .expect("validated context file");
        let result = (|| -> Result<BodyDelivery> {
            let limit = file
                .body
                .as_ref()
                .map_or(128, |body| {
                    body.snapshot()
                        .block
                        .iter()
                        .map(|block| block.text.row_count())
                        .sum::<usize>()
                        .max(128)
                })
                .min(2048);
            reset_body(&mut document, &mut file)?;
            file.context_requested = true;
            file.old_syntax = old;
            file.new_syntax = new;
            file.cursor = Some(
                DisplayCursor::compact(analysis.clone(), 3, body_kind(&file))
                    .with_syntax_context(file.old_syntax.as_ref(), file.new_syntax.as_ref()),
            );
            file.state = BodyState::Partial;
            preserve_identity(
                &mut document,
                &mut file,
                &before.file[&file_id],
                &VecDeque::new(),
            );
            let mut rows = 0;
            while file.state == BodyState::Partial && rows < limit {
                self.deliver(&mut document, &target, &mut file)?;
                rows += 128;
            }
            Ok(full_delivery(&document, &file))
        })();
        document.file.insert(target, file);
        let delivery = result?;
        document.revision = document.revision.next()?;
        remember(&mut document, before);
        let update = StatusUpdate {
            document: id.clone(),
            delta: crate::reconcile::delta(&snapshot, &document.snapshot()),
            body: vec![delivery],
            operation_id: 0,
            phase: "context",
            diagnostic,
            elapsed_us: started.elapsed().as_micros(),
            resync: false,
        };
        let _ = self.mutation.event.send(Arc::new(update));
        Ok(())
    }

    async fn drain_mutations(&self, journal: Arc<AsyncMutex<WorktreeJournal>>) {
        loop {
            let (intent, operation, targets, started) = {
                let mut state = journal.lock().await;
                let Some(pending) = state.pending.front_mut() else {
                    state.running = false;
                    state.confirmed.clear();
                    state.confirmed_source.clear();
                    state.head_source.clear();
                    state.worktree_source.clear();
                    state.projected.clear();
                    state.transition.clear();
                    state.repository = None;
                    state.head = None;
                    return;
                };
                let Some(intent) = pending.intent.take() else {
                    return;
                };
                (
                    intent,
                    pending.operation,
                    pending.target.clone(),
                    pending.started,
                )
            };
            let outcome = match self.writer.submit(intent) {
                Ok(ticket) => match ticket.finish().await {
                    Ok(outcome) => outcome,
                    Err(error) => rejected(operation, &targets, format!("{error:#}")),
                },
                Err(error) => rejected(operation, &targets, format!("{error:#}")),
            };
            let mut state = journal.lock().await;
            let Some(completed) = state.pending.pop_front() else {
                return;
            };
            let outcome = if completed.omitted.is_empty() {
                outcome
            } else {
                let mut outcome = (*outcome).clone();
                outcome.target.extend(completed.omitted.clone());
                Arc::new(outcome)
            };
            let head_changed = outcome
                .settled
                .as_ref()
                .is_some_and(|settled| state.head.as_ref() != Some(&settled.head));
            let mut diagnostic: Vec<_> = outcome
                .target
                .iter()
                .filter_map(|target| target.diagnostic.clone())
                .collect();
            diagnostic.extend(outcome.settlement_diagnostic.clone());
            let mut affected: HashSet<_> = targets
                .iter()
                .map(|target| target.observed.change.path.clone())
                .collect();
            for target in &targets {
                let path = &target.observed.change.path;
                let succeeded = outcome.target.iter().any(|target| {
                    target.path.as_ref() == Some(path)
                        && target.completion == TargetCompletion::Completed
                });
                if succeeded {
                    let source = if let Some(edit) = &target.edit {
                        edit.apply().ok().map(|applied| applied.after().clone())
                    } else {
                        target.source.as_ref().map(|source| {
                            if target.stage {
                                source.new.clone()
                            } else {
                                source.old.clone()
                            }
                        })
                    };
                    if let Some(source) = source {
                        state.confirmed_source.insert(path.clone(), source);
                    } else {
                        state.confirmed_source.remove(path);
                    }
                }
                let actual = outcome.settled.as_ref().and_then(|settled| {
                    settled
                        .path
                        .iter()
                        .find(|record| record.change.path == *path)
                });
                if let Some(actual) = actual {
                    state.confirmed.insert(path.clone(), actual.clone());
                } else if outcome.settled.is_some() {
                    if let Some(mut observed) = state.confirmed.get(path).cloned() {
                        observed.change.staged = ChangeKind::Unchanged;
                        observed.change.unstaged = ChangeKind::Unchanged;
                        let (head, _) = object_state(&observed.change);
                        observed.change.state = PathState::Tracked {
                            head: head.clone(),
                            index: head.clone(),
                            relocation: None,
                        };
                        observed.change.worktree_mode = (head.mode != 0).then_some(head.mode);
                        state.confirmed.insert(path.clone(), observed);
                    }
                }
                if !succeeded && let Some(edit) = &target.edit {
                    if let Ok(inverse) = edit.apply().and_then(|applied| applied.inverse().apply())
                    {
                        state.transition.push_back((path.clone(), inverse));
                    }
                }
                if let Some(actual) = state.confirmed.get(path) {
                    let (_, index) = object_state(&actual.change);
                    if state.confirmed_source.get(path).is_some_and(|source| {
                        !source_matches(
                            source,
                            &index,
                            state
                                .repository
                                .as_ref()
                                .expect("journal repository")
                                .object_hash(),
                        )
                    }) {
                        state.confirmed_source.remove(path);
                    }
                }
            }
            if head_changed {
                diagnostic.push(
                    "HEAD changed while staging. Queued selections require a fresh status view."
                        .into(),
                );
                if let Some(repository) = state.repository.clone() {
                    match repository.observe(&self.store).await {
                        Ok(observation) => {
                            let owner: Vec<_> = self
                                .document
                                .lock()
                                .expect("status document lock")
                                .values()
                                .cloned()
                                .collect();
                            for owner in owner {
                                let document = owner.lock().await;
                                if document.repository.identity.worktree
                                    == repository.identity.worktree
                                {
                                    affected.extend(
                                        document.file.values().map(|file| file.record.path.clone()),
                                    );
                                }
                            }
                            affected.extend(
                                observation.path.iter().map(|path| path.change.path.clone()),
                            );
                            state.head = Some(observation.head.clone());
                            state.confirmed = observation
                                .path
                                .iter()
                                .map(|path| (path.change.path.clone(), path.clone()))
                                .collect();
                            state.confirmed_source.clear();
                            state.head_source.clear();
                            state.worktree_source.clear();
                        }
                        Err(error) => diagnostic
                            .push(format!("HEAD reconciliation requires refresh: {error:#}")),
                    }
                }
            }
            state.projected.clear();
            let mut pending = std::mem::take(&mut state.pending);
            for mut next in pending.drain(..) {
                affected.extend(
                    next.target
                        .iter()
                        .map(|target| target.observed.change.path.clone()),
                );
                let result = if outcome.settled.is_none() {
                    Err(anyhow::anyhow!(
                        "repository state is unverified, refresh status before further writes"
                    ))
                } else if head_changed {
                    Err(anyhow::anyhow!("HEAD changed after selection"))
                } else {
                    replay_pending(&mut state, &mut next)
                };
                if result.is_ok() && !next.target.is_empty() {
                    state.pending.push_back(next);
                    continue;
                }
                let mut cancelled = match result {
                    Ok(()) => WriteOutcome {
                        operation: next.operation,
                        target: Vec::new(),
                        affected: Vec::new(),
                        settled: outcome.settled.clone(),
                        settlement_diagnostic: None,
                    },
                    Err(error) => (*rejected(
                        next.operation,
                        &next.target,
                        format!("Queued action cancelled: {error:#}"),
                    ))
                    .clone(),
                };
                cancelled.target.extend(next.omitted);
                let mut update = recovery_update(
                    &next.document,
                    next.operation.value(),
                    "settled",
                    cancelled
                        .target
                        .iter()
                        .filter_map(|target| target.diagnostic.clone())
                        .collect(),
                    next.started,
                );
                update.resync = false;
                let _ = self.mutation.event.send(Arc::new(update));
                drop(next.intent.take());
                self.writer.acknowledge(next.operation);
                next.completion.send_replace(Some(Arc::new(cancelled)));
            }
            match self
                .project_mutations(
                    &mut state,
                    &affected,
                    operation.value(),
                    "settled",
                    diagnostic.clone(),
                    started,
                )
                .await
            {
                Ok(updates)
                    if updates
                        .iter()
                        .any(|update| update.document == completed.document) => {}
                result => {
                    if let Err(error) = result {
                        diagnostic.push(format!("Status reconciliation failed: {error:#}"));
                    }
                    let mut update = recovery_update(
                        &completed.document,
                        operation.value(),
                        "settled",
                        diagnostic,
                        started,
                    );
                    update.resync = self.get(&completed.document).is_ok();
                    let _ = self.mutation.event.send(Arc::new(update));
                }
            }
            self.writer.acknowledge(operation);
            completed.completion.send_replace(Some(outcome));
        }
    }
}

fn recovery_update(
    document: &DocumentId,
    operation_id: u64,
    phase: &'static str,
    diagnostic: Vec<String>,
    started: Instant,
) -> StatusUpdate {
    StatusUpdate {
        document: document.clone(),
        delta: crate::StatusDelta {
            document: document.clone(),
            base: DocumentRevision(0),
            next: DocumentRevision(0),
            removed: Vec::new(),
            file: Vec::new(),
            section: Vec::new(),
            pending: Vec::new(),
            head: None,
            context: None,
        },
        body: Vec::new(),
        operation_id,
        phase,
        diagnostic,
        elapsed_us: started.elapsed().as_micros(),
        resync: true,
    }
}

fn source_matches(source: &SourceVersion, index: &ObjectState, hash: gix::hash::Kind) -> bool {
    if index.mode == 0 {
        source.bytes().is_empty()
    } else {
        gix::objs::compute_hash(hash, gix::objs::Kind::Blob, source.bytes())
            .is_ok_and(|object| object == index.object)
    }
}

fn prediction_copy(state: &WorktreeJournal) -> WorktreeJournal {
    WorktreeJournal {
        confirmed: state.confirmed.clone(),
        confirmed_source: state.confirmed_source.clone(),
        head_source: state.head_source.clone(),
        worktree_source: state.worktree_source.clone(),
        projected: state.projected.clone(),
        transition: state.transition.clone(),
        head: state.head.clone(),
        repository: state.repository.clone(),
        ..WorktreeJournal::default()
    }
}

fn target_cost(target: &MutationTarget) -> usize {
    512 + target.observed.change.path.retained_bytes()
        + target
            .edit
            .as_ref()
            .map_or(0, EditSelection::retained_bytes)
        + target.source.as_ref().map_or(0, |source| {
            source.old.bytes().len() + source.new.bytes().len()
        })
}

fn prediction_cost(state: &WorktreeJournal) -> usize {
    state
        .confirmed_source
        .values()
        .map(|source| source.bytes().len())
        .sum::<usize>()
        + state
            .head_source
            .values()
            .chain(state.worktree_source.values())
            .map(|source| source.bytes().len())
            .sum::<usize>()
        + state
            .projected
            .values()
            .map(|projection| {
                512 + projection.original.change.path.retained_bytes()
                    + projection
                        .head
                        .as_ref()
                        .map_or(0, |source| source.bytes().len())
                    + projection
                        .worktree
                        .as_ref()
                        .map_or(0, |source| source.bytes().len())
                    + match &projection.index {
                        IndexSource::Captured(source) => source.bytes().len(),
                        _ => 0,
                    }
            })
            .sum::<usize>()
        + state
            .transition
            .iter()
            .map(|(path, edit)| path.retained_bytes() + edit.retained_bytes())
            .sum::<usize>()
}

fn remember(document: &mut StatusDocument, before: SelectionVersion) {
    document.history.push_back(before);
    let mut retained: usize = document
        .history
        .iter()
        .map(SelectionVersion::retained_bytes)
        .sum();
    while document.history.len() > 64 || retained > 16 * 1024 * 1024 {
        let Some(retired) = document.history.pop_front() else {
            break;
        };
        retained = retained.saturating_sub(retired.retained_bytes());
    }
}

fn preserve_identity(
    document: &mut StatusDocument,
    file: &mut FileTarget,
    previous: &SelectedSource,
    transition: &VecDeque<(RepositoryPath, AppliedEdit)>,
) {
    let (Some(before), Some(after)) = (&previous.analysis, &file.analysis) else {
        return;
    };
    let direction = if file.section == StatusSection::Staged {
        EditDirection::Reverse
    } else {
        EditDirection::Forward
    };
    let destination = if direction == EditDirection::Reverse {
        &after.source().new
    } else {
        &after.source().old
    };
    let mut correspondence = HashMap::new();
    let next: Vec<_> = after
        .hunks()
        .iter()
        .filter_map(|hunk| {
            EditSelection::capture(after, &[hunk.id], direction)
                .ok()
                .map(|selection| (hunk.id, selection))
        })
        .collect();
    for raw in before.hunks() {
        let Ok(selected) = EditSelection::capture(before, &[raw.id], direction) else {
            continue;
        };
        let Ok(selected) = rebase(&selected, destination, &file.record.path, transition) else {
            continue;
        };
        if selected.is_empty() {
            continue;
        }
        if let Some((next, _)) = next
            .iter()
            .find(|(_, candidate)| selected.same_edit(candidate))
        {
            correspondence.insert(raw.id, *next);
            if let Some(target) = previous.hunk.get(&raw.id) {
                file.hunk.insert(*next, target.clone());
                document.hunk.insert(
                    target.clone(),
                    crate::document::HunkTarget {
                        file: file_target(file.id),
                        raw: vec![*next],
                    },
                );
            }
        }
    }
    let Some(cursor) = &file.cursor else {
        return;
    };
    let mut index = 0;
    while let Some(group) = cursor.group(index) {
        for (identity, raw) in &previous.group {
            let mapped: Option<HashSet<_>> = raw
                .iter()
                .map(|raw| correspondence.get(raw).copied())
                .collect();
            if mapped.is_some_and(|mapped| {
                mapped.len() == group.raw_ids.len()
                    && group.raw_ids.iter().all(|raw| mapped.contains(raw))
            }) {
                file.group.insert(index, identity.clone());
                break;
            }
        }
        index += 1;
    }
}

fn reset_body(document: &mut StatusDocument, file: &mut FileTarget) -> Result<()> {
    if file.body.is_some() {
        file.generation = file
            .generation
            .checked_add(1)
            .context("body generation exhausted")?;
    }
    document.retained_bytes = document.retained_bytes.saturating_sub(file.retained_bytes);
    file.retained_bytes = 0;
    file.body = None;
    file.hunk.clear();
    file.group.clear();
    file.source_row.clear();
    file.loading = None;
    file.notice_delivered = false;
    file.context_requested = false;
    document
        .hunk
        .retain(|_, hunk| hunk.file != file_target(file.id));
    Ok(())
}

fn body_kind(file: &FileTarget) -> BodyKind {
    match if file.section == StatusSection::Staged {
        file.record.staged
    } else {
        file.record.unstaged
    } {
        ChangeKind::Added => BodyKind::Added,
        ChangeKind::Deleted => BodyKind::Deleted,
        _ => BodyKind::Modified,
    }
}

fn full_delivery(document: &StatusDocument, file: &FileTarget) -> BodyDelivery {
    BodyDelivery {
        document: document.id.clone(),
        file: file.id,
        generation: file.generation,
        snapshot: file.body.as_ref().map(|body| body.snapshot()),
        patch: None,
        state: file.state.clone(),
        more: file.state == BodyState::Partial,
        syntax_diagnostic: None,
    }
}

fn rejected(
    operation: OperationId,
    target: &[MutationTarget],
    diagnostic: String,
) -> Arc<WriteOutcome> {
    Arc::new(WriteOutcome {
        operation,
        target: target
            .iter()
            .map(|target| TargetOutcome {
                path: Some(target.observed.change.path.clone()),
                completion: TargetCompletion::NotStarted,
                diagnostic: Some(diagnostic.clone()),
                exit_code: None,
            })
            .collect(),
        affected: target
            .iter()
            .map(|target| target.observed.change.path.clone())
            .collect(),
        settled: None,
        settlement_diagnostic: None,
    })
}

fn replay_pending(state: &mut WorktreeJournal, pending: &mut PendingMutation) -> Result<()> {
    let mut prediction = prediction_copy(state);
    let mut retained = Vec::new();
    let hash = state
        .repository
        .as_ref()
        .context("journal repository missing")?
        .object_hash();
    for mut target in std::mem::take(&mut pending.target) {
        let path = target.observed.change.path.clone();
        let result = (|| -> Result<bool> {
            let stamp = target.observed.worktree.clone();
            let staged_stats = target.observed.change.staged_stats;
            let unstaged_stats = target.observed.change.unstaged_stats;
            if let Some(projected) = prediction.projected.get(&path) {
                target.observed = projected.record(hash)?;
            } else if let Some(confirmed) = prediction.confirmed.get(&path) {
                target.observed = confirmed.clone();
            }
            target.observed.worktree = stamp;
            target.observed.change.staged_stats = staged_stats;
            target.observed.change.unstaged_stats = unstaged_stats;
            if let Some(selection) = &target.edit {
                let destination = prediction
                    .projected
                    .get(&path)
                    .and_then(|projected| match &projected.index {
                        IndexSource::Captured(source) => Some(source.clone()),
                        IndexSource::Head => projected.head.clone(),
                        IndexSource::Worktree => projected.worktree.clone(),
                    })
                    .or_else(|| prediction.confirmed_source.get(&path).cloned())
                    .or_else(|| {
                        source_matches(
                            selection.before(),
                            &object_state(&target.observed.change).1,
                            hash,
                        )
                        .then(|| selection.before().clone())
                    })
                    .context("queued edit cannot reuse externally changed index content")?;
                target.edit = Some(rebase(
                    selection,
                    &destination,
                    &path,
                    &prediction.transition,
                )?);
                if target.edit.as_ref().is_some_and(EditSelection::is_empty) {
                    return Ok(false);
                }
            }
            write_action(std::slice::from_ref(&target))?;
            apply_prediction(&mut prediction, &target)?;
            Ok(true)
        })();
        match result {
            Ok(true) => retained.push(target),
            result => pending.omitted.push(TargetOutcome {
                path: Some(path),
                completion: if result.is_ok() {
                    TargetCompletion::Completed
                } else {
                    TargetCompletion::NotStarted
                },
                diagnostic: result
                    .err()
                    .map(|error| format!("Queued selection cancelled: {error:#}")),
                exit_code: None,
            }),
        }
    }
    pending.target = retained;
    if pending.target.is_empty() {
        return Ok(());
    }
    ensure!(
        prediction_cost(&prediction) <= 64 * 1024 * 1024,
        "replayed status journal exceeds 64 MiB"
    );
    let intent = pending
        .intent
        .as_mut()
        .context("queued operation already started")?;
    intent.replace_action(write_action(&pending.target)?)?;
    intent.validate_observed_sources(
        state.head.as_ref().context("mutation HEAD missing")?,
        &pending
            .target
            .iter()
            .map(|target| target.observed.clone())
            .collect::<Vec<_>>(),
    )?;
    state.projected = prediction.projected;
    state.transition = prediction.transition;
    Ok(())
}

fn rebase(
    selection: &EditSelection,
    destination: &SourceVersion,
    path: &RepositoryPath,
    transition: &VecDeque<(RepositoryPath, AppliedEdit)>,
) -> Result<EditSelection> {
    let mut pending = VecDeque::from([selection.clone()]);
    let mut visited = HashSet::new();
    while let Some(selection) = pending.pop_front() {
        if selection.before().identity() == destination.identity() {
            return Ok(selection);
        }
        if !visited.insert(selection.before().identity()) {
            continue;
        }
        for (owner, transition) in transition {
            if owner == path
                && let Ok(next) = selection.rebase_over(transition)
            {
                pending.push_back(next);
            }
        }
    }
    anyhow::bail!("selected edit conflicts with a newer source version")
}

fn write_action(target: &[MutationTarget]) -> Result<GitWriteAction> {
    let mut action = Vec::new();
    for target in target {
        let path = target.observed.change.path.clone();
        if let Some(selection) = &target.edit {
            let applied = selection.apply()?;
            let analysis = compute_hunks(SourcePair {
                old: selection.before().clone(),
                new: applied.after().clone(),
            })?;
            let selected = analysis.hunks().iter().map(|hunk| hunk.id).collect();
            let before_exists = object_state(&target.observed.change).1.mode != 0;
            action.push(GitWriteAction::Patch {
                direction: PatchDirection::Index {
                    before_exists,
                    after_exists: target.after_exists,
                },
                target: PatchTarget {
                    path,
                    analysis,
                    selected,
                },
            });
        } else {
            let mut path = vec![path];
            if let PathState::Tracked {
                relocation: Some(relocation),
                ..
            } = &target.observed.change.state
                && relocation.kind == ChangeKind::Renamed
            {
                path.push(relocation.origin.clone());
            }
            action.push(if target.stage {
                GitWriteAction::Stage { path }
            } else {
                GitWriteAction::Unstage { path }
            });
        }
    }
    ensure!(
        !action.is_empty(),
        "status operation has no remaining changes"
    );
    Ok(if action.len() == 1 {
        action.pop().expect("one action")
    } else {
        GitWriteAction::Batch { action }
    })
}

fn apply_prediction(state: &mut WorktreeJournal, target: &MutationTarget) -> Result<()> {
    let path = target.observed.change.path.clone();
    let original = state
        .confirmed
        .get(&path)
        .unwrap_or(&target.observed)
        .clone();
    let mut original = original;
    original.change.staged_stats = target.observed.change.staged_stats;
    original.change.unstaged_stats = target.observed.change.unstaged_stats;
    let (head_state, index_state) = object_state(&original.change);
    let previous = state.projected.get(&path);
    let mut projection = ProjectedIndex {
        original,
        index: if target.stage {
            IndexSource::Worktree
        } else {
            IndexSource::Head
        },
        mode: if target.stage {
            target.observed.change.worktree_mode.unwrap_or_else(|| {
                if matches!(target.observed.change.state, PathState::Untracked | PathState::Ignored) {
                    0o100644
                } else {
                    0
                }
            })
        } else {
            head_state.mode
        },
        worktree: previous
            .and_then(|previous| previous.worktree.clone())
            .or_else(|| state.worktree_source.get(&path).cloned()),
        head: previous
            .and_then(|previous| previous.head.clone())
            .or_else(|| state.head_source.get(&path).cloned())
            .or_else(|| (head_state.mode == 0).then(empty)),
    };
    if let Some(source) = &target.source {
        if target.section == StatusSection::Staged {
            projection.head = Some(source.old.clone());
            if projection.worktree.is_none()
                && target.observed.change.unstaged == ChangeKind::Unchanged
            {
                projection.worktree = Some(source.new.clone());
            }
        } else {
            projection.worktree = Some(source.new.clone());
            if projection.head.is_none() && head_state == index_state {
                projection.head = Some(source.old.clone());
            }
        }
    }
    if let Some(edit) = &target.edit {
        let applied = edit.apply()?;
        projection.index = IndexSource::Captured(applied.after().clone());
        projection.mode = if !target.after_exists {
            0
        } else if index_state.mode != 0 {
            index_state.mode
        } else if target.stage {
            target.observed.change.worktree_mode.unwrap_or(0o100644)
        } else {
            head_state.mode
        };
        state.transition.push_back((path.clone(), applied));
        while state.transition.len() > 128 {
            state.transition.pop_front();
        }
    }
    state.projected.insert(path, projection);
    Ok(())
}
