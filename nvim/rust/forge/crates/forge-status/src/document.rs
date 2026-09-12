use crate::{BodyState, StatusSection, protocol::*};
use anyhow::{Result, ensure};
use forge_buffer::{
    block::{BlockMetadata, BufferBlock, Decoration, TargetRange, TextPosition, TextRange},
    document::BufferDocument,
    identity::{BlockId, DocumentId, DocumentRevision, InputSequence, TargetId, ViewId},
    text::BufferText,
};
use forge_diff::{
    cache::AnalysisHandle, display::DisplayCursor, raw::RawHunkId, syntax::SyntaxHandle,
};
use forge_git::{
    repository::RepositoryState,
    snapshot::{
        ChangeKind, HeadState, PathRecord, PathState, RepositoryObservation, WorktreeStamp,
    },
};
use std::collections::HashMap;
use std::sync::{Arc, atomic::AtomicBool};

pub(crate) const DOCUMENT_BYTES: usize = 16 * 1024 * 1024;
pub(crate) use forge_buffer::admission::{DocumentAdmission, DocumentAdmissionStore};

pub(crate) struct StatusDocument {
    pub id: DocumentId,
    pub revision: DocumentRevision,
    pub repository: Arc<RepositoryState>,
    pub observed_head: HeadState,
    pub file: HashMap<TargetId, FileTarget>,
    pub order: Vec<TargetId>,
    pub hunk: HashMap<TargetId, HunkTarget>,
    pub ignored: crate::ignored::IgnoredPathSet,
    pub input: HashMap<ViewId, InputSequence>,
    pub sequence: u64,
    pub retained_bytes: usize,
    pub context: Option<crate::context::StatusContext>,
    pub comparison: Option<forge_git::revision::comparison::ComparisonRequest>,
    pub comparison_label: Option<String>,
    pub local_path: Option<forge_git::RepositoryPath>,
    pub _admission: DocumentAdmission,
    pub history: std::collections::VecDeque<crate::mutation::SelectionVersion>,
    pub pending: Vec<u64>,
}

pub(crate) struct HunkTarget {
    pub file: TargetId,
    pub raw: Vec<RawHunkId>,
}

#[derive(Clone)]
pub(crate) struct DisplayGroupTarget {
    pub header: BlockId,
    pub target: TargetId,
}

pub(crate) struct FileTarget {
    pub id: u64,
    pub generation: u64,
    pub body: Option<BufferDocument>,
    pub prepared: Option<forge_git::snapshot::PreparedAnalysis>,
    pub notice_delivered: bool,
    pub record: PathRecord,
    pub stamp: WorktreeStamp,
    pub origin_stamp: Option<WorktreeStamp>,
    pub retained_bytes: usize,
    pub section: StatusSection,
    pub loading: Option<Arc<AtomicBool>>,
    pub hunk: HashMap<RawHunkId, TargetId>,
    pub group: HashMap<usize, DisplayGroupTarget>,
    pub source_row: HashMap<BlockId, Vec<forge_diff::source::SourceCoordinate>>,
    pub state: BodyState,
    pub analysis: Option<AnalysisHandle>,
    pub cursor: Option<DisplayCursor>,
    pub old_syntax: Option<SyntaxHandle>,
    pub new_syntax: Option<SyntaxHandle>,
    pub projected: Option<crate::mutation::ProjectedIndex>,
    pub context_requested: bool,
}

impl FileTarget {
    pub fn body_mut(&mut self) -> Result<&mut BufferDocument> {
        if self.body.is_none() {
            self.body = Some(BufferDocument::new(
                DocumentId(format!("body:{}:{}", self.id, self.generation)),
                Vec::new(),
            )?);
        }
        Ok(self.body.as_mut().expect("initialized body"))
    }

    pub fn semantic(&self) -> StatusFile {
        let record = &self.record;
        let kind = match self.section {
            StatusSection::Staged => record.staged,
            StatusSection::Untracked => ChangeKind::Added,
            _ => record.unstaged,
        };
        let change = match kind {
            ChangeKind::Added => "added",
            ChangeKind::Deleted => "deleted",
            ChangeKind::Renamed => "renamed",
            ChangeKind::Copied => "copied",
            ChangeKind::Unmerged => "conflicted",
            _ => "modified",
        };
        let stats = if self.section == StatusSection::Staged {
            record.staged_stats
        } else {
            record.unstaged_stats
        };
        let stats = match stats {
            forge_git::snapshot::LineStats::Unknown => StatusStatistics::Unknown,
            forge_git::snapshot::LineStats::ExceedsLimit => StatusStatistics::ExceedsLimit,
            forge_git::snapshot::LineStats::Exact { added, deleted } => {
                StatusStatistics::Exact { added, deleted }
            }
        };
        let origin = match &record.state {
            PathState::Tracked {
                relocation: Some(relocation),
                ..
            } => Some(relocation.origin.display_label().replace(['\n', '\r'], " ")),
            _ => None,
        };
        StatusFile {
            id: self.id,
            section: self.section,
            change,
            path: record.path.display_label().replace(['\n', '\r'], " "),
            origin,
            untracked: matches!(record.state, PathState::Untracked),
            stats,
            generation: self.generation,
        }
    }
}

impl StatusDocument {
    pub fn new(
        id: DocumentId,
        repository: Arc<RepositoryState>,
        observation: &RepositoryObservation,
        admission: DocumentAdmission,
        context: crate::context::StatusContext,
        ignored: crate::ignored::IgnoredPathSet,
    ) -> Result<Self> {
        Self::create(
            id,
            repository,
            observation.head.clone(),
            &observation.path,
            admission,
            Some(context),
            ignored,
            None,
            None,
            None,
        )
    }

    pub fn comparison(
        id: DocumentId,
        repository: Arc<RepositoryState>,
        observation: &forge_git::revision::comparison::ComparisonObservation,
        request: forge_git::revision::comparison::ComparisonRequest,
        admission: DocumentAdmission,
        title: String,
    ) -> Result<Self> {
        Self::create(
            id,
            repository,
            HeadState::Detached {
                target: observation.commit,
            },
            &observation.path,
            admission,
            None,
            Default::default(),
            Some(request),
            Some(title),
            None,
        )
    }

    pub fn local(
        id: DocumentId,
        repository: Arc<RepositoryState>,
        observation: &RepositoryObservation,
        path: forge_git::RepositoryPath,
        admission: DocumentAdmission,
        ignored: crate::ignored::IgnoredPathSet,
    ) -> Result<Self> {
        let observed: Vec<_> = observation
            .path
            .iter()
            .filter(|observed| observed.change.affects(&path))
            .cloned()
            .collect();
        Self::create(
            id,
            repository,
            observation.head.clone(),
            &observed,
            admission,
            None,
            ignored,
            None,
            None,
            Some(path),
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn create(
        id: DocumentId,
        repository: Arc<RepositoryState>,
        observed_head: HeadState,
        observed: &[forge_git::snapshot::ObservedPath],
        admission: DocumentAdmission,
        context: Option<crate::context::StatusContext>,
        ignored: crate::ignored::IgnoredPathSet,
        comparison: Option<forge_git::revision::comparison::ComparisonRequest>,
        comparison_label: Option<String>,
        local_path: Option<forge_git::RepositoryPath>,
    ) -> Result<Self> {
        let mut sequence = 0;
        let (order, file, retained_bytes) =
            inventory(observed, &ignored, &HashMap::new(), &mut sequence)?;
        let retained_bytes = retained_bytes
            + context_cost(context.as_ref())
            + local_path.as_ref().map_or(0, |path| path.retained_bytes());
        ensure!(
            retained_bytes <= DOCUMENT_BYTES,
            "status inventory exceeds document budget"
        );
        Ok(Self {
            id,
            revision: DocumentRevision(0),
            repository,
            observed_head,
            file,
            order,
            hunk: HashMap::new(),
            ignored,
            input: HashMap::new(),
            sequence,
            retained_bytes,
            context,
            comparison,
            comparison_label,
            local_path,
            _admission: admission,
            history: Default::default(),
            pending: Vec::new(),
        })
    }

    pub fn snapshot(&self) -> StatusSnapshot {
        let view = if let Some(comparison) = &self.comparison {
            StatusView::Comparison {
                title: self
                    .comparison_label
                    .clone()
                    .unwrap_or_else(|| comparison.reference.clone()),
                path: comparison.path.as_ref().map(|path| path.display_label()),
                worktree: comparison.worktree,
            }
        } else if let Some(path) = &self.local_path {
            StatusView::Local {
                path: path.display_label(),
            }
        } else {
            StatusView::Status
        };
        let head = match &self.observed_head {
            HeadState::Unborn { reference } => StatusHead {
                state: "unborn",
                reference: Some(String::from_utf8_lossy(reference).into()),
                object: None,
            },
            HeadState::Attached { reference, target } => StatusHead {
                state: "attached",
                reference: Some(String::from_utf8_lossy(reference).into()),
                object: Some(target.to_string()),
            },
            HeadState::Detached { target } => StatusHead {
                state: "detached",
                reference: None,
                object: Some(target.to_string()),
            },
        };
        let mut section: Vec<StatusSectionRecord> = Vec::new();
        let file: Vec<_> = self
            .order
            .iter()
            .map(|target| self.file[target].semantic())
            .collect();
        for entry in &file {
            let kind = if entry.section == StatusSection::Untracked {
                StatusSection::Unstaged
            } else {
                entry.section
            };
            if section.last().is_none_or(|previous| previous.kind != kind) {
                section.push(StatusSectionRecord {
                    kind,
                    file: Vec::new(),
                });
            }
            section
                .last_mut()
                .expect("section inserted")
                .file
                .push(entry.id);
        }
        StatusSnapshot {
            document: self.id.clone(),
            revision: self.revision,
            view,
            head,
            context: self.context.clone(),
            section,
            file,
            pending: self.pending.clone(),
        }
    }

    pub fn next_id(&mut self, kind: &str) -> Result<String> {
        self.sequence = next_handle(self.sequence)?;
        Ok(format!("{kind}:{}", self.sequence))
    }
}

fn next_handle(previous: u64) -> Result<u64> {
    ensure!(
        previous < forge_buffer::MAX_COUNTER,
        "status identity exhausted"
    );
    Ok(previous + 1)
}

pub(crate) fn context_cost(context: Option<&crate::context::StatusContext>) -> usize {
    context.map_or(0, |context| {
        context.config_source.as_ref().map_or(0, Vec::capacity)
            + context
                .recent
                .iter()
                .chain(context.head.iter())
                .chain(context.upstream.iter())
                .chain(context.push.iter())
                .map(|commit| {
                    commit.oid.len() + commit.reference.len() + commit.subject.len() + 128
                })
                .sum::<usize>()
            + 4096
    })
}

pub(crate) fn inventory(
    observed_path: &[forge_git::snapshot::ObservedPath],
    ignored: &crate::ignored::IgnoredPathSet,
    previous: &HashMap<TargetId, FileTarget>,
    sequence: &mut u64,
) -> Result<(Vec<TargetId>, HashMap<TargetId, FileTarget>, usize)> {
    let previous: HashMap<_, _> = previous
        .values()
        .map(|file| {
            (
                (file.section, &file.record.path),
                (file.id, file.generation),
            )
        })
        .collect();
    let mut file = HashMap::new();
    let mut order = Vec::new();
    let mut retained_bytes = 1024
        + ignored
            .iter()
            .map(|path| path.retained_bytes() + 64)
            .sum::<usize>();
    for section in [
        StatusSection::Unstaged,
        StatusSection::Staged,
        StatusSection::Conflicted,
        StatusSection::Ignored,
    ] {
        let mut selected: Vec<_> = observed_path
            .iter()
            .filter(|path| {
                let hidden = ignored.contains(&path.change.path);
                match section {
                    StatusSection::Unstaged => {
                        !hidden
                            && (belongs(&path.change, section)
                                || belongs(&path.change, StatusSection::Untracked))
                    }
                    StatusSection::Ignored => {
                        hidden
                            && (belongs(&path.change, StatusSection::Unstaged)
                                || belongs(&path.change, StatusSection::Untracked))
                    }
                    _ => belongs(&path.change, section),
                }
            })
            .collect();
        selected.sort_unstable_by(|left, right| left.change.path.raw().cmp(right.change.path.raw()));
        for observed in selected {
            let record = &observed.change;
            let section = if section == StatusSection::Unstaged
                && matches!(record.state, PathState::Untracked)
            {
                StatusSection::Untracked
            } else {
                section
            };
            let (id, generation) = match previous.get(&(section, &record.path)) {
                Some(previous) => *previous,
                None => {
                    *sequence = next_handle(*sequence)?;
                    (*sequence, 1)
                }
            };
            retained_bytes += record.path.retained_bytes().saturating_mul(4) + 1024;
            retained_bytes += observed.analysis[usize::from(section == StatusSection::Staged)]
                .as_ref()
                .map_or(0, forge_git::snapshot::PreparedAnalysis::retained_bytes);
            ensure!(
                retained_bytes <= DOCUMENT_BYTES,
                "status inventory exceeds 16 MiB document budget"
            );
            let target = file_target(id);
            order.push(target.clone());
            file.insert(
                target,
                FileTarget {
                    id,
                    generation,
                    body: None,
                    prepared: observed.analysis[usize::from(section == StatusSection::Staged)]
                        .clone(),
                    record: record.clone(),
                    stamp: observed.worktree.clone(),
                    origin_stamp: observed.origin.clone(),
                    retained_bytes: 0,
                    notice_delivered: false,
                    source_row: HashMap::new(),
                    loading: None,
                    hunk: HashMap::new(),
                    group: HashMap::new(),
                    section,
                    state: BodyState::Deferred,
                    analysis: None,
                    cursor: None,
                    old_syntax: None,
                    new_syntax: None,
                    projected: None,
                    context_requested: false,
                },
            );
        }
    }
    Ok((order, file, retained_bytes))
}

fn belongs(record: &PathRecord, section: StatusSection) -> bool {
    match section {
        StatusSection::Staged => {
            matches!(record.state, PathState::Tracked { .. })
                && record.staged != ChangeKind::Unchanged
        }
        StatusSection::Unstaged => {
            matches!(record.state, PathState::Tracked { .. })
                && record.unstaged != ChangeKind::Unchanged
        }
        StatusSection::Untracked => matches!(record.state, PathState::Untracked),
        StatusSection::Conflicted => matches!(record.state, PathState::Conflict { .. }),
        StatusSection::Ignored => false,
    }
}

pub(crate) fn label(
    id: BlockId,
    text: &str,
    capture: &str,
    target: Option<TargetId>,
) -> Result<BufferBlock> {
    let range = TextRange {
        start: TextPosition { row: 0, column: 0 },
        end: TextPosition {
            row: 0,
            column: text.len(),
        },
    };
    let mut metadata = BlockMetadata::default();
    metadata.decoration.push(Decoration {
        range: range.clone(),
        capture: capture.into(),
        priority: 100,
    });
    if let Some(id) = target {
        metadata.target.push(TargetRange { id, range });
    }
    Ok(BufferBlock {
        id,
        text: BufferText::from_rows([text])?,
        metadata,
    })
}
