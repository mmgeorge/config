use anyhow::{Context, Result, bail, ensure};
use std::collections::HashSet;

use crate::RepositoryPath;

mod analysis;
mod collect;
mod count_cache;
mod stats;
pub use analysis::PreparedAnalysis;
pub use collect::{
    HeadState, IndexFileStamp, IndexStamp, MetadataStamp, ObservedPath, WorktreeStamp,
};
pub(crate) use collect::{
    collect_affected, collect_comparison, collect_observation, metadata_stamp, read_head, read_index_stamp,
    read_worktree_stamp,
};
pub(crate) use count_cache::LineStatsCache;
pub(crate) use stats::collect as collect_line_stats;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct ObservationId(pub(crate) u64);

/// Reports the bounded filesystem and Git phases used to establish one stable observation.
#[derive(Clone, Debug, Default)]
pub struct ObservationTiming {
    pub queue_wait_ms: u128,
    pub head_and_index_ms: u128,
    pub initial_status_ms: u128,
    pub initial_metadata_ms: u128,
    pub verification_status_ms: u128,
    pub line_stats_ms: u128,
    pub line_stats_detail: LineStatsTiming,
    pub verification_metadata_ms: u128,
    pub identity_verification_ms: u128,
    pub collection_total_ms: u128,
    pub retry_ms: u128,
    pub retry_reason: Option<String>,
    pub overall_ms: u128,
    pub attempt_count: usize,
}

/// Aggregates line-statistics work without retaining paths or file contents.
#[derive(Clone, Debug, Default)]
pub struct LineStatsTiming {
    pub skipped: bool,
    pub eligible_pairs: usize,
    pub skipped_pairs: usize,
    pub preparation_us: u128,
    pub verification_us: u128,
    pub source_reads: usize,
    pub fast_count_pairs: usize,
    pub cache_hits: usize,
    pub count_cache_hits: usize,
    pub count_metadata_us: u128,
    pub cache_retained: usize,
    pub cache_skipped: usize,
    pub retained_entries: usize,
    pub retained_source_bytes: usize,
    pub retained_result_bytes: usize,
    pub staged_source_us: u128,
    pub worktree_source_us: u128,
    pub diff_us: u128,
    pub compared_pairs: usize,
    pub unchanged_side_pairs: usize,
    pub source_bytes: usize,
}

#[derive(Debug)]
/// Retains a full-worktree metadata observation, not exact file-content write preconditions.
pub struct RepositoryObservation {
    pub id: ObservationId,
    pub generation: crate::repository::RepositoryGeneration,
    pub worktree: crate::WorktreeId,
    pub head: HeadState,
    pub index: IndexStamp,
    pub backend: crate::reader::StatusBackend,
    pub path: Vec<ObservedPath>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ChangeKind {
    Unchanged,
    Modified,
    TypeChanged,
    Added,
    Deleted,
    Renamed,
    Copied,
    Unmerged,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LineStats {
    Unknown,
    Exact { added: u64, deleted: u64 },
    ExceedsLimit,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ContentClassification {
    Unknown,
    Text,
    Binary,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ObjectState {
    pub mode: u32,
    pub object: gix::ObjectId,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Relocation {
    pub kind: ChangeKind,
    pub origin: RepositoryPath,
    pub similarity: u8,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PathState {
    Tracked {
        head: ObjectState,
        index: ObjectState,
        relocation: Option<Relocation>,
    },
    Conflict {
        base: ObjectState,
        ours: ObjectState,
        theirs: ObjectState,
    },
    Untracked,
    Ignored,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SubmoduleState {
    pub commit_changed: bool,
    pub tracked_changed: bool,
    pub untracked_changed: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PathRecord {
    pub path: RepositoryPath,
    pub staged: ChangeKind,
    pub unstaged: ChangeKind,
    pub worktree_mode: Option<u32>,
    pub submodule: Option<SubmoduleState>,
    pub state: PathState,
    pub staged_stats: LineStats,
    pub unstaged_stats: LineStats,
    pub content: ContentClassification,
}

impl PathRecord {
    /// Includes a rename's removed origin, while a copy leaves its origin unaffected.
    pub fn affects(&self, path: &RepositoryPath) -> bool {
        self.path == *path
            || matches!(&self.state, PathState::Tracked {
                relocation: Some(relocation), ..
            } if relocation.kind == ChangeKind::Renamed && relocation.origin == *path)
    }
}

/// Parses NUL-delimited status metadata without decoding repository path bytes as text.
///
/// Rejects malformed or truncated records and caller-specified byte or record limits.
/// Unknown headers are ignored. No partial result escapes after a parse failure.
/// Returned statistics and content classifications remain unknown until content analysis.
pub fn parse_porcelain_v2(
    input: &[u8],
    hash: gix::hash::Kind,
    max_bytes: usize,
    max_records: usize,
) -> Result<Vec<PathRecord>> {
    ensure!(input.len() <= max_bytes, "status byte limit exceeded");
    if input.is_empty() {
        return Ok(Vec::new());
    }
    ensure!(input.last() == Some(&0), "status is not NUL terminated");
    let mut record = Vec::new();
    let mut seen = HashSet::new();
    let mut segment = input[..input.len() - 1].split(|byte| *byte == 0);
    while let Some(entry) = segment.next() {
        if entry.starts_with(b"# ") {
            continue;
        }
        ensure!(record.len() < max_records, "status record limit exceeded");
        ensure!(
            entry.len() >= 3 && entry[1] == b' ',
            "invalid status record"
        );
        let kind = entry[0];
        if matches!(kind, b'?' | b'!') {
            ensure!(seen.insert(&entry[2..]), "duplicate status path");
            record.push(PathRecord {
                path: RepositoryPath::new(entry[2..].to_vec())?,
                staged: ChangeKind::Unchanged,
                unstaged: ChangeKind::Unchanged,
                worktree_mode: None,
                submodule: None,
                state: if kind == b'?' {
                    PathState::Untracked
                } else {
                    PathState::Ignored
                },
                staged_stats: LineStats::Unknown,
                unstaged_stats: LineStats::Unknown,
                content: ContentClassification::Unknown,
            });
            continue;
        }
        let field_count = match kind {
            b'1' => 8,
            b'2' => 9,
            b'u' => 10,
            _ => bail!("unsupported status record type"),
        };
        let field = entry[2..]
            .splitn(field_count, |byte| *byte == b' ')
            .collect::<Vec<_>>();
        ensure!(field.len() == field_count, "missing status fields");
        ensure!(seen.insert(field[field_count - 1]), "duplicate status path");
        ensure!(field[0].len() == 2, "invalid status change pair");
        let staged = change_kind(field[0][0])?;
        let unstaged = change_kind(field[0][1])?;
        let submodule = submodule_state(field[1])?;
        let (state, worktree_mode) = if kind == b'u' {
            ensure!(
                matches!(
                    field[0],
                    b"DD" | b"AU" | b"UD" | b"UA" | b"DU" | b"AA" | b"UU"
                ),
                "invalid conflict pair"
            );
            (
                PathState::Conflict {
                    base: parse_object_state(field[2], field[6], hash)?,
                    ours: parse_object_state(field[3], field[7], hash)?,
                    theirs: parse_object_state(field[4], field[8], hash)?,
                },
                parse_mode(field[5])?,
            )
        } else {
            ensure!(
                staged != ChangeKind::Unmerged && unstaged != ChangeKind::Unmerged,
                "conflict requires unmerged record"
            );
            let relocation = if kind == b'2' {
                let score = field[7];
                ensure!(
                    score.len() >= 2 && matches!(score[0], b'R' | b'C'),
                    "invalid relocation score"
                );
                ensure!(
                    score[1..].iter().all(u8::is_ascii_digit),
                    "invalid similarity percentage"
                );
                let similarity = std::str::from_utf8(&score[1..])?.parse::<u8>()?;
                ensure!(similarity <= 100, "similarity exceeds 100");
                let relocation_kind = change_kind(score[0])?;
                ensure!(
                    staged == relocation_kind || unstaged == relocation_kind,
                    "relocation does not match change pair"
                );
                Some(Relocation {
                    kind: relocation_kind,
                    origin: RepositoryPath::new(
                        segment
                            .next()
                            .context("missing relocation origin")?
                            .to_vec(),
                    )?,
                    similarity,
                })
            } else {
                ensure!(
                    !matches!(staged, ChangeKind::Renamed | ChangeKind::Copied)
                        && !matches!(unstaged, ChangeKind::Renamed | ChangeKind::Copied),
                    "relocation requires origin record"
                );
                None
            };
            (
                PathState::Tracked {
                    head: parse_object_state(field[2], field[5], hash)?,
                    index: parse_object_state(field[3], field[6], hash)?,
                    relocation,
                },
                parse_mode(field[4])?,
            )
        };
        record.push(PathRecord {
            path: RepositoryPath::new(field[field_count - 1].to_vec())?,
            staged,
            unstaged,
            worktree_mode: Some(worktree_mode),
            submodule,
            state,
            staged_stats: LineStats::Unknown,
            unstaged_stats: LineStats::Unknown,
            content: ContentClassification::Unknown,
        });
    }
    Ok(record)
}

fn change_kind(value: u8) -> Result<ChangeKind> {
    Ok(match value {
        b'.' => ChangeKind::Unchanged,
        b'M' => ChangeKind::Modified,
        b'T' => ChangeKind::TypeChanged,
        b'A' => ChangeKind::Added,
        b'D' => ChangeKind::Deleted,
        b'R' => ChangeKind::Renamed,
        b'C' => ChangeKind::Copied,
        b'U' => ChangeKind::Unmerged,
        _ => bail!("unsupported status change kind"),
    })
}

fn parse_mode(value: &[u8]) -> Result<u32> {
    ensure!(
        value.len() == 6 && value.iter().all(|byte| matches!(byte, b'0'..=b'7')),
        "invalid Git file mode"
    );
    Ok(u32::from_str_radix(std::str::from_utf8(value)?, 8)?)
}

pub(crate) fn parse_object_state(
    mode_value: &[u8],
    object: &[u8],
    hash: gix::hash::Kind,
) -> Result<ObjectState> {
    let object = gix::ObjectId::from_hex(object)?;
    ensure!(object.kind() == hash, "Git object hash kind mismatch");
    Ok(ObjectState {
        mode: parse_mode(mode_value)?,
        object,
    })
}

fn submodule_state(value: &[u8]) -> Result<Option<SubmoduleState>> {
    if value == b"N..." {
        return Ok(None);
    }
    ensure!(
        value.len() == 4
            && value[0] == b'S'
            && matches!(value[1], b'.' | b'C')
            && matches!(value[2], b'.' | b'M')
            && matches!(value[3], b'.' | b'U'),
        "invalid submodule state"
    );
    Ok(Some(SubmoduleState {
        commit_changed: value[1] == b'C',
        tracked_changed: value[2] == b'M',
        untracked_changed: value[3] == b'U',
    }))
}
