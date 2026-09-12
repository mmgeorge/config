use std::time::Instant;

use anyhow::Result;
use forge_diff::{
    cache::{AnalysisKey, AnalysisStore},
    engine::reservation_bytes,
    raw::{compute_counts, compute_hunks},
    source::{Representation, SourcePair, SourceVersion},
};

use super::count_cache::{
    CountEntry, CountInput, CountStamp, LineStatsCache, MAX_COUNT_BYTES, MAX_COUNT_ENTRIES,
};
use super::{ChangeKind, LineStats, LineStatsTiming, ObservedPath, PathState, PreparedAnalysis};
use crate::{
    RepositoryIdentity,
    content::{
        ContentBatch, ContentOrigin, ContentResult, ContentSource, FileContent, WorktreeConversion,
    },
};

pub(crate) fn collect(
    local: &gix::Repository,
    identity: &RepositoryIdentity,
    paths: &mut [ObservedPath],
    analysis: Option<&AnalysisStore>,
    count_cache: Option<&LineStatsCache>,
    skip_line_stats: bool,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<LineStatsTiming> {
    let mut timing = LineStatsTiming::default();
    if skip_line_stats {
        check()?;
        timing.skipped = true;
        for path in paths {
            for staged in [false, true] {
                timing.skipped_pairs += usize::from(eligible(&path.change, staged));
            }
            path.change.staged_stats = LineStats::Unknown;
            path.change.unstaged_stats = LineStats::Unknown;
            path.analysis = Default::default();
        }
        timing.eligible_pairs = timing.skipped_pairs;
        return Ok(timing);
    }
    let epoch = count_cache.map_or(0, LineStatsCache::epoch);
    let mut pending = Vec::new();
    let mut pending_bytes = 0;
    let mut batch = ContentBatch::new(local, identity);
    let empty = SourceVersion::new(Vec::new(), Representation::GitCanonical)?;
    let retention_limit = analysis.map_or(0, |cache| cache.limits().entries);
    let unstaged_count = paths
        .iter()
        .filter(|path| eligible(&path.change, false))
        .count();
    let mut unstaged_position = 0;
    let mut staged_position = unstaged_count;
    for path in paths {
        // Keep only this path's index allocation between its two comparisons.
        let mut index_content = None;
        for staged in [false, true] {
            if !eligible(&path.change, staged) {
                continue;
            }
            check()?;
            timing.eligible_pairs += 1;
            let position = if staged {
                &mut staged_position
            } else {
                &mut unstaged_position
            };
            let retain = *position < retention_limit;
            *position += 1;
            let count_key = (path.change.path.clone(), staged);
            let input = CountInput::new(&path.change, staged);
            if let Some(cached) = count_cache.and_then(|cache| cache.get(&count_key))
                && cached.input == input
            {
                let started = Instant::now();
                let matches = if staged {
                    true
                } else if let Some(root) = &identity.worktree_root {
                    Some(CountStamp::read(root, &path.change.path)?) == cached.worktree
                } else {
                    false
                };
                timing.count_metadata_us += started.elapsed().as_micros();
                if matches {
                    timing.cache_hits += 1;
                    timing.count_cache_hits += 1;
                    if staged {
                        path.change.staged_stats = cached.count;
                    } else {
                        path.change.unstaged_stats = cached.count;
                    }
                    path.analysis[usize::from(staged)] = cached.analysis;
                    continue;
                }
            }
            let started = Instant::now();
            let pair = source_pair(
                &mut batch,
                &path.change,
                staged,
                &empty,
                &mut index_content,
                check,
            )?;
            if staged {
                timing.staged_source_us += started.elapsed().as_micros();
            } else {
                timing.worktree_source_us += started.elapsed().as_micros();
            }
            let Some((source, origin)) = pair else {
                continue;
            };
            if !staged
                && path.worktree == super::WorktreeStamp::Unobserved
                && let ContentOrigin::Worktree { stamp, .. } = &origin
            {
                path.worktree = stamp.clone();
            }
            timing.source_bytes += source.old.bytes().len() + source.new.bytes().len();
            let started = Instant::now();
            let key = retain.then(|| AnalysisKey {
                old: source.old.identity(),
                new: source.new.identity(),
            });
            let cached = key.and_then(|key| analysis.and_then(|cache| cache.get(key)));
            let (counts, retained) = if let Some(cached) = cached {
                timing.cache_hits += 1;
                (cached.line_counts(), true)
            } else {
                let fast = source.old.bytes().is_empty()
                    || source.new.bytes().is_empty()
                    || source.old.bytes() == source.new.bytes();
                if fast {
                    timing.fast_count_pairs += 1
                } else {
                    timing.compared_pairs += 1
                }
                let reservation = analysis
                    .filter(|_| retain)
                    .and_then(|cache| cache.try_reserve(reservation_bytes(&source)).ok());
                if let (Some(cache), Some(reservation)) = (analysis, reservation) {
                    let diff = compute_hunks(source)?;
                    let counts = diff.line_counts();
                    cache
                        .insert(
                            key.expect("retained analysis has an identity"),
                            reservation,
                            diff,
                        )
                        .map_err(|error| {
                            anyhow::anyhow!("statistics cache publication failed: {error:?}")
                        })?;
                    timing.cache_retained += 1;
                    (counts, true)
                } else {
                    timing.cache_skipped += 1;
                    (compute_counts(&source), false)
                }
            };
            timing.diff_us += started.elapsed().as_micros();
            let count = LineStats::Exact {
                added: counts.added,
                deleted: counts.deleted,
            };
            if staged {
                path.change.staged_stats = count
            } else {
                path.change.unstaged_stats = count
            }
            if retained {
                path.analysis[usize::from(staged)] = Some(PreparedAnalysis::new(
                    key.expect("retained analysis has an identity"),
                    origin,
                ));
            }
            if count_cache.is_some() {
                let entry = CountEntry {
                    input,
                    worktree: if staged {
                        None
                    } else {
                        CountStamp::acquired(&path.worktree)
                    },
                    count,
                    analysis: path.analysis[usize::from(staged)].clone(),
                };
                let bytes = entry.retained_bytes(&count_key);
                if pending.len() < MAX_COUNT_ENTRIES && pending_bytes + bytes <= MAX_COUNT_BYTES {
                    pending_bytes += bytes;
                    pending.push((count_key, entry));
                }
            }
        }
    }
    let started = Instant::now();
    batch.verify(check)?;
    if let Some(cache) = count_cache {
        for (key, entry) in pending {
            cache.insert(epoch, key, entry);
        }
    }
    timing.verification_us = started.elapsed().as_micros();
    timing.preparation_us = batch.preparation_us;
    timing.source_reads = batch.source_reads;
    if let Some(analysis) = analysis {
        let usage = analysis.usage();
        timing.retained_entries = usage.admitted_entries;
        timing.retained_source_bytes = usage.bytes.source;
        timing.retained_result_bytes = usage.bytes.result;
    }
    Ok(timing)
}

fn eligible(record: &super::PathRecord, staged: bool) -> bool {
    if record.submodule.is_some() {
        return false;
    }
    match record.state {
        PathState::Tracked { .. } => {
            (if staged {
                record.staged
            } else {
                record.unstaged
            }) != ChangeKind::Unchanged
        }
        PathState::Untracked => !staged,
        _ => false,
    }
}

fn source_pair(
    batch: &mut ContentBatch,
    record: &super::PathRecord,
    staged: bool,
    empty: &SourceVersion,
    index_content: &mut Option<Option<FileContent>>,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<Option<(SourcePair, ContentOrigin)>> {
    let old = match &record.state {
        PathState::Tracked { head, index, .. } => {
            if index_content.is_none() {
                *index_content = Some(object_source(batch, index, empty, check)?);
            }
            if staged {
                object_source(batch, head, empty, check)?
            } else {
                index_content.clone().flatten()
            }
        }
        PathState::Untracked => Some(FileContent {
            source: empty.clone(),
            origin: ContentOrigin::Supplied,
        }),
        _ => return Ok(None),
    };
    let Some(old) = old else { return Ok(None) };
    let new = if staged {
        index_content.clone().flatten()
    } else {
        source(
            batch,
            ContentSource::Worktree {
                path: record.path.clone(),
                conversion: WorktreeConversion::GitCanonical,
            },
            empty,
            check,
        )?
    };
    Ok(new.map(|new| {
        (
            SourcePair {
                old: old.source,
                new: new.source,
            },
            new.origin,
        )
    }))
}

fn object_source(
    batch: &mut ContentBatch,
    state: &super::ObjectState,
    empty: &SourceVersion,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<Option<FileContent>> {
    if state.mode == 0 || state.object.is_null() {
        return Ok(Some(FileContent {
            source: empty.clone(),
            origin: ContentOrigin::Supplied,
        }));
    }
    if state.mode == 0o160000 {
        return Ok(None);
    }
    source(batch, ContentSource::Object(state.object), empty, check)
}

fn source(
    batch: &mut ContentBatch,
    source: ContentSource,
    empty: &SourceVersion,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<Option<FileContent>> {
    match batch.acquire(source, check)? {
        ContentResult::Ready(content) => Ok(Some(content)),
        ContentResult::Missing => Ok(Some(FileContent {
            source: empty.clone(),
            origin: ContentOrigin::Supplied,
        })),
        ContentResult::Binary | ContentResult::TooLarge { .. } | ContentResult::Unavailable(_) => {
            Ok(None)
        }
        ContentResult::Failed(error) => Err(error),
    }
}
