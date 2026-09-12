use std::{
    collections::{HashMap, VecDeque},
    fs, io,
    path::Path,
    sync::Mutex,
    time::SystemTime,
};

use anyhow::{Context, Result};

use super::{LineStats, ObjectState, PathRecord, PathState, PreparedAnalysis, WorktreeStamp};
use crate::{RepositoryPath, resolve_argument};

pub(super) const MAX_COUNT_ENTRIES: usize = 8192;
pub(super) const MAX_COUNT_BYTES: usize = 4 * 1024 * 1024;

type CountKey = (RepositoryPath, bool);

#[derive(Default)]
pub(crate) struct LineStatsCache {
    state: Mutex<CountState>,
}

#[derive(Default)]
struct CountState {
    epoch: u64,
    bytes: usize,
    entry: HashMap<CountKey, CountEntry>,
    order: VecDeque<CountKey>,
}

#[derive(Clone)]
pub(super) struct CountEntry {
    pub input: CountInput,
    pub worktree: Option<CountStamp>,
    pub count: LineStats,
    pub analysis: Option<PreparedAnalysis>,
}

#[derive(Clone, Eq, PartialEq)]
pub(super) struct CountInput {
    old: Option<ObjectState>,
    new: Option<ObjectState>,
    worktree_mode: Option<u32>,
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub(super) enum CountStamp {
    Missing,
    File { modified: SystemTime, length: u64 },
    Symlink { modified: SystemTime, length: u64 },
    Other,
}

impl LineStatsCache {
    pub(super) fn epoch(&self) -> u64 {
        self.state.lock().expect("line statistics cache lock").epoch
    }

    pub(crate) fn invalidate(&self) {
        let mut state = self.state.lock().expect("line statistics cache lock");
        state.epoch = state
            .epoch
            .checked_add(1)
            .expect("line statistics epoch exhausted");
        state.entry.retain(|(_, staged), _| *staged);
        state.order.retain(|(_, staged)| *staged);
        state.bytes = state
            .entry
            .iter()
            .map(|(key, entry)| entry.retained_bytes(key))
            .sum();
    }

    pub(super) fn get(&self, key: &CountKey) -> Option<CountEntry> {
        self.state
            .lock()
            .expect("line statistics cache lock")
            .entry
            .get(key)
            .cloned()
    }

    pub(super) fn insert(&self, epoch: u64, key: CountKey, entry: CountEntry) {
        let mut state = self.state.lock().expect("line statistics cache lock");
        if epoch != state.epoch {
            return;
        }
        let bytes = entry.retained_bytes(&key);
        if bytes > MAX_COUNT_BYTES {
            return;
        }
        if let Some(previous) = state.entry.remove(&key) {
            state.bytes -= previous.retained_bytes(&key);
            state.order.retain(|retained| retained != &key);
        }
        while state.entry.len() >= MAX_COUNT_ENTRIES || state.bytes + bytes > MAX_COUNT_BYTES {
            let oldest = state
                .order
                .pop_front()
                .expect("count cache eviction has an entry");
            if let Some(previous) = state.entry.remove(&oldest) {
                state.bytes -= previous.retained_bytes(&oldest);
            }
        }
        state.bytes += bytes;
        state.order.push_back(key.clone());
        state.entry.insert(key, entry);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn entry() -> CountEntry {
        CountEntry {
            input: CountInput {
                old: None,
                new: None,
                worktree_mode: None,
            },
            worktree: Some(CountStamp::Missing),
            count: LineStats::Exact {
                added: 0,
                deleted: 0,
            },
            analysis: None,
        }
    }

    #[test]
    fn invalidation_expires_worktree_entries_and_rejects_old_publications() {
        let cache = LineStatsCache::default();
        let epoch = cache.epoch();
        let worktree = (RepositoryPath::new(b"worktree".to_vec()).unwrap(), false);
        let staged = (RepositoryPath::new(b"staged".to_vec()).unwrap(), true);
        cache.insert(epoch, worktree.clone(), entry());
        cache.insert(epoch, staged.clone(), entry());
        cache.invalidate();
        cache.insert(epoch, worktree.clone(), entry());
        assert!(cache.get(&worktree).is_none());
        assert!(cache.get(&staged).is_some());
        cache.insert(cache.epoch(), worktree.clone(), entry());
        assert!(cache.get(&worktree).is_some());
    }

    #[test]
    fn count_retention_evicts_old_entries_within_both_limits() {
        let cache = LineStatsCache::default();
        for number in 0..MAX_COUNT_ENTRIES + 8 {
            let path = RepositoryPath::new(format!("file-{number}").into_bytes()).unwrap();
            cache.insert(0, (path, false), entry());
        }
        let state = cache.state.lock().unwrap();
        assert!(state.entry.len() <= MAX_COUNT_ENTRIES);
        assert_eq!(state.order.len(), state.entry.len());
        assert!(state.bytes <= MAX_COUNT_BYTES);
        assert!(
            !state
                .entry
                .contains_key(&(RepositoryPath::new(b"file-0".to_vec()).unwrap(), false))
        );
    }
}

impl CountEntry {
    pub(super) fn retained_bytes(&self, key: &CountKey) -> usize {
        std::mem::size_of::<Self>()
            + 2 * (std::mem::size_of::<CountKey>() + key.0.retained_bytes())
            + self
                .analysis
                .as_ref()
                .map_or(0, PreparedAnalysis::retained_bytes)
    }
}

impl CountInput {
    pub(super) fn new(record: &PathRecord, staged: bool) -> Self {
        let (old, new) = match &record.state {
            PathState::Tracked { head, index, .. } if staged => {
                (Some(head.clone()), Some(index.clone()))
            }
            PathState::Tracked { index, .. } => (Some(index.clone()), None),
            _ => (None, None),
        };
        Self {
            old,
            new,
            worktree_mode: (!staged).then_some(record.worktree_mode).flatten(),
        }
    }
}

impl CountStamp {
    pub(super) fn read(root: &Path, path: &RepositoryPath) -> Result<Self> {
        let metadata = match fs::symlink_metadata(root.join(resolve_argument(path)?)) {
            Ok(metadata) => metadata,
            Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok(Self::Missing),
            Err(error) => return Err(error).context("inspect cached count metadata"),
        };
        let modified = metadata.modified()?;
        let length = metadata.len();
        Ok(if metadata.is_symlink() {
            Self::Symlink { modified, length }
        } else if metadata.is_file() {
            Self::File { modified, length }
        } else {
            Self::Other
        })
    }

    pub(super) fn acquired(stamp: &WorktreeStamp) -> Option<Self> {
        match stamp {
            WorktreeStamp::File(metadata) => Some(Self::File {
                modified: metadata.modified,
                length: metadata.length,
            }),
            WorktreeStamp::Symlink { metadata, .. } => Some(Self::Symlink {
                modified: metadata.modified,
                length: metadata.length,
            }),
            WorktreeStamp::Missing => Some(Self::Missing),
            WorktreeStamp::Directory(_) | WorktreeStamp::Other(_) => Some(Self::Other),
            WorktreeStamp::Unobserved => None,
        }
    }
}
