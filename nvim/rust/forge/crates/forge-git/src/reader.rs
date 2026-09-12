use std::{collections::HashMap, sync::Arc};

use anyhow::{Context, Result, ensure};

use crate::{
    RepositoryPath,
    repository::{RepositoryRead, RepositoryState},
    snapshot::{
        ChangeKind, ContentClassification, LineStats, ObjectState, PathRecord, PathState,
        SubmoduleState,
    },
    store::RepositoryStore,
};

const STATUS_RECORDS: usize = 65_536;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StatusBackend {
    Gix,
}

#[derive(Debug, Eq, PartialEq)]
pub struct StatusEnumeration {
    pub backend: StatusBackend,
    pub path: Vec<PathRecord>,
}

pub struct StatusReader;

impl StatusReader {
    pub async fn status(
        store: &RepositoryStore,
        repository: Arc<RepositoryState>,
    ) -> Result<RepositoryRead<StatusEnumeration>> {
        store
            .read(repository, 0, move |local, cancellation| {
                Self::status_native(&local, &mut || cancellation.check())
            })
            .await
    }

    pub(crate) fn status_native(
        repository: &gix::Repository,
        check: &mut dyn FnMut() -> Result<()>,
    ) -> Result<StatusEnumeration> {
        Self::status_selected(
            repository,
            None,
            gix::status::tree_index::TrackRenames::AsConfigured,
            check,
        )
    }

    pub(crate) fn status_paths(
        repository: &gix::Repository,
        paths: &[RepositoryPath],
        check: &mut dyn FnMut() -> Result<()>,
    ) -> Result<StatusEnumeration> {
        ensure!(
            !paths.is_empty() && paths.len() <= 256,
            "status requires 1 to 256 selected paths"
        );
        Self::status_selected(
            repository,
            Some(paths),
            gix::status::tree_index::TrackRenames::Disabled,
            check,
        )
    }

    pub fn status_comparison(
        repository: &gix::Repository,
        check: &mut dyn FnMut() -> Result<()>,
    ) -> Result<StatusEnumeration> {
        let mut result = Self::status_selected(
            repository,
            None,
            gix::status::tree_index::TrackRenames::Disabled,
            check,
        )?;
        let candidate: Vec<_> = result
            .path
            .iter()
            .filter(|record| matches!(record.state, PathState::Untracked))
            .map(|record| record.path.clone())
            .collect();
        if candidate.is_empty() {
            return Ok(result);
        }
        let index = repository.index_or_empty()?;
        let lookup = index.prepare_icase_backing();
        let root = repository
            .workdir()
            .context("comparison requires a worktree")?;
        for path in candidate {
            check()?;
            let Some(entry) = index.entry_by_path_icase(path.raw().into(), true, &lookup) else {
                continue;
            };
            let original = entry.path(&index);
            if original == path.raw() || exact_worktree_path(root, original.as_ref(), check)? {
                continue;
            }
            if let Some(record) = result
                .path
                .iter_mut()
                .find(|record| record.path.raw() == original)
            {
                record.unstaged = ChangeKind::Deleted;
                record.worktree_mode = None;
            } else {
                let mut record = tracked(original.to_vec(), entry.mode.bits(), entry.id);
                record.unstaged = ChangeKind::Deleted;
                record.worktree_mode = None;
                result.path.push(record);
            }
        }
        result
            .path
            .sort_unstable_by(|left, right| left.path.raw().cmp(right.path.raw()));
        Ok(result)
    }

    fn status_selected(
        repository: &gix::Repository,
        selected: Option<&[RepositoryPath]>,
        renames: gix::status::tree_index::TrackRenames,
        check: &mut dyn FnMut() -> Result<()>,
    ) -> Result<StatusEnumeration> {
        check()?;
        repository.workdir().context("status requires a worktree")?;
        let patterns = selected
            .map(|paths| {
                paths
                    .iter()
                    .map(|path| gix::bstr::BString::from(path.raw()))
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        let mut iterator = repository
            .status(gix::progress::Discard)?
            .untracked_files(gix::status::UntrackedFiles::Files)
            .index_worktree_submodules(gix::status::Submodule::AsConfigured { check_dirty: true })
            .tree_index_track_renames(renames)
            .into_iter(patterns)?;
        let mut records = HashMap::new();
        while let Some(item) = iterator.next() {
            check()?;
            ensure!(
                records.len() < STATUS_RECORDS,
                "status record limit exceeded"
            );
            match item? {
                gix::status::Item::TreeIndex(change) => insert_tree_index(&mut records, change)?,
                gix::status::Item::IndexWorktree(change) => {
                    insert_index_worktree(&mut records, change)?
                }
            }
        }
        ensure!(iterator.into_outcome().is_some(), "status did not complete");
        let mut path = records.into_values().collect::<Vec<_>>();
        path.sort_unstable_by(|left, right| left.path.raw().cmp(right.path.raw()));
        check()?;
        Ok(StatusEnumeration {
            backend: StatusBackend::Gix,
            path,
        })
    }
}

fn exact_worktree_path(
    root: &std::path::Path,
    path: &[u8],
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<bool> {
    let path = RepositoryPath::new(path.to_vec())?;
    let relative = std::path::PathBuf::from(crate::identity::resolve_argument(&path)?);
    let mut directory = root.to_owned();
    for component in relative.components() {
        check()?;
        let entries = match std::fs::read_dir(&directory) {
            Ok(entries) => entries,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(false),
            Err(error) => return Err(error.into()),
        };
        let mut found = false;
        for entry in entries {
            check()?;
            if entry?.file_name() == component.as_os_str() {
                found = true;
                break;
            }
        }
        if !found {
            return Ok(false);
        }
        directory.push(component);
    }
    Ok(true)
}

fn insert_tree_index(
    records: &mut HashMap<Vec<u8>, PathRecord>,
    change: gix::diff::index::Change,
) -> Result<()> {
    use gix::diff::index::Change;
    let (path, staged, head, index) = match change {
        Change::Addition {
            location,
            entry_mode,
            id,
            ..
        } => {
            let index = ObjectState {
                mode: entry_mode.bits(),
                object: id.into_owned().into(),
            };
            (
                location.to_vec(),
                ChangeKind::Added,
                ObjectState {
                    mode: 0,
                    object: index.object.kind().null(),
                },
                index,
            )
        }
        Change::Deletion {
            location,
            entry_mode,
            id,
            ..
        } => {
            let head = ObjectState {
                mode: entry_mode.bits(),
                object: id.into_owned().into(),
            };
            (
                location.to_vec(),
                ChangeKind::Deleted,
                head.clone(),
                ObjectState {
                    mode: 0,
                    object: head.object.kind().null(),
                },
            )
        }
        Change::Modification {
            location,
            previous_entry_mode,
            previous_id,
            entry_mode,
            id,
            ..
        } => (
            location.to_vec(),
            if previous_entry_mode == entry_mode {
                ChangeKind::Modified
            } else {
                ChangeKind::TypeChanged
            },
            ObjectState {
                mode: previous_entry_mode.bits(),
                object: previous_id.into_owned().into(),
            },
            ObjectState {
                mode: entry_mode.bits(),
                object: id.into_owned().into(),
            },
        ),
        Change::Rewrite {
            location,
            source_location,
            source_entry_mode,
            source_id,
            entry_mode,
            id,
            copy,
            ..
        } => {
            let index = ObjectState {
                mode: entry_mode.bits(),
                object: id.into_owned().into(),
            };
            let head = ObjectState {
                mode: source_entry_mode.bits(),
                object: source_id.into_owned().into(),
            };
            let record = tracked(location.to_vec(), index.mode, index.object);
            let mut record = record;
            record.staged = if copy {
                ChangeKind::Copied
            } else {
                ChangeKind::Renamed
            };
            record.state = PathState::Tracked {
                head,
                index,
                relocation: Some(crate::snapshot::Relocation {
                    kind: if copy {
                        ChangeKind::Copied
                    } else {
                        ChangeKind::Renamed
                    },
                    origin: RepositoryPath::new(source_location.to_vec())?,
                    similarity: 100,
                }),
            };
            merge_worktree_status(&mut record, records.get(location.as_ref()));
            records.insert(location.to_vec(), record);
            return Ok(());
        }
    };
    let mut record = tracked(path.clone(), index.mode, index.object);
    record.staged = staged;
    record.state = PathState::Tracked {
        head,
        index,
        relocation: None,
    };
    merge_worktree_status(&mut record, records.get(path.as_slice()));
    records.insert(path, record);
    Ok(())
}

fn insert_index_worktree(
    records: &mut HashMap<Vec<u8>, PathRecord>,
    change: gix::status::index_worktree::Item,
) -> Result<()> {
    use gix::{
        status::index_worktree::Item,
        status::plumbing::index_as_worktree::{Change, EntryStatus},
    };
    match change {
        Item::Modification {
            entry,
            rela_path,
            status,
            ..
        } => {
            let path = rela_path.to_vec();
            match status {
                EntryStatus::NeedsUpdate(_) => {}
                EntryStatus::IntentToAdd => {
                    let record = records
                        .entry(path.clone())
                        .or_insert_with(|| tracked(path, entry.mode.bits(), entry.id));
                    record.unstaged = ChangeKind::Added;
                    if let PathState::Tracked { head, index, .. } = &mut record.state {
                        *head = ObjectState {
                            mode: 0,
                            object: entry.id.kind().null(),
                        };
                        *index = head.clone();
                    }
                }
                EntryStatus::Conflict { entries, .. } => {
                    let state = |stage: usize| ObjectState {
                        mode: entries[stage].as_ref().map_or(0, |item| item.mode.bits()),
                        object: entries[stage]
                            .as_ref()
                            .map_or_else(|| entry.id.kind().null(), |item| item.id),
                    };
                    records.insert(
                        path.clone(),
                        PathRecord {
                            path: RepositoryPath::new(path)?,
                            staged: ChangeKind::Unmerged,
                            unstaged: ChangeKind::Unmerged,
                            worktree_mode: Some(entry.mode.bits()),
                            submodule: None,
                            state: PathState::Conflict {
                                base: state(0),
                                ours: state(1),
                                theirs: state(2),
                            },
                            staged_stats: LineStats::Unknown,
                            unstaged_stats: LineStats::Unknown,
                            content: ContentClassification::Unknown,
                        },
                    );
                }
                EntryStatus::Change(change) => {
                    let (unstaged, worktree_mode, submodule) = match change {
                        Change::Removed => (ChangeKind::Deleted, None, None),
                        Change::Type { worktree_mode } => {
                            (ChangeKind::TypeChanged, Some(worktree_mode.bits()), None)
                        }
                        Change::Modification { .. } => {
                            (ChangeKind::Modified, Some(entry.mode.bits()), None)
                        }
                        Change::SubmoduleModification(_) => (
                            ChangeKind::Modified,
                            Some(entry.mode.bits()),
                            Some(SubmoduleState {
                                commit_changed: false,
                                tracked_changed: true,
                                untracked_changed: false,
                            }),
                        ),
                    };
                    let record = records
                        .entry(path.clone())
                        .or_insert_with(|| tracked(path, entry.mode.bits(), entry.id));
                    record.unstaged = unstaged;
                    record.worktree_mode = worktree_mode;
                    record.submodule = submodule;
                }
            }
        }
        Item::DirectoryContents { entry, .. } => match entry.status {
            gix::dir::entry::Status::Untracked => {
                records
                    .entry(entry.rela_path.to_vec())
                    .or_insert_with(|| untracked(entry.rela_path.to_vec()));
            }
            gix::dir::entry::Status::Ignored(_) => {
                records
                    .entry(entry.rela_path.to_vec())
                    .or_insert_with(|| ignored(entry.rela_path.to_vec()));
            }
            _ => {}
        },
        Item::Rewrite { .. } => {}
    }
    Ok(())
}

fn tracked(path: Vec<u8>, mode: u32, object: gix::ObjectId) -> PathRecord {
    PathRecord {
        path: RepositoryPath::new(path).expect("gix status path"),
        staged: ChangeKind::Unchanged,
        unstaged: ChangeKind::Unchanged,
        worktree_mode: Some(mode),
        submodule: None,
        state: PathState::Tracked {
            head: ObjectState { mode, object },
            index: ObjectState { mode, object },
            relocation: None,
        },
        staged_stats: LineStats::Unknown,
        unstaged_stats: LineStats::Unknown,
        content: ContentClassification::Unknown,
    }
}

fn merge_worktree_status(record: &mut PathRecord, existing: Option<&PathRecord>) {
    let Some(existing) = existing else {
        return;
    };
    record.unstaged = existing.unstaged;
    record.worktree_mode = existing.worktree_mode;
    record.submodule = existing.submodule;
}

fn untracked(path: Vec<u8>) -> PathRecord {
    PathRecord {
        path: RepositoryPath::new(path).expect("gix status path"),
        staged: ChangeKind::Unchanged,
        unstaged: ChangeKind::Unchanged,
        worktree_mode: None,
        submodule: None,
        state: PathState::Untracked,
        staged_stats: LineStats::Unknown,
        unstaged_stats: LineStats::Unknown,
        content: ContentClassification::Unknown,
    }
}
fn ignored(path: Vec<u8>) -> PathRecord {
    PathRecord {
        path: RepositoryPath::new(path).expect("gix status path"),
        staged: ChangeKind::Unchanged,
        unstaged: ChangeKind::Unchanged,
        worktree_mode: None,
        submodule: None,
        state: PathState::Ignored,
        staged_stats: LineStats::Unknown,
        unstaged_stats: LineStats::Unknown,
        content: ContentClassification::Unknown,
    }
}
