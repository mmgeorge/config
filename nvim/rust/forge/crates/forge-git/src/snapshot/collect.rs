use std::{
    ffi::OsString,
    fs::{self, File, Metadata},
    io::{self, Read, Seek, SeekFrom},
    path::{Path, PathBuf},
    time::{Instant, SystemTime},
};

use anyhow::{Context, Result, ensure};

use crate::{
    RepositoryIdentity, RepositoryPath,
    reader::{StatusEnumeration, StatusReader},
    validate_path,
};

use super::{ObservationTiming, PathRecord};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum HeadState {
    Unborn {
        reference: Vec<u8>,
    },
    Attached {
        reference: Vec<u8>,
        target: gix::ObjectId,
    },
    Detached {
        target: gix::ObjectId,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
/// Detects ordinary filesystem changes without claiming to identify exact content bytes.
pub struct MetadataStamp {
    pub length: u64,
    pub modified: SystemTime,
    pub created: Option<SystemTime>,
    pub readonly: bool,
    #[cfg(unix)]
    pub device: u64,
    #[cfg(unix)]
    pub inode: u64,
    #[cfg(unix)]
    pub changed_seconds: i64,
    #[cfg(unix)]
    pub changed_nanoseconds: i64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WorktreeStamp {
    Unobserved,
    Missing,
    File(MetadataStamp),
    Directory(MetadataStamp),
    Symlink {
        metadata: MetadataStamp,
        target: PathBuf,
    },
    Other(MetadataStamp),
}

#[derive(Clone, Debug, Eq, PartialEq)]
/// Reuses the stored Git checksum without rereading index contents.
pub struct IndexFileStamp {
    pub name: OsString,
    pub length: u64,
    pub checksum_tail: Vec<u8>,
    pub modified_without_checksum: Option<SystemTime>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
/// Include the main index and every shared-index backing file in its directory.
pub struct IndexStamp {
    pub index: Option<IndexFileStamp>,
    pub shared: Vec<IndexFileStamp>,
}

#[derive(Clone, Debug)]
pub struct ObservedPath {
    pub change: PathRecord,
    pub worktree: WorktreeStamp,
    pub origin: Option<WorktreeStamp>,
    pub analysis: [Option<super::PreparedAnalysis>; 2],
}

impl PartialEq for ObservedPath {
    fn eq(&self, other: &Self) -> bool {
        // Cache admission does not change the repository state being compared.
        self.change == other.change
            && self.worktree == other.worktree
            && self.origin == other.origin
    }
}

impl Eq for ObservedPath {}

pub(crate) struct CollectedObservation {
    pub head: HeadState,
    pub index: IndexStamp,
    pub provider: crate::reader::StatusProvider,
    pub path: Vec<ObservedPath>,
    pub timing: ObservationTiming,
}

struct PathStamp {
    worktree: WorktreeStamp,
    origin: Option<WorktreeStamp>,
}

enum CollectionMode<'cache> {
    Display(
        &'cache forge_diff::cache::AnalysisStore,
        &'cache super::LineStatsCache,
        bool,
    ),
    Verified,
    Comparison,
}

pub(crate) fn collect_observation(
    repository: &gix::Repository,
    identity: &RepositoryIdentity,
    analysis: &forge_diff::cache::AnalysisStore,
    counts: &super::LineStatsCache,
    skip_line_stats: bool,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<CollectedObservation> {
    collect_with(
        repository,
        identity,
        CollectionMode::Display(analysis, counts, skip_line_stats),
        check,
        |check| StatusReader::status_native(repository, check),
    )
}

pub(crate) fn collect_comparison(
    repository: &gix::Repository,
    identity: &RepositoryIdentity,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<CollectedObservation> {
    collect_with(
        repository,
        identity,
        CollectionMode::Comparison,
        check,
        |check| StatusReader::status_comparison(repository, check),
    )
}

fn collect_with(
    repository: &gix::Repository,
    identity: &RepositoryIdentity,
    mode: CollectionMode<'_>,
    check: &mut dyn FnMut() -> Result<()>,
    mut status: impl FnMut(&mut dyn FnMut() -> Result<()>) -> Result<StatusEnumeration>,
) -> Result<CollectedObservation> {
    let collection_started = Instant::now();
    let verify_metadata = matches!(mode, CollectionMode::Verified | CollectionMode::Comparison);
    let (analysis, counts, skip_line_stats) = match mode {
        CollectionMode::Display(analysis, counts, skip) => (Some(analysis), Some(counts), skip),
        CollectionMode::Verified | CollectionMode::Comparison => (None, None, true),
    };
    check()?;
    let root = identity
        .worktree_root
        .as_ref()
        .context("observation requires a worktree")?;
    let phase_started = Instant::now();
    let head = read_head(repository)?;
    let index = read_index_stamp(&identity.index, check)?;
    let head_and_index_ms = phase_started.elapsed().as_millis();
    let phase_started = Instant::now();
    let initial = status(check)?;
    let initial_status_ms = phase_started.elapsed().as_millis();
    let phase_started = Instant::now();
    let mut path = Vec::with_capacity(initial.path.len());
    let stamp = if !verify_metadata {
        initial
            .path
            .iter()
            .map(|change| PathStamp {
                worktree: WorktreeStamp::Unobserved,
                origin: rename_origin(change).map(|_| WorktreeStamp::Unobserved),
            })
            .collect()
    } else {
        read_path_stamps(root, &initial.path, check)?
    };
    for (change, stamp) in initial.path.into_iter().zip(stamp) {
        path.push(ObservedPath {
            analysis: Default::default(),
            change,
            worktree: stamp.worktree,
            origin: stamp.origin,
        });
    }
    let initial_metadata_ms = phase_started.elapsed().as_millis();
    let phase_started = Instant::now();
    // Display inventory may age while files are edited. Exact worktree validation
    // belongs to verified observations and mutation preconditions, not opening a view.
    let verification_status_ms = if verify_metadata {
        let verified = status(check)?;
        ensure!(
            initial.provider == verified.provider,
            "status provider changed during observation"
        );
        ensure!(
            path.len() == verified.path.len()
                && path
                    .iter()
                    .zip(&verified.path)
                    .all(|(initial, verified)| initial.change == *verified),
            "status changed during observation"
        );
        phase_started.elapsed().as_millis()
    } else {
        0
    };
    let phase_started = Instant::now();
    let line_stats_detail = super::stats::collect(
        repository,
        identity,
        &mut path,
        analysis,
        counts,
        skip_line_stats,
        check,
    )?;
    let line_stats_ms = phase_started.elapsed().as_millis();
    let phase_started = Instant::now();
    if verify_metadata {
        let changes = path
            .iter()
            .map(|observed| observed.change.clone())
            .collect::<Vec<_>>();
        let stamp = read_path_stamps(root, &changes, check)?;
        for (observed, stamp) in path.iter().zip(stamp) {
            check()?;
            ensure!(
                observed.worktree == stamp.worktree,
                "worktree metadata changed during observation"
            );
            ensure!(
                observed.origin == stamp.origin,
                "rename origin changed during observation"
            );
        }
    }
    let verification_metadata_ms = phase_started.elapsed().as_millis();
    let phase_started = Instant::now();
    ensure!(
        index == read_index_stamp(&identity.index, check)?,
        "index changed during observation"
    );
    ensure!(
        head == read_head(repository)?,
        "HEAD changed during observation"
    );
    check()?;
    let identity_verification_ms = phase_started.elapsed().as_millis();
    Ok(CollectedObservation {
        head,
        index,
        provider: initial.provider,
        path,
        timing: ObservationTiming {
            head_and_index_ms,
            initial_status_ms,
            initial_metadata_ms,
            verification_status_ms,
            line_stats_ms,
            line_stats_detail,
            verification_metadata_ms,
            identity_verification_ms,
            collection_total_ms: collection_started.elapsed().as_millis(),
            ..ObservationTiming::default()
        },
    })
}

fn read_path_stamps(
    root: &Path,
    path: &[PathRecord],
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<Vec<PathStamp>> {
    let read = |change: &PathRecord| -> Result<PathStamp> {
        Ok(PathStamp {
            worktree: read_worktree_stamp(root, &change.path)?,
            origin: rename_origin(change)
                .map(|origin| read_worktree_stamp(root, origin))
                .transpose()?,
        })
    };
    path.iter()
        .map(|change| {
            check()?;
            read(change)
        })
        .collect()
}

pub(crate) fn collect_affected(
    repository: &gix::Repository,
    identity: &RepositoryIdentity,
    paths: &[RepositoryPath],
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<CollectedObservation> {
    collect_with(
        repository,
        identity,
        CollectionMode::Verified,
        check,
        |check| StatusReader::status_paths(repository, paths, check),
    )
}

pub(crate) fn read_head(repository: &gix::Repository) -> Result<HeadState> {
    let head = repository.head().context("read observation HEAD")?;
    let reference = head.referent_name().map(|name| name.as_bstr().to_vec());
    match (reference, head.id().map(|object| object.detach())) {
        (Some(reference), None) => Ok(HeadState::Unborn { reference }),
        (Some(reference), Some(target)) => Ok(HeadState::Attached { reference, target }),
        (None, Some(target)) => Ok(HeadState::Detached { target }),
        (None, None) => anyhow::bail!("HEAD has neither a reference nor an object"),
    }
}

pub(crate) fn read_index_stamp(
    index: &Path,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<IndexStamp> {
    let index_file = read_index_file(index, check)?;
    let directory = index.parent().context("index has no parent directory")?;
    let mut shared_path = Vec::new();
    for entry in fs::read_dir(directory).context("enumerate shared indexes")? {
        check()?;
        let entry = entry.context("read shared index directory entry")?;
        let name = entry.file_name();
        if let Some(hash) = name.as_encoded_bytes().strip_prefix(b"sharedindex.")
            && matches!(hash.len(), 40 | 64)
            && hash.iter().all(u8::is_ascii_hexdigit)
        {
            shared_path.push(entry.path());
        }
    }
    shared_path.sort_unstable();
    let mut shared = Vec::with_capacity(shared_path.len());
    for path in shared_path {
        shared.push(
            read_index_file(&path, check)?
                .context("shared index disappeared during observation")?,
        );
    }
    Ok(IndexStamp {
        index: index_file,
        shared,
    })
}

fn read_index_file(
    path: &Path,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<Option<IndexFileStamp>> {
    check()?;
    let mut file = match File::open(path) {
        Ok(file) => file,
        Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok(None),
        Err(error) => return Err(error).context("open observation index"),
    };
    let initial = file.metadata().context("inspect observation index")?;
    ensure!(initial.is_file(), "observation index is not a regular file");
    let length = initial.len();
    let tail_length = length.min(32) as usize;
    file.seek(SeekFrom::End(-(tail_length as i64)))?;
    let mut checksum_tail = vec![0; tail_length];
    file.read_exact(&mut checksum_tail)?;
    check()?;
    let has_checksum = checksum_tail.len() >= 20
        && checksum_tail[checksum_tail.len() - 20..]
            .iter()
            .any(|byte| *byte != 0);
    ensure!(
        metadata_stamp(&initial)? == metadata_stamp(&file.metadata()?)?
            && metadata_stamp(&initial)? == metadata_stamp(&fs::metadata(path)?)?,
        "index changed while reading its stamp"
    );
    Ok(Some(IndexFileStamp {
        name: path
            .file_name()
            .context("index has no filename")?
            .to_os_string(),
        length,
        checksum_tail,
        modified_without_checksum: if has_checksum {
            None
        } else {
            Some(initial.modified()?)
        },
    }))
}

pub(crate) fn read_worktree_stamp(root: &Path, path: &RepositoryPath) -> Result<WorktreeStamp> {
    let path = validate_path(root, path)?;
    let metadata = match fs::symlink_metadata(&path) {
        Ok(metadata) => metadata,
        Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok(WorktreeStamp::Missing),
        Err(error) => return Err(error).context("inspect observed worktree path"),
    };
    let stamp = metadata_stamp(&metadata)?;
    Ok(if metadata.is_symlink() {
        WorktreeStamp::Symlink {
            metadata: stamp,
            target: fs::read_link(path)?,
        }
    } else if metadata.is_file() {
        WorktreeStamp::File(stamp)
    } else if metadata.is_dir() {
        WorktreeStamp::Directory(stamp)
    } else {
        WorktreeStamp::Other(stamp)
    })
}

pub(crate) fn metadata_stamp(metadata: &Metadata) -> Result<MetadataStamp> {
    #[cfg(unix)]
    use std::os::unix::fs::MetadataExt;
    Ok(MetadataStamp {
        length: metadata.len(),
        modified: metadata
            .modified()
            .context("read observation modification time")?,
        created: metadata.created().ok(),
        readonly: metadata.permissions().readonly(),
        #[cfg(unix)]
        device: metadata.dev(),
        #[cfg(unix)]
        inode: metadata.ino(),
        #[cfg(unix)]
        changed_seconds: metadata.ctime(),
        #[cfg(unix)]
        changed_nanoseconds: metadata.ctime_nsec(),
    })
}

fn rename_origin(change: &PathRecord) -> Option<&RepositoryPath> {
    match &change.state {
        super::PathState::Tracked {
            relocation: Some(relocation),
            ..
        } if relocation.kind == super::ChangeKind::Renamed => Some(&relocation.origin),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::init_repository;

    #[test]
    fn worktree_change_after_status_rejects_the_observation() {
        let fixture = tempfile::tempdir().unwrap();
        let root = fixture.path();
        init_repository(root);
        fs::write(root.join("untracked"), "before").unwrap();
        for number in 0..128 {
            fs::write(root.join(format!("other-{number}")), "unchanged").unwrap();
        }
        let (identity, shared) = crate::identity::discover_repository(root).unwrap().unwrap();
        let local = shared.to_thread_local();
        let mut calls = 0;
        let result = collect_with(
            &local,
            &identity,
            CollectionMode::Verified,
            &mut || Ok(()),
            |check| {
                let status = StatusReader::status_native(&local, check)?;
                calls += 1;
                if calls == 2 {
                    fs::write(root.join("untracked"), "after and longer")?;
                }
                Ok(status)
            },
        );
        assert!(
            result
                .err()
                .unwrap()
                .to_string()
                .contains("worktree metadata changed")
        );
    }
}
