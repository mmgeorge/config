use std::{
    collections::{HashMap, HashSet},
    fs::OpenOptions,
    io::Read,
};

use anyhow::{Context, Result, ensure};
use forge_diff::source::{Representation, SourceIdentity};

use crate::{
    RepositoryPath,
    command::{CommandLimits, read_command},
    content::{
        ContentLimits, ContentRequest, ContentResult, ContentSource, IndexStage,
        WorktreeConversion, acquire,
    },
    repository::RepositoryState,
    resolve_argument,
    snapshot::{
        HeadState, IndexStamp, WorktreeStamp, metadata_stamp, read_head, read_index_stamp,
        read_worktree_stamp,
    },
    validate_path,
};

use super::{GitWriteAction, PatchDirection, git_command};

#[derive(Debug, Eq, PartialEq)]
struct PathPrecondition {
    path: RepositoryPath,
    index: Vec<u8>,
    head_tracked: bool,
    worktree_policy: WorktreePolicy,
    worktree: Option<(WorktreeStamp, Option<gix::ObjectId>)>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum WorktreePolicy {
    Current,
    IndexOnly,
    Metadata,
    Content,
}

impl WorktreePolicy {
    fn capture(
        self,
        repository: &RepositoryState,
        path: &RepositoryPath,
        check: &mut dyn FnMut() -> Result<()>,
    ) -> Result<Option<(WorktreeStamp, Option<gix::ObjectId>)>> {
        check()?;
        match self {
            Self::Current | Self::IndexOnly => Ok(None),
            Self::Metadata => {
                let root = repository.identity.worktree_root.as_ref().context("missing worktree root")?;
                Ok(Some((read_worktree_stamp(root, path)?, None)))
            }
            Self::Content => fingerprint(repository, path, check).map(Some),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub(super) struct WritePrecondition {
    head: HeadState,
    index: Option<IndexStamp>,
    path: Vec<PathPrecondition>,
    publish_upstream: Option<Vec<u8>>,
}

impl WritePrecondition {
    pub(super) fn validate_observed(
        &self,
        head: &HeadState,
        observed: &[crate::snapshot::ObservedPath],
    ) -> Result<()> {
        ensure!(
            self.head == *head,
            "HEAD changed since the displayed observation; refresh Forge"
        );
        let selected: HashMap<_, _> = self
            .path
            .iter()
            .map(|target| (&target.path, target))
            .collect();
        for source in observed {
            let target = selected
                .get(&source.change.path)
                .context("observed source was not prepared")?;
            if target.worktree_policy == WorktreePolicy::Current {
                continue;
            }
            let mut expected_index = Vec::new();
            match &source.change.state {
                crate::snapshot::PathState::Tracked { head, index, .. } => {
                    if head.mode == 0
                        && index.mode == 0
                        && source.change.staged == crate::snapshot::ChangeKind::Unchanged
                        && source.change.unstaged == crate::snapshot::ChangeKind::Added
                    {
                        let placeholder = crate::snapshot::ObjectState {
                            mode: source
                                .change
                                .worktree_mode
                                .context("intent-to-add file mode missing")?,
                            object: gix::objs::compute_hash(
                                index.object.kind(),
                                gix::objs::Kind::Blob,
                                &[],
                            )?,
                        };
                        append_index(&mut expected_index, &source.change.path, &placeholder, 0);
                    } else {
                        append_index(&mut expected_index, &source.change.path, index, 0);
                    }
                }
                crate::snapshot::PathState::Conflict { base, ours, theirs } => {
                    for (stage, object) in [(1, base), (2, ours), (3, theirs)] {
                        append_index(&mut expected_index, &source.change.path, object, stage);
                    }
                }
                _ => {}
            }
            ensure!(
                target.index == expected_index,
                "selected index changed since the displayed observation; refresh Forge"
            );
            if let Some((stamp, _)) = &target.worktree
                && source.worktree != WorktreeStamp::Unobserved
            {
                ensure!(
                    stamp == &source.worktree,
                    "selected source changed since the displayed observation; refresh Forge"
                );
            }
            if let crate::snapshot::PathState::Tracked {
                relocation: Some(relocation),
                ..
            } = &source.change.state
                && relocation.kind == crate::snapshot::ChangeKind::Renamed
                && let Some(origin) = selected.get(&relocation.origin)
            {
                if source.change.staged == crate::snapshot::ChangeKind::Renamed {
                    ensure!(
                        origin.index.is_empty(),
                        "rename index changed since the displayed observation; refresh Forge"
                    );
                }
                if let Some((stamp, _)) = &origin.worktree
                    && source.origin != Some(WorktreeStamp::Unobserved)
                {
                    ensure!(
                        Some(stamp) == source.origin.as_ref(),
                        "rename source changed since the displayed observation; refresh Forge"
                    );
                }
            }
        }
        Ok(())
    }

    pub(super) fn capture(
        local: &mut gix::Repository,
        repository: &RepositoryState,
        action: &GitWriteAction,
        check: &mut dyn FnMut() -> Result<()>,
    ) -> Result<Self> {
        check()?;
        if let GitWriteAction::UpdateRepositoryConfig { expected, .. } = action {
            super::config::validate(repository, expected, check)?;
        }
        let head = read_head(local)?;
        let publish_upstream = if let GitWriteAction::PublishBranch {
            name,
            expected_head,
        } = action
        {
            ensure!(
                matches!(&head, HeadState::Attached { reference, .. } if reference.as_slice() == format!("refs/heads/{name}").as_bytes()),
                "publish branch is no longer the current branch"
            );
            if let Some(expected) = expected_head {
                ensure!(
                    matches!(&head, HeadState::Attached { target, .. } if target.to_string().eq_ignore_ascii_case(expected)),
                    "publish HEAD changed after generation capture"
                );
            }
            let output = read_command(
                git_command(repository)?.args([
                    "rev-parse",
                    "--abbrev-ref",
                    "--symbolic-full-name",
                    "@{upstream}",
                ]),
                CommandLimits {
                    stdout_bytes: 65536,
                    stderr_bytes: 65536,
                    timeout: std::time::Duration::from_secs(10),
                },
                &mut *check,
            )?;
            output.status.success().then_some(output.stdout)
        } else {
            None
        };
        let index = if action.paths().is_empty() {
            Some(read_index_stamp(&repository.identity.index, check)?)
        } else {
            None
        };
        let mut path = Vec::new();
        let mut index_bytes = 0;
        let selected = action.paths();
        let actions = match action {
            GitWriteAction::Batch { action } => action.as_slice(),
            action => std::slice::from_ref(action),
        };
        let policy: HashMap<_, _> = actions.iter().flat_map(|action| {
            let policy = match action {
                GitWriteAction::Stage { .. } => WorktreePolicy::Current,
                GitWriteAction::Unstage { .. }
                | GitWriteAction::Patch { direction: PatchDirection::Unstage, .. } => WorktreePolicy::IndexOnly,
                GitWriteAction::Patch { direction: PatchDirection::Stage | PatchDirection::Index { .. }, .. } => WorktreePolicy::Metadata,
                _ => WorktreePolicy::Content,
            };
            action.paths().into_iter().map(move |path| (path, policy))
        }).collect();
        let mut selected_index = bulk_index(repository, &selected, check)?;
        let selected_head = bulk_head(repository, &selected, &head, check)?;
        for target in &selected {
            let worktree_policy = policy[target];
            let index = if worktree_policy == WorktreePolicy::Current {
                Vec::new()
            } else {
                selected_index.remove(target).unwrap_or_default()
            };
            index_bytes += index.len();
            ensure!(
                index_bytes <= 2 * 1024 * 1024,
                "write precondition index exceeds 2 MiB"
            );
            let worktree = worktree_policy.capture(repository, target, check)?;
            path.push(PathPrecondition {
                head_tracked: selected_head.contains(target),
                worktree_policy,
                path: target.clone(),
                index,
                worktree,
            });
        }
        validate_patch(local, repository, action, check)?;
        ensure!(
            head == read_head(local)?,
            "HEAD changed while preparing write"
        );
        if let Some(index) = &index {
            ensure!(
                *index == read_index_stamp(&repository.identity.index, check)?,
                "index changed while preparing write"
            );
        }
        let mut current_index = bulk_index(repository, &selected, check)?;
        for target in &path {
            ensure!(
                target.worktree_policy == WorktreePolicy::Current
                    || target.index == current_index.remove(&target.path).unwrap_or_default(),
                "target index changed while preparing write"
            );
            if let Some((stamp, _)) = &target.worktree {
                ensure!(
                    *stamp
                        == read_worktree_stamp(
                            repository
                                .identity
                                .worktree_root
                                .as_ref()
                                .context("missing worktree")?,
                            &target.path
                        )?,
                    "target changed while preparing write"
                );
            }
        }
        if let GitWriteAction::UpdateRepositoryConfig { expected, .. } = action {
            super::config::validate(repository, expected, check)?;
        }
        Ok(Self {
            head,
            index,
            path,
            publish_upstream,
        })
    }

    pub(super) fn validate(
        &self,
        local: &mut gix::Repository,
        repository: &RepositoryState,
        action: &GitWriteAction,
        check: &mut dyn FnMut() -> Result<()>,
    ) -> Result<()> {
        let current = Self::capture(local, repository, action, check)?;
        ensure!(
            *self == current,
            "captured HEAD, index entry, mode, or source bytes changed"
        );
        Ok(())
    }

    pub(super) fn has_publish_upstream(&self) -> bool {
        self.publish_upstream.is_some()
    }

    pub(super) fn unborn(&self) -> bool {
        matches!(self.head, HeadState::Unborn { .. })
    }

    pub(super) fn validate_target(
        &self,
        repository: &RepositoryState,
        path: &RepositoryPath,
        check: &mut dyn FnMut() -> Result<()>,
    ) -> Result<()> {
        check()?;
        let target = self
            .path
            .iter()
            .find(|target| &target.path == path)
            .context("target was not captured")?;
        ensure!(
            self.head == read_head(&repository.repository.to_thread_local())?,
            "HEAD changed before target execution"
        );
        ensure!(
            target.worktree_policy == WorktreePolicy::Current
                || target.index == index_entries(repository, path, check)?,
            "target index changed before execution"
        );
        if let Some(expected) = &target.worktree {
            ensure!(
                Some(expected) == target.worktree_policy.capture(repository, path, check)?.as_ref(),
                "target source changed before execution"
            );
        }
        Ok(())
    }

    pub(super) fn validate_targets(
        &self,
        repository: &RepositoryState,
        path: &[RepositoryPath],
        check: &mut dyn FnMut() -> Result<()>,
    ) -> Result<()> {
        check()?;
        ensure!(
            self.head == read_head(&repository.repository.to_thread_local())?,
            "HEAD changed before chunk execution"
        );
        let mut index = bulk_index(repository, path, check)?;
        for path in path {
            let captured = self
                .path
                .iter()
                .find(|captured| &captured.path == path)
                .context("chunk target was not captured")?;
            ensure!(
                captured.worktree_policy == WorktreePolicy::Current
                    || captured.index == index.remove(path).unwrap_or_default(),
                "chunk target index changed before execution"
            );
            if let Some(expected) = &captured.worktree {
                ensure!(
                    Some(expected) == captured.worktree_policy.capture(repository, path, check)?.as_ref(),
                    "chunk target source changed before execution"
                );
            }
        }
        Ok(())
    }

    pub(super) fn tracked(&self, path: &RepositoryPath) -> bool {
        self.path
            .iter()
            .find(|target| &target.path == path)
            .is_some_and(|target| !target.index.is_empty() || target.head_tracked)
    }

    pub(super) fn patch_presence(
        &self,
        path: &RepositoryPath,
        direction: PatchDirection,
    ) -> (bool, bool) {
        if let PatchDirection::Index { before_exists, after_exists } = direction {
            return (before_exists, after_exists);
        }
        let target = self
            .path
            .iter()
            .find(|target| &target.path == path)
            .expect("captured patch target");
        if matches!(
            direction,
            PatchDirection::Unstage | PatchDirection::DiscardStaged
        ) {
            (target.head_tracked, !target.index.is_empty())
        } else {
            (
                !target.index.is_empty(),
                !matches!(target.worktree, Some((WorktreeStamp::Missing, _))),
            )
        }
    }
}

fn append_index(
    output: &mut Vec<u8>,
    path: &RepositoryPath,
    object: &crate::snapshot::ObjectState,
    stage: u8,
) {
    if object.mode == 0 || object.object.is_null() {
        return;
    }
    output.extend_from_slice(format!("{:06o} {} {stage}\t", object.mode, object.object).as_bytes());
    output.extend_from_slice(path.raw());
    output.push(0);
}

fn bulk_index(
    repository: &RepositoryState,
    path: &[RepositoryPath],
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<HashMap<RepositoryPath, Vec<u8>>> {
    if path.is_empty() {
        return Ok(HashMap::new());
    }
    let selected: HashSet<&[u8]> = path.iter().map(RepositoryPath::raw).collect();
    let mut command = git_command(repository)?;
    command.args(["ls-files", "--stage", "-z"]);
    let output = read_command(
        &mut command,
        CommandLimits {
            stdout_bytes: 16 * 1024 * 1024,
            stderr_bytes: 4096,
            timeout: std::time::Duration::from_secs(30),
        },
        &mut *check,
    )?;
    ensure!(
        output.status.success(),
        "cannot inspect selected index: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let mut captured: HashMap<RepositoryPath, Vec<u8>> = HashMap::new();
    let mut retained = 0;
    for record in output
        .stdout
        .split(|byte| *byte == 0)
        .filter(|record| !record.is_empty())
    {
        check()?;
        let offset = record
            .iter()
            .position(|byte| *byte == b'\t')
            .context("invalid index record")?
            + 1;
        let indexed = &record[offset..];
        for (position, byte) in indexed.iter().enumerate() {
            ensure!(
                *byte != b'/' || !selected.contains(&indexed[..position]),
                "write target denotes a directory rather than one path"
            );
        }
        if selected.contains(indexed) {
            retained += record.len() + 1;
            ensure!(retained <= 2 * 1024 * 1024, "selected index exceeds 2 MiB");
            let entry = captured
                .entry(RepositoryPath::new(indexed.to_vec())?)
                .or_default();
            entry.extend_from_slice(record);
            entry.push(0);
        }
    }
    Ok(captured)
}

fn bulk_head(
    repository: &RepositoryState,
    path: &[RepositoryPath],
    head: &HeadState,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<HashSet<RepositoryPath>> {
    if path.is_empty() || matches!(head, HeadState::Unborn { .. }) {
        return Ok(HashSet::new());
    }
    let selected: HashSet<&[u8]> = path.iter().map(RepositoryPath::raw).collect();
    let mut command = git_command(repository)?;
    command.args(["ls-tree", "-r", "-z", "HEAD"]);
    let output = read_command(
        &mut command,
        CommandLimits {
            stdout_bytes: 16 * 1024 * 1024,
            stderr_bytes: 4096,
            timeout: std::time::Duration::from_secs(30),
        },
        &mut *check,
    )?;
    ensure!(
        output.status.success(),
        "cannot inspect selected HEAD: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let mut captured = HashSet::new();
    for record in output
        .stdout
        .split(|byte| *byte == 0)
        .filter(|record| !record.is_empty())
    {
        check()?;
        let offset = record
            .iter()
            .position(|byte| *byte == b'\t')
            .context("invalid HEAD tree record")?
            + 1;
        if selected.contains(&record[offset..]) {
            captured.insert(RepositoryPath::new(record[offset..].to_vec())?);
        }
    }
    Ok(captured)
}

fn head_contains(
    repository: &RepositoryState,
    path: &RepositoryPath,
    head: &HeadState,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<bool> {
    if matches!(head, HeadState::Unborn { .. }) {
        return Ok(false);
    }
    let mut command = git_command(repository)?;
    command
        .args(["ls-tree", "-z", "HEAD", "--"])
        .arg(resolve_argument(path)?);
    let output = read_command(
        &mut command,
        CommandLimits {
            stdout_bytes: 16 * 1024,
            stderr_bytes: 4096,
            timeout: std::time::Duration::from_secs(30),
        },
        check,
    )?;
    ensure!(
        output.status.success(),
        "cannot inspect target HEAD: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    Ok(!output.stdout.is_empty())
}

fn index_entries(
    repository: &RepositoryState,
    path: &RepositoryPath,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<Vec<u8>> {
    let mut command = git_command(repository)?;
    command
        .args(["ls-files", "--stage", "-z", "--"])
        .arg(resolve_argument(path)?);
    let output = read_command(
        &mut command,
        CommandLimits {
            stdout_bytes: 16 * 1024,
            stderr_bytes: 4096,
            timeout: std::time::Duration::from_secs(30),
        },
        check,
    )?;
    ensure!(
        output.status.success(),
        "cannot inspect target index: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    for record in output
        .stdout
        .split(|byte| *byte == 0)
        .filter(|record| !record.is_empty())
    {
        let (_, indexed) = record.split_at(
            record
                .iter()
                .position(|byte| *byte == b'\t')
                .context("invalid index entry")?
                + 1,
        );
        ensure!(
            indexed == path.raw(),
            "write target denotes a directory rather than one path"
        );
    }
    Ok(output.stdout)
}

fn fingerprint(
    repository: &RepositoryState,
    path: &RepositoryPath,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<(WorktreeStamp, Option<gix::ObjectId>)> {
    let root = repository
        .identity
        .worktree_root
        .as_ref()
        .context("missing worktree root")?;
    let stamp = read_worktree_stamp(root, path)?;
    let digest = match &stamp {
        WorktreeStamp::Missing | WorktreeStamp::Symlink { .. } => None,
        WorktreeStamp::File(metadata) => {
            let mut options = OpenOptions::new();
            options.read(true);
            #[cfg(unix)]
            {
                use std::os::unix::fs::OpenOptionsExt;
                options.custom_flags(libc::O_NOFOLLOW);
            }
            #[cfg(windows)]
            {
                use std::os::windows::fs::OpenOptionsExt;
                options.custom_flags(0x0020_0000);
            }
            let mut file = options
                .open(validate_path(root, path)?)
                .context("open exact mutation source")?;
            let opened = file.metadata()?;
            ensure!(
                opened.is_file() && !opened.is_symlink() && *metadata == metadata_stamp(&opened)?,
                "source changed before capture"
            );
            let mut hasher = gix::hash::hasher(gix::hash::Kind::Sha256);
            let mut scratch = [0; 8192];
            let mut length = 0;
            loop {
                check()?;
                let count = file.read(&mut scratch)?;
                if count == 0 {
                    break;
                }
                length += count as u64;
                ensure!(length <= metadata.length, "source grew while captured");
                hasher.update(&scratch[..count]);
            }
            ensure!(
                length == metadata.length && *metadata == metadata_stamp(&file.metadata()?)?,
                "source changed while captured"
            );
            Some(hasher.try_finalize()?)
        }
        _ => anyhow::bail!("mutation requires a file, symlink, or missing path"),
    };
    ensure!(
        stamp == read_worktree_stamp(root, path)?,
        "source path changed while captured"
    );
    Ok((stamp, digest))
}

fn validate_patch(
    local: &mut gix::Repository,
    repository: &RepositoryState,
    action: &GitWriteAction,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<()> {
    if let GitWriteAction::Batch { action } = action {
        for action in action {
            validate_patch(local, repository, action, check)?;
        }
        return Ok(());
    }
    if let GitWriteAction::DiscardCombined { staged, unstaged } = action {
        validate_patch_target(
            local,
            repository,
            PatchDirection::DiscardStaged,
            staged,
            check,
        )?;
        return validate_patch_target(local, repository, PatchDirection::Discard, unstaged, check);
    }
    let GitWriteAction::Patch { direction, target } = action else {
        return Ok(());
    };
    validate_patch_target(local, repository, *direction, target, check)
}

fn validate_patch_target(
    local: &mut gix::Repository,
    repository: &RepositoryState,
    direction: PatchDirection,
    target: &super::PatchTarget,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<()> {
    let index_expected = if matches!(
        direction,
        PatchDirection::Unstage | PatchDirection::DiscardStaged
    ) {
        target.analysis.source().new.identity()
    } else {
        target.analysis.source().old.identity()
    };
    validate_content(
        local,
        repository,
        ContentSource::IndexStage {
            path: target.path.clone(),
            stage: IndexStage::Normal,
        },
        index_expected,
        check,
    )?;
    if matches!(direction, PatchDirection::Index { .. }) {
        return Ok(());
    }
    if matches!(
        direction,
        PatchDirection::Unstage | PatchDirection::DiscardStaged
    ) {
        let head = read_head(local)?;
        if !head_contains(repository, &target.path, &head, check)? {
            ensure!(
                target.analysis.source().old.identity()
                    == SourceIdentity::from_bytes(b"", Representation::GitCanonical),
                "patch HEAD source is missing"
            );
        } else {
            let mut command = git_command(repository)?;
            let mut source = std::ffi::OsString::from("HEAD:");
            source.push(resolve_argument(&target.path)?);
            command
                .args(["show", "--no-ext-diff", "--no-textconv"])
                .arg(source);
            let output = read_command(
                &mut command,
                CommandLimits {
                    stdout_bytes: 8 * 1024 * 1024,
                    stderr_bytes: 4096,
                    timeout: std::time::Duration::from_secs(30),
                },
                &mut *check,
            )?;
            ensure!(output.status.success(), "patch base is unavailable in HEAD");
            ensure!(
                SourceIdentity::from_bytes(&output.stdout, Representation::GitCanonical)
                    == target.analysis.source().old.identity(),
                "patch HEAD source changed"
            );
        }
    }
    if !matches!(
        direction,
        PatchDirection::Unstage | PatchDirection::DiscardStaged
    ) {
        validate_content(
            local,
            repository,
            ContentSource::Worktree {
                path: target.path.clone(),
                conversion: WorktreeConversion::GitCanonical,
            },
            target.analysis.source().new.identity(),
            check,
        )?;
    }
    Ok(())
}

fn validate_content(
    local: &mut gix::Repository,
    repository: &RepositoryState,
    source: ContentSource,
    expected: SourceIdentity,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<()> {
    let result = acquire(
        local,
        repository,
        ContentRequest {
            source,
            limits: ContentLimits::default(),
            expected: Some(expected),
        },
        check,
    )?;
    match result {
        ContentResult::Ready(_) => Ok(()),
        ContentResult::Missing
            if expected == SourceIdentity::from_bytes(b"", Representation::GitCanonical) =>
        {
            Ok(())
        }
        unavailable => anyhow::bail!("patch source unavailable: {unavailable:?}"),
    }
}
