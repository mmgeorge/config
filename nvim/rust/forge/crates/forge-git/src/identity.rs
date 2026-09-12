use std::ffi::OsString;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, ensure};

/// Identifies one worktree's private Git directory, independently of shared refs.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct WorktreeId(PathBuf);

impl WorktreeId {
    pub(crate) fn retained_bytes(&self) -> usize {
        self.0.capacity()
    }
}

/// Identifies the common Git directory shared by linked worktrees.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct GitStorageId(PathBuf);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RepositoryIdentity {
    pub worktree: Option<WorktreeId>,
    pub storage: GitStorageId,
    pub worktree_root: Option<PathBuf>,
    pub git_directory: PathBuf,
    pub common_directory: PathBuf,
    pub index: PathBuf,
}

/// Preserves Git's relative path bytes independently of a lossy display label.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct RepositoryPath {
    raw: Vec<u8>,
}

impl RepositoryPath {
    pub fn new(raw: Vec<u8>) -> Result<Self> {
        ensure!(
            !raw.is_empty() && !raw.contains(&0),
            "repository path is empty or contains NUL"
        );
        ensure!(
            raw.split(|byte| *byte == b'/')
                .all(|component| !component.is_empty() && component != b"." && component != b".."),
            "repository path must contain only relative normal components"
        );
        Ok(Self { raw })
    }

    pub fn raw(&self) -> &[u8] {
        &self.raw
    }

    pub fn retained_bytes(&self) -> usize {
        self.raw.capacity()
    }

    pub fn display_label(&self) -> String {
        String::from_utf8_lossy(&self.raw).into_owned()
    }
}

/// Discovers canonical storage and worktree identities without requiring a born HEAD or index.
/// Returns `None` when discovery finds no repository and preserves configuration and I/O failures.
pub fn discover_identity(path: &Path) -> Result<Option<RepositoryIdentity>> {
    Ok(discover_repository(path)?.map(|(identity, _)| identity))
}

pub(crate) fn discover_repository(
    path: &Path,
) -> Result<Option<(RepositoryIdentity, gix::ThreadSafeRepository)>> {
    let directory = if path.is_file() {
        path.parent().context("repository file has no parent")?
    } else {
        path
    };
    let repository = match gix::discover(directory) {
        Ok(repository) => repository,
        Err(gix::discover::Error::Discover(
            gix::discover::upwards::Error::NoGitRepository { .. }
            | gix::discover::upwards::Error::NoGitRepositoryWithinCeiling { .. }
            | gix::discover::upwards::Error::NoGitRepositoryWithinFs { .. },
        )) => return Ok(None),
        Err(error) => return Err(error).context("discover Git repository"),
    };
    let git_directory =
        dunce::canonicalize(repository.git_dir()).context("resolve Git directory")?;
    let common_directory =
        dunce::canonicalize(repository.common_dir()).context("resolve common Git directory")?;
    let worktree_root = repository
        .workdir()
        .map(dunce::canonicalize)
        .transpose()
        .context("resolve worktree root")?;
    let index_path = repository.index_path();
    let index = dunce::canonicalize(index_path.parent().context("index has no parent")?)
        .context("resolve index parent")?
        .join(index_path.file_name().context("index has no filename")?);
    let identity = RepositoryIdentity {
        worktree: worktree_root
            .as_ref()
            .map(|_| WorktreeId(git_directory.clone())),
        storage: GitStorageId(common_directory.clone()),
        worktree_root,
        git_directory,
        common_directory,
        index,
    };
    Ok(Some((identity, repository.into_sync())))
}

/// Returns one exact native argument. Git callers must separately disable pathspec interpretation.
pub fn resolve_argument(path: &RepositoryPath) -> Result<OsString> {
    #[cfg(unix)]
    {
        use std::os::unix::ffi::OsStringExt;
        Ok(OsString::from_vec(path.raw.clone()))
    }
    #[cfg(windows)]
    {
        let text = std::str::from_utf8(&path.raw)
            .context("Git path is not representable as a Windows path")?;
        for component in text.split('/') {
            ensure!(
                !component.contains(['\\', ':', '<', '>', '"', '|', '?', '*'])
                    && !component.chars().any(|character| character < ' ')
                    && !component.ends_with(['.', ' ']),
                "Git path has an ambiguous Windows component"
            );
            let stem = component
                .split('.')
                .next()
                .unwrap_or_default()
                .to_ascii_uppercase();
            let reserved = matches!(stem.as_str(), "CON" | "PRN" | "AUX" | "NUL")
                || ["COM", "LPT"].iter().any(|prefix| {
                    stem.strip_prefix(prefix).is_some_and(|number| {
                        matches!(
                            number,
                            "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "¹" | "²" | "³"
                        )
                    })
                });
            ensure!(!reserved, "Git path names a Windows device");
        }
        Ok(OsString::from(text))
    }
}

/// Validates existing parent directories without following the tracked leaf's symlink contents.
///
/// This is a path precondition, not an atomic filesystem transaction. Writers must recheck it
/// under mutation admission immediately before operating on the path.
pub fn validate_path(root: &Path, path: &RepositoryPath) -> Result<PathBuf> {
    let root = dunce::canonicalize(root).context("resolve worktree root")?;
    let relative = PathBuf::from(resolve_argument(path)?);
    let destination = root.join(&relative);
    let mut parent = root.clone();
    let components = relative.components().collect::<Vec<_>>();
    for component in components.iter().take(components.len().saturating_sub(1)) {
        ensure!(
            matches!(component, std::path::Component::Normal(_)),
            "repository path escapes worktree"
        );
        parent.push(component);
        match std::fs::symlink_metadata(&parent) {
            Ok(_) => {
                let resolved =
                    dunce::canonicalize(&parent).context("resolve repository path parent")?;
                ensure!(
                    resolved.starts_with(&root),
                    "repository path parent escapes worktree"
                );
                ensure!(
                    resolved.is_dir(),
                    "repository path parent is not a directory"
                );
            }
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => return Err(error).context("inspect repository path parent"),
        }
    }
    Ok(destination)
}
