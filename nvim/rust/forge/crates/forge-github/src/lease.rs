use std::fs::{self, File, TryLockError};
use std::path::Path;

use anyhow::{Context, Result, anyhow, ensure};

/// A repository lease shared by cooperating Forge processes.
///
/// The persistent lock file is a sibling of the cache directory. Callers must not remove it after
/// releasing ownership, because another process can already hold a descriptor to that file.
#[derive(Debug)]
pub struct RepositoryLease {
    _file: File,
}

/// Retains deletion exclusion and exclusive ownership of one repository refresh category.
#[derive(Debug)]
pub struct RepositoryRefreshLease {
    _refresh: RepositoryLease,
    _operation: RepositoryLease,
}

impl RepositoryLease {
    /// Acquires shared operation ownership without waiting for a deleting repository.
    pub fn operation(directory: &Path) -> Result<Self> {
        Self::acquire(directory, false, ".forge-issues.lock")
    }

    /// Acquires exclusive deletion ownership, returning Busy while operations retain leases.
    pub fn deletion(directory: &Path) -> Result<Self> {
        Self::acquire(directory, true, ".forge-issues.lock")
    }

    /// Excludes another cooperating sync while allowing short detail and state transactions.
    pub fn sync(directory: &Path) -> Result<RepositoryRefreshLease> {
        let operation = Self::operation(directory)?;
        let sync = Self::acquire(directory, true, ".forge-issue-sync.lock")?;
        ensure!(
            !directory.join("issues/sync.lock").try_exists()?,
            "Busy: legacy issue sync owns its cache"
        );
        Ok(RepositoryRefreshLease {
            _refresh: sync,
            _operation: operation,
        })
    }

    /// Excludes another metadata refresh without blocking issue sync or detail reads.
    pub fn metadata(directory: &Path) -> Result<RepositoryRefreshLease> {
        let operation = Self::operation(directory)?;
        let refresh = Self::acquire(directory, true, ".forge-metadata.lock")?;
        Ok(RepositoryRefreshLease {
            _refresh: refresh,
            _operation: operation,
        })
    }

    fn acquire(directory: &Path, exclusive: bool, suffix: &str) -> Result<Self> {
        let absolute = std::path::absolute(directory)?;
        let name = absolute
            .file_name()
            .context("repository cache directory needs a name")?;
        let parent = absolute
            .parent()
            .context("repository cache directory needs a parent")?;
        fs::create_dir_all(parent)?;
        let parent = fs::canonicalize(parent)?;
        let directory = parent.join(name);
        if let Ok(metadata) = fs::symlink_metadata(&directory) {
            ensure!(
                !metadata.file_type().is_symlink(),
                "repository cache directory must not be a symlink"
            );
            ensure!(
                metadata.is_dir(),
                "repository cache path must be a directory"
            );
        }
        let mut lock_name = std::ffi::OsString::from(".");
        lock_name.push(name);
        lock_name.push(suffix);
        let file = File::options()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(parent.join(lock_name))
            .context("open repository operation lease")?;
        let outcome = if exclusive {
            file.try_lock()
        } else {
            file.try_lock_shared()
        };
        match outcome {
            Ok(()) => Ok(Self { _file: file }),
            Err(TryLockError::WouldBlock) => {
                Err(anyhow!("Busy: repository issue storage is in use"))
            }
            Err(TryLockError::Error(failure)) => {
                Err(failure).context("acquire repository operation lease")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn operations_share_ownership_and_exclude_deletion() -> Result<()> {
        let root = tempfile::tempdir()?;
        let directory = root.path().join("repo");
        let first = RepositoryLease::operation(&directory)?;
        let second = RepositoryLease::operation(&directory)?;
        assert!(
            RepositoryLease::deletion(&directory)
                .unwrap_err()
                .to_string()
                .contains("Busy")
        );
        drop(first);
        assert!(RepositoryLease::deletion(&directory).is_err());
        drop(second);
        let deletion = RepositoryLease::deletion(&directory)?;
        assert!(RepositoryLease::operation(&directory).is_err());
        drop(deletion);
        assert!(RepositoryLease::operation(&directory).is_ok());
        Ok(())
    }

    #[test]
    fn removing_the_cache_does_not_replace_the_lease_file() -> Result<()> {
        let root = tempfile::tempdir()?;
        let directory = root.path().join("repo");
        fs::create_dir(&directory)?;
        let deletion = RepositoryLease::deletion(&directory)?;
        fs::remove_dir(&directory)?;
        assert!(RepositoryLease::operation(&directory).is_err());
        fs::create_dir(&directory)?;
        assert!(RepositoryLease::operation(&directory).is_err());
        drop(deletion);
        assert!(RepositoryLease::operation(&directory).is_ok());
        Ok(())
    }
}
