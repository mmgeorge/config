use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::{ExitStatus, Output};

use anyhow::{Context, Result, ensure};

use crate::mutation::OperationId;
use crate::repository::RepositoryState;

const MAX_CONFIG_BYTES: usize = 1024 * 1024;

struct ConfigTemporary {
    path: PathBuf,
    file: Option<File>,
    published: bool,
}

impl Drop for ConfigTemporary {
    fn drop(&mut self) {
        drop(self.file.take());
        if !self.published {
            if let Err(error) = std::fs::remove_file(&self.path) {
                if error.kind() != std::io::ErrorKind::NotFound {
                    eprintln!("Forge repository config temporary cleanup failed: {error}");
                }
            }
        }
    }
}

pub(super) fn validate(
    repository: &RepositoryState,
    expected: &Option<Vec<u8>>,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<()> {
    check()?;
    ensure!(
        expected
            .as_ref()
            .is_none_or(|bytes| bytes.len() <= MAX_CONFIG_BYTES),
        "repository config exceeds 1 MiB"
    );
    let root = repository
        .identity
        .worktree_root
        .as_ref()
        .context("repository config requires a worktree")?;
    ensure!(
        &read(&root.join(".forge.json"))? == expected,
        ".forge.json changed since the edit began"
    );
    check()
}

pub(super) fn replace(
    repository: &RepositoryState,
    expected: &Option<Vec<u8>>,
    replacement: &[u8],
    operation: OperationId,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<Output> {
    ensure!(
        replacement.len() <= MAX_CONFIG_BYTES,
        "repository config replacement exceeds 1 MiB"
    );
    validate(repository, expected, check)?;
    let root = repository
        .identity
        .worktree_root
        .as_ref()
        .context("repository config requires a worktree")?;
    let destination = root.join(".forge.json");
    let temporary_path = root.join(format!(
        ".forge.json.{}-{}.tmp",
        std::process::id(),
        operation.value()
    ));
    let mut options = OpenOptions::new();
    options.write(true).create_new(true);
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt;
        options.mode(0o600);
    }
    let output = options
        .open(&temporary_path)
        .context("create repository config temporary")?;
    let mut temporary = ConfigTemporary {
        path: temporary_path,
        file: Some(output),
        published: false,
    };
    let output = temporary.file.as_mut().expect("created config temporary");
    for chunk in replacement.chunks(65536) {
        check()?;
        output.write_all(chunk)?;
    }
    #[cfg(unix)]
    if let Ok(metadata) = std::fs::metadata(&destination) {
        output
            .set_permissions(metadata.permissions())
            .context("preserve repository config permissions")?;
    }
    output.flush()?;
    output
        .sync_all()
        .context("persist repository config temporary")?;
    drop(temporary.file.take());
    validate(repository, expected, check)?;
    std::fs::rename(&temporary.path, &destination)
        .context("publish repository config replacement")?;
    temporary.published = true;
    #[cfg(unix)]
    File::open(root)?
        .sync_all()
        .context("persist repository config directory")?;
    #[cfg(unix)]
    let status = {
        use std::os::unix::process::ExitStatusExt;
        ExitStatus::from_raw(0)
    };
    #[cfg(windows)]
    let status = {
        use std::os::windows::process::ExitStatusExt;
        ExitStatus::from_raw(0)
    };
    Ok(Output {
        status,
        stdout: Vec::new(),
        stderr: Vec::new(),
    })
}

fn read(path: &Path) -> Result<Option<Vec<u8>>> {
    let metadata = match std::fs::symlink_metadata(path) {
        Ok(metadata) => metadata,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(error) => return Err(error).context("inspect repository config"),
    };
    ensure!(
        metadata.is_file() && !metadata.file_type().is_symlink(),
        "repository config must be a regular file"
    );
    ensure!(
        metadata.len() <= MAX_CONFIG_BYTES as u64,
        "repository config exceeds 1 MiB"
    );
    let file = File::open(path).context("open repository config")?;
    let opened = file.metadata()?;
    ensure!(
        opened.is_file()
            && opened.len() == metadata.len()
            && opened.modified()? == metadata.modified()?,
        "repository config changed while opening"
    );
    let mut bytes = Vec::new();
    file.take((MAX_CONFIG_BYTES + 1) as u64)
        .read_to_end(&mut bytes)?;
    ensure!(
        bytes.len() <= MAX_CONFIG_BYTES,
        "repository config exceeds 1 MiB"
    );
    let after = std::fs::symlink_metadata(path)?;
    ensure!(
        after.is_file()
            && !after.file_type().is_symlink()
            && after.len() == metadata.len()
            && after.modified()? == metadata.modified()?,
        "repository config changed while reading"
    );
    Ok(Some(bytes))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::store::RepositoryStore;

    #[tokio::test]
    async fn replacement_preserves_original_on_conflict_and_cancel() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let output = std::process::Command::new("git")
            .arg("-C")
            .arg(directory.path())
            .args(["init", "--quiet"])
            .output()?;
        ensure!(output.status.success(), "fixture Git init failed");
        let store = RepositoryStore::default();
        let repository = store.open(directory.path().to_owned()).await?.unwrap();
        let destination = directory.path().join(".forge.json");
        let expected = Some(b"original".to_vec());
        std::fs::write(&destination, expected.as_ref().unwrap())?;
        assert!(
            replace(
                &repository,
                &None,
                b"replacement",
                OperationId(1),
                &mut || Ok(())
            )
            .is_err()
        );
        assert_eq!(std::fs::read(&destination)?, b"original");
        let mut calls = 0;
        let cancelled = replace(
            &repository,
            &expected,
            b"replacement",
            OperationId(2),
            &mut || {
                calls += 1;
                ensure!(calls < 4, "cancelled fixture");
                Ok(())
            },
        );
        assert!(cancelled.is_err());
        assert_eq!(std::fs::read(&destination)?, b"original");
        assert!(
            !directory
                .path()
                .join(format!(".forge.json.{}-2.tmp", std::process::id()))
                .exists()
        );
        let output = replace(
            &repository,
            &expected,
            b"replacement",
            OperationId(3),
            &mut || Ok(()),
        )?;
        assert!(output.status.success());
        assert_eq!(std::fs::read(&destination)?, b"replacement");
        assert!(
            !directory
                .path()
                .join(format!(".forge.json.{}-3.tmp", std::process::id()))
                .exists()
        );
        Ok(())
    }
}
