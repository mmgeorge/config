use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;
use std::process::{ExitStatus, Output};

use anyhow::{Context, Result, ensure};

use crate::mutation::OperationId;
use crate::repository::RepositoryState;

pub(super) fn validate(
    repository: &RepositoryState,
    expected: &Option<Vec<u8>>,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<()> {
    check()?;
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
    validate(repository, expected, check)?;
    let root = repository
        .identity
        .worktree_root
        .as_ref()
        .context("repository config requires a worktree")?;
    let destination = root.join(".forge.json");
    let prefix = format!(".forge.json.{}-{}.", std::process::id(), operation.value());
    let mut temporary = tempfile::Builder::new()
        .prefix(&prefix)
        .suffix(".tmp")
        .tempfile_in(root)
        .context("create repository config temporary")?;
    let prepared = (|| -> Result<()> {
        let output = temporary.as_file_mut();
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
        validate(repository, expected, check)
    })();
    if let Err(error) = prepared {
        return Err(cleanup_failure(temporary, error));
    }
    let published = temporary.persist(&destination).map_err(|failure| {
        cleanup_failure(
            failure.file,
            anyhow::Error::new(failure.error).context("publish repository config replacement"),
        )
    })?;
    drop(published);
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

fn cleanup_failure(temporary: tempfile::NamedTempFile, error: anyhow::Error) -> anyhow::Error {
    match temporary.close() {
        Ok(()) => error,
        Err(cleanup) => error.context(format!(
            "repository config temporary cleanup failed: {cleanup}"
        )),
    }
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
    let mut file = File::open(path).context("open repository config")?;
    let opened = file.metadata()?;
    ensure!(
        opened.is_file()
            && opened.len() == metadata.len()
            && opened.modified()? == metadata.modified()?,
        "repository config changed while opening"
    );
    let mut bytes = Vec::new();
    file.read_to_end(&mut bytes)?;
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

    #[test]
    fn cleanup_failure_preserves_the_operation_and_cleanup_errors() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let temporary = tempfile::NamedTempFile::new_in(directory.path())?;
        let path = temporary.path().to_owned();
        std::fs::remove_file(&path)?;
        std::fs::create_dir(&path)?;
        let error = cleanup_failure(temporary, anyhow::anyhow!("original write failure"));
        let message = format!("{error:#}");
        assert!(message.contains("original write failure"));
        assert!(message.contains("repository config temporary cleanup failed"));
        std::fs::remove_dir(path)?;
        Ok(())
    }

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
        for entry in std::fs::read_dir(directory.path())? {
            assert!(!entry?.file_name().to_string_lossy().ends_with(".tmp"));
        }
        let output = replace(
            &repository,
            &expected,
            b"replacement",
            OperationId(3),
            &mut || Ok(()),
        )?;
        assert!(output.status.success());
        assert_eq!(std::fs::read(&destination)?, b"replacement");
        for entry in std::fs::read_dir(directory.path())? {
            assert!(!entry?.file_name().to_string_lossy().ends_with(".tmp"));
        }
        Ok(())
    }
}
