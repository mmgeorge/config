use std::io::Write;
use std::process::Command;
use std::time::Duration;

use anyhow::{Result, ensure};
use forge_git::read_pool::BlockingReadPool;

#[tokio::test]
#[ignore = "runs in a subprocess with its stderr reader already closed"]
async fn checkpoint_worker_with_closed_stderr() -> Result<()> {
    assert_eq!(
        writeln!(std::io::stderr(), "probe disconnected stderr")
            .unwrap_err()
            .kind(),
        std::io::ErrorKind::BrokenPipe,
    );
    exercise_checkpoint().await
}

#[tokio::test]
#[ignore = "runs in a subprocess with captured stderr"]
async fn checkpoint_worker_without_diagnostics() -> Result<()> {
    exercise_checkpoint().await
}

async fn exercise_checkpoint() -> Result<()> {
    let directory = tempfile::tempdir()?;
    let initialized = Command::new("git")
        .args(["init", "--quiet"])
        .current_dir(directory.path())
        .output()?;
    ensure!(
        initialized.status.success(),
        "{}",
        String::from_utf8_lossy(&initialized.stderr)
    );
    std::fs::write(directory.path().join("changed.txt"), "checkpoint content\n")?;
    let root = directory.path().to_path_buf();
    let pool = BlockingReadPool::new(1)?;
    let signature = pool
        .submit(1, move |cancellation| {
            forge_git::checkpoint::signature(&root, &mut || cancellation.check())
        })?
        .finish()
        .await?;
    let inventory = forge_git::checkpoint::inventory(directory.path(), &signature, &mut || Ok(()))?;
    assert!(inventory.candidate.contains("changed.txt"));
    assert!(
        pool.shutdown(Duration::from_secs(1))
            .await
            .unfinished
            .is_empty()
    );
    Ok(())
}
