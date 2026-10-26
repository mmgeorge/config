use std::io::{Read, Seek, Write};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

use anyhow::{Result, ensure};
use forge_git::read_pool::BlockingReadPool;

#[test]
fn checkpoint_worker_survives_closed_stderr() -> Result<()> {
    let (reader, writer) = std::io::pipe()?;
    drop(reader);
    let mut child = Command::new(std::env::current_exe()?)
        .args([
            "--exact",
            "checkpoint_worker_with_closed_stderr",
            "--ignored",
            "--nocapture",
        ])
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(writer)
        .spawn()?;
    wait_for_worker(&mut child)
}

#[test]
fn checkpoint_worker_emits_no_diagnostics() -> Result<()> {
    let mut diagnostics = tempfile::tempfile()?;
    let mut child = Command::new(std::env::current_exe()?)
        .args([
            "--exact",
            "checkpoint_worker_without_diagnostics",
            "--ignored",
            "--nocapture",
        ])
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(diagnostics.try_clone()?)
        .spawn()?;
    wait_for_worker(&mut child)?;
    diagnostics.rewind()?;
    let mut output = String::new();
    diagnostics.read_to_string(&mut output)?;
    ensure!(
        output.is_empty(),
        "unexpected checkpoint diagnostics: {output}"
    );
    Ok(())
}

fn wait_for_worker(child: &mut Child) -> Result<()> {
    let deadline = Instant::now() + Duration::from_secs(10);
    loop {
        if let Some(status) = child.try_wait()? {
            ensure!(status.success(), "checkpoint worker failed: {status}");
            return Ok(());
        }
        if Instant::now() >= deadline {
            child.kill()?;
            child.wait()?;
            anyhow::bail!("checkpoint worker did not exit within 10 seconds");
        }
        std::thread::sleep(Duration::from_millis(10));
    }
}

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
