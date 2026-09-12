use std::future::Future;
use std::panic::{AssertUnwindSafe, catch_unwind, resume_unwind};

use anyhow::{Context, Result};

use crate::runtime::SHUTDOWN_DEADLINE;

/// Runs an entry point and bounds executor teardown after its service cleanup has returned.
/// Native workers can outlive this deadline. Their owners must report unfinished work first.
pub fn run(entry: impl Future<Output = Result<()>>) -> Result<()> {
    let executor = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .context("create Forge executor")?;
    let result = catch_unwind(AssertUnwindSafe(|| executor.block_on(entry)));
    executor.shutdown_timeout(SHUTDOWN_DEADLINE);
    match result {
        Ok(result) => result,
        Err(panic) => resume_unwind(panic),
    }
}

#[cfg(test)]
mod tests {
    use std::process::{Command, Stdio};
    use std::time::{Duration, Instant};

    use super::*;

    #[test]
    fn executor_teardown_cannot_wait_forever_for_a_native_worker() {
        let mut child = Command::new(std::env::current_exe().unwrap())
            .args([
                "--ignored",
                "--exact",
                "shutdown::tests::nonreturning_native_worker_process",
                "--nocapture",
            ])
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap();
        let deadline = Instant::now() + Duration::from_secs(5);
        loop {
            if child.try_wait().unwrap().is_some() {
                break;
            }
            if Instant::now() >= deadline {
                child.kill().unwrap();
                child.wait().unwrap();
                panic!("executor teardown exceeded the subprocess deadline");
            }
            std::thread::sleep(Duration::from_millis(10));
        }
        let output = child.wait_with_output().unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(String::from_utf8_lossy(&output.stdout).contains("unfinished worker reported"));
    }

    #[test]
    #[ignore = "isolated nonreturning-worker fixture invoked by the parent subprocess test"]
    fn nonreturning_native_worker_process() {
        let result = run(async {
            let (started, receiver) = tokio::sync::oneshot::channel();
            tokio::task::spawn_blocking(move || {
                started.send(()).unwrap();
                loop {
                    std::thread::park();
                }
            });
            receiver.await.unwrap();
            anyhow::bail!("unfinished worker reported")
        });
        assert_eq!(
            result.unwrap_err().to_string(),
            "unfinished worker reported"
        );
        println!("unfinished worker reported");
    }
}
