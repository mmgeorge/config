use std::future::Future;
use std::panic::{AssertUnwindSafe, catch_unwind, resume_unwind};

use anyhow::{Context, Result};

use crate::runtime::SHUTDOWN_DEADLINE;

/// Runs an entry point and bounds executor teardown after its service cleanup has returned.
/// Native workers can outlive this deadline. Their owners must report unfinished work first.
pub fn run(entry: impl Future<Output = Result<()>>) -> Result<()> {
    let executor = tokio::runtime::Builder::new_multi_thread()
        // Foreground settlement polls large provider futures; the Windows default stack can overflow.
        .thread_stack_size(8 * 1024 * 1024)
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
