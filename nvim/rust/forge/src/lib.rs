mod host;
mod router;
mod runtime;
mod shutdown;

use anyhow::Result;
use clap::Subcommand;
use std::sync::Arc;

#[derive(Subcommand)]
/// Selects the process-owned standard-input service.
pub enum Command {
    /// Run the Neovim host over standard input and output.
    Nvim,
    /// Run the plan and goal control-tool MCP server.
    Mcp,
}

#[inline(never)]
/// Runs the selected service and shuts down its executor before returning.
///
/// The diagnostic flag skips repository line statistics in Neovim mode.
/// Returns service, initialization, or shutdown errors to the process entry point.
pub fn run(command: Command, diagnostic_skip_line_stats: bool) -> Result<()> {
    shutdown::run(async {
        match command {
            Command::Mcp => forge_harness::control_tools::run_stdio().await,
            Command::Nvim => {
                let host = Arc::new(runtime::ForgeRuntime::new()?);
                host.repositories.diagnostic_skip_line_stats.store(
                    diagnostic_skip_line_stats,
                    std::sync::atomic::Ordering::Relaxed,
                );
                let outcome = host::run_nvim(Arc::clone(&host)).await;
                let result = outcome.result;
                let shutdown = if outcome.shutdown_owned {
                    Ok(())
                } else {
                    tokio::time::timeout(runtime::SHUTDOWN_DEADLINE, host.shutdown())
                        .await
                        .map_err(|_| anyhow::anyhow!("Forge runtime shutdown deadline expired"))
                        .and_then(|result| result)
                };
                match (result, shutdown) {
                    (Err(error), Err(shutdown)) => Err(error.context(format!("{shutdown:#}"))),
                    (Err(error), _) | (_, Err(error)) => Err(error),
                    _ => Ok(()),
                }
            }
        }
    })
}
