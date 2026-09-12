mod host;
mod router;
mod runtime;
mod shutdown;

use anyhow::Result;
use clap::{Parser, Subcommand};
use std::sync::Arc;

#[derive(Parser)]
#[command(name = "forge")]
struct Argument {
    /// Skip display line statistics to measure status startup without source acquisition.
    #[arg(long, global = true)]
    diagnostic_skip_line_stats: bool,
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand)]
enum Command {
    /// Run the Neovim host over standard input and output.
    Nvim,
    /// Run the plan and goal control-tool MCP server.
    Mcp,
}

fn main() -> Result<()> {
    let argument = Argument::parse();
    shutdown::run(async {
        match argument.command {
            Some(Command::Mcp) => forge_harness::control_tools::run_stdio().await,
            None | Some(Command::Nvim) => {
                let host = Arc::new(runtime::ForgeRuntime::new()?);
                host.repositories.diagnostic_skip_line_stats.store(
                    argument.diagnostic_skip_line_stats,
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
