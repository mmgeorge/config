use anyhow::Result;
use clap::Parser;
use forge::Command;

#[derive(Parser)]
#[command(name = "forge")]
struct Argument {
    /// Skip display line statistics to measure status startup without source acquisition.
    #[arg(long, global = true)]
    diagnostic_skip_line_stats: bool,
    #[command(subcommand)]
    command: Option<Command>,
}

fn main() -> Result<()> {
    let argument = Argument::parse();
    forge::run(
        argument.command.unwrap_or(Command::Nvim),
        argument.diagnostic_skip_line_stats,
    )
}
