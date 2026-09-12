use std::path::PathBuf;
use std::time::Instant;

use anyhow::{Context, Result, ensure};

fn main() -> Result<()> {
    let mut arguments = std::env::args_os().skip(1);
    let path = PathBuf::from(arguments.next().context("expected repository path")?);
    let mode = arguments.next();
    ensure!(
        mode.as_deref()
            .is_none_or(|value| value == "--isolate-system-prefix")
            && arguments.next().is_none(),
        "usage: discovery_profile <repository> [--isolate-system-prefix]"
    );
    let isolate_system_prefix = mode.is_some();
    if isolate_system_prefix {
        let started = Instant::now();
        let found = gix::path::env::system_prefix().is_some();
        println!(
            "system_prefix_us={} found={found}",
            started.elapsed().as_micros()
        );
    }
    for iteration in 1..=3 {
        let started = Instant::now();
        let repository = gix::discover(&path)?;
        let discovery_us = started.elapsed().as_micros();
        let started = Instant::now();
        let git_directory = dunce::canonicalize(repository.git_dir())?;
        let common_directory = dunce::canonicalize(repository.common_dir())?;
        let worktree = repository.workdir().map(dunce::canonicalize).transpose()?;
        let index_path = repository.index_path();
        let index_parent = dunce::canonicalize(index_path.parent().context("index parent")?)?;
        std::hint::black_box((git_directory, common_directory, worktree, index_parent));
        println!(
            "iteration={iteration} gix_discover_us={discovery_us} canonical_paths_us={}",
            started.elapsed().as_micros()
        );
    }
    Ok(())
}
