use anyhow::{Context, Result};
use std::path::{Path, PathBuf};

pub mod watch;

/// Represents whether a workspace can provide tracked interaction checkpoints.
#[derive(Clone, Debug)]
pub enum WorkspaceKind {
    Git(PathBuf),
    Untracked(PathBuf),
}

/// Resolve the exact Git worktree root without silently substituting the process directory.
pub fn resolve(path: &Path) -> Result<WorkspaceKind> {
    let output = std::process::Command::new("git")
        .args(["rev-parse", "--show-toplevel"])
        .current_dir(path)
        .output()
        .context("resolve Git worktree root")?;
    if output.status.success() {
        let root = String::from_utf8(output.stdout)?.trim().to_owned();
        return Ok(WorkspaceKind::Git(PathBuf::from(root)));
    }
    Ok(WorkspaceKind::Untracked(path.to_path_buf()))
}
