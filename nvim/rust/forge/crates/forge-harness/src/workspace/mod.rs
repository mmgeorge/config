use anyhow::Result;
use std::path::{Path, PathBuf};

/// Represents whether a workspace can provide tracked interaction checkpoints.
#[derive(Clone, Debug)]
pub enum WorkspaceKind {
    Git(PathBuf),
    Untracked(PathBuf),
}

/// Resolve the exact Git worktree root without silently substituting the process directory.
pub fn resolve(path: &Path) -> Result<WorkspaceKind> {
    if let Some(identity) = forge_git::discover_identity(path)?
        && let Some(root) = identity.worktree_root
    {
        return Ok(WorkspaceKind::Git(root));
    }
    Ok(WorkspaceKind::Untracked(path.to_path_buf()))
}
