use forge_provider_api::permissions::{PermissionRequest, PermissionTarget};
use std::path::{Component, Path, PathBuf};

/// Restrict automatic SDK storage access to one retained session's directory.
#[derive(Clone)]
pub(super) struct SessionDirectory {
    root: PathBuf,
    workspace: PathBuf,
}

impl SessionDirectory {
    /// Create a directory boundary from the SDK session's own workspace and cwd.
    pub(super) fn from_session(session: &super::handle::CopilotSession) -> Option<Self> {
        let root = session.workspace_path()?;
        if !root.is_absolute() {
            return None;
        }
        let cwd = root_path(session.cwd()).ok()?;
        let workspace = canonicalize_existing(&cwd).ok()?;
        let root = resolve_path(root, &workspace).ok()?;
        Some(Self { root, workspace })
    }

    /// Check that every target is a read or write inside the exact native session directory.
    pub(super) fn approves(&self, request: &PermissionRequest) -> bool {
        if request.target_list.is_empty() {
            return false;
        }
        request.target_list.iter().all(|target| {
            let path = match target {
                PermissionTarget::Read { path } | PermissionTarget::Write { path } => path,
                _ => return false,
            };
            resolve_path(Path::new(path), &self.workspace)
                .is_ok_and(|path| path.starts_with(&self.root))
        })
    }
}

/// Make an SDK cwd absolute and resolve its existing filesystem identity.
fn root_path(path: &Path) -> std::io::Result<PathBuf> {
    if path.is_absolute() {
        canonicalize_existing(path)
    } else {
        canonicalize_existing(&std::env::current_dir()?.join(path))
    }
}

/// Resolve an input path while following symlinks in its existing prefix.
fn resolve_path(path: &Path, workspace: &Path) -> std::io::Result<PathBuf> {
    let joined = if path.is_absolute() {
        path.to_owned()
    } else {
        workspace.join(path)
    };
    let normalized = normalize(&joined);
    canonicalize_existing(&normalized)
}

/// Canonicalize the existing prefix and retain a normalized nonexistent suffix.
fn canonicalize_existing(path: &Path) -> std::io::Result<PathBuf> {
    let normalized = normalize(path);
    let mut existing = normalized.as_path();
    let mut suffix = Vec::new();
    loop {
        match std::fs::symlink_metadata(existing) {
            Ok(_) => break,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                let name = existing.file_name().ok_or_else(|| {
                    std::io::Error::new(
                        std::io::ErrorKind::InvalidInput,
                        "path has no existing ancestor",
                    )
                })?;
                suffix.push(name.to_owned());
                existing = existing.parent().ok_or_else(|| {
                    std::io::Error::new(
                        std::io::ErrorKind::InvalidInput,
                        "path has no existing ancestor",
                    )
                })?;
            }
            Err(error) => return Err(error),
        }
    }
    let mut canonical = dunce::canonicalize(existing)?;
    for component in suffix.into_iter().rev() {
        canonical.push(component);
    }
    Ok(normalize(&canonical))
}

/// Remove lexical parent traversals before checking canonical filesystem ancestry.
fn normalize(path: &Path) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                normalized.pop();
            }
            component => normalized.push(component.as_os_str()),
        }
    }
    normalized
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn session_storage_is_scoped_and_rejects_escape_targets() -> std::io::Result<()> {
        let temporary = tempfile::tempdir()?;
        let workspace = temporary.path().join("workspace");
        let root = workspace.join("session-state/current");
        std::fs::create_dir_all(&root)?;
        let directory = SessionDirectory {
            root: dunce::canonicalize(&root)?,
            workspace: dunce::canonicalize(&workspace)?,
        };
        let request = |paths: Vec<PermissionTarget>| PermissionRequest {
            id: "permission".into(),
            provider: forge_provider_api::ProviderId::new("copilot").expect("registered identity"),
            reason: None,
            target_list: paths,
        };
        assert!(directory.approves(&request(vec![
            PermissionTarget::Read {
                path: root.join("plan.md").to_string_lossy().into_owned()
            },
            PermissionTarget::Write {
                path: root.join("nested/notes.txt").to_string_lossy().into_owned()
            },
        ])));
        assert!(!directory.approves(&request(vec![
                PermissionTarget::Read {
                    path: root.join("plan.md").to_string_lossy().into_owned()
                },
                PermissionTarget::Write {
                    path: workspace
                        .join("session-state/other/notes.txt")
                        .to_string_lossy()
                        .into_owned(),
                },
            ])));
        for outside in [
            root.join("../other/plan.md"),
            root.join("../../config.json"),
            workspace.join("session-state/current-sibling/plan.md"),
        ] {
            assert!(!directory.approves(&request(vec![PermissionTarget::Write {
                path: outside.to_string_lossy().into_owned(),
            }])));
        }
        assert!(
            !directory.approves(&request(vec![PermissionTarget::Command {
                command: "echo session-file".into(),
                shell: None,
            }]))
        );
        assert!(!directory.approves(&request(Vec::new())));
        Ok(())
    }

    #[test]
    fn session_storage_rejects_symlink_escape() -> std::io::Result<()> {
        let temporary = tempfile::tempdir()?;
        let root = temporary.path().join("current");
        let outside = temporary.path().join("outside");
        std::fs::create_dir_all(&root)?;
        std::fs::create_dir_all(&outside)?;
        #[cfg(unix)]
        std::os::unix::fs::symlink(&outside, root.join("linked"))?;
        #[cfg(windows)]
        junction::create(&outside, root.join("linked"))?;
        let directory = SessionDirectory {
            root: dunce::canonicalize(&root)?,
            workspace: dunce::canonicalize(temporary.path())?,
        };
        let request = PermissionRequest {
            id: "permission".into(),
            provider: forge_provider_api::ProviderId::new("copilot").expect("registered identity"),
            reason: None,
            target_list: vec![PermissionTarget::Write {
                path: root.join("linked/plan.md").to_string_lossy().into_owned(),
            }],
        };
        assert!(!directory.approves(&request));
        Ok(())
    }
}
