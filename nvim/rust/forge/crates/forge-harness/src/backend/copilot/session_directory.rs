use crate::permissions::{
    directory::resolve_path,
    matcher::{PermissionRequest, PermissionTarget, path_starts_with},
};
use anyhow::Result;
use github_copilot_sdk::{SessionId, session::Session};
use std::path::{Path, PathBuf};

/// Limits automatic file access to the directory reported for one native session.
#[derive(Clone)]
pub(super) struct SessionDirectory {
    session_id: SessionId,
    root: PathBuf,
}

impl SessionDirectory {
    pub(super) fn new(session: &Session) -> Result<Option<Self>> {
        let Some(root) = session.workspace_path().filter(|root| root.is_absolute()) else {
            return Ok(None);
        };
        Ok(Some(Self {
            session_id: session.id().clone(),
            root: resolve_path(root, session.cwd())?,
        }))
    }

    pub(super) fn approves(
        &self,
        session_id: &SessionId,
        request: &PermissionRequest,
        workspace: &Path,
    ) -> Result<bool> {
        if session_id != self.session_id || request.target_list.is_empty() {
            return Ok(false);
        }
        for target in &request.target_list {
            let path = match target {
                PermissionTarget::Read { path } | PermissionTarget::Write { path } => path,
                _ => return Ok(false),
            };
            let path = resolve_path(Path::new(path), workspace)?;
            if !path_starts_with(&path, &self.root) {
                return Ok(false);
            }
        }
        Ok(true)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn session_files_are_scoped_to_native_identity_and_resolved_directory() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        let root = temporary.path().join("session-state/current");
        std::fs::create_dir_all(&root)?;
        let session_id = SessionId::new("current");
        let directory = SessionDirectory {
            session_id: session_id.clone(),
            root: resolve_path(&root, temporary.path())?,
        };
        let mut request = PermissionRequest {
            id: "session-files".into(),
            provider: "Copilot CLI".into(),
            reason: None,
            target_list: vec![
                PermissionTarget::Read {
                    path: root.join("plan.md").to_string_lossy().into_owned(),
                },
                PermissionTarget::Write {
                    path: root.join("files/notes.txt").to_string_lossy().into_owned(),
                },
            ],
        };
        assert!(directory.approves(&session_id, &request, temporary.path())?);
        assert!(!directory.approves(&SessionId::new("other"), &request, temporary.path())?);
        for outside in [
            root.join("../other/plan.md"),
            root.join("../../config.json"),
            temporary
                .path()
                .join("session-state/current-sibling/plan.md"),
        ] {
            request.target_list.push(PermissionTarget::Write {
                path: outside.to_string_lossy().into_owned(),
            });
            assert!(!directory.approves(&session_id, &request, temporary.path())?);
            request.target_list.pop();
        }
        request.target_list.push(PermissionTarget::Command {
            command: "echo session-file".into(),
            shell: None,
        });
        assert!(!directory.approves(&session_id, &request, temporary.path())?);
        request.target_list.clear();
        assert!(!directory.approves(&session_id, &request, temporary.path())?);
        Ok(())
    }

    #[test]
    fn session_files_do_not_grant_symlink_escape() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        let root = temporary.path().join("current");
        let outside = temporary.path().join("outside");
        std::fs::create_dir_all(&root)?;
        std::fs::create_dir_all(&outside)?;
        #[cfg(unix)]
        std::os::unix::fs::symlink(&outside, root.join("linked"))?;
        #[cfg(windows)]
        junction::create(&outside, root.join("linked"))?;
        let session_id = SessionId::new("current");
        let directory = SessionDirectory {
            session_id: session_id.clone(),
            root: resolve_path(&root, temporary.path())?,
        };
        let request = PermissionRequest {
            id: "symlink-escape".into(),
            provider: "Copilot CLI".into(),
            reason: None,
            target_list: vec![PermissionTarget::Write {
                path: root.join("linked/plan.md").to_string_lossy().into_owned(),
            }],
        };
        assert!(!directory.approves(&session_id, &request, temporary.path())?);
        Ok(())
    }
}
