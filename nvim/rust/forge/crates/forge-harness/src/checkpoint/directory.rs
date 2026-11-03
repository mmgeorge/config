use std::path::{Path, PathBuf};

use anyhow::{Context, Result, ensure};

use super::{CheckpointFile, CheckpointRecord};
use crate::storage::objects::ObjectStore;

/// Capture a folder without Git as a complete immutable file inventory.
/// Harness data and Git administrative directories do not belong to the workspace snapshot.
/// Links retain their targets; capture never traverses them outside the workspace.
pub(crate) fn capture_directory(
    workspace: &Path,
    data_root: &Path,
    objects: &ObjectStore,
    session_id: String,
    now_ms: i64,
) -> Result<CheckpointRecord> {
    let workspace = dunce::canonicalize(workspace)?;
    let data_root = dunce::canonicalize(data_root)?;
    ensure!(
        workspace != data_root,
        "folder snapshots require Harness data in a separate directory; select a data_root outside the workspace root"
    );
    let mut file = Vec::new();
    let mut excluded = std::collections::BTreeSet::new();
    let mut pending = vec![workspace.clone()];
    while let Some(directory) = pending.pop() {
        let mut entries = std::fs::read_dir(&directory)?
            .map(|entry| entry.map(|entry| entry.path()))
            .collect::<std::io::Result<Vec<PathBuf>>>()?;
        entries.sort();
        for path in entries {
            if path == data_root || path.file_name().is_some_and(|name| name == ".git") {
                continue;
            }
            let metadata = std::fs::symlink_metadata(&path)?;
            if metadata.is_dir() && !metadata.is_symlink() {
                pending.push(path);
                continue;
            }
            let relative = path
                .strip_prefix(&workspace)?
                .to_str()
                .context("workspace snapshot path is not UTF-8")?
                .replace('\\', "/");
            let kind = super::file_kind::classify(&path, &metadata)?;
            if matches!(kind, super::file_kind::FileKind::Excluded) {
                excluded.insert(relative);
                continue;
            }
            let (object_id, mode) = if matches!(kind, super::file_kind::FileKind::Symlink) {
                let target = std::fs::read_link(&path)?;
                (
                    objects.put(target.as_os_str().as_encoded_bytes())?,
                    0o120000,
                )
            } else if metadata.is_file() {
                #[cfg(unix)]
                let mode = {
                    use std::os::unix::fs::PermissionsExt;
                    if metadata.permissions().mode() & 0o111 == 0 {
                        0o100644
                    } else {
                        0o100755
                    }
                };
                #[cfg(not(unix))]
                let mode = 0o100644;
                let Some(captured) =
                    objects.capture_file(&path, &workspace, None, &mut || Ok(()))?
                else {
                    excluded.insert(relative);
                    continue;
                };
                (
                    captured
                        .object_id
                        .context("capture without a baseline requires an object")?,
                    mode,
                )
            } else {
                anyhow::bail!(
                    "workspace snapshot cannot capture special file {}",
                    path.display()
                );
            };
            file.push(CheckpointFile {
                path: relative,
                object_id,
                mode,
            });
        }
    }
    file.sort_by(|left, right| left.path.cmp(&right.path));
    let id = crate::plan::digest(&serde_json::to_vec(&(
        &session_id,
        &workspace,
        &file,
        &excluded,
    ))?);
    Ok(CheckpointRecord {
        id,
        session_id,
        workspace: workspace.to_string_lossy().into_owned(),
        head: "UNTRACKED".into(),
        tree: None,
        file,
        deleted: Vec::new(),
        checkout: Default::default(),
        excluded,
        created_at_ms: now_ms,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Keep creation, modification, and deletion observable while excluding Harness's own writes.
    #[test]
    fn folder_snapshots_track_content_without_self_invalidation() -> Result<()> {
        let workspace = tempfile::tempdir()?;
        let data_root = workspace.path().join("data");
        let objects = ObjectStore::open(&data_root)?;
        std::fs::write(workspace.path().join("source.rs"), "fn original() {}\n")?;
        let original =
            capture_directory(workspace.path(), &data_root, &objects, "session".into(), 1)?;
        std::fs::write(data_root.join("runtime.log"), "Harness activity")?;
        let same = capture_directory(workspace.path(), &data_root, &objects, "session".into(), 2)?;
        assert_eq!(original.id, same.id);
        std::fs::write(workspace.path().join("source.rs"), "fn repaired() {}\n")?;
        std::fs::write(workspace.path().join("helper.rs"), "fn helper() {}\n")?;
        let changed =
            capture_directory(workspace.path(), &data_root, &objects, "session".into(), 3)?;
        assert_eq!(
            original.changed_paths(&changed)?,
            vec!["helper.rs", "source.rs"]
        );
        assert_eq!(
            original.read(&objects, "source.rs")?.unwrap(),
            b"fn original() {}\n"
        );
        std::fs::remove_file(workspace.path().join("source.rs"))?;
        let removed =
            capture_directory(workspace.path(), &data_root, &objects, "session".into(), 4)?;
        assert_eq!(changed.changed_paths(&removed)?, vec!["source.rs"]);
        Ok(())
    }
}
