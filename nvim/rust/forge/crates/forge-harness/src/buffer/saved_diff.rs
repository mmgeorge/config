use super::projection::SavedCheckpoint;
use crate::{
    checkpoint::CheckpointRecord,
    storage::{SqliteStore, objects::ObjectStore},
};
use anyhow::{Context, Result, ensure};
use forge_diff::{
    patch::UnifiedPatch,
    saved_source::{SavedDiffSource, SavedFileSource},
    source::{Representation, SourcePair, SourceVersion},
};
use std::{path::Path, sync::Arc};

pub(crate) fn capture(
    data_root: &Path,
    checkpoint: Option<&SavedCheckpoint>,
    text: String,
) -> SavedDiffSource {
    let text = Arc::new(text);
    let parsed = match UnifiedPatch::parse(&text) {
        Ok(parsed) => parsed,
        Err(failure) => {
            return SavedDiffSource {
                text: text.clone(),
                file: Vec::new(),
                diagnostic: Some(format!("saved patch cannot be classified: {failure}")),
            };
        }
    };
    let captured = checkpoint.map(|checkpoint| load_checkpoint_pair(data_root, checkpoint));
    let file = parsed
        .file
        .iter()
        .map(|file| {
            let source = (|| -> Result<SourcePair> {
                let captured = captured
                    .as_ref()
                    .context("complete captured sources are unavailable")?;
                let (before, after, objects) = captured
                    .as_ref()
                    .map_err(|failure| anyhow::anyhow!("{failure:#}"))?;
                Ok(SourcePair {
                    old: read_source(before, objects, file.old_path.as_deref())?,
                    new: read_source(after, objects, file.new_path.as_deref())?,
                })
            })()
            .map_err(|failure| format!("{failure:#}"));
            SavedFileSource {
                old_path: file.old_path.clone(),
                new_path: file.new_path.clone(),
                source,
            }
        })
        .collect();
    SavedDiffSource {
        text,
        file,
        diagnostic: None,
    }
}

fn load_checkpoint_pair(
    data_root: &Path,
    checkpoint: &SavedCheckpoint,
) -> Result<(CheckpointRecord, CheckpointRecord, ObjectStore)> {
    let store = SqliteStore::read_only(data_root)?;
    let before = store
        .load_checkpoint(&checkpoint.before)?
        .context("saved before checkpoint is unavailable")?;
    let after = store
        .load_checkpoint(&checkpoint.after)?
        .context("saved after checkpoint is unavailable")?;
    ensure!(
        before.id == checkpoint.before && after.id == checkpoint.after,
        "saved checkpoint content identity differs from its requested record"
    );
    ensure!(
        before.session_id == checkpoint.session_id && after.session_id == checkpoint.session_id,
        "saved checkpoint belongs to another session"
    );
    ensure!(
        before.workspace == after.workspace,
        "saved checkpoint workspaces differ"
    );
    Ok((before, after, ObjectStore::open(data_root)?))
}

fn read_source(
    checkpoint: &CheckpointRecord,
    objects: &ObjectStore,
    path: Option<&str>,
) -> Result<SourceVersion> {
    let bytes = match path {
        Some(path) => checkpoint
            .read(objects, path)?
            .with_context(|| format!("captured source is unavailable: {path}"))?,
        None => Vec::new(),
    };
    SourceVersion::new(bytes, Representation::Raw).map_err(anyhow::Error::from)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checkpoint::CheckpointFile;
    use std::collections::{BTreeMap, BTreeSet};

    #[test]
    fn saved_sources_use_immutable_checkpoint_objects_and_reject_foreign_provenance() -> Result<()>
    {
        let workspace = tempfile::tempdir()?;
        let initialized = std::process::Command::new("git")
            .args(["init", "-q"])
            .current_dir(workspace.path())
            .status()?;
        assert!(initialized.success());
        let data = tempfile::tempdir()?;
        let mut store = SqliteStore::open(data.path())?;
        let connection = rusqlite::Connection::open(data.path().join("harness.sqlite3"))?;
        connection.execute("INSERT INTO session_record(id, workspace, updated_at_ms, payload) VALUES('captured-session', ?1, 0, '{}')", [workspace.path().to_string_lossy().as_ref()])?;
        let before_text = b"pub fn saved() { previous(); }\n";
        let after_text = b"pub fn saved() { updated(); }\n";
        let before = record("before", workspace.path(), store.objects.put(before_text)?);
        let after = record("after", workspace.path(), store.objects.put(after_text)?);
        store.save_checkpoint(&before)?;
        store.save_checkpoint(&after)?;
        std::fs::write(
            workspace.path().join("source.rs"),
            "current worktree bytes are unrelated\n",
        )?;
        let patch = "--- a/source.rs\n+++ b/source.rs\n@@ -1 +1 @@\n-pub fn saved() { previous(); }\n+pub fn saved() { updated(); }\n";
        let checkpoint = SavedCheckpoint {
            session_id: "captured-session".into(),
            before: before.id.clone(),
            after: after.id.clone(),
        };
        let source = capture(data.path(), Some(&checkpoint), patch.into());
        let source = source.file[0].source.as_ref().unwrap();
        assert_eq!(source.old.bytes(), before_text);
        assert_eq!(source.new.bytes(), after_text);
        let mut foreign = checkpoint.clone();
        foreign.session_id = "foreign-session".into();
        let rejected = capture(data.path(), Some(&foreign), patch.into());
        assert!(
            rejected.file[0]
                .source
                .as_ref()
                .unwrap_err()
                .contains("another session")
        );
        let mut missing = checkpoint.clone();
        missing.after = "missing-checkpoint".into();
        let rejected = capture(data.path(), Some(&missing), patch.into());
        assert!(
            rejected.file[0]
                .source
                .as_ref()
                .unwrap_err()
                .contains("unavailable")
        );
        let unavailable = capture(
            data.path(),
            Some(&checkpoint),
            patch.replace("source.rs", "uncaptured.rs"),
        );
        assert!(
            unavailable.file[0]
                .source
                .as_ref()
                .unwrap_err()
                .contains("captured source is unavailable")
        );
        let unavailable = capture(data.path(), None, patch.into());
        assert!(
            unavailable.file[0]
                .source
                .as_ref()
                .unwrap_err()
                .contains("complete captured sources are unavailable")
        );
        let malformed = capture(data.path(), Some(&checkpoint), "invalid saved patch".into());
        assert_eq!(malformed.text.as_str(), "invalid saved patch");
        assert!(malformed.file.is_empty() && malformed.diagnostic.is_some());
        Ok(())
    }

    fn record(id: &str, workspace: &Path, object_id: String) -> CheckpointRecord {
        CheckpointRecord {
            id: id.into(),
            session_id: "captured-session".into(),
            workspace: workspace.to_string_lossy().into_owned(),
            head: "UNBORN".into(),
            tree: None,
            file: vec![CheckpointFile {
                path: "source.rs".into(),
                object_id,
                mode: 0o100644,
            }],
            deleted: Vec::new(),
            checkout: BTreeMap::new(),
            excluded: BTreeSet::new(),
            created_at_ms: 0,
        }
    }
}
