use crate::storage::objects::ObjectStore;
use anyhow::{Context, Result};
use forge_git::checkpoint::{BaseFile, CheckoutRule};
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, path::Path};

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
/// Stores one override relative to the checkpoint's existing Git tree.
pub struct CheckpointFile {
    /// Repository-relative path.
    pub path: String,
    /// Immutable Forge content identity.
    pub object_id: String,
    /// File kind and executable mode using Git's representation.
    pub mode: u32,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Retains a Git baseline and complete overrides without changing Git history or staging.
pub struct CheckpointRecord {
    /// Immutable record identity, independent of capture time and staging.
    pub id: String,
    /// Session that owns this record.
    pub session_id: String,
    /// Canonical working-tree root.
    pub workspace: String,
    /// Observed commit, or UNBORN.
    pub head: String,
    /// Existing root tree, absent for an unborn repository.
    pub tree: Option<String>,
    /// Changed or new file contents only.
    pub file: Vec<CheckpointFile>,
    /// Baseline paths absent from this checkpoint.
    pub deleted: Vec<String>,
    /// Non-default checkout conversion sampled for baseline paths.
    pub checkout: BTreeMap<String, CheckoutRule>,
    /// Time at which capture completed.
    pub created_at_ms: i64,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub(super) enum Content {
    Stored(String),
    Git(BaseFile),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub(super) struct ResolvedFile {
    pub source: Content,
    pub mode: u32,
}

impl CheckpointRecord {
    /// Resolves metadata without loading any file contents.
    pub(super) fn resolved(&self) -> Result<BTreeMap<String, ResolvedFile>> {
        let mut files =
            forge_git::checkpoint::tree_files(Path::new(&self.workspace), self.tree.as_deref())?
                .into_iter()
                .map(|(path, mut file)| {
                    file.checkout = self.checkout.get(&path).cloned().unwrap_or_default();
                    let mode = file.mode;
                    (
                        path,
                        ResolvedFile {
                            source: Content::Git(file),
                            mode,
                        },
                    )
                })
                .collect::<BTreeMap<_, _>>();
        for path in &self.deleted {
            files.remove(path);
        }
        for file in &self.file {
            files.insert(
                file.path.clone(),
                ResolvedFile {
                    source: Content::Stored(file.object_id.clone()),
                    mode: file.mode,
                },
            );
        }
        Ok(files)
    }

    /// Compares resolved content independently of session IDs, Git history, and staging.
    pub fn equivalent(&self, other: &Self, objects: &ObjectStore) -> Result<bool> {
        let left = self.resolved()?;
        let right = other.resolved()?;
        if !left.keys().eq(right.keys()) {
            return Ok(false);
        }
        for (path, file) in &left {
            let other_file = &right[path];
            if file.mode != other_file.mode {
                return Ok(false);
            }
            if file == other_file {
                continue;
            }
            let identity = |file: &ResolvedFile, root: &str| -> Result<String> {
                match &file.source {
                    Content::Stored(identity) => Ok(identity.clone()),
                    Content::Git(base) => objects.baseline_identity(Path::new(root), base),
                }
            };
            if identity(file, &self.workspace)? != identity(other_file, &other.workspace)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// Retrieves one captured source, including clean files supplied by the Git baseline.
    pub fn read(&self, objects: &ObjectStore, path: &str, limit: usize) -> Result<Option<Vec<u8>>> {
        if let Some(file) = self.file.iter().find(|file| file.path == path) {
            return Ok(Some(
                objects
                    .get(&file.object_id, limit)?
                    .context("checkpoint source exceeds the requested byte limit")?,
            ));
        }
        if self.deleted.iter().any(|deleted| deleted == path) {
            return Ok(None);
        }
        let Some(mut file) = forge_git::checkpoint::tree_file(
            Path::new(&self.workspace),
            self.tree.as_deref(),
            path,
        )?
        else {
            return Ok(None);
        };
        file.checkout = self.checkout.get(path).cloned().unwrap_or_default();
        Ok(Some(
            forge_git::checkpoint::read_blob(Path::new(&self.workspace), &file, limit)?
                .context("checkpoint source exceeds the requested byte limit")?,
        ))
    }
}

impl ResolvedFile {
    pub(super) fn read(
        &self,
        objects: &ObjectStore,
        root: &Path,
        limit: usize,
    ) -> Result<Option<Vec<u8>>> {
        match &self.source {
            Content::Stored(identity) => objects.get(identity, limit),
            Content::Git(file) => forge_git::checkpoint::read_blob(root, file, limit)
                .context("checkpoint baseline content is unavailable"),
        }
    }
}
