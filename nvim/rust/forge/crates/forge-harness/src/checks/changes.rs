use crate::{
    checkpoint::{CheckpointRecord, GitCheckpoint},
    storage::objects::ObjectStore,
};
use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::{
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};

/// Compares the current worktree with the task's captured state, including pre-existing edits.
pub(super) struct CheckChanges {
    path: PathBuf,
    checkpoint: GitCheckpoint,
    objects: ObjectStore,
    repositories: Arc<forge_git::store::RepositoryStore>,
    baseline: Mutex<Option<CheckBaseline>>,
    current: Mutex<Option<Vec<String>>>,
}

impl CheckChanges {
    /// Returns the identity whose initial checkpoint owns current changes.
    pub(super) fn owner(&self) -> Result<Option<String>> {
        Ok(self.baseline.lock().map_err(|_| anyhow::anyhow!("check baseline lock poisoned"))?.as_ref().map(|baseline| baseline.owner.clone()))
    }

    /// Captures inputs through the same repository boundary as task applicability.
    pub(super) async fn snapshot(&self) -> Result<Option<CheckpointRecord>> {
        let baseline = self.baseline.lock().map_err(|_| anyhow::anyhow!("check baseline lock poisoned"))?.clone();
        let Some(baseline) = baseline else { return Ok(None); };
        Ok(Some(self.checkpoint.capture(&self.objects, &self.repositories, &baseline.checkpoint.session_id, super::supervisor::now_ms()).await?))
    }

    /// Restores the exact task baseline without substituting the Git index or HEAD.
    pub(super) fn open(
        workspace: &Path,
        root: &Path,
        objects: ObjectStore,
        repositories: Arc<forge_git::store::RepositoryStore>,
    ) -> Result<Self> {
        let path = root.join("change-baseline.json");
        let baseline = match std::fs::read(&path) {
            Ok(source) => Some(serde_json::from_slice(&source)?),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
            Err(error) => return Err(error.into()),
        };
        Ok(Self {
            path,
            checkpoint: GitCheckpoint::new(workspace),
            objects,
            repositories,
            baseline: Mutex::new(baseline),
            current: Mutex::new(None),
        })
    }

    /// Retains the first checkpoint across continuations of the same task.
    pub(super) fn begin(&self, owner: String, checkpoint: &CheckpointRecord) -> Result<()> {
        let mut baseline = self
            .baseline
            .lock()
            .map_err(|_| anyhow::anyhow!("check baseline lock poisoned"))?;
        if baseline
            .as_ref()
            .is_some_and(|baseline| baseline.owner == owner)
        {
            return Ok(());
        }
        let next = CheckBaseline {
            owner,
            checkpoint: checkpoint.clone(),
        };
        let temporary = self.path.with_extension("tmp");
        std::fs::write(&temporary, serde_json::to_vec(&next)?)?;
        std::fs::rename(temporary, &self.path)?;
        *baseline = Some(next);
        *self
            .current
            .lock()
            .map_err(|_| anyhow::anyhow!("check changes lock poisoned"))? = Some(Vec::new());
        Ok(())
    }

    /// Refreshes path identities through the shared checkpoint acquisition boundary.
    pub(super) async fn refresh(&self) -> Result<()> {
        let baseline = self
            .baseline
            .lock()
            .map_err(|_| anyhow::anyhow!("check baseline lock poisoned"))?
            .clone();
        let Some(baseline) = baseline else {
            return Ok(());
        };
        let current = self
            .checkpoint
            .capture(
                &self.objects,
                &self.repositories,
                &baseline.checkpoint.session_id,
                super::supervisor::now_ms(),
            )
            .await?;
        let files = baseline.checkpoint.changed_paths(&current)?;
        let active = self
            .baseline
            .lock()
            .map_err(|_| anyhow::anyhow!("check baseline lock poisoned"))?;
        if active
            .as_ref()
            .is_some_and(|active| active.owner == baseline.owner)
        {
            *self
                .current
                .lock()
                .map_err(|_| anyhow::anyhow!("check changes lock poisoned"))? = Some(files);
        }
        Ok(())
    }

    /// Returns the latest task-relative paths, or no context before the first checkpoint.
    pub(super) fn files(&self) -> Result<Option<Vec<String>>> {
        Ok(self
            .current
            .lock()
            .map_err(|_| anyhow::anyhow!("check changes lock poisoned"))?
            .clone())
    }
}

#[derive(Clone, Deserialize, Serialize)]
struct CheckBaseline {
    owner: String,
    checkpoint: CheckpointRecord,
}
