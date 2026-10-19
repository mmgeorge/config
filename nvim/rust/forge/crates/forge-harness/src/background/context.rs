use anyhow::{Context, Result, ensure};
use forge_git::store::RepositoryStore;
use serde::{Deserialize, Serialize};
use std::{path::PathBuf, sync::Arc};

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Pins repository identity and complete commit-message examples for one request.
pub struct BackgroundContext {
    /// Canonical worktree root, independent of the caller's subdirectory.
    pub workspace: String,
    /// Full symbolic branch reference, absent for detached HEAD.
    pub branch: Option<String>,
    /// Commit captured before collecting the diff.
    pub head: Option<String>,
    /// Up to ten complete commit messages, each prefixed by its object ID.
    pub history: String,
}

impl BackgroundContext {
    /// Read committed examples without creating a Harness or provider session.
    pub async fn read(store: &RepositoryStore, workspace: &str) -> Result<Self> {
        let repository = store
            .open(PathBuf::from(workspace))
            .await?
            .context("Background generation requires a Git worktree")?;
        store
            .read(repository, 0, |local, cancellation| {
                let head = local.head().context("Read Background HEAD")?;
                let branch = head
                    .referent_name()
                    .map(|name| {
                        String::from_utf8(name.as_bstr().to_vec())
                            .context("Decode Background branch reference as UTF-8")
                    })
                    .transpose()?;
                let object = head.id().map(|object| object.detach());
                let mut history = String::new();
                if let Some(object) = object {
                    for commit in forge_git::history::recent(&local, object, 10, &mut || {
                        cancellation.check()
                    })? {
                        history.push_str(&format!("Commit {}\n{}\n\n", commit.id, commit.message));
                    }
                }
                Ok(Self {
                    workspace: local
                        .workdir()
                        .context("Background generation requires a worktree")?
                        .to_string_lossy()
                        .into_owned(),
                    branch,
                    head: object.map(|object| object.to_string()),
                    history,
                })
            })
            .await
            .map(|read| read.value)
    }

    /// Reject a changed HEAD rather than pairing history with another revision's diff.
    pub async fn verify(&self, store: &RepositoryStore) -> Result<()> {
        let current = Self::read(store, &self.workspace).await?;
        ensure!(
            current.head == self.head && current.branch == self.branch,
            "repository changed during commit comparison"
        );
        Ok(())
    }

    /// Stream ancestry until the saved revision is found, without retaining a graph-sized list.
    pub(crate) async fn follows(
        &self,
        store: &RepositoryStore,
        previous: Option<&str>,
    ) -> Result<bool> {
        let Some(previous) = previous else {
            return Ok(true);
        };
        let Some(current) = self.head.clone() else {
            return Ok(false);
        };
        if previous == current {
            return Ok(true);
        }
        let previous = previous.to_owned();
        let repository = store
            .open(PathBuf::from(&self.workspace))
            .await?
            .context("Background worktree disappeared")?;
        store
            .read(Arc::clone(&repository), 0, move |local, cancellation| {
                let previous = local
                    .rev_parse_single(format!("{previous}^{{commit}}").as_str())
                    .context("Read previous Background revision")?
                    .detach();
                let commit = local
                    .rev_parse_single(format!("{current}^{{commit}}").as_str())?
                    .object()?
                    .try_into_commit()?;
                for entry in commit.ancestors().all()? {
                    cancellation.check()?;
                    if entry?.id == previous {
                        return Ok(true);
                    }
                }
                Ok(false)
            })
            .await
            .map(|read| read.value)
    }
}
