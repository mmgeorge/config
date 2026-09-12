use std::{
    sync::Arc,
    time::{Duration, Instant},
};

use anyhow::{Result, ensure};
use forge_diff::cache::{AnalysisHandle, AnalysisKey};

use crate::{
    content::{ContentOrigin, ConversionIdentity},
    repository::RepositoryState,
    store::RepositoryStore,
};

/// Associates a completed snapshot with an evictable analysis, without retaining source bytes.
#[derive(Clone, Debug)]
pub struct PreparedAnalysis {
    pub(crate) generation: Option<crate::repository::RepositoryGeneration>,
    key: AnalysisKey,
    worktree: Option<WorktreeProof>,
}

#[derive(Clone, Debug)]
struct WorktreeProof {
    worktree: crate::WorktreeId,
    path: crate::RepositoryPath,
    stamp: super::WorktreeStamp,
    conversion: Option<Arc<ConversionIdentity>>,
}

impl PreparedAnalysis {
    pub(crate) fn new(key: AnalysisKey, origin: ContentOrigin) -> Self {
        let worktree = match origin {
            ContentOrigin::Worktree {
                worktree,
                path,
                stamp,
                conversion,
            } => Some(WorktreeProof {
                worktree,
                path,
                stamp,
                conversion,
            }),
            _ => None,
        };
        Self {
            key,
            worktree,
            generation: None,
        }
    }

    pub fn retained_bytes(&self) -> usize {
        std::mem::size_of::<Self>()
            + self.worktree.as_ref().map_or(0, |proof| {
                proof.path.retained_bytes()
                    + proof.worktree.retained_bytes()
                    + match &proof.stamp {
                        super::WorktreeStamp::Symlink { target, .. } => target.capacity(),
                        _ => 0,
                    }
                    + proof.conversion.as_ref().map_or(0, |conversion| {
                        std::mem::size_of::<ConversionIdentity>()
                            + conversion
                                .index
                                .index
                                .as_ref()
                                .map_or(0, |entry| entry.name.capacity())
                            + conversion.index.shared.capacity()
                                * std::mem::size_of::<super::IndexFileStamp>()
                            + conversion
                                .index
                                .shared
                                .iter()
                                .map(|entry| entry.name.capacity())
                                .sum::<usize>()
                    })
            })
    }

    /// Returns a retained analysis only while its worktree metadata and conversion inputs match.
    /// Eviction, prior invalidation, or changed inputs return `None`. I/O failures and
    /// invalidation during validation remain errors. Validation has a 30-second cooperative deadline.
    pub async fn load(
        &self,
        repository: &Arc<RepositoryState>,
        store: &RepositoryStore,
    ) -> Result<Option<AnalysisHandle>> {
        if self
            .generation
            .is_some_and(|generation| generation != repository.generation())
        {
            return Ok(None);
        }
        let Some(analysis) = store.analysis.get(self.key) else {
            return Ok(None);
        };
        let Some(proof) = self.worktree.clone() else {
            return Ok(Some(analysis));
        };
        let identity = repository.identity.clone();
        let result = store
            .read(
                Arc::clone(repository),
                self.retained_bytes(),
                move |mut local, cancellation| {
                    let started = Instant::now();
                    let mut check = || {
                        cancellation.check()?;
                        ensure!(
                            started.elapsed() < Duration::from_secs(30),
                            "analysis validation exceeded 30-second deadline"
                        );
                        Ok(())
                    };
                    ensure!(
                        identity.worktree.as_ref() == Some(&proof.worktree),
                        "cached analysis belongs to another worktree"
                    );
                    let root = identity
                        .worktree_root
                        .as_ref()
                        .expect("worktree proof has a root");
                    check()?;
                    if super::read_worktree_stamp(root, &proof.path)? != proof.stamp {
                        return Ok(false);
                    }
                    if let Some(conversion) = proof.conversion
                        && !crate::content::verify_conversion(
                            &mut local,
                            &identity,
                            &proof.path,
                            &conversion,
                            &mut check,
                        )?
                    {
                        return Ok(false);
                    }
                    check()?;
                    Ok(super::read_worktree_stamp(root, &proof.path)? == proof.stamp)
                },
            )
            .await?;
        Ok(result.value.then_some(analysis))
    }
}
