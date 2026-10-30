use anyhow::{Context, Result, ensure};
use forge_buffer::{identity::DocumentId, patch::BufferPatch};
use forge_diff::view::{DiffViewAction, DiffViewOptions};

use super::{
    ReviewSectionKind, ReviewService,
    file::{FileViewChange, file_patch, prepare_file_view},
};

pub(super) struct DiffViewDelivery {
    pub patch: Vec<BufferPatch>,
    pub options: Option<DiffViewOptions>,
}

impl ReviewService {
    pub(super) async fn change_diff_view(
        &self,
        id: &DocumentId,
        action: DiffViewAction,
    ) -> Result<DiffViewDelivery> {
        let owner = self.owner(id)?;
        let pool = self
            .analysis
            .clone()
            .context("review analysis service is unavailable")?;
        let admission = self
            .job_admission
            .clone()
            .try_acquire_owned()
            .context("review diff admission is full")?;
        let request = owner
            .document
            .lock()
            .expect("review document poisoned")
            .diff_view
            .request(action)
            .map_err(|error| anyhow::anyhow!("review diff control failed: {error:?}"))?;
        let rollback = owner.clone();
        let (sender, receiver) = tokio::sync::oneshot::channel();
        let spawned = self.spawn(async move {
            let _admission = admission;
            let _projection = owner.section_projection.lock().await;
            let result = async {
                loop {
                    let (revision, files, commits) = {
                        let document = owner.document.lock().expect("review document poisoned");
                        if document.diff_view.requested() != request {
                            return Ok(DiffViewDelivery {
                                patch: Vec::new(),
                                options: None,
                            });
                        }
                        if document.diff_view.committed() == request.options {
                            return Ok(DiffViewDelivery {
                                patch: Vec::new(),
                                options: Some(request.options),
                            });
                        }
                        let comparison = document
                            .section
                            .get(&ReviewSectionKind::Overview)
                            .and_then(|section| section.item.first())
                            .and_then(|item| item.comparison.as_ref());
                        (
                            document.section_revision,
                            document
                                .file
                                .values()
                                .filter(|file| comparison == Some(&file.identity))
                                .cloned()
                                .collect::<Vec<_>>(),
                            document.commit_view.clone(),
                        )
                    };
                    let mut prepared = Vec::with_capacity(files.len());
                    for file in files {
                        prepared.push(
                            prepare_file_view(
                                pool.clone(),
                                file,
                                FileViewChange::Options(request.options),
                            )
                            .await?,
                        );
                    }
                    let mut prepared_commit = Vec::with_capacity(commits.len());
                    for (commit, view) in commits {
                        prepared_commit.push((
                            commit,
                            super::commit::prepare_commit_view(pool.clone(), view, request.options)
                                .await?,
                        ));
                    }
                    let mut document = owner.document.lock().expect("review document poisoned");
                    if document.diff_view.requested() != request {
                        return Ok(DiffViewDelivery {
                            patch: Vec::new(),
                            options: None,
                        });
                    }
                    if document.section_revision != revision {
                        continue;
                    }
                    let next_revision = revision
                        .checked_add(1)
                        .context("review section revision exhausted")?;
                    let mut projection = document.projection.clone();
                    let mut patch = Vec::new();
                    for file in &mut prepared {
                        let previous = document
                            .file
                            .get(&file.summary.path)
                            .context("review file disappeared")?;
                        ensure!(
                            previous.identity == file.identity,
                            "review source changed during control preparation"
                        );
                        if let Some(projection) = projection.as_mut()
                            && let Some(changed) =
                                file_patch(projection, file, previous.projected_blocks)?
                        {
                            patch.push(changed);
                        }
                    }
                    for (commit, view) in &mut prepared_commit {
                        let previous = document
                            .commit_view
                            .get(commit)
                            .context("commit diff disappeared")?;
                        if let Some(projection) = projection.as_mut()
                            && let Some(changed) = super::commit::commit_patch(
                                projection,
                                view,
                                previous.projected_blocks,
                            )?
                        {
                            patch.push(changed);
                        }
                    }
                    for (commit, view) in prepared_commit {
                        if let Some(previous) = document.commit_view.get(&commit) {
                            let targets = previous.target.keys().cloned().collect::<Vec<_>>();
                            for target in targets {
                                document.projection_target.remove(&target);
                            }
                        }
                        document.projection_target.extend(
                            view.target
                                .iter()
                                .map(|(id, target)| (id.clone(), target.clone())),
                        );
                        document.commit_view.insert(commit, view);
                    }
                    for file in prepared {
                        if let Some(previous) = document.file.get(&file.summary.path) {
                            let previous = previous.target.keys().cloned().collect::<Vec<_>>();
                            for target in previous {
                                document.projection_target.remove(&target);
                            }
                        }
                        document.projection_target.extend(
                            file.target
                                .iter()
                                .map(|(id, target)| (id.clone(), target.clone())),
                        );
                        document.file.insert(file.summary.path.clone(), file);
                    }
                    document.projection = projection;
                    document.section_revision = next_revision;
                    ensure!(
                        document.diff_view.commit(request),
                        "review diff request was superseded"
                    );
                    return Ok(DiffViewDelivery {
                        patch,
                        options: Some(request.options),
                    });
                }
            }
            .await;
            if result.is_err() {
                owner
                    .document
                    .lock()
                    .expect("review document poisoned")
                    .diff_view
                    .fail(request);
            }
            let _ = sender.send(result);
        });
        if let Err(failure) = spawned {
            rollback
                .document
                .lock()
                .expect("review document poisoned")
                .diff_view
                .fail(request);
            return Err(failure);
        }
        receiver.await.context("review diff control task ended")?
    }
}
