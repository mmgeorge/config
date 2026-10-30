use std::{path::PathBuf, sync::Arc};

use anyhow::{Context, Result, ensure};
use forge_buffer::{
    block::{BlockMetadata, BufferBlock, TargetRange, TextPosition, TextRange},
    identity::{BlockId, DocumentId, TargetId},
    text::BufferText,
};
use forge_diff::{
    cache::AnalysisHandle,
    engine::DiffRequest,
    source::{Representation, SourcePair, SourceVersion},
    syntax::{SyntaxHandle, SyntaxLanguage, SyntaxRequest},
    workers::{WorkBudget, WorkPriority},
};
use forge_github::{
    model::GithubRepositoryId,
    review_api::GithubReviewRemote,
    review_source::{ReviewComparisonRequest, ReviewSourceRequest},
};
use serde::Serialize;

use super::{ReviewComparisonIdentity, ReviewSectionKind, ReviewService};

#[derive(Clone, Debug, Serialize)]
pub struct ReviewFileAnalysis {
    pub path: String,
    pub base_commit: String,
    pub head_commit: String,
    pub merge_base: String,
    pub old_blob: Option<String>,
    pub new_blob: Option<String>,
    pub syntax_diagnostic: Option<String>,
    pub more: bool,
}

#[derive(Debug, Serialize)]
pub struct ReviewFileDelivery {
    #[serde(flatten)]
    pub file: ReviewFileAnalysis,
    pub patch: Option<forge_buffer::patch::BufferPatch>,
}

impl std::ops::Deref for ReviewFileDelivery {
    type Target = ReviewFileAnalysis;
    fn deref(&self) -> &Self::Target {
        &self.file
    }
}

#[derive(Clone)]
pub(crate) struct RetainedFile {
    pub commit: Option<String>,
    pub identity: ReviewComparisonIdentity,
    pub summary: ReviewFileAnalysis,
    pub diff: AnalysisHandle,
    pub old_syntax: Option<SyntaxHandle>,
    pub new_syntax: Option<SyntaxHandle>,
    pub block: Vec<BufferBlock>,
    pub target: std::collections::BTreeMap<TargetId, super::thread_projection::ReviewTarget>,
    pub source_projection: Option<forge_diff::source_projection::SourceDiffProjection>,
    pub view_revision: u64,
    pub projected_blocks: usize,
}

pub(super) enum FileViewChange {
    Continuation,
    Declaration { identity: String, expanded: bool },
    Options(forge_diff::view::DiffViewOptions),
}

pub(super) async fn prepare_file_view(
    pool: Arc<forge_diff::workers::AnalysisPool>,
    mut file: RetainedFile,
    change: FileViewChange,
) -> Result<RetainedFile> {
    let permit = pool
        .reserve(WorkPriority::Foreground, WorkBudget::new(512 * 1024, None))
        .map_err(|error| anyhow::anyhow!("PR source projection admission failed: {error:?}"))?;
    let ticket = permit.ticket();
    let (sender, receiver) = tokio::sync::oneshot::channel();
    permit.submit(move |_| {
        let result = (|| {
            match change {
                FileViewChange::Continuation => {
                    ensure!(file.summary.more, "review file has no continuation");
                    project_next(&mut file)?;
                }
                FileViewChange::Declaration { identity, expanded } => {
                    file.source_projection
                        .as_mut()
                        .context("source projection is missing")?
                        .declaration(
                            &identity,
                            expanded,
                            forge_diff::display::ChunkLimits {
                                rows: 128,
                                ..Default::default()
                            },
                        )?;
                    project_current(&mut file)?;
                }
                FileViewChange::Options(options) => {
                    let mut projection = file
                        .source_projection
                        .as_ref()
                        .context("source projection is missing")?
                        .reconfigure(options)?;
                    if !projection.materialized() {
                        projection.demand(forge_diff::display::ChunkLimits {
                            rows: 128,
                            ..Default::default()
                        })?;
                    }
                    file.source_projection = Some(projection);
                    project_current(&mut file)?;
                }
            }
            Ok::<_, anyhow::Error>(file)
        })();
        let _ = sender.send(result);
    });
    let result = receiver.await.context("PR source projection worker ended");
    ticket.completed().await;
    result?
}

pub(super) fn publish_file(
    document: &mut crate::review::ReviewDocument,
    mut file: RetainedFile,
) -> Result<ReviewFileDelivery> {
    let current = document
        .section
        .get(&ReviewSectionKind::Overview)
        .and_then(|section| section.item.first())
        .and_then(|item| item.comparison.as_ref());
    ensure!(
        current == Some(&file.identity),
        "PR comparison changed during source projection"
    );
    let path = file.summary.path.clone();
    let previous = document
        .file
        .get(&path)
        .map_or(0, |file| file.projected_blocks);
    let patch = match document.projection.as_mut() {
        Some(projection) => file_patch(projection, &mut file, previous)?,
        None => None,
    };
    if let Some(previous) = document.file.get(&path) {
        for target in previous.target.keys() {
            document.projection_target.remove(target);
        }
    }
    document.projection_target.extend(
        file.target
            .iter()
            .map(|(id, target)| (id.clone(), target.clone())),
    );
    let summary = file.summary.clone();
    document.file.insert(path, file);
    document.section_revision = document
        .section_revision
        .checked_add(1)
        .context("review section revision exhausted")?;
    Ok(ReviewFileDelivery {
        file: summary,
        patch,
    })
}

pub(super) fn file_patch(
    projection: &mut forge_buffer::document::BufferDocument,
    file: &mut RetainedFile,
    previous: usize,
) -> Result<Option<forge_buffer::patch::BufferPatch>> {
    file.projected_blocks = 0;
    let path = &file.summary.path;
    let Some(anchor) = projection.block_index(&BlockId(format!("{}:title", section_prefix(path))))
    else {
        return Ok(None);
    };
    let mut changed = file.block.clone();
    if file.summary.more {
        changed.push(continuation_block(path)?);
    }
    file.projected_blocks = changed.len();
    Ok(projection.splice(anchor + 1..anchor + 1 + previous, changed)?)
}

impl ReviewService {
    pub async fn read_file_more(
        &self,
        id: &DocumentId,
        path: String,
    ) -> Result<ReviewFileDelivery> {
        self.change_file_view(id, path, FileViewChange::Continuation)
            .await
    }

    pub(super) async fn change_file_view(
        &self,
        id: &DocumentId,
        path: String,
        change: FileViewChange,
    ) -> Result<ReviewFileDelivery> {
        let owner = self.owner(id)?;
        let pool = self
            .analysis
            .clone()
            .context("review analysis service is unavailable")?;
        let admission = self
            .job_admission
            .clone()
            .try_acquire_owned()
            .context("review file admission is full")?;
        let (sender, receiver) = tokio::sync::oneshot::channel();
        self.spawn(async move {
            let _admission = admission;
            let _projection = owner.section_projection.lock().await;
            let result = async {
                let (work, revision) = {
                    let document = owner.document.lock().expect("review document poisoned");
                    ensure!(
                        !document.file_loading.contains(&path),
                        "review file acquisition is already running"
                    );
                    let file = document
                        .file
                        .get(&path)
                        .context("review file is not acquired")?;
                    (file.clone(), file.view_revision)
                };
                let work = prepare_file_view(pool, work, change).await?;
                let mut document = owner.document.lock().expect("review document poisoned");
                ensure!(
                    document
                        .file
                        .get(&path)
                        .is_some_and(|file| file.view_revision == revision),
                    "review source presentation was superseded"
                );
                publish_file(&mut document, work)
            }
            .await;
            let _ = sender.send(result);
        })?;
        receiver
            .await
            .context("review source projection task ended")?
    }

    pub async fn read_file(
        &self,
        id: &DocumentId,
        directory: PathBuf,
        remote: Arc<dyn GithubReviewRemote>,
        path: String,
    ) -> Result<ReviewFileDelivery> {
        let owner = self.owner(id)?;
        owner.validate_directory(&directory)?;
        let diff = self
            .diff
            .clone()
            .context("review diff service is unavailable")?;
        let syntax = self
            .syntax
            .clone()
            .context("review syntax service is unavailable")?;
        let admission = self
            .job_admission
            .clone()
            .try_acquire_owned()
            .context("review file admission is full")?;
        let (repository, identity, source) = {
            let mut document = owner.document.lock().expect("review document poisoned");
            ensure!(
                !document.file_loading.contains(&path),
                "review file acquisition is already running"
            );
            let identity = document
                .section
                .get(&ReviewSectionKind::Overview)
                .and_then(|section| section.item.first())
                .and_then(|item| item.comparison.clone())
                .context("PR overview omitted immutable comparison identity")?;
            let source = document
                .section
                .get(&ReviewSectionKind::Files)
                .and_then(|section| section.item.iter().find(|item| item.identity == path))
                .and_then(|item| item.source.clone())
                .context("file is not in the observed PR page")?;
            document.file_loading.insert(path.clone());
            (document.target.repository.clone(), identity, source)
        };
        let service = self.clone();
        let rollback = owner.clone();
        let rollback_path = path.clone();
        let (sender, receiver) = tokio::sync::oneshot::channel();
        let spawned = self.spawn(async move {
            let _admission = admission;
            let observed: Result<RetainedFile> = async {
                let comparison = service
                    .github
                    .review_comparison(
                        directory.clone(),
                        remote.clone(),
                        ReviewComparisonRequest {
                            repository: repository.clone(),
                            base: identity.base_commit.clone(),
                            head: identity.head_commit.clone(),
                        },
                    )
                    .await?;
                let (head_owner, head_name) = identity
                    .head_repository
                    .split_once('/')
                    .context("head repository identity is incomplete")?;
                let head_repository =
                    GithubRepositoryId::new(repository.hostname(), head_owner, head_name)
                        .map_err(anyhow::Error::msg)?;
                let old = service
                    .github
                    .review_source(
                        directory.clone(),
                        remote.clone(),
                        ReviewSourceRequest {
                            repository,
                            commit: comparison.merge_base.clone(),
                            path: source.previous_path.clone().unwrap_or_else(|| path.clone()),
                        },
                    )
                    .await?;
                let new = service
                    .github
                    .review_source(
                        directory,
                        remote,
                        ReviewSourceRequest {
                            repository: head_repository,
                            commit: identity.head_commit.clone(),
                            path: path.clone(),
                        },
                    )
                    .await?;
                ensure!(
                    old.blob.is_none() == (source.status == "added"),
                    "old PR file presence disagrees with the captured file status"
                );
                ensure!(
                    new.blob.is_none() == (source.status == "removed"),
                    "new PR file presence disagrees with the captured file status"
                );
                ensure!(
                    new.blob.as_ref().is_none_or(|blob| blob == &source.blob),
                    "PR file page belongs to a different head blob"
                );
                let summary = ReviewFileAnalysis {
                    path: path.clone(),
                    base_commit: comparison.base,
                    head_commit: comparison.head,
                    merge_base: comparison.merge_base,
                    old_blob: old.blob,
                    new_blob: new.blob,
                    syntax_diagnostic: None,
                    more: false,
                };
                analyze_file(
                    &diff, &syntax, identity, summary, old.bytes, new.bytes, None,
                )
                .await
            }
            .await;
            let _projection = owner.section_projection.lock().await;
            let result = async {
                let mut observed = observed?;
                let options = owner
                    .document
                    .lock()
                    .expect("review document poisoned")
                    .diff_view
                    .committed();
                if observed
                    .source_projection
                    .as_ref()
                    .is_some_and(|projection| projection.options() != options)
                {
                    observed = prepare_file_view(
                        diff.analysis_pool(),
                        observed,
                        FileViewChange::Options(options),
                    )
                    .await?;
                }
                let mut document = owner.document.lock().expect("review document poisoned");
                document.file_loading.remove(&path);
                publish_file(&mut document, observed)
            }
            .await;
            if result.is_err() {
                owner
                    .document
                    .lock()
                    .expect("review document poisoned")
                    .file_loading
                    .remove(&path);
            }
            let _ = sender.send(result);
        });
        if let Err(failure) = spawned {
            rollback
                .document
                .lock()
                .expect("review document poisoned")
                .file_loading
                .remove(&rollback_path);
            return Err(failure);
        }
        receiver.await.context("review file worker ended")?
    }
}

pub(super) async fn analyze_file(
    diff: &Arc<forge_diff::engine::DiffEngine>,
    syntax: &Arc<forge_diff::syntax::SyntaxEngine>,
    identity: ReviewComparisonIdentity,
    summary: ReviewFileAnalysis,
    old_bytes: Vec<u8>,
    new_bytes: Vec<u8>,
    commit: Option<String>,
) -> Result<RetainedFile> {
    let path = summary.path.clone();
    let pool = diff.analysis_pool();
    let permit = pool
        .reserve(
            WorkPriority::Foreground,
            WorkBudget::new(old_bytes.capacity() + new_bytes.capacity(), None),
        )
        .map_err(|error| anyhow::anyhow!("source validation admission failed: {error:?}"))?;
    let ticket = permit.ticket();
    let (sender, receiver) = tokio::sync::oneshot::channel();
    permit.submit(move |_| {
        let pair = (|| {
            Ok::<_, anyhow::Error>(SourcePair {
                old: SourceVersion::new(old_bytes, Representation::GitCanonical)?,
                new: SourceVersion::new(new_bytes, Representation::GitCanonical)?,
            })
        })();
        let _ = sender.send(pair);
    });
    let source = receiver.await.context("source validation worker ended");
    ticket.completed().await;
    let source = source??;
    let analysis = diff
        .compare(DiffRequest {
            source: source.clone(),
            priority: WorkPriority::Foreground,
        })
        .await
        .map_err(|error| anyhow::anyhow!("native PR diff failed: {error:?}"))?;
    let mut result = RetainedFile {
        commit,
        identity,
        summary,
        diff: analysis,
        old_syntax: None,
        new_syntax: None,
        block: Vec::new(),
        target: std::collections::BTreeMap::new(),
        source_projection: None,
        view_revision: 0,
        projected_blocks: 0,
    };
    if let Some(language) = SyntaxLanguage::for_path(&path) {
        for (source, target) in [
            (source.old, &mut result.old_syntax),
            (source.new, &mut result.new_syntax),
        ] {
            match syntax
                .analyze(SyntaxRequest {
                    source,
                    language,
                    priority: WorkPriority::Foreground,
                    deadline: None,
                })
                .await
            {
                Ok(handle) => *target = Some(handle),
                Err(error) => {
                    result.summary.syntax_diagnostic =
                        Some(format!("native syntax unavailable: {error:?}"))
                }
            }
        }
    }
    let permit = pool
        .reserve(WorkPriority::Foreground, WorkBudget::new(512 * 1024, None))
        .map_err(|error| anyhow::anyhow!("PR row projection admission failed: {error:?}"))?;
    let ticket = permit.ticket();
    let (sender, receiver) = tokio::sync::oneshot::channel();
    permit.submit(move |_| {
        let projected = project_next(&mut result).map(|()| result);
        let _ = sender.send(projected);
    });
    let projected = receiver.await.context("PR row projection worker ended");
    ticket.completed().await;
    let result = projected??;
    Ok(result)
}

pub(crate) fn section_prefix(path: &str) -> String {
    use std::hash::{Hash, Hasher};
    let mut hash = std::hash::DefaultHasher::new();
    path.hash(&mut hash);
    format!("section:Files:{:016x}", hash.finish())
}

pub(crate) fn continuation_block(path: &str) -> Result<BufferBlock> {
    Ok(BufferBlock {
        id: BlockId(format!("{}:more", section_prefix(path))),
        text: BufferText::from_rows(vec!["Additional diff rows are available"])?,
        metadata: BlockMetadata {
            target: vec![TargetRange {
                id: TargetId(format!("{}:more", section_prefix(path))),
                range: TextRange {
                    start: TextPosition { row: 0, column: 0 },
                    end: TextPosition { row: 1, column: 0 },
                },
            }],
            ..Default::default()
        },
    })
}

fn project_next(file: &mut RetainedFile) -> Result<()> {
    if file.source_projection.is_none() {
        file.source_projection = Some(forge_diff::source_projection::SourceDiffProjection::new(
            file.diff.clone(),
            file.old_syntax.clone(),
            file.new_syntax.clone(),
            Default::default(),
        ));
    }
    file.source_projection
        .as_mut()
        .expect("initialized source projection")
        .demand(forge_diff::display::ChunkLimits {
            rows: 128,
            ..Default::default()
        })?;
    project_current(file)
}

pub(super) fn project_current(file: &mut RetainedFile) -> Result<()> {
    use forge_diff::source_projection::SourceDiffElement;
    use std::hash::{Hash, Hasher};
    let revision = file.identity.head_commit.clone();
    let path = file.summary.path.clone();
    let projection = file
        .source_projection
        .as_ref()
        .context("source projection is missing")?;
    let mut hash = std::hash::DefaultHasher::new();
    file.summary.path.hash(&mut hash);
    let prefix = match &file.commit {
        Some(commit) => format!("commit:{commit}:file:{:016x}", hash.finish()),
        None => format!("file:{:016x}", hash.finish()),
    };
    let mut target = std::collections::BTreeMap::new();
    let block = projection.render(&prefix, |element, block| {
        let action = match element {
            SourceDiffElement::Hunk(_) => None,
            SourceDiffElement::Row(display) => file
                .commit
                .is_none()
                .then(|| inline_anchor(&revision, &path, display))
                .flatten()
                .map(|anchor| super::thread_projection::ReviewTarget::InlineComment { anchor }),
            SourceDiffElement::Declaration(signature) => Some(match &file.commit {
                Some(commit) => super::thread_projection::ReviewTarget::CommitSource {
                    commit: commit.clone(),
                    path: path.clone(),
                    declaration: Some(signature.identity.clone()),
                },
                None => super::thread_projection::ReviewTarget::SourceSection {
                    path: path.clone(),
                    identity: signature.identity.clone(),
                },
            }),
        };
        if let Some(action) = action {
            let id = TargetId(block.id.0.clone());
            if target.insert(id.clone(), action).is_some() {
                return Err(forge_buffer::ContractError(
                    "review source target identity collision",
                ));
            }
            block.metadata.target.push(TargetRange {
                id,
                range: TextRange {
                    start: TextPosition { row: 0, column: 0 },
                    end: TextPosition {
                        row: block.text.row_count(),
                        column: 0,
                    },
                },
            });
        }
        Ok(())
    })?;
    file.summary.more = projection.more();
    if file.summary.more && file.commit.is_none() {
        target.insert(
            TargetId(format!("{}:more", section_prefix(&path))),
            super::thread_projection::ReviewTarget::FileMore { path },
        );
    }
    file.view_revision = file
        .view_revision
        .checked_add(1)
        .context("review source presentation revision exhausted")?;
    file.block = block;
    file.target = target;
    Ok(())
}

fn inline_anchor(
    revision: &str,
    path: &str,
    display: &forge_diff::display::DisplayRow,
) -> Option<crate::comments::CommentAnchor> {
    use crate::comments::CommentSide;
    use forge_diff::display::{DisplaySource, RowCoordinates};
    let (line, side) = match display.source {
        DisplaySource::Diff(RowCoordinates::Added { new_line }) => (new_line, CommentSide::Right),
        DisplaySource::Diff(RowCoordinates::Removed { old_line }) => (old_line, CommentSide::Left),
        DisplaySource::Diff(RowCoordinates::Context { .. })
        | DisplaySource::Context(_)
        | DisplaySource::Gap => return None,
    };
    Some(crate::comments::CommentAnchor {
        revision: revision.to_owned(),
        path: path.to_owned(),
        side,
        first_line: (line + 1) as u64,
        last_line: (line + 1) as u64,
    })
}
