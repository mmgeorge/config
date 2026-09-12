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

pub(crate) struct RetainedFile {
    pub identity: ReviewComparisonIdentity,
    pub summary: ReviewFileAnalysis,
    pub diff: AnalysisHandle,
    pub old_syntax: Option<SyntaxHandle>,
    pub new_syntax: Option<SyntaxHandle>,
    pub block: Vec<BufferBlock>,
    pub target: std::collections::BTreeMap<TargetId, super::thread_projection::ReviewTarget>,
    pub cursor: Option<forge_diff::display::DisplayCursor>,
    pub group: Option<usize>,
    pub next_block: usize,
    pub projected_blocks: usize,
}

impl ReviewService {
    pub async fn read_file_more(
        &self,
        id: &DocumentId,
        path: String,
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
        let permit = pool
            .reserve(WorkPriority::Foreground, WorkBudget::new(512 * 1024, None))
            .map_err(|error| anyhow::anyhow!("PR row projection admission failed: {error:?}"))?;
        let mut work = {
            let mut document = owner.document.lock().expect("review document poisoned");
            ensure!(
                !document.file_loading.contains(&path),
                "review file acquisition is already running"
            );
            let bytes: usize = document.file.values().map(retained_bytes).sum();
            ensure!(
                bytes + 1536 * 1024 <= 16 * 1024 * 1024,
                "review document has no remaining diff row admission"
            );
            let file = document
                .file
                .get_mut(&path)
                .context("review file is not acquired")?;
            ensure!(file.summary.more, "review file has no continuation");
            let work = RetainedFile {
                identity: file.identity.clone(),
                summary: file.summary.clone(),
                diff: file.diff.clone(),
                old_syntax: file.old_syntax.clone(),
                new_syntax: file.new_syntax.clone(),
                block: Vec::new(),
                target: std::collections::BTreeMap::new(),
                cursor: file.cursor.take(),
                group: file.group,
                next_block: file.next_block,
                projected_blocks: file.projected_blocks,
            };
            ensure!(
                work.cursor.is_some(),
                "review file continuation is unavailable"
            );
            document.file_loading.insert(path.clone());
            work
        };
        let ticket = permit.ticket();
        let (worker_sender, worker_receiver) = tokio::sync::oneshot::channel();
        permit.submit(move |_| {
            let result = project_next(&mut work).map(|()| work);
            let _ = worker_sender.send(result);
        });
        let rollback = owner.clone();
        let rollback_path = path.clone();
        let (sender, receiver) = tokio::sync::oneshot::channel();
        let spawned = self.spawn(async move {
            let _admission = admission;
            let result = worker_receiver
                .await
                .context("PR continuation worker ended");
            ticket.completed().await;
            let result = (|| {
                let mut document = owner.document.lock().expect("review document poisoned");
                document.file_loading.remove(&path);
                let result = result.and_then(|result| result);
                let work = match result {
                    Ok(work) => work,
                    Err(failure) => {
                        if let Some(file) = document.file.get_mut(&path) {
                            file.summary.more = false;
                            file.summary.syntax_diagnostic = Some(failure.to_string());
                        }
                        return Err(failure);
                    }
                };
                let current = document
                    .section
                    .get(&ReviewSectionKind::Overview)
                    .and_then(|section| section.item.first())
                    .and_then(|item| item.comparison.as_ref());
                ensure!(
                    current == Some(&work.identity),
                    "PR comparison changed during continuation"
                );
                let file = document
                    .file
                    .get(&path)
                    .context("review file disappeared")?;
                ensure!(
                    file.identity == work.identity
                        && file.next_block == work.next_block - work.block.len(),
                    "review file continuation was superseded"
                );
                let projected_blocks = file.projected_blocks;
                let old_more = file.summary.more;
                let patch = if projected_blocks > 0 {
                    let projection = document
                        .projection
                        .as_mut()
                        .context("review file projection disappeared")?;
                    let start = projection
                        .block_index(&BlockId(format!("{}:title", section_prefix(&path))))
                        .context("review file projection anchor disappeared")?
                        + 1
                        + projected_blocks
                        - usize::from(old_more);
                    let mut changed = work.block.clone();
                    if work.summary.more {
                        changed.push(continuation_block(&path)?);
                    }
                    projection.edit(start..start + usize::from(old_more), changed)?
                } else {
                    None
                };
                document.projection_target.extend(
                    work.target
                        .iter()
                        .map(|(id, target)| (id.clone(), target.clone())),
                );
                let file = document.file.get_mut(&path).expect("validated review file");
                file.summary = work.summary;
                file.cursor = work.cursor;
                file.group = work.group;
                file.next_block = work.next_block;
                if projected_blocks > 0 {
                    file.projected_blocks = projected_blocks - usize::from(old_more)
                        + work.block.len()
                        + usize::from(file.summary.more);
                }
                file.block.extend(work.block);
                file.target.extend(work.target);
                let summary = file.summary.clone();
                document.section_revision = document
                    .section_revision
                    .checked_add(1)
                    .context("review section revision exhausted")?;
                Ok(ReviewFileDelivery {
                    file: summary,
                    patch,
                })
            })();
            let _ = sender.send(result);
        });
        if let Err(failure) = spawned {
            let mut document = rollback.document.lock().expect("review document poisoned");
            document.file_loading.remove(&rollback_path);
            if let Some(file) = document.file.get_mut(&rollback_path) {
                file.summary.more = false;
            }
            return Err(failure);
        }
        receiver.await.context("review continuation task ended")?
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
            ensure!(
                document.file.contains_key(&path)
                    || document.file.len() + document.file_loading.len() < 64,
                "review file retention limit reached"
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
                let pool = diff.analysis_pool();
                let permit = pool
                    .reserve(
                        WorkPriority::Foreground,
                        WorkBudget::new(old.bytes.capacity() + new.bytes.capacity(), None),
                    )
                    .map_err(|error| {
                        anyhow::anyhow!("source validation admission failed: {error:?}")
                    })?;
                let ticket = permit.ticket();
                let (sender, receiver) = tokio::sync::oneshot::channel();
                permit.submit(move |_| {
                    let pair = (|| {
                        Ok::<_, anyhow::Error>(SourcePair {
                            old: SourceVersion::new(old.bytes, Representation::GitCanonical)?,
                            new: SourceVersion::new(new.bytes, Representation::GitCanonical)?,
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
                    identity: identity.clone(),
                    summary,
                    diff: analysis,
                    old_syntax: None,
                    new_syntax: None,
                    block: Vec::new(),
                    target: std::collections::BTreeMap::new(),
                    cursor: None,
                    group: None,
                    next_block: 0,
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
                    .map_err(|error| {
                        anyhow::anyhow!("PR row projection admission failed: {error:?}")
                    })?;
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
            .await;
            let result = (|| {
                let mut document = owner.document.lock().expect("review document poisoned");
                document.file_loading.remove(&path);
                let mut observed = observed?;
                let current = document
                    .section
                    .get(&ReviewSectionKind::Overview)
                    .and_then(|section| section.item.first())
                    .and_then(|item| item.comparison.as_ref());
                ensure!(
                    current == Some(&identity),
                    "PR comparison changed during file acquisition"
                );
                let retained = document
                    .file
                    .iter()
                    .filter(|(existing, _)| **existing != path)
                    .map(|(_, file)| retained_bytes(file))
                    .sum::<usize>();
                ensure!(
                    retained + retained_bytes(&observed) <= 16 * 1024 * 1024,
                    "review document diff row budget is full"
                );
                let previous = document
                    .file
                    .get(&path)
                    .map_or(0, |file| file.projected_blocks);
                let patch = if let Some(projection) = document.projection.as_mut() {
                    if let Some(anchor) =
                        projection.block_index(&BlockId(format!("{}:title", section_prefix(&path))))
                    {
                        let mut changed = observed.block.clone();
                        if observed.summary.more {
                            changed.push(continuation_block(&path)?);
                        }
                        observed.projected_blocks = changed.len();
                        projection.edit(anchor + 1..anchor + 1 + previous, changed)?
                    } else {
                        None
                    }
                } else {
                    None
                };
                let previous_targets = document
                    .file
                    .get(&path)
                    .into_iter()
                    .flat_map(|previous| previous.block.iter())
                    .flat_map(|block| block.metadata.target.iter())
                    .map(|target| target.id.clone())
                    .collect::<Vec<_>>();
                for target in previous_targets {
                    document.projection_target.remove(&target);
                }
                document.projection_target.extend(
                    observed
                        .target
                        .iter()
                        .map(|(id, target)| (id.clone(), target.clone())),
                );
                let summary = observed.summary.clone();
                document.file.insert(path, observed);
                document.section_revision = document
                    .section_revision
                    .checked_add(1)
                    .context("review section revision exhausted")?;
                Ok(ReviewFileDelivery {
                    file: summary,
                    patch,
                })
            })();
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
        metadata: BlockMetadata::default(),
    })
}

fn retained_bytes(file: &RetainedFile) -> usize {
    file.block
        .iter()
        .map(|block| {
            block.text.byte_count()
                + (block.metadata.decoration.len()
                    + block.metadata.visible_decoration.len()
                    + block.metadata.target.len())
                    * 160
                + block.metadata.gutter.len() * 128
        })
        .sum()
}

fn project_next(file: &mut RetainedFile) -> Result<()> {
    use forge_diff::{
        body::BodyKind,
        display::{ChunkLimits, DisplayCursor, DisplayState},
    };
    use std::hash::{Hash, Hasher};
    if file.cursor.is_none() {
        let kind = if file.summary.old_blob.is_none() {
            BodyKind::Added
        } else if file.summary.new_blob.is_none() {
            BodyKind::Deleted
        } else {
            BodyKind::Modified
        };
        file.cursor = Some(DisplayCursor::new(file.diff.clone(), 3, kind));
    }
    let revision = file.identity.head_commit.clone();
    let path = file.summary.path.clone();
    let cursor = file.cursor.as_mut().expect("initialized display cursor");
    let chunk = cursor.next_rows(ChunkLimits {
        rows: 128,
        ..ChunkLimits::default()
    });
    let mut hash = std::hash::DefaultHasher::new();
    file.summary.path.hash(&mut hash);
    let prefix = format!("file:{:016x}", hash.finish());
    let mut candidate = Vec::new();
    let mut decorations = 0;
    for display in &chunk.rows {
        if file.group != Some(display.group_index) {
            let group = cursor
                .group(display.group_index)
                .context("diff display group is missing")?;
            let text = format!(
                "@@ -{},{} +{},{} @@",
                group.old_lines.start + 1,
                group.old_lines.len(),
                group.new_lines.start + 1,
                group.new_lines.len()
            );
            candidate.push(BufferBlock {
                id: BlockId(format!("{prefix}:group:{}", display.group_index)),
                text: BufferText::from_rows(vec![text])?,
                metadata: BlockMetadata::default(),
            });
            file.group = Some(display.group_index);
        }
        let mut metadata = BlockMetadata::default();
        forge_diff::projection::append_display_row(
            &mut metadata,
            0,
            display,
            cursor
                .group(display.group_index)
                .context("diff display group is missing")?,
            file.old_syntax.as_ref(),
            file.new_syntax.as_ref(),
        )?;
        decorations += metadata.decoration.len() + metadata.visible_decoration.len();
        ensure!(
            decorations <= 8192,
            "PR display batch exceeds decoration admission"
        );
        if let Some(anchor) = inline_anchor(&revision, &path, display) {
            let target = TargetId(format!(
                "{prefix}:inline:{}",
                file.next_block + candidate.len()
            ));
            ensure!(
                file.target
                    .insert(
                        target.clone(),
                        super::thread_projection::ReviewTarget::InlineComment { anchor },
                    )
                    .is_none(),
                "review inline target identity collision"
            );
            metadata.target.push(TargetRange {
                id: target,
                range: TextRange {
                    start: TextPosition { row: 0, column: 0 },
                    end: TextPosition { row: 1, column: 0 },
                },
            });
        }
        candidate.push(BufferBlock {
            id: BlockId(format!(
                "{prefix}:row:{}",
                file.next_block + candidate.len()
            )),
            text: BufferText::from_rows(vec![display.text.clone()])?,
            metadata,
        });
    }
    if let DisplayState::Unavailable(reason) = &chunk.state {
        candidate.push(BufferBlock {
            id: BlockId(format!("{prefix}:unavailable")),
            text: BufferText::from_rows(vec![format!("Diff unavailable: {reason:?}")])?,
            metadata: BlockMetadata::default(),
        });
    }
    file.summary.more = chunk.state == DisplayState::More;
    file.next_block += candidate.len();
    file.block.extend(candidate);
    Ok(())
}

fn inline_anchor(
    revision: &str,
    path: &str,
    display: &forge_diff::display::DisplayRow,
) -> Option<crate::comments::CommentAnchor> {
    use crate::comments::CommentSide;
    use forge_diff::display::RowKind;
    let (coordinate, side) = match display.kind {
        RowKind::Added => (display.new?, CommentSide::Right),
        RowKind::Removed => (display.old?, CommentSide::Left),
        RowKind::Context => return None,
    };
    Some(crate::comments::CommentAnchor {
        revision: revision.to_owned(),
        path: path.to_owned(),
        side,
        first_line: (coordinate.line + 1) as u64,
        last_line: (coordinate.line + 1) as u64,
    })
}
