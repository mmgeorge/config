use anyhow::{Context, Result, ensure};
use forge_buffer::block::{
    BlockAnchor, BlockMetadata, BufferBlock, Decoration, EditableRegion, FoldRange, Gutter,
    TextChunk, TextPosition, TextRange,
};
use forge_buffer::document::{BufferDocument, LocalEditPreparation};
use forge_buffer::editable::{EditAcknowledgement, LocalEdit, LocalEditResult};
use forge_buffer::identity::{
    BlockId, DocumentId, EditSequence, FoldId, RegionId, RegionRevision, TargetId,
};
use forge_buffer::patch::{BufferPatch, BufferSnapshot};
use forge_buffer::text::BufferText;
use forge_buffer::width::WidthProfile;
use forge_diff::workers::{WorkBudget, WorkPriority};
use serde::Serialize;

use super::ReviewService;
use crate::edit::RegionEdit;
use crate::review::ReviewDocument;

#[derive(Debug, Serialize)]
pub struct ReviewEditResult {
    #[serde(flatten)]
    pub acknowledgement: EditAcknowledgement,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub patch: Option<BufferPatch>,
}

impl std::ops::Deref for ReviewEditResult {
    type Target = EditAcknowledgement;

    fn deref(&self) -> &Self::Target {
        &self.acknowledgement
    }
}

#[derive(Debug, Serialize)]
pub struct ReviewMaterialization {
    pub field: Vec<crate::review::ReviewField>,
    /// Durations in microseconds for the current presentation.
    pub timing: std::collections::BTreeMap<&'static str, u64>,
    pub snapshot: BufferSnapshot,
    pub patch: Option<BufferPatch>,
}

struct RegionPresentation {
    region: RegionId,
    revision: RegionRevision,
    sequence: EditSequence,
    text: String,
    editable: bool,
}

impl ReviewService {
    pub async fn materialize(
        &self,
        id: &DocumentId,
        width: WidthProfile,
    ) -> Result<ReviewMaterialization> {
        let started = std::time::Instant::now();
        width.validate()?;
        let analysis = self
            .analysis
            .as_ref()
            .context("review analysis service is unavailable")?;
        let owner = self.owner(id)?;
        let _section_projection = owner.section_projection.lock().await;
        let (
            identity,
            captured,
            region,
            section_revision,
            mut section,
            mut file,
            owner_width,
            repository,
            mode,
            viewed_file,
        ) = {
            let document = owner.document.lock().expect("review document poisoned");
            let owner_width = document.views.profile().cloned();
            ensure!(
                owner_width.as_ref().is_none_or(|owner| *owner == width),
                "review presentation width must follow its oldest live view"
            );
            let captured = document.draft_payload()?;
            let snapshot = document.snapshot()?;
            let mut region: Vec<_> = snapshot
                .field
                .into_iter()
                .map(|field| RegionPresentation {
                    region: field.region,
                    revision: field.revision,
                    sequence: field.sequence,
                    text: field.text,
                    editable: true,
                })
                .collect();
            if let Some(summary) = snapshot.summary {
                region.push(RegionPresentation {
                    region: summary.region,
                    revision: summary.revision,
                    sequence: summary.sequence,
                    text: summary.text,
                    editable: true,
                });
            }
            for (identity, _) in document.comments.records() {
                let comment = document.comment_snapshot(identity)?;
                if !comment.deleted {
                    region.push(RegionPresentation {
                        region: comment.region,
                        revision: comment.revision,
                        sequence: comment.sequence,
                        text: comment.text,
                        editable: comment.viewer_did_author,
                    });
                }
            }
            (
                document.id.clone(),
                captured,
                region,
                document.section_revision,
                document.section.clone(),
                document
                    .file
                    .iter()
                    .filter_map(|(path, file)| {
                        let current = document
                            .section
                            .get(&super::ReviewSectionKind::Overview)
                            .and_then(|section| section.item.first())
                            .and_then(|item| item.comparison.as_ref());
                        (current == Some(&file.identity)).then(|| {
                            (
                                path.clone(),
                                (file.block.clone(), file.summary.more, file.target.clone()),
                            )
                        })
                    })
                    .collect::<std::collections::BTreeMap<_, _>>(),
                owner_width,
                document.target.repository.repository_name(),
                document.mode,
                document.viewed_file.clone(),
            )
        };
        for kind in [
            super::ReviewSectionKind::Overview,
            super::ReviewSectionKind::RequestedReviewers,
            super::ReviewSectionKind::Files,
            super::ReviewSectionKind::Checks,
            super::ReviewSectionKind::Conversation,
            super::ReviewSectionKind::Commits,
        ] {
            section.entry(kind).or_default();
        }
        let syntax_started = std::time::Instant::now();
        let body_syntax = if let (Some(engine), Some(body)) = (
            self.syntax.as_ref(),
            region.iter().find(|field| field.region.0 == "body"),
        ) {
            Some(
                engine
                    .analyze(forge_diff::syntax::SyntaxRequest {
                        source: forge_diff::source::SourceVersion::new(
                            body.text.as_bytes().to_vec(),
                            forge_diff::source::Representation::Raw,
                        )?,
                        language: forge_diff::syntax::SyntaxLanguage::Markdown,
                        priority: WorkPriority::Visible,
                        deadline: Some(
                            std::time::Instant::now() + std::time::Duration::from_secs(10),
                        ),
                    })
                    .await
                    .map_err(|failure| {
                        anyhow::anyhow!("PR description syntax analysis failed: {failure:?}")
                    })?,
            )
        } else {
            None
        };
        let syntax_us = syntax_started.elapsed().as_micros() as u64;
        let analysis_started = std::time::Instant::now();
        let input_bytes = region.iter().map(|field| field.text.len()).sum::<usize>()
            + section.values().map(|state| state.bytes).sum::<usize>()
            + file
                .values()
                .flat_map(|(block, _, _)| block)
                .map(|block| {
                    block.text.byte_count()
                        + (block.metadata.decoration.len()
                            + block.metadata.visible_decoration.len())
                            * 160
                })
                .sum::<usize>();
        let permit = analysis
            .reserve(WorkPriority::Foreground, WorkBudget::new(input_bytes, None))
            .map_err(|failure| anyhow::anyhow!("review analysis admission failed: {failure:?}"))?;
        let ticket = permit.ticket();
        let projection_width = width.clone();
        let (sender, receiver) = tokio::sync::oneshot::channel();
        permit.submit(move |_| {
            let result = (|| {
                let mut block = Vec::with_capacity(region.len() * 2);
                let mut description = Vec::new();
                let mut reviewer_block = None;
                let mut link = std::collections::BTreeMap::new();
                let mut target = std::collections::BTreeMap::new();
                for region in region {
                    let region_id = region.region.0.clone();
                    let mut body_block = region_block(region, &width, &mut link)?;
                    if region_id == "body"
                        && let Some(syntax) = &body_syntax
                    {
                        body_block.metadata.decoration.clear();
                        for row in 0..body_block.text.row_count() {
                            forge_diff::projection::append_source_syntax_row(
                                &mut body_block.metadata,
                                syntax,
                                row,
                                row,
                                body_block.text.row(row).expect("description row"),
                            )?;
                        }
                    }
                    if region_id == "reviewers" {
                        reviewer_block = Some(body_block);
                        continue;
                    }
                    let label_block = region_label(&region_id, &body_block)?;
                    if region_id == "body" {
                        description.push(label_block);
                        description.push(body_block);
                    } else {
                        if region_id != "title" {
                            block.push(label_block);
                        }
                        block.push(body_block);
                    }
                }
                block.push(comment_boundary()?);
                let reviewer = section
                    .get(&super::ReviewSectionKind::RequestedReviewers)
                    .into_iter()
                    .flat_map(|state| &state.item)
                    .flat_map(|item| &item.detail)
                    .map(|(_, value)| format!("@{value}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                let reviewer = if section
                    .get(&super::ReviewSectionKind::RequestedReviewers)
                    .is_some_and(|state| state.diagnostic.is_some())
                {
                    "Unavailable".to_owned()
                } else if section
                    .get(&super::ReviewSectionKind::RequestedReviewers)
                    .is_some_and(|state| {
                        !state.complete && state.item.is_empty() && state.diagnostic.is_none()
                    })
                {
                    "Loading...".to_owned()
                } else if reviewer.is_empty() {
                    reviewer
                } else {
                    format!("◷ {}", reviewer.replace(", ", " "))
                };
                let head_commit = section
                    .get(&super::ReviewSectionKind::Overview)
                    .and_then(|state| state.item.first())
                    .and_then(|overview| {
                        overview
                            .detail
                            .iter()
                            .find_map(|(label, value)| (label == "Head commit").then_some(value))
                    })
                    .and_then(|head_sha| {
                        section
                            .get(&super::ReviewSectionKind::Commits)
                            .into_iter()
                            .flat_map(|state| &state.item)
                            .filter_map(|item| item.commit.as_ref())
                            .find(|commit| commit.sha == *head_sha)
                    })
                    .cloned()
                    .or_else(|| {
                        section
                            .get(&super::ReviewSectionKind::Overview)
                            .and_then(|state| state.item.first())
                            .and_then(|item| item.commit.clone())
                    });
                let commit_activity = section
                    .get(&super::ReviewSectionKind::Commits)
                    .into_iter()
                    .flat_map(|state| &state.item)
                    .filter_map(|item| item.commit.as_ref())
                    .map(|commit| commit.committed_at.clone())
                    .collect::<Vec<_>>();
                let mut section: Vec<_> = section.into_iter().collect();
                section.sort_by_key(|(kind, _)| match kind {
                    super::ReviewSectionKind::Overview => 0,
                    super::ReviewSectionKind::RequestedReviewers => 1,
                    super::ReviewSectionKind::Checks => 2,
                    super::ReviewSectionKind::Reviews => 3,
                    super::ReviewSectionKind::Conversation => 4,
                    super::ReviewSectionKind::Files => 5,
                    super::ReviewSectionKind::ReviewComments => 6,
                    super::ReviewSectionKind::Threads => 7,
                    super::ReviewSectionKind::Commits => 8,
                });
                let mut projected_section = Vec::with_capacity(section.len() + 1);
                for (kind, state) in section {
                    if kind == super::ReviewSectionKind::Files
                        && mode == crate::review::ReviewMode::Batched
                    {
                        let (unviewed, viewed) = state.partition_files(&viewed_file);
                        projected_section.push((
                            kind,
                            unviewed,
                            "section:Files:Unviewed".to_owned(),
                            Some("Unviewed Changes"),
                            true,
                        ));
                        projected_section.push((
                            kind,
                            viewed,
                            "section:Files:Viewed".to_owned(),
                            Some("Viewed Changes"),
                            true,
                        ));
                    } else {
                        projected_section.push((
                            kind,
                            state,
                            format!("section:{kind:?}"),
                            None,
                            false,
                        ));
                    }
                }
                for (kind, mut state, prefix, heading, keep_empty) in projected_section {
                    if kind == super::ReviewSectionKind::Commits {
                        state.item.reverse();
                    }
                    if kind == super::ReviewSectionKind::RequestedReviewers {
                        continue;
                    }
                    if kind == super::ReviewSectionKind::Overview {
                        if state.item.is_empty() {
                            for name in ["Repo", "Head", "Release", "Review", "Status", "Activity"]
                            {
                                let value = if name == "Repo" {
                                    repository.as_str()
                                } else if state.diagnostic.is_some() {
                                    "Unavailable"
                                } else {
                                    "Loading..."
                                };
                                block.push(metadata_block(
                                    format!("overview:{name}"),
                                    &format!("{name}: {value}"),
                                )?);
                            }
                        }
                        for item in &state.item {
                            block.extend(overview_blocks(
                                item,
                                &repository,
                                &reviewer,
                                reviewer_block.as_ref(),
                                head_commit.as_ref(),
                                &commit_activity,
                                &mut target,
                            )?);
                        }
                        if let Some(diagnostic) = &state.diagnostic {
                            block.push(render_markdown(
                                BlockId(format!("{prefix}:diagnostic")),
                                diagnostic,
                                &width,
                                &mut link,
                            )?);
                        }
                        if !description.is_empty() {
                            block.push(title_block("space:description".into(), "")?);
                            block.append(&mut description);
                        }
                        continue;
                    }
                    if state.complete
                        && state.item.is_empty()
                        && state.diagnostic.is_none()
                        && kind != super::ReviewSectionKind::Checks
                        && !keep_empty
                    {
                        continue;
                    }
                    if kind != super::ReviewSectionKind::Overview {
                        if !description.is_empty() {
                            block.push(title_block("space:description".into(), "")?);
                            block.append(&mut description);
                        }
                        block.push(title_block(format!("{prefix}:space"), "")?);
                    }
                    let section_start = block.len();
                    if kind != super::ReviewSectionKind::Overview {
                        block.push(title_block(
                            format!("{prefix}:heading"),
                            &if let Some(heading) = heading {
                                format!("{heading} ({}):", state.item.len())
                            } else if kind == super::ReviewSectionKind::Files {
                                format!("Changes ({}):", state.item.len())
                            } else if kind == super::ReviewSectionKind::Reviews {
                                format!("Reviews ({}):", state.item.len())
                            } else if kind == super::ReviewSectionKind::Conversation {
                                format!("Comments ({}):", state.item.len())
                            } else if kind == super::ReviewSectionKind::Commits {
                                format!("Recent Commits ({}):", state.item.len())
                            } else {
                                format!("{}:", kind.label())
                            },
                        )?);
                    }
                    if kind == super::ReviewSectionKind::Checks
                        && state.complete
                        && state.item.is_empty()
                        && state.diagnostic.is_none()
                    {
                        let mut empty = title_block(format!("{prefix}:empty"), "No checks")?;
                        empty.metadata.decoration[0].capture = "ForgeStatusDate".into();
                        block.push(empty);
                    }
                    if !state.complete && state.item.is_empty() && state.diagnostic.is_none() {
                        block.push(title_block(format!("{prefix}:loading"), "Loading...")?);
                    }
                    if let Some(diagnostic) = state.diagnostic {
                        block.push(render_markdown(
                            BlockId(format!("{prefix}:diagnostic")),
                            &diagnostic,
                            &width,
                            &mut link,
                        )?);
                    }
                    let review_alignment = if kind == super::ReviewSectionKind::Reviews {
                        Some(review_summary_alignment(&state.item, &width)?)
                    } else {
                        None
                    };
                    let commit_alignment = if kind == super::ReviewSectionKind::Commits {
                        Some(commit_date_alignment(&state.item, &width)?)
                    } else {
                        None
                    };
                    let has_items = !state.item.is_empty();
                    for item in state.item {
                        use std::hash::{Hash, Hasher};
                        let mut hash = std::hash::DefaultHasher::new();
                        item.identity.hash(&mut hash);
                        let item_prefix = if kind == super::ReviewSectionKind::Files {
                            "section:Files"
                        } else {
                            &prefix
                        };
                        let identity = format!("{item_prefix}:{:016x}", hash.finish());
                        let item_start = block.len();
                        if kind == super::ReviewSectionKind::Checks {
                            block.push(check_block(
                                format!("{identity}:summary"),
                                &item,
                                &mut target,
                            )?);
                            continue;
                        }
                        if kind == super::ReviewSectionKind::Commits {
                            block.push(commit_block(
                                format!("{identity}:summary"),
                                &item,
                                commit_alignment.expect("commit alignment"),
                                &width,
                                &mut target,
                            )?);
                            continue;
                        }
                        if kind == super::ReviewSectionKind::Conversation {
                            let mut summary = conversation_summary_block(
                                format!("{identity}:summary"),
                                &item,
                                &mut target,
                            )?;
                            if let Some(body) = &item.body {
                                let body = render_markdown(
                                    BlockId(format!("{identity}:body")),
                                    body,
                                    &width,
                                    &mut link,
                                )?;
                                attach_fold(
                                    &mut summary,
                                    format!("review:item:{identity}"),
                                    &body,
                                    true,
                                );
                                block.push(summary);
                                block.push(body);
                            } else {
                                block.push(summary);
                            }
                            continue;
                        }
                        if kind == super::ReviewSectionKind::Reviews {
                            let mut summary = review_summary_block(
                                format!("{identity}:summary"),
                                &item,
                                review_alignment.expect("review alignment"),
                                &width,
                            )?;
                            if let Some(body) = &item.body {
                                let body = render_markdown(
                                    BlockId(format!("{identity}:body")),
                                    body,
                                    &width,
                                    &mut link,
                                )?;
                                attach_fold(
                                    &mut summary,
                                    format!("review:item:{identity}"),
                                    &body,
                                    true,
                                );
                                block.push(summary);
                                block.push(body);
                            } else {
                                block.push(summary);
                            }
                            continue;
                        }
                        if let Some(source) = &item.source {
                            let mut header = file_header(format!("{identity}:title"), &item)?;
                            let target_id = TargetId(format!("{identity}:open-diff"));
                            target.insert(
                                target_id.clone(),
                                super::thread_projection::ReviewTarget::FileDiff {
                                    path: source.path.clone(),
                                },
                            );
                            header
                                .metadata
                                .target
                                .push(forge_buffer::block::TargetRange {
                                    id: target_id,
                                    range: TextRange {
                                        start: TextPosition { row: 0, column: 0 },
                                        end: TextPosition { row: 1, column: 0 },
                                    },
                                });
                            block.push(header);
                        } else if kind != super::ReviewSectionKind::Overview {
                            block.push(title_block(format!("{identity}:title"), &item.title)?);
                        }
                        if !item.detail.is_empty() && item.source.is_none() {
                            let detail = item
                                .detail
                                .iter()
                                .map(|(label, value)| format!("{label}: {value}"))
                                .collect::<Vec<_>>()
                                .join("\n");
                            block.push(metadata_block(format!("{identity}:detail"), &detail)?);
                        }
                        if let Some(body) = &item.body {
                            block.push(render_markdown(
                                BlockId(format!("{identity}:body")),
                                &body,
                                &width,
                                &mut link,
                            )?);
                        }
                        if let Some(source) = &item.source {
                            if let Some((rows, more, targets)) = file.remove(&source.path) {
                                block.extend(rows);
                                target.extend(targets);
                                if more {
                                    block.push(super::file::continuation_block(&source.path)?);
                                }
                            }
                            block.push(title_block(format!("{identity}:deferred"), "")?);
                        }
                        if let Some(url) = &item.url {
                            block.push(action_block(
                                format!("{identity}:browser"),
                                &format!("URL:     {url}"),
                                super::thread_projection::ReviewTarget::Browser {
                                    url: url.clone(),
                                },
                                &mut target,
                            )?);
                        }
                        if let Some(thread) = &item.thread {
                            let projected = super::thread_projection::render(thread, &width)?;
                            block.extend(projected.block);
                            link.extend(projected.link);
                            for (identity, action) in projected.target {
                                ensure!(
                                    target.insert(identity, action).is_none(),
                                    "review target identity collision"
                                );
                            }
                        }
                        if block.len() > item_start + 1 {
                            let endpoint =
                                block.last().expect("review item has an endpoint").clone();
                            attach_fold(
                                &mut block[item_start],
                                format!("review:item:{identity}"),
                                &endpoint,
                                item.source.is_some(),
                            );
                        }
                    }
                    if (state.loading || !state.complete)
                        && (has_items || state.next_cursor.is_some())
                    {
                        let status = if kind == super::ReviewSectionKind::Checks || state.loading {
                            "Loading..."
                        } else {
                            "Additional records are available"
                        };
                        let mut completion = render_markdown(
                            BlockId(format!("{prefix}:completion")),
                            status,
                            &width,
                            &mut link,
                        )?;
                        if kind == super::ReviewSectionKind::Checks || state.loading {
                            completion.metadata.decoration = vec![Decoration {
                                range: TextRange {
                                    start: TextPosition { row: 0, column: 0 },
                                    end: TextPosition { row: 1, column: 0 },
                                },
                                capture: "ForgeLoading".into(),
                                priority: 200,
                            }];
                        }
                        block.push(completion);
                    }
                    if block.len() > section_start + 1 {
                        let endpoint = block
                            .last()
                            .expect("review section has an endpoint")
                            .clone();
                        attach_fold(
                            &mut block[section_start],
                            format!("review:{prefix}"),
                            &endpoint,
                            matches!(
                                kind,
                                super::ReviewSectionKind::Commits
                                    | super::ReviewSectionKind::Conversation
                            ),
                        );
                    }
                }
                if !description.is_empty() {
                    block.push(title_block("space:description".into(), "")?);
                    block.append(&mut description);
                }
                ensure!(
                    block
                        .iter()
                        .map(|block| block.text.byte_count())
                        .sum::<usize>()
                        <= 16 * 1024 * 1024,
                    "review presentation byte limit exceeded"
                );
                ensure!(
                    block
                        .iter()
                        .map(|block| block.text.row_count())
                        .sum::<usize>()
                        <= 65_536,
                    "review presentation row limit exceeded"
                );
                Ok::<_, anyhow::Error>((block, link, target))
            })();
            let _ = sender.send(result);
        });
        let completed = receiver
            .await
            .context("review analysis ended without a result");
        ticket.completed().await;
        let (block, link, target) = completed??;
        let analysis_us = analysis_started.elapsed().as_micros() as u64;
        let publication_started = std::time::Instant::now();
        let _publication = owner.publication.lock().await;
        let mut document = owner.document.lock().expect("review document poisoned");
        ensure!(
            document.draft_payload()? == captured
                && document.section_revision == section_revision
                && document.views.profile() == owner_width.as_ref(),
            "review projection capture was superseded"
        );
        let patch = if let Some(projection) = document.projection.as_mut() {
            projection.edit(0..projection.block_count(), block)?
        } else {
            document.projection = Some(BufferDocument::new(identity, block)?);
            None
        };
        document.projection_link = link;
        document.projection_target = target;
        document.projection_width = Some(projection_width);
        let comparison = document
            .section
            .get(&super::ReviewSectionKind::Overview)
            .and_then(|section| section.item.first())
            .and_then(|item| item.comparison.clone());
        for file in document.file.values_mut() {
            file.projected_blocks = if comparison.as_ref() == Some(&file.identity) {
                file.block.len() + usize::from(file.summary.more)
            } else {
                0
            };
        }
        Ok(ReviewMaterialization {
            field: document.snapshot()?.field,
            timing: std::collections::BTreeMap::from([
                (
                    "capture_us",
                    syntax_started.duration_since(started).as_micros() as u64,
                ),
                ("syntax_us", syntax_us),
                ("analysis_us", analysis_us),
                (
                    "publication_us",
                    publication_started.elapsed().as_micros() as u64,
                ),
                ("total_us", started.elapsed().as_micros() as u64),
            ]),
            snapshot: document
                .projection
                .as_ref()
                .expect("published projection")
                .snapshot(),
            patch,
        })
    }
}

fn attach_fold(owner: &mut BufferBlock, identity: String, endpoint: &BufferBlock, closed: bool) {
    owner.metadata.fold.push(FoldRange {
        collapse_children: false,
        id: FoldId(identity),
        start: TextPosition { row: 0, column: 0 },
        end: BlockAnchor {
            block: endpoint.id.clone(),
            position: TextPosition {
                row: endpoint.text.row_count(),
                column: 0,
            },
        },
        closed,
    });
}

impl ReviewDocument {
    pub(crate) fn project_comment(
        &mut self,
        comment: &crate::review::ReviewCommentSnapshot,
    ) -> Result<Option<BufferPatch>> {
        let Some(projection) = self.projection.as_mut() else {
            return Ok(None);
        };
        let identity = BlockId(format!("region:{}", comment.region.0));
        let width = self
            .projection_width
            .as_ref()
            .context("comment projection width is missing")?;
        let mut link = std::collections::BTreeMap::new();
        if let Some(index) = projection.block_index(&identity) {
            if comment.deleted {
                ensure!(index > 0, "comment label is missing");
                let label = BlockId(format!("label:{}", comment.region.0));
                ensure!(
                    projection.block_index(&label) == Some(index - 1),
                    "comment label is not adjacent"
                );
                let patch = projection.edit(index - 1..index + 1, Vec::new())?;
                self.projection_link.remove(&identity);
                self.projection_link.remove(&label);
                return Ok(patch);
            }
            let block = region_block(
                RegionPresentation {
                    region: comment.region.clone(),
                    revision: comment.revision,
                    sequence: comment.sequence,
                    text: comment.text.clone(),
                    editable: comment.viewer_did_author,
                },
                width,
                &mut link,
            )?;
            let label = region_label(&comment.region.0, &block)?;
            let patch = projection.edit(index - 1..index + 1, vec![label, block])?;
            self.projection_link.extend(link);
            return Ok(patch);
        }
        if comment.deleted {
            return Ok(None);
        }
        let index = projection
            .block_index(&BlockId("comments:end".into()))
            .context("comment projection boundary is missing")?;
        let body = region_block(
            RegionPresentation {
                region: comment.region.clone(),
                revision: comment.revision,
                sequence: comment.sequence,
                text: comment.text.clone(),
                editable: comment.viewer_did_author,
            },
            width,
            &mut link,
        )?;
        let label = region_label(&comment.region.0, &body)?;
        let patch = projection.edit(index..index, vec![label, body])?;
        self.projection_link.extend(link);
        Ok(patch)
    }

    pub(crate) fn projected_edit(&mut self, edit: RegionEdit) -> Result<ReviewEditResult> {
        let mut projection = self.projection.take();
        let result = (|| {
            let prepared = if let Some(projection) = projection.as_mut() {
                match projection.prepare_local_edit(LocalEdit {
                    document: edit.document.clone(),
                    region: edit.region.clone(),
                    base: edit.base,
                    sequence: edit.sequence,
                    text: BufferText::from_rows(edit.text.split('\n'))?,
                })? {
                    LocalEditPreparation::Prepared(prepared) => Some(prepared),
                    LocalEditPreparation::Rejected(result) => {
                        anyhow::bail!("review projection rejected local edit: {result:?}")
                    }
                }
            } else {
                None
            };
            let acknowledgement = self.region_edit(edit)?;
            let patch = prepared.map(|prepared| match prepared.commit() {
                LocalEditResult::Accepted { patch, .. } => *patch,
                _ => unreachable!("validated local edit commit"),
            });
            Ok(ReviewEditResult {
                acknowledgement,
                patch,
            })
        })();
        self.projection = projection;
        result
    }
}

fn region_label(identity: &str, body: &BufferBlock) -> Result<BufferBlock> {
    let label = match identity {
        "title" => "Title".to_owned(),
        "body" => "Description".to_owned(),
        "review_summary" => "Review summary".to_owned(),
        identity => format!(
            "Comment {}",
            identity
                .strip_prefix("comment-")
                .unwrap_or(identity)
                .trim_end_matches("/body")
        ),
    };
    let mut block = title_block(format!("label:{identity}"), &format!("{label}:"))?;
    if identity == "body" {
        attach_fold(&mut block, "review:description".into(), body, false);
    } else if identity.starts_with("comment-") {
        attach_fold(&mut block, format!("review:comment:{identity}"), body, true);
    }
    Ok(block)
}

fn region_block(
    region: RegionPresentation,
    width: &WidthProfile,
    link: &mut std::collections::BTreeMap<BlockId, Vec<forge_buffer::markdown::MarkdownLink>>,
) -> Result<BufferBlock> {
    if !region.editable {
        return render_markdown(
            BlockId(format!("region:{}", region.region.0)),
            &region.text,
            width,
            link,
        );
    }
    let identity = BlockId(format!("region:{}", region.region.0));
    let (text, mut metadata) = if matches!(region.region.0.as_str(), "title" | "reviewers") {
        link.insert(identity, Vec::new());
        (
            BufferText::from_rows(region.text.split('\n'))?,
            BlockMetadata::default(),
        )
    } else {
        let rendered = forge_buffer::markdown::MarkdownRenderer::source(
            identity.clone(),
            &region.text,
            width,
        )?;
        link.insert(identity, rendered.link);
        (rendered.block.text, rendered.block.metadata)
    };
    let end_row = text.row_count() - 1;
    if matches!(region.region.0.as_str(), "title" | "reviewers") {
        metadata.decoration.push(Decoration {
            range: TextRange {
                start: TextPosition { row: 0, column: 0 },
                end: TextPosition {
                    row: 0,
                    column: text.row(0).expect("title row").len(),
                },
            },
            capture: if region.region.0 == "title" {
                "ForgeStatusPath"
            } else {
                "ForgeReviewPending"
            }
            .into(),
            priority: 100,
        });
        metadata.gutter.push(Gutter {
            placement: Default::default(),
            position: TextPosition { row: 0, column: 0 },
            chunk: vec![TextChunk {
                text: if region.region.0 == "title" {
                    "Title:  "
                } else {
                    "Review: "
                }
                .into(),
                capture: "ForgeStatusLabel".into(),
            }],
            priority: 110,
        });
    }
    if region.editable {
        metadata.editable_region.push(EditableRegion {
            id: region.region.clone(),
            revision: region.revision,
            sequence: region.sequence,
            range: TextRange {
                start: TextPosition { row: 0, column: 0 },
                end: TextPosition {
                    row: end_row,
                    column: text.row(end_row).expect("last region row").len(),
                },
            },
        });
    }
    Ok(BufferBlock {
        id: BlockId(format!("region:{}", region.region.0)),
        text,
        metadata,
    })
}

fn file_header(identity: String, item: &super::ReviewSectionItem) -> Result<BufferBlock> {
    let source = item
        .source
        .as_ref()
        .context("file header requires source")?;
    let (label, capture) = match source.status.as_str() {
        "added" => ("New", "ForgeStatusFileNew"),
        "removed" => ("Deleted", "ForgeStatusFileDeleted"),
        "renamed" => ("Renamed", "ForgeStatusFileRenamed"),
        _ => ("Modified", "ForgeStatusFileModified"),
    };
    let mut text = format!("{label:8} {}", source.path);
    let mut spans = vec![(0, label.len(), capture)];
    for (field, sign, capture) in [
        ("additions", '+', "ForgeAddRange"),
        ("deletions", '-', "ForgeDeleteRange"),
    ] {
        if let Some(value) = item
            .detail
            .iter()
            .find_map(|(key, value)| (key == field).then_some(value))
        {
            text.push(' ');
            let start = text.len();
            text.push(sign);
            text.push_str(value);
            spans.push((start, text.len(), capture));
        }
    }
    let mut block = title_block(identity, &text)?;
    block.metadata.decoration[0].capture = "ForgeStatusPath".into();
    for (start, end, capture) in spans {
        block.metadata.decoration.push(Decoration {
            range: TextRange {
                start: TextPosition {
                    row: 0,
                    column: start,
                },
                end: TextPosition {
                    row: 0,
                    column: end,
                },
            },
            capture: capture.into(),
            priority: 110,
        });
    }
    Ok(block)
}

fn check_block(
    identity: String,
    item: &super::ReviewSectionItem,
    target: &mut std::collections::BTreeMap<TargetId, super::thread_projection::ReviewTarget>,
) -> Result<BufferBlock> {
    let check = item
        .check
        .as_ref()
        .context("check presentation is missing")?;
    let (icon, icon_capture) = match check.state.to_ascii_lowercase().as_str() {
        "success" => ("✓", "ForgeAddRange"),
        "failure" | "error" | "timed_out" | "startup_failure" | "action_required" => {
            ("✗", "ForgeDeleteRange")
        }
        "cancelled" | "skipped" | "neutral" => ("!", "ForgeStatusFetching"),
        _ => ("◷", "ForgeStatusFetching"),
    };
    let mut text = format!("{icon} {}", check.name);
    let name_start = icon.len() + 1;
    let mut decoration = vec![
        Decoration {
            range: TextRange {
                start: TextPosition { row: 0, column: 0 },
                end: TextPosition {
                    row: 0,
                    column: icon.len(),
                },
            },
            capture: icon_capture.into(),
            priority: 120,
        },
        Decoration {
            range: TextRange {
                start: TextPosition {
                    row: 0,
                    column: name_start,
                },
                end: TextPosition {
                    row: 0,
                    column: text.len(),
                },
            },
            capture: "ForgeStatusPath".into(),
            priority: 110,
        },
    ];
    if let Some(workflow) = &check.workflow {
        let separator_start = text.len();
        text.push_str(" | ");
        let workflow_start = text.len();
        text.push_str(workflow);
        decoration.push(Decoration {
            range: TextRange {
                start: TextPosition {
                    row: 0,
                    column: separator_start,
                },
                end: TextPosition {
                    row: 0,
                    column: workflow_start,
                },
            },
            capture: "Comment".into(),
            priority: 100,
        });
        decoration.push(Decoration {
            range: TextRange {
                start: TextPosition {
                    row: 0,
                    column: workflow_start,
                },
                end: TextPosition {
                    row: 0,
                    column: text.len(),
                },
            },
            capture: "ForgeStatusRemote".into(),
            priority: 110,
        });
    }
    let mut metadata = BlockMetadata {
        decoration,
        ..Default::default()
    };
    if let Some(url) = &item.url {
        let target_id = TargetId(format!("{identity}:browser"));
        ensure!(
            target
                .insert(
                    target_id.clone(),
                    super::thread_projection::ReviewTarget::Browser { url: url.clone() },
                )
                .is_none(),
            "review target identity collision"
        );
        metadata.target.push(forge_buffer::block::TargetRange {
            id: target_id,
            range: TextRange {
                start: TextPosition { row: 0, column: 0 },
                end: TextPosition {
                    row: 0,
                    column: text.len(),
                },
            },
        });
    }
    Ok(BufferBlock {
        id: BlockId(identity),
        text: BufferText::from_rows([text])?,
        metadata,
    })
}

fn commit_date_alignment(
    item: &[std::sync::Arc<super::ReviewSectionItem>],
    width: &WidthProfile,
) -> Result<usize> {
    item.iter().try_fold(0, |alignment, item| {
        let commit = item
            .commit
            .as_ref()
            .context("commit presentation is missing")?;
        Ok(alignment
            .max(width.cells(&crate::presentation_time::relative(&commit.committed_at), 0)?))
    })
}

fn commit_block(
    identity: String,
    item: &super::ReviewSectionItem,
    date_alignment: usize,
    width: &WidthProfile,
    target: &mut std::collections::BTreeMap<TargetId, super::thread_projection::ReviewTarget>,
) -> Result<BufferBlock> {
    let commit = item
        .commit
        .as_ref()
        .context("commit presentation is missing")?;
    let sha = commit.sha.get(..7).unwrap_or(&commit.sha);
    let date = crate::presentation_time::relative(&commit.committed_at);
    let date_padding = date_alignment.saturating_sub(width.cells(&date, 0)?) + 1;
    let text = format!(
        "{sha}  {date}{}{}",
        " ".repeat(date_padding),
        commit.headline
    );
    let date_start = sha.len() + 2;
    let headline_start = date_start + date.len() + date_padding;
    let target_id = TargetId(format!("{identity}:message"));
    let diff_target_id = TargetId(format!("{identity}:diff"));
    ensure!(
        target
            .insert(
                target_id.clone(),
                super::thread_projection::ReviewTarget::CommitMessage {
                    oid: commit.sha.clone(),
                },
            )
            .is_none(),
        "review target identity collision"
    );
    ensure!(
        target
            .insert(
                diff_target_id.clone(),
                super::thread_projection::ReviewTarget::CommitDiff {
                    oid: commit.sha.clone(),
                    anchor: BlockId(identity.clone()),
                },
            )
            .is_none(),
        "review target identity collision"
    );
    Ok(BufferBlock {
        id: BlockId(identity),
        text: BufferText::from_rows([text.clone()])?,
        metadata: BlockMetadata {
            decoration: vec![
                Decoration {
                    range: TextRange {
                        start: TextPosition { row: 0, column: 0 },
                        end: TextPosition {
                            row: 0,
                            column: sha.len(),
                        },
                    },
                    capture: "ForgeStatusObjectId".into(),
                    priority: 110,
                },
                Decoration {
                    range: TextRange {
                        start: TextPosition {
                            row: 0,
                            column: date_start,
                        },
                        end: TextPosition {
                            row: 0,
                            column: date_start + date.len(),
                        },
                    },
                    capture: "ForgeStatusDate".into(),
                    priority: 110,
                },
                Decoration {
                    range: TextRange {
                        start: TextPosition {
                            row: 0,
                            column: headline_start,
                        },
                        end: TextPosition {
                            row: 0,
                            column: text.len(),
                        },
                    },
                    capture: "ForgeStatusPath".into(),
                    priority: 100,
                },
            ],
            target: vec![
                forge_buffer::block::TargetRange {
                    id: diff_target_id,
                    range: TextRange {
                        start: TextPosition { row: 0, column: 0 },
                        end: TextPosition {
                            row: 0,
                            column: headline_start,
                        },
                    },
                },
                forge_buffer::block::TargetRange {
                    id: target_id,
                    range: TextRange {
                        start: TextPosition {
                            row: 0,
                            column: headline_start,
                        },
                        end: TextPosition {
                            row: 0,
                            column: text.len(),
                        },
                    },
                },
            ],
            ..Default::default()
        },
    })
}

fn conversation_summary_block(
    identity: String,
    item: &super::ReviewSectionItem,
    target: &mut std::collections::BTreeMap<TargetId, super::thread_projection::ReviewTarget>,
) -> Result<BufferBlock> {
    let timestamp = item
        .detail
        .iter()
        .find_map(|(label, value)| (label == "updated_at").then_some(value.as_str()))
        .or_else(|| {
            item.detail
                .iter()
                .find_map(|(label, value)| (label == "created_at").then_some(value.as_str()))
        })
        .unwrap_or("");
    let date = crate::presentation_time::relative(timestamp);
    let preview = item
        .body
        .as_deref()
        .unwrap_or("")
        .lines()
        .find(|line| !line.trim().is_empty())
        .unwrap_or("")
        .trim();
    let text = format!("󰅺 {} {date}  {preview}", item.title);
    let icon_end = "󰅺".len();
    let mut metadata = BlockMetadata {
        decoration: vec![
            Decoration {
                range: TextRange {
                    start: TextPosition { row: 0, column: 0 },
                    end: TextPosition {
                        row: 0,
                        column: text.len(),
                    },
                },
                capture: "ForgeStatusPath".into(),
                priority: 100,
            },
            Decoration {
                range: TextRange {
                    start: TextPosition { row: 0, column: 0 },
                    end: TextPosition {
                        row: 0,
                        column: icon_end,
                    },
                },
                capture: "ForgeStatusPR".into(),
                priority: 110,
            },
        ],
        ..Default::default()
    };
    if let Some(url) = &item.url {
        let target_id = TargetId(format!("{identity}:browser"));
        ensure!(
            target
                .insert(
                    target_id.clone(),
                    super::thread_projection::ReviewTarget::Browser { url: url.clone() },
                )
                .is_none(),
            "review target identity collision"
        );
        metadata.target.push(forge_buffer::block::TargetRange {
            id: target_id,
            range: TextRange {
                start: TextPosition { row: 0, column: 0 },
                end: TextPosition {
                    row: 0,
                    column: text.len(),
                },
            },
        });
    }
    Ok(BufferBlock {
        id: BlockId(identity),
        text: BufferText::from_rows([text])?,
        metadata,
    })
}

fn review_summary_alignment(
    item: &[std::sync::Arc<super::ReviewSectionItem>],
    width: &WidthProfile,
) -> Result<(usize, usize)> {
    let mut author_width = 0;
    let mut date_width = 0;
    for item in item {
        let review = item
            .review
            .as_ref()
            .context("review summary presentation is missing")?;
        author_width = author_width.max(width.cells(&review.author, 0)?);
        date_width = date_width
            .max(width.cells(&crate::presentation_time::relative(&review.submitted_at), 0)?);
    }
    Ok((author_width, date_width))
}

fn review_summary_block(
    identity: String,
    item: &super::ReviewSectionItem,
    alignment: (usize, usize),
    width: &WidthProfile,
) -> Result<BufferBlock> {
    let review = item
        .review
        .as_ref()
        .context("review summary presentation is missing")?;
    let (icon, icon_capture) = match review.state.as_str() {
        "APPROVED" => ("✓", "ForgeAddRange"),
        "CHANGES_REQUESTED" => ("✗", "ForgeDeleteRange"),
        _ => ("󰅺", "ForgeReviewComment"),
    };
    let date = crate::presentation_time::relative(&review.submitted_at);
    let preview = review_preview(item.body.as_deref().unwrap_or(""));
    let mut text = format!("{icon} {}", review.author);
    let icon_end = icon.len();
    let author_start = icon_end + 1;
    text.push_str(&" ".repeat(alignment.0.saturating_sub(width.cells(&review.author, 0)?)));
    let author_end = text.len();
    let mut date_start = None;
    let mut date_end = None;
    if !date.is_empty() {
        text.push(' ');
        date_start = Some(text.len());
        text.push_str(&date);
        if !preview.is_empty() {
            text.push_str(&" ".repeat(alignment.1.saturating_sub(width.cells(&date, 0)?)));
        }
        date_end = Some(text.len());
    }
    let mut preview_start = None;
    if !preview.is_empty() {
        text.push_str("  ");
        preview_start = Some(text.len());
        text.push_str(&preview);
    }
    let mut decoration = vec![
        Decoration {
            range: TextRange {
                start: TextPosition { row: 0, column: 0 },
                end: TextPosition {
                    row: 0,
                    column: icon_end,
                },
            },
            capture: icon_capture.into(),
            priority: 120,
        },
        Decoration {
            range: TextRange {
                start: TextPosition {
                    row: 0,
                    column: author_start,
                },
                end: TextPosition {
                    row: 0,
                    column: author_end,
                },
            },
            capture: "ForgeReviewComment".into(),
            priority: 100,
        },
    ];
    if let (Some(start), Some(end)) = (date_start, date_end) {
        decoration.push(Decoration {
            range: TextRange {
                start: TextPosition {
                    row: 0,
                    column: start,
                },
                end: TextPosition {
                    row: 0,
                    column: end,
                },
            },
            capture: "ForgeStatusDate".into(),
            priority: 110,
        });
    }
    if let Some(start) = preview_start {
        decoration.push(Decoration {
            range: TextRange {
                start: TextPosition {
                    row: 0,
                    column: start,
                },
                end: TextPosition {
                    row: 0,
                    column: text.len(),
                },
            },
            capture: "ForgeReviewComment".into(),
            priority: 100,
        });
    }
    Ok(BufferBlock {
        id: BlockId(identity),
        text: BufferText::from_rows([text])?,
        metadata: BlockMetadata {
            decoration,
            ..Default::default()
        },
    })
}

fn review_preview(body: &str) -> String {
    let mut in_fence = false;
    body.replace("\r\n", "\n")
        .replace('\r', "\n")
        .lines()
        .filter_map(|line| {
            let line = line.trim();
            if line.starts_with("```") {
                in_fence = !in_fence;
                return None;
            }
            (!in_fence && !line.is_empty()).then_some(line)
        })
        .collect::<Vec<_>>()
        .join(" ")
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

fn metadata_block(identity: String, detail: &str) -> Result<BufferBlock> {
    let mut rows = Vec::new();
    let mut decoration = Vec::new();
    for detail in detail.lines() {
        let (label, value) = detail.split_once(": ").unwrap_or((detail, ""));
        let (label, value, capture) = match label {
            "Base commit" => (
                "Base",
                value.get(..7).unwrap_or(value),
                "ForgeStatusObjectId",
            ),
            "Head commit" => (
                "Head",
                value.get(..7).unwrap_or(value),
                "ForgeStatusObjectId",
            ),
            "Head" | "Base" => (label, value, "ForgeStatusBranch"),
            "State" => ("Status", value, "ForgeStatusOpen"),
            _ => (label, value, "ForgeStatusPR"),
        };
        let prefix = format!("{label}:");
        let padding = 8.max(prefix.len() + 1);
        let text = format!("{prefix:padding$}{value}");
        let row = rows.len();
        decoration.push(Decoration {
            range: TextRange {
                start: TextPosition { row, column: 0 },
                end: TextPosition {
                    row,
                    column: text.len(),
                },
            },
            capture: capture.into(),
            priority: 90,
        });
        decoration.push(Decoration {
            range: TextRange {
                start: TextPosition { row, column: 0 },
                end: TextPosition {
                    row,
                    column: prefix.len(),
                },
            },
            capture: "ForgeStatusLabel".into(),
            priority: 110,
        });
        rows.push(text);
    }
    Ok(BufferBlock {
        id: BlockId(identity),
        text: BufferText::from_rows(rows)?,
        metadata: BlockMetadata {
            decoration,
            ..Default::default()
        },
    })
}

fn comment_boundary() -> Result<BufferBlock> {
    Ok(BufferBlock {
        id: BlockId("comments:end".into()),
        text: BufferText::default(),
        metadata: BlockMetadata::default(),
    })
}

fn title_block(identity: String, title: &str) -> Result<BufferBlock> {
    let text = BufferText::from_rows([title])?;
    Ok(BufferBlock {
        id: BlockId(identity),
        text,
        metadata: BlockMetadata {
            decoration: vec![forge_buffer::block::Decoration {
                range: TextRange {
                    start: TextPosition { row: 0, column: 0 },
                    end: TextPosition { row: 1, column: 0 },
                },
                capture: if title == "Loading..." {
                    "ForgeLoading"
                } else {
                    "ForgeStatusHeader"
                }
                .into(),
                priority: 100,
            }],
            ..BlockMetadata::default()
        },
    })
}

fn action_block(
    identity: String,
    label: &str,
    action: super::thread_projection::ReviewTarget,
    target: &mut std::collections::BTreeMap<TargetId, super::thread_projection::ReviewTarget>,
) -> Result<BufferBlock> {
    let target_id = TargetId(identity.clone());
    ensure!(
        target.insert(target_id.clone(), action).is_none(),
        "review target identity collision"
    );
    Ok(BufferBlock {
        id: BlockId(identity),
        text: BufferText::from_rows([label])?,
        metadata: BlockMetadata {
            target: vec![forge_buffer::block::TargetRange {
                id: target_id,
                range: TextRange {
                    start: TextPosition { row: 0, column: 0 },
                    end: TextPosition {
                        row: 0,
                        column: label.len(),
                    },
                },
            }],
            ..BlockMetadata::default()
        },
    })
}

fn overview_blocks(
    item: &super::ReviewSectionItem,
    repository: &str,
    reviewer: &str,
    reviewer_block: Option<&BufferBlock>,
    head_commit: Option<&super::ReviewCommitPresentation>,
    commit_activity: &[String],
    target: &mut std::collections::BTreeMap<TargetId, super::thread_projection::ReviewTarget>,
) -> Result<Vec<BufferBlock>> {
    let detail = |name: &str| {
        item.detail
            .iter()
            .find_map(|(label, value)| (label == name).then_some(value.as_str()))
            .unwrap_or("")
    };
    let head_sha = detail("Head commit");
    let head = head_sha.get(..7).unwrap_or(head_sha);
    let state = if detail("Draft") == "Yes" {
        "DRAFT".into()
    } else {
        detail("State").to_uppercase()
    };
    let milestone = detail("Milestone");
    let release = if milestone.is_empty() {
        String::new()
    } else {
        format!("◆ {milestone}")
    };
    let activity = crate::presentation_time::latest_activity(
        [detail("Created"), detail("Updated")]
            .into_iter()
            .chain(commit_activity.iter().map(String::as_str)),
    );
    let mut block = Vec::new();
    for (name, value, capture) in [
        ("Repo", repository, "ForgeStatusRemote"),
        ("Head", head, "ForgeStatusObjectId"),
        ("Release", release.as_str(), "ForgeStatusBranch"),
        ("Review", reviewer, "ForgeReviewPending"),
        (
            "Status",
            state.as_str(),
            match state.as_str() {
                "OPEN" => "ForgeStatusOpen",
                "CLOSED" | "MERGED" => "ForgeStatusClosed",
                _ => "ForgeStatusFetching",
            },
        ),
        ("Activity", activity.as_str(), "ForgeStatusDate"),
    ] {
        if name == "Review"
            && let Some(reviewer) = reviewer_block
        {
            block.push(reviewer.clone());
            continue;
        }
        if name == "Head" {
            if let Some(commit) = head_commit {
                block.push(head_overview_block(commit, detail("Head"))?);
                continue;
            }
        }
        let text = format!("{name}: {value}");
        let mut row = metadata_block(format!("overview:{name}"), &text)?;
        for decoration in &mut row.metadata.decoration {
            if decoration.priority == 90 {
                decoration.capture = capture.into();
                if decoration.range.start.row == 0 {
                    decoration.range.start.column = 8.max(name.len() + 2);
                }
            }
        }
        let action = match name {
            "Status" => Some(super::thread_projection::ReviewTarget::Lifecycle {
                available: available_lifecycle(&item.detail),
            }),
            _ => None,
        };
        if let Some(action) = action {
            let identity = TargetId(format!("overview:{name}"));
            target.insert(identity.clone(), action);
            row.metadata.target.push(forge_buffer::block::TargetRange {
                id: identity,
                range: TextRange {
                    start: TextPosition { row: 0, column: 0 },
                    end: TextPosition {
                        row: row.text.row_count(),
                        column: 0,
                    },
                },
            });
        }
        block.push(row);
    }
    Ok(block)
}

/// Caps the complete commit summary and maps its highlights onto indented physical rows.
fn head_overview_block(
    commit: &super::ReviewCommitPresentation,
    branch: &str,
) -> Result<BufferBlock> {
    let sha = commit.sha.get(..7).unwrap_or(&commit.sha);
    let date = crate::presentation_time::relative(&commit.committed_at);
    let mut summary = format!("{sha} {date} {branch} {}", commit.headline);
    let truncated = summary.chars().count() > 100;
    if truncated {
        summary.truncate(summary.char_indices().nth(99).expect("long summary").0);
    }
    let content_end = "Head:   ".len() + summary.len();
    if truncated {
        summary.push('…');
    }
    let text = format!("Head:   {summary}");
    let sha_start = "Head:   ".len();
    let date_start = sha_start + sha.len() + 1;
    let branch_start = date_start + date.len() + 1;
    let headline_start = branch_start + branch.len() + 1;
    let mut spans = vec![
        (0, "Head:".len(), "ForgeStatusLabel", 110),
        (sha_start, sha_start + sha.len(), "ForgeStatusObjectId", 110),
        (date_start, date_start + date.len(), "ForgeStatusDate", 110),
        (
            branch_start,
            branch_start + branch.len(),
            "ForgeStatusBranch",
            110,
        ),
    ];
    if let Some(type_end) = conventional_type_end(&commit.headline) {
        spans.push((
            headline_start,
            headline_start + type_end,
            "ForgeStatusCommitType",
            120,
        ));
    }
    let decoration = spans
        .into_iter()
        .filter_map(|(start, end, capture, priority)| {
            let end = end.min(content_end);
            (start < end).then(|| Decoration {
                range: TextRange {
                    start: TextPosition {
                        row: 0,
                        column: start,
                    },
                    end: TextPosition {
                        row: 0,
                        column: end,
                    },
                },
                capture: capture.into(),
                priority,
            })
        })
        .collect();
    Ok(BufferBlock {
        id: BlockId("overview:Head".into()),
        text: BufferText::from_rows([text])?,
        metadata: BlockMetadata {
            decoration,
            ..Default::default()
        },
    })
}

fn conventional_type_end(subject: &str) -> Option<usize> {
    let bytes = subject.as_bytes();
    let type_end = bytes
        .iter()
        .position(|byte| !(byte.is_ascii_lowercase() || byte.is_ascii_digit() || *byte == b'-'))?;
    if type_end == 0 || !bytes[0].is_ascii_lowercase() {
        return None;
    }
    let suffix = &subject[type_end..];
    if suffix.starts_with(": ") || suffix.starts_with("!: ") {
        return Some(type_end);
    }
    let scope = suffix.strip_prefix('(')?;
    let close = scope.find(')')?;
    if close == 0 || scope[..close].chars().any(char::is_whitespace) {
        return None;
    }
    matches!(&scope[close + 1..], ": " | "!: ").then_some(type_end)
}

fn available_lifecycle(
    detail: &[(String, String)],
) -> Vec<forge_github::pull_request::DesiredPullRequestState> {
    use forge_github::pull_request::DesiredPullRequestState;
    let state = detail
        .iter()
        .find_map(|(label, value)| (label == "State").then(|| value.to_uppercase()));
    let draft = detail
        .iter()
        .find_map(|(label, value)| (label == "Draft").then_some(value.as_str()))
        == Some("Yes");
    let current = match state.as_deref() {
        Some("CLOSED") => Some(DesiredPullRequestState::Closed),
        Some("OPEN") if draft => Some(DesiredPullRequestState::Draft),
        Some("OPEN") => Some(DesiredPullRequestState::Open),
        _ => None,
    };
    [
        DesiredPullRequestState::Draft,
        DesiredPullRequestState::Open,
        DesiredPullRequestState::Closed,
    ]
    .into_iter()
    .filter(|desired| Some(*desired) != current)
    .collect()
}

fn render_markdown(
    identity: BlockId,
    source: &str,
    width: &WidthProfile,
    link: &mut std::collections::BTreeMap<BlockId, Vec<forge_buffer::markdown::MarkdownLink>>,
) -> Result<BufferBlock> {
    let rendered =
        forge_buffer::markdown::MarkdownRenderer::render(identity.clone(), source, width)?;
    link.insert(identity, rendered.link);
    Ok(rendered.block)
}

#[cfg(test)]
mod header_tests {
    use super::*;

    #[test]
    fn head_summary_caps_the_whole_value_in_one_native_line() {
        let commit = super::super::ReviewCommitPresentation {
            sha: "a".repeat(40),
            committed_at: "1 day ago".into(),
            headline: format!("refactor: {}", "界 change ".repeat(30)),
        };
        for branch in ["origin/feature".to_owned(), "branch-é".repeat(30)] {
            let block = head_overview_block(&commit, &branch).unwrap();
            let rows = block.text.wire_rows();
            assert_eq!(rows.len(), 1);
            let value = &rows[0][8..];
            let original = format!("aaaaaaa 1 day ago {branch} {}", commit.headline);
            assert_eq!(
                value,
                format!("{}…", original.chars().take(99).collect::<String>())
            );
            assert_eq!(value.chars().count(), 100);
            for decoration in &block.metadata.decoration {
                let range = &decoration.range;
                assert_eq!(range.start.row, 0);
                assert_eq!(range.end.row, 0);
                assert!(range.start.column < range.end.column && range.end.column <= rows[0].len());
                assert!(
                    rows[0].is_char_boundary(range.start.column)
                        && rows[0].is_char_boundary(range.end.column)
                );
            }
            if branch == "origin/feature" {
                let span = block
                    .metadata
                    .decoration
                    .iter()
                    .find(|span| span.capture == "ForgeStatusCommitType")
                    .unwrap();
                assert_eq!(
                    &rows[0][span.range.start.column..span.range.end.column],
                    "refactor"
                );
            }
        }
    }

    #[test]
    fn metadata_keeps_long_values_on_their_native_lines() {
        let detail = format!("Repo: {}\nStatus: DRAFT", "long-name/".repeat(20));
        let block = metadata_block("overview:test".into(), &detail).unwrap();
        let rows = block.text.wire_rows();
        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0], format!("Repo:   {}", "long-name/".repeat(20)));
        assert_eq!(rows[1], "Status: DRAFT");
    }
}
