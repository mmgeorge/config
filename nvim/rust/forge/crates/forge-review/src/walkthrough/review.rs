use super::*;
use forge_buffer::block::BlockMetadata;
use forge_diff::{
    display::{ChunkLimits, DisplayCursor, DisplayState},
    engine::{DiffEngine, DiffRequest},
    source::{Representation, SourcePair, SourceSide, SourceVersion},
    syntax::{SyntaxEngine, SyntaxLanguage, SyntaxRequest},
    workers::WorkPriority,
};
use forge_git::{
    content::{ContentRequest, ContentResult},
    snapshot::{PathState, WorktreeStamp},
};

pub(super) struct ReviewExcerpt {
    block: Vec<BufferBlock>,
    target: HashMap<TargetId, usize>,
    anchor: usize,
}

impl ReviewExcerpt {
    pub(super) fn prepend(&self, blocks: &mut Vec<BufferBlock>) -> Result<()> {
        for block in blocks.iter_mut() {
            block.metadata.target.push(TargetRange {
                id: TargetId("walkthrough:review:anchor".into()),
                range: TextRange {
                    start: TextPosition { row: 0, column: 0 },
                    end: TextPosition {
                        row: block.text.row_count(),
                        column: 0,
                    },
                },
            });
        }
        blocks.splice(0..0, self.block.clone());
        let retained = blocks
            .iter()
            .map(|block| {
                block.text.byte_count() * 2
                    + 4096
                    + (block.metadata.decoration.len() + block.metadata.visible_decoration.len())
                        * 256
            })
            .sum::<usize>();
        ensure!(
            retained <= 16 * 1024 * 1024,
            "Walkthrough review exceeds 16 MiB"
        );
        Ok(())
    }
}

impl WalkthroughService {
    pub async fn project_review(
        &self,
        input: &DocumentInput,
        annotation_id: &DocumentId,
        change: &mut WalkthroughChange,
        diff: &Arc<DiffEngine>,
        syntax: &Arc<SyntaxEngine>,
    ) -> Result<()> {
        let owner = self.get(&input.document)?;
        let (repository, observed, old_side) = {
            let document = owner.lock().expect("walkthrough document");
            document.admission.check()?;
            let &(task, subtask, item) = document
                .target
                .get(input.target.as_ref().context("missing target")?)
                .context("unknown Walkthrough change")?;
            (
                Arc::clone(&document.repository),
                document
                    .observation
                    .path
                    .iter()
                    .find(|observed| observed.change.path == change.path)
                    .cloned(),
                matches!(
                    document.artifact.tasks[task].subtasks[subtask].changes[item].action,
                    schema::Action::Remove
                ),
            )
        };
        let mut excerpt = ReviewExcerpt {
            block: Vec::new(),
            target: HashMap::new(),
            anchor: change.row,
        };
        let width = WidthProfile::default();
        let pair = if !change.stale {
            if let Some(observed) = observed {
                let old = match &observed.change.state {
                    PathState::Tracked { head, .. } if !head.object.is_null() => {
                        acquired_source(
                            &repository,
                            &self.store,
                            ContentSource::Object(head.object),
                        )
                        .await?
                    }
                    PathState::Untracked | PathState::Tracked { .. } => {
                        SourceVersion::new(Vec::new(), Representation::GitCanonical)?
                    }
                    _ => anyhow::bail!("Walkthrough review requires a resolved file"),
                };
                let new = if old_side {
                    if matches!(observed.worktree, WorktreeStamp::Missing) {
                        SourceVersion::new(Vec::new(), Representation::Raw)?
                    } else {
                        let content = repository
                            .content(
                                &self.store,
                                ContentRequest {
                                    source: ContentSource::Worktree {
                                        path: change.path.clone(),
                                        conversion: WorktreeConversion::Raw,
                                    },
                                    limits: Default::default(),
                                    expected: None,
                                },
                            )
                            .await?
                            .value;
                        let ContentResult::Ready(content) = content else {
                            anyhow::bail!("Walkthrough worktree is unavailable: {content:?}")
                        };
                        ensure!(
                            matches!(&content.origin, ContentOrigin::Worktree { stamp, .. } if stamp == &observed.worktree),
                            "Walkthrough source changed after opening; reopen the walkthrough before navigating"
                        );
                        content.source
                    }
                } else {
                    change.source.clone()
                };
                Some(SourcePair { old, new })
            } else {
                None
            }
        } else {
            None
        };
        if let Some(pair) = pair {
            let analysis = diff
                .compare(DiffRequest {
                    source: pair,
                    priority: WorkPriority::Visible,
                })
                .await
                .map_err(|failure| anyhow::anyhow!("Walkthrough diff unavailable: {failure:?}"))?;
            let side = if old_side {
                SourceSide::Old
            } else {
                SourceSide::New
            };
            let mut old_syntax = None;
            let mut new_syntax = None;
            if let Some(language) = SyntaxLanguage::for_path(&change.path.display_label()) {
                let (old, new) = tokio::join!(
                    syntax.analyze(SyntaxRequest {
                        source: analysis.source().old.clone(),
                        language,
                        priority: WorkPriority::Visible,
                        deadline: None
                    }),
                    syntax.analyze(SyntaxRequest {
                        source: analysis.source().new.clone(),
                        language,
                        priority: WorkPriority::Visible,
                        deadline: None
                    })
                );
                match (old, new) {
                    (Ok(old), Ok(new)) => {
                        old_syntax = Some(old);
                        new_syntax = Some(new);
                    }
                    (old, new) => push(
                        &mut excerpt.block,
                        "review:syntax",
                        &format!("Syntax unavailable: {:?} {:?}", old.err(), new.err()),
                        &width,
                    )?,
                }
            }
            let mut cursor = DisplayCursor::at_source_line(analysis, 3, side, change.row);
            let chunk = cursor.next_rows(ChunkLimits::default());
            if let Some(group) = cursor.group(0) {
                push(
                    &mut excerpt.block,
                    "review:hunk",
                    &format!(
                        "@@ -{},{} +{},{} @@",
                        group.old_lines.start + 1,
                        group.old_lines.len(),
                        group.new_lines.start + 1,
                        group.new_lines.len()
                    ),
                    &width,
                )?;
                color_block(excerpt.block.last_mut().unwrap(), "ForgeHunkHeader");
            }
            let anchor_visible = chunk.rows.iter().any(|row| {
                (if old_side { row.old } else { row.new })
                    .is_some_and(|coordinate| coordinate.line == change.row)
            });
            if anchor_visible {
                for (index, display) in chunk.rows.iter().enumerate() {
                    let mut block = BufferBlock {
                        id: BlockId(format!("walkthrough:review:row:{index}")),
                        text: BufferText::from_rows([display.text.as_str()])?,
                        metadata: BlockMetadata::default(),
                    };
                    forge_diff::projection::append_display_row(
                        &mut block.metadata,
                        0,
                        display,
                        cursor
                            .group(display.group_index)
                            .context("missing review hunk")?,
                        old_syntax.as_ref(),
                        new_syntax.as_ref(),
                    )?;
                    if let Some(coordinate) = if old_side { display.old } else { display.new } {
                        source_target(&mut excerpt, &mut block, coordinate.line);
                    }
                    excerpt.block.push(block);
                }
                if chunk.state != DisplayState::Complete {
                    push(
                        &mut excerpt.block,
                        "review:bounded",
                        "Diff excerpt is limited to 256 rows and 128 KiB. Open source for the full captured file.",
                        &width,
                    )?;
                }
            } else {
                push(
                    &mut excerpt.block,
                    "review:context",
                    "Captured source at the annotation. The changed hunk is outside this bounded excerpt.",
                    &width,
                )?;
                source_excerpt(&mut excerpt, &change.source, change.row)?;
            }
        } else {
            push(
                &mut excerpt.block,
                "review:context",
                if change.stale {
                    "Captured source from the artifact revision."
                } else {
                    "Captured source. No working-tree diff was observed for this file."
                },
                &width,
            )?;
            source_excerpt(&mut excerpt, &change.source, change.row)?;
        }
        let mut document = owner.lock().expect("walkthrough document");
        document.admission.check()?;
        ensure!(
            document.buffer.revision() == input.revision
                && document.input.get(&input.view) == Some(&input.sequence),
            "Walkthrough review input was superseded"
        );
        let annotation = document
            .annotation
            .get_mut(annotation_id)
            .context("Walkthrough review was closed")?;
        let mut blocks = project_annotation(
            &annotation.content,
            &annotation.anchor,
            annotation.stale,
            &width,
        )?;
        excerpt.prepend(&mut blocks)?;
        annotation.buffer = BufferDocument::new(annotation_id.clone(), blocks)?;
        annotation.review = Some(excerpt);
        change.annotation = annotation.buffer.snapshot();
        Ok(())
    }

    pub fn annotation_source(&self, parent: &DocumentId, input: DocumentInput) -> Result<usize> {
        input.validate()?;
        ensure!(
            input.action == "open",
            "Walkthrough review has no mutation actions"
        );
        let owner = self.get(parent)?;
        let mut document = owner.lock().expect("walkthrough document");
        document.admission.check()?;
        let annotation = document
            .annotation
            .get_mut(&input.document)
            .context("unknown Walkthrough review")?;
        ensure!(
            annotation.buffer.revision() == input.revision,
            "Walkthrough review input uses a stale revision"
        );
        let previous = annotation
            .input
            .get_mut(&input.view)
            .context("Walkthrough review view is not registered")?;
        ensure!(
            *previous < input.sequence,
            "Walkthrough review input was superseded"
        );
        let block = annotation
            .buffer
            .block(&input.block)
            .context("Walkthrough review block is unavailable")?;
        let row = block
            .text
            .row(input.position.row)
            .context("Walkthrough review row is unavailable")?;
        ensure!(
            row.is_char_boundary(input.position.column),
            "Walkthrough review byte position is invalid"
        );
        let target = input
            .target
            .as_ref()
            .context("This row has no captured source coordinate")?;
        ensure!(
            block.metadata.target.iter().any(|range| &range.id == target
                && range.range.start <= input.position
                && input.position < range.range.end),
            "Walkthrough review target is not at the captured position"
        );
        let review = annotation
            .review
            .as_ref()
            .context("annotation is not a source review")?;
        let source_row = if target.0 == "walkthrough:review:anchor" {
            review.anchor
        } else {
            *review
                .target
                .get(target)
                .context("unknown Walkthrough source coordinate")?
        };
        *previous = input.sequence;
        Ok(source_row)
    }
}

async fn acquired_source(
    repository: &Arc<RepositoryState>,
    store: &RepositoryStore,
    source: ContentSource,
) -> Result<SourceVersion> {
    match repository
        .content(
            store,
            ContentRequest {
                source,
                limits: Default::default(),
                expected: None,
            },
        )
        .await?
        .value
    {
        ContentResult::Ready(content) => Ok(content.source),
        other => anyhow::bail!("Walkthrough baseline is unavailable: {other:?}"),
    }
}

fn source_target(excerpt: &mut ReviewExcerpt, block: &mut BufferBlock, row: usize) {
    let target = TargetId(format!("walkthrough:review:source:{row}"));
    excerpt.target.insert(target.clone(), row);
    block.metadata.target.push(TargetRange {
        id: target,
        range: TextRange {
            start: TextPosition { row: 0, column: 0 },
            end: TextPosition { row: 1, column: 0 },
        },
    });
}

fn source_excerpt(
    excerpt: &mut ReviewExcerpt,
    source: &SourceVersion,
    anchor: usize,
) -> Result<()> {
    for (row, text) in source
        .text()
        .split_terminator('\n')
        .enumerate()
        .skip(anchor.saturating_sub(3))
        .take(7)
    {
        let text = text.strip_suffix('\r').unwrap_or(text);
        let mut block = BufferBlock {
            id: BlockId(format!("walkthrough:review:context:{row}")),
            text: BufferText::from_rows([text])?,
            metadata: BlockMetadata::default(),
        };
        source_target(excerpt, &mut block, row);
        excerpt.block.push(block);
    }
    Ok(())
}
