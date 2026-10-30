use super::*;
use forge_buffer::block::{BlockMetadata, SourceAnchor};
use forge_diff::{
    display::{ChunkLimits, DisplayCursor, DisplayState},
    engine::{DiffEngine, DiffRequest},
    source::{Representation, SourcePair, SourceSide, SourceVersion},
    source_projection::{SourceDiffElement, SourceDiffProjection},
    syntax::{SyntaxEngine, SyntaxLanguage, SyntaxRequest},
    view::{DiffViewAction, DiffViewOptions},
    workers::WorkPriority,
};
use forge_git::{
    content::{ContentRequest, ContentResult},
    snapshot::{PathState, WorktreeStamp},
};

#[derive(Clone)]
pub(super) struct ReviewExcerpt {
    block: Vec<BufferBlock>,
    target: HashMap<TargetId, usize>,
    anchor: usize,
    projection: Option<SourceDiffProjection>,
    section: HashMap<FoldId, String>,
    side: SourceSide,
    original: Option<(Vec<BufferBlock>, HashMap<TargetId, usize>)>,
    operation: Arc<tokio::sync::Mutex<()>>,
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
        Ok(())
    }

    fn render(&mut self, options: DiffViewOptions) -> Result<()> {
        if options == DiffViewOptions::default()
            && let Some((block, target)) = &self.original
        {
            self.block = block.clone();
            self.target = target.clone();
            self.section.clear();
            return Ok(());
        }
        let Some(projection) = &self.projection else {
            return Ok(());
        };
        let side = self.side;
        let mut target = HashMap::new();
        let mut section = HashMap::new();
        let mut block = projection.render("walkthrough:review", |element, block| {
            let coordinate = match element {
                SourceDiffElement::Row(row) => match side {
                    SourceSide::Old => row.old_coordinate().map(|coordinate| coordinate.line),
                    SourceSide::New => row.new_coordinate().map(|coordinate| coordinate.line),
                },
                SourceDiffElement::Declaration(signature) => {
                    if let Some(owner) = &block.metadata.content_node {
                        section.insert(owner.clone(), signature.identity.clone());
                    }
                    match side {
                        SourceSide::Old => signature.old_line,
                        SourceSide::New => signature.new_line,
                    }
                }
                SourceDiffElement::Hunk(_) => None,
            };
            if let Some(row) = coordinate {
                let id = TargetId(format!("{}:source:{row}", block.id.0));
                target.insert(id.clone(), row);
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
        if projection.more() {
            block.push(BufferBlock {
                id: BlockId("walkthrough:review:more".into()),
                text: BufferText::from_rows(["Additional diff rows are available"])?,
                metadata: BlockMetadata::default(),
            });
        }
        self.block = block;
        self.target = target;
        self.section = section;
        Ok(())
    }
}

#[derive(Serialize)]
/// Publishes a prepared review patch and its controls as one native result.
pub struct ReviewDelivery {
    pub patch: Option<BufferPatch>,
    pub diff_view: DiffViewOptions,
    pub more: bool,
}

#[derive(Serialize)]
/// Recovers the review buffer and its committed controls from the same document revision.
pub struct ReviewSnapshot {
    #[serde(flatten)]
    pub snapshot: BufferSnapshot,
    pub diff_view: DiffViewOptions,
    pub more: bool,
}

impl WalkthroughService {
    /// Capture a review snapshot without reacquiring its historical source or changing its layout.
    /// Closed parents and retired annotation identities return an error.
    pub fn review_snapshot(
        &self,
        parent: &DocumentId,
        annotation: &DocumentId,
    ) -> Result<ReviewSnapshot> {
        let owner = self.get(parent)?;
        let document = owner.lock().expect("walkthrough document");
        document.admission.check()?;
        let annotation = document
            .annotation
            .get(annotation)
            .context("unknown Walkthrough review")?;
        Ok(ReviewSnapshot {
            snapshot: annotation.buffer.snapshot(),
            diff_view: annotation.diff_view.committed(),
            more: annotation.diff_view.committed() != DiffViewOptions::default()
                && annotation
                    .review
                    .as_ref()
                    .and_then(|review| review.projection.as_ref())
                    .is_some_and(|projection| projection.more()),
        })
    }

    /// Prepare controls or deferred source rows outside the document lock, then publish atomically.
    /// Invalid inputs, closed annotations, and preparation failures preserve the usable projection.
    pub async fn review_input(
        &self,
        parent: &DocumentId,
        input: DocumentInput,
        diff: &Arc<DiffEngine>,
    ) -> Result<ReviewDelivery> {
        let owner = self.get(parent)?;
        let (request, identity, operation) = {
            let mut document = owner.lock().expect("walkthrough document");
            document.admission.check()?;
            let annotation = document
                .annotation
                .get_mut(&input.document)
                .context("unknown Walkthrough review")?;
            validate_review_input(annotation, &input)?;
            let review = annotation
                .review
                .as_ref()
                .context("annotation is not a source review")?;
            let identity = annotation
                .buffer
                .block(&input.block)
                .and_then(|block| block.metadata.content_node.as_ref())
                .and_then(|owner| review.section.get(owner))
                .cloned();
            let identity = if matches!(
                input.action.as_str(),
                "source_expand" | "source_collapse" | "demand"
            ) {
                identity
            } else {
                None
            };
            let operation = review.operation.clone();
            let request = match input.action.as_str() {
                "toggle_visibility" | "toggle_presentation" => Some(
                    annotation
                        .diff_view
                        .request(if input.action == "toggle_visibility" {
                            DiffViewAction::ToggleVisibility
                        } else {
                            DiffViewAction::TogglePresentation
                        })
                        .map_err(|failure| {
                            anyhow::anyhow!("Walkthrough diff control failed: {failure:?}")
                        })?,
                ),
                "source_expand" | "source_collapse" => {
                    ensure!(identity.is_some(), "Walkthrough source section is missing");
                    None
                }
                "demand" => None,
                _ => anyhow::bail!("unsupported Walkthrough review input"),
            };
            annotation.input.insert(input.view.clone(), input.sequence);
            (request, identity, operation)
        };
        let pool = diff.analysis_pool();
        let (sender, receiver) = tokio::sync::oneshot::channel();
        tokio::spawn(async move {
            let _operation = operation.lock().await;
            let result = async {
                let (mut excerpt, mut buffer, content, anchor, stale, width, options) = {
                    let document = owner.lock().expect("walkthrough document");
                    document.admission.check()?;
                    let annotation = document
                        .annotation
                        .get(&input.document)
                        .context("Walkthrough review was closed")?;
                    if request.is_some_and(|request| annotation.diff_view.requested() != request) {
                        return Ok(review_delivery(annotation, None));
                    }
                    ensure!(
                        request.is_some() || annotation.buffer.revision() == input.revision,
                        "Walkthrough source expansion was superseded"
                    );
                    (
                        annotation
                            .review
                            .as_ref()
                            .context("Walkthrough review disappeared")?
                            .clone(),
                        annotation.buffer.clone(),
                        annotation.content.clone(),
                        annotation.anchor.clone(),
                        annotation.stale,
                        annotation.views.profile().cloned().unwrap_or_default(),
                        request.map_or(annotation.diff_view.committed(), |request| request.options),
                    )
                };
                let base = buffer.revision();
                let bytes = excerpt
                    .block
                    .iter()
                    .map(|block| block.text.byte_count())
                    .sum();
                let permit = pool
                    .reserve(
                        WorkPriority::Foreground,
                        forge_diff::workers::WorkBudget::new(bytes, None),
                    )
                    .map_err(|failure| {
                        anyhow::anyhow!("Walkthrough projection admission failed: {failure:?}")
                    })?;
                let ticket = permit.ticket();
                let (prepared_sender, prepared_receiver) = tokio::sync::oneshot::channel();
                let action = input.action.clone();
                permit.submit(move |_| {
                    let prepared = (|| -> Result<_> {
                        if let Some(projection) = &excerpt.projection {
                            let mut projection = projection.reconfigure(options)?;
                            let limits = ChunkLimits {
                                rows: 128,
                                ..Default::default()
                            };
                            if let Some(identity) = identity {
                                projection.declaration(
                                    &identity,
                                    action != "source_collapse",
                                    limits,
                                )?;
                            } else if !projection.materialized() || action == "demand" {
                                projection.demand(limits)?;
                            }
                            excerpt.projection = Some(projection);
                        }
                        excerpt.render(options)?;
                        let mut blocks = project_annotation(&content, &anchor, stale, &width)?;
                        excerpt.prepend(&mut blocks)?;
                        let patch = buffer.splice(0..buffer.block_count(), blocks)?;
                        Ok((excerpt, buffer, patch))
                    })();
                    let _ = prepared_sender.send(prepared);
                });
                let prepared = prepared_receiver
                    .await
                    .context("Walkthrough projection worker ended");
                ticket.completed().await;
                let (excerpt, buffer, patch) = prepared??;
                let mut document = owner.lock().expect("walkthrough document");
                document.admission.check()?;
                let annotation = document
                    .annotation
                    .get_mut(&input.document)
                    .context("Walkthrough review was closed")?;
                if request.is_some_and(|request| annotation.diff_view.requested() != request) {
                    return Ok(review_delivery(annotation, None));
                }
                ensure!(
                    annotation.buffer.revision() == base,
                    "Walkthrough layout changed during projection preparation"
                );
                if let Some(request) = request {
                    ensure!(
                        annotation.diff_view.commit(request),
                        "Walkthrough diff control was superseded"
                    );
                }
                annotation.review = Some(excerpt);
                annotation.buffer = buffer;
                Ok(review_delivery(annotation, patch))
            }
            .await;
            if result.is_err()
                && let Some(request) = request
            {
                let mut document = owner.lock().expect("walkthrough document");
                if let Some(annotation) = document.annotation.get_mut(&input.document) {
                    annotation.diff_view.fail(request);
                }
            }
            let _ = sender.send(result);
        });
        receiver
            .await
            .context("Walkthrough projection task ended")?
    }

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
            projection: None,
            section: HashMap::new(),
            side: if old_side {
                SourceSide::Old
            } else {
                SourceSide::New
            },
            original: None,
            operation: Arc::new(tokio::sync::Mutex::new(())),
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
            let presenter = forge_diff::presentation::DiffPresenter::new(Default::default());
            excerpt.projection = Some(SourceDiffProjection::new(
                analysis.clone(),
                old_syntax.clone(),
                new_syntax.clone(),
                Default::default(),
            ));
            let navigation_scope = excerpt
                .projection
                .as_ref()
                .expect("prepared review comparison")
                .navigation_scope("walkthrough:review");
            let mut cursor = DisplayCursor::at_source_line(analysis, 3, side, change.row);
            if let Some(group) = cursor.group(0) {
                let mut heading = presenter.hunk(
                    BlockId("review:hunk".into()),
                    group,
                    forge_diff::presentation::HunkHeading::Ranges,
                )?;
                heading.metadata.source_anchor = Some(SourceAnchor {
                    owner: navigation_scope.clone(),
                    before: (!group.old_lines.is_empty()).then(|| group.old_lines.clone()),
                    after: (!group.new_lines.is_empty()).then(|| group.new_lines.clone()),
                });
                excerpt.block.push(heading);
            }
            let anchor_visible = cursor.group(0).is_some_and(|group| {
                (if old_side {
                    &group.old_lines
                } else {
                    &group.new_lines
                })
                .contains(&change.row)
            });
            if anchor_visible {
                let mut row_index = 0;
                loop {
                    owner
                        .lock()
                        .expect("walkthrough document")
                        .admission
                        .check()?;
                    let chunk = cursor.next_rows(ChunkLimits::default());
                    for display in &chunk.rows {
                        let mut block = presenter.rows(
                            BlockId(format!("walkthrough:review:row:{row_index}")),
                            std::slice::from_ref(display),
                            cursor
                                .group(display.group_index)
                                .context("missing review hunk")?,
                            old_syntax.as_ref(),
                            new_syntax.as_ref(),
                        )?;
                        if !display.is_gap() {
                            block.metadata.source_anchor = Some(SourceAnchor {
                                owner: navigation_scope.clone(),
                                before: display
                                    .old_coordinate()
                                    .map(|coordinate| coordinate.line..coordinate.line + 1),
                                after: display
                                    .new_coordinate()
                                    .map(|coordinate| coordinate.line..coordinate.line + 1),
                            });
                        }
                        if let Some(coordinate) = if old_side {
                            display.old_coordinate()
                        } else {
                            display.new_coordinate()
                        } {
                            source_target(&mut excerpt, &mut block, coordinate.line);
                        }
                        excerpt.block.push(block);
                        row_index += 1;
                    }
                    if chunk.state == DisplayState::Complete {
                        break;
                    }
                    tokio::task::yield_now().await;
                }
            } else {
                push(
                    &mut excerpt.block,
                    "review:context",
                    "Captured source at the annotation. The nearest changed hunk does not contain this source line.",
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
        excerpt.original = Some((excerpt.block.clone(), excerpt.target.clone()));
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
        validate_review_input(annotation, &input)?;
        let target = input
            .target
            .as_ref()
            .context("This row has no captured source coordinate")?;
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
        annotation.input.insert(input.view, input.sequence);
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

fn validate_review_input(annotation: &WalkthroughAnnotation, input: &DocumentInput) -> Result<()> {
    input.validate()?;
    ensure!(
        annotation.buffer.revision() == input.revision,
        "Walkthrough review input uses a stale revision"
    );
    ensure!(
        annotation
            .input
            .get(&input.view)
            .is_some_and(|previous| input.sequence > *previous),
        "Walkthrough review view is closed or its input is stale"
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
    ensure!(
        block.target_at(input.position) == input.target.as_ref(),
        "Walkthrough review target is not at the captured position"
    );
    Ok(())
}

fn review_delivery(
    annotation: &WalkthroughAnnotation,
    patch: Option<BufferPatch>,
) -> ReviewDelivery {
    ReviewDelivery {
        patch,
        diff_view: annotation.diff_view.committed(),
        more: annotation.diff_view.committed() != DiffViewOptions::default()
            && annotation
                .review
                .as_ref()
                .and_then(|review| review.projection.as_ref())
                .is_some_and(|projection| projection.more()),
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
