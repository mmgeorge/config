use std::collections::{HashMap, HashSet};
use std::time::Instant;

use anyhow::{Context, Result};
use forge_buffer::block::{
    BlockAnchor, BlockMetadata, BufferBlock, Decoration, TargetRange, TextChunk, TextPosition,
    TextRange,
};
use forge_buffer::identity::{BlockId, EditSequence, FoldId, RegionRevision, TargetId};
use forge_buffer::text::BufferText;
use forge_buffer::width::WidthProfile;
use forge_diff::display::{DisplayRow, DisplaySource, RowCoordinates, RowKind};
use forge_diff::file_header::FileChange;
use forge_diff::intraline::{IntralineResult, compare_replacement};
use forge_diff::presentation::{FoldPolicy, RowChange};
use forge_diff::source::SourceSide;
use forge_diff::syntax::{
    AlgorithmDiffLineKind, DeclarationFilter, DeclarationOverview, DeclarationVisibility,
    classify_algorithm_diff_line,
};

use super::review_annotation::ReviewAnnotation;
use super::review_source::ReviewTrace;
use super::{
    PlanDocument, PlanNavigationAnchor, PlanNavigationIndex, PlanReviewTarget, RenderedPlan,
};

pub(super) struct ReviewAnnotationView<'view> {
    pub(super) annotation: &'view [ReviewAnnotation],
    pub(super) revision: &'view HashMap<String, (RegionRevision, EditSequence)>,
    pub(super) focused: Option<&'view str>,
}

struct DesignReviewRows {
    block: Vec<BufferBlock>,
    navigation: PlanNavigationIndex,
    hidden: HashSet<BlockId>,
    contextual: HashSet<BlockId>,
    skipped: HashMap<BlockId, i64>,
    walkthrough: WalkthroughPlacement,
}

/// Frame placement is separate from canonical rows used by reviewer annotations.
#[derive(Default)]
struct WalkthroughPlacement {
    before: HashMap<BlockId, Vec<usize>>,
    after_heading: HashMap<BlockId, usize>,
    documentation: HashMap<BlockId, Vec<BlockId>>,
}

/// Identify the projected block for a zero-based step.
pub(super) fn walkthrough_block(index: usize) -> BlockId {
    BlockId(format!("plan:walkthrough:{}", index + 1))
}

const FILE_FOLD: FoldPolicy = FoldPolicy {
    closed: true,
    collapse_children: false,
    expand_children: false,
};

fn file_heading(
    presenter: &forge_diff::presentation::DiffPresenter,
    id: BlockId,
    change: FileChange,
    label: &str,
    delta: Option<super::design::DeclarationDelta>,
) -> Result<BufferBlock> {
    let mut chunks = change.header(label, None, false);
    if let Some(delta) = delta {
        for (count, marker, capture) in [
            (delta.added, '+', "ForgeAddRange"),
            (delta.modified, '~', "ForgeModifyRange"),
            (delta.removed, '-', "ForgeDeleteRange"),
        ] {
            if count > 0 {
                chunks.push(TextChunk {
                    text: format!(" {marker}{count}"),
                    capture: capture.into(),
                });
            }
        }
    }
    Ok(presenter.heading(id, chunks)?)
}

pub(super) fn render(document: &PlanDocument) -> Result<RenderedPlan> {
    let DesignReviewRows {
        block, navigation, ..
    } = rows(
        document,
        &HashMap::new(),
        DeclarationFilter::default(),
        false,
        None,
        &HashSet::new(),
        None,
        None,
    )?;
    let markdown = block
        .iter()
        .flat_map(|block| block.text.wire_rows())
        .map(|row| format!("{row}\n"))
        .collect::<String>();
    Ok(RenderedPlan {
        markdown,
        navigation,
    })
}

/// Locates the canonical rows owned by the test section, excluding its separator.
pub(super) fn test_source_range(rendered: &RenderedPlan) -> Result<(usize, usize)> {
    let mut lines = rendered
        .navigation
        .anchor
        .iter()
        .filter(|anchor| {
            matches!(
                anchor.target,
                PlanReviewTarget::TestCase { .. }
                    | PlanReviewTarget::Section {
                        section: super::PlanSection::Tests
                    }
            )
        })
        .map(|anchor| anchor.line as usize);
    let first = lines.next().context("saved plan has no test section")?;
    let (mut start, mut end) = (first, first);
    for line in lines {
        start = start.min(line);
        end = end.max(line);
    }
    // The inventory always owns a blank row after its heading, even without cases.
    Ok((start - 1, end.max(start + 1)))
}

/// The saved heading fixes the canonical source offset of a requested subtree.
#[derive(Clone, Copy)]
pub(super) struct FileBody<'a> {
    pub id: &'a str,
    pub path: &'a str,
    pub first_line: u32,
}

pub(super) fn project(
    document: &PlanDocument,
    width: &WidthProfile,
    annotations: ReviewAnnotationView<'_>,
    syntax: &HashMap<(String, String), super::review_source::ReviewSyntax>,
    filter: DeclarationFilter,
    trace: Option<&ReviewTrace>,
    revealed: &HashSet<(String, String, u32)>,
    files: Option<&super::review_files::ReviewFiles>,
    body: Option<FileBody<'_>>,
) -> Result<(Vec<BufferBlock>, HashMap<TargetId, PlanNavigationAnchor>)> {
    let ReviewAnnotationView {
        annotation,
        revision,
        focused,
    } = annotations;
    let DesignReviewRows {
        block: mut source,
        navigation,
        mut hidden,
        contextual,
        skipped,
        mut walkthrough,
    } = rows(document, syntax, filter, true, trace, revealed, files, body)?;
    let check_rows = if body.is_none() {
        review_checks(document.design.as_ref(), Some(width))?
    } else {
        Vec::new()
    };
    let mut target = HashMap::new();
    let design = document
        .design
        .as_ref()
        .context("declaration review has no design")?;
    // Decorate each full Markdown region so fences and multiline spans retain their context.
    for markdown in source
        .split_mut(|row| !row.metadata.markdown)
        .filter(|rows| !rows.is_empty())
    {
        let text = markdown
            .iter()
            .map(|row| row.text.row(0).unwrap_or_default())
            .collect::<Vec<_>>()
            .join("\n");
        let rendered =
            forge_buffer::markdown::MarkdownRenderer::source(markdown[0].id.clone(), &text, width)?;
        for mut decoration in rendered.block.metadata.decoration {
            let row = decoration.range.start.row;
            decoration.range.start.row = 0;
            decoration.range.end.row = 0;
            markdown[row].metadata.decoration.push(decoration);
        }
    }
    let narrative_spans = if body.is_none() {
        super::review_symbols::spans(design)
    } else {
        HashMap::new()
    };
    let needs_symbols = !narrative_spans.is_empty()
        || navigation.anchor.iter().any(|anchor| {
            matches!(
                anchor.target,
                PlanReviewTarget::Call {
                    kind: super::CallKind::Value | super::CallKind::Construct,
                    ..
                }
            )
        });
    let uncached;
    let narrative_symbols = if !needs_symbols {
        uncached = super::review_symbols::NarrativeSymbols::default();
        &uncached
    } else if let Some(files) = files {
        files.symbols(design, trace)?
    } else {
        uncached = super::review_symbols::NarrativeSymbols::new(design, std::path::Path::new(""))?;
        &uncached
    };
    let mut block = Vec::new();
    let mut source_end = HashMap::new();
    let mut omitted = 0;
    let mut deferred_rows = body.map_or(0, |file| i64::from(file.first_line.saturating_sub(1)));
    for (index, mut row) in source.into_iter().enumerate() {
        if let Some(index) = row
            .id
            .0
            .strip_prefix("plan:configured-checks:")
            .and_then(|index| index.parse::<usize>().ok())
        {
            row.text = check_rows[index].text.clone();
            row.metadata.decoration = check_rows[index].metadata.decoration.clone();
        } else if !row.metadata.markdown
            && row.id.0.starts_with("plan:metadata:")
            && !row.id.0.starts_with("plan:metadata:checks:")
        {
            row = forge_buffer::markdown::MarkdownRenderer::source(
                row.id.clone(),
                &row.text.wire_rows().join("\n"),
                width,
            )?
            .block;
            row.metadata.markdown = true;
            if row.id.0.starts_with("plan:metadata:decisions:") {
                for decoration in &mut row.metadata.decoration {
                    if decoration.capture == "@markup.strong" {
                        decoration.capture = "ForgeStatusHeader".into();
                    }
                }
            }
        }
        deferred_rows += skipped.get(&row.id).copied().unwrap_or(0);
        let context = contextual.contains(&row.id);
        if context {
            omitted += 1;
        }
        let line = index as u32 + 1;
        let first = navigation
            .anchor
            .partition_point(|anchor| anchor.line < line);
        let line_anchor = &navigation.anchor[first
            ..navigation
                .anchor
                .partition_point(|anchor| anchor.line <= line)];
        if let Some(anchor) = line_anchor.last() {
            // An anchor with columns marks one of several targets on its row.
            let entry_span = line_anchor
                .iter()
                .filter_map(|anchor| {
                    anchor
                        .columns
                        .map(|[start, end]| ((*anchor).clone(), start as usize, end as usize))
                })
                .collect::<Vec<_>>();
            if matches!(
                anchor.target,
                PlanReviewTarget::Call { .. } | PlanReviewTarget::Behavior { .. }
            ) && !row
                .metadata
                .visible_decoration
                .iter()
                .any(|decoration| decoration.capture.ends_with(".algorithm"))
            {
                let text = row.text.row(0).unwrap_or_default().to_owned();
                if entry_span.is_empty() {
                    super::review_projection::append_semantic_style(
                        &mut row.metadata,
                        &anchor.target,
                        &text,
                    );
                    restyle_narrative_reference(
                        &mut row.metadata,
                        &anchor.target,
                        narrative_symbols,
                    );
                } else {
                    for (entry, start, end) in &entry_span {
                        super::review_projection::append_semantic_span(
                            &mut row.metadata,
                            &entry.target,
                            &text,
                            *start..*end,
                        );
                        restyle_narrative_reference(
                            &mut row.metadata,
                            &entry.target,
                            narrative_symbols,
                        );
                    }
                }
            }
            for (entry, start, end) in &entry_span {
                let id = TargetId(format!("plan:declaration:{}:{start}", row.id.0));
                row.metadata.target.push(TargetRange {
                    id: id.clone(),
                    range: TextRange {
                        start: TextPosition {
                            row: 0,
                            column: *start,
                        },
                        end: TextPosition {
                            row: 0,
                            column: *end,
                        },
                    },
                });
                let mut entry = entry.clone();
                entry.line = if context {
                    0
                } else {
                    (i64::from(entry.line - omitted) + deferred_rows).try_into()?
                };
                target.insert(id, entry);
            }
            let id = TargetId(format!("plan:declaration:{}", row.id.0));
            row.metadata.target.push(TargetRange {
                id: id.clone(),
                range: TextRange {
                    start: TextPosition { row: 0, column: 0 },
                    end: TextPosition {
                        row: row.text.row_count(),
                        column: 0,
                    },
                },
            });
            let mut anchor = anchor.clone();
            anchor.line = if context {
                0
            } else {
                (i64::from(anchor.line - omitted) + deferred_rows).try_into()?
            };
            super::review_symbols::decorate(
                &mut row,
                &anchor,
                &narrative_spans,
                narrative_symbols,
                &mut target,
            );
            target.insert(id, anchor);
        }
        let row_id = row.id.clone();
        for index in walkthrough.before.remove(&row_id).into_iter().flatten() {
            block.push(walkthrough_frame(index, design, width)?);
        }
        source_end.insert(
            row_id.clone(),
            BlockAnchor {
                block: row_id.clone(),
                position: TextPosition {
                    row: row.text.row_count(),
                    column: 0,
                },
            },
        );
        block.push(row);
        if let Some(index) = walkthrough.after_heading.remove(&row_id) {
            let start = block.len() - 1;
            let frame = walkthrough_frame(index, design, width)?;
            source_end.insert(
                row_id.clone(),
                BlockAnchor {
                    block: frame.id.clone(),
                    position: TextPosition {
                        row: frame.text.row_count(),
                        column: 0,
                    },
                },
            );
            block.push(frame);
            if block[start].metadata.fold.is_empty() {
                fold(&mut block, start, &row_id.0, FILE_FOLD)?;
            }
        }
        for annotation in annotation.iter().filter(|annotation| {
            i64::from(annotation.source.end_line) == index as i64 + 1 + deferred_rows
        }) {
            let comment = super::review_projection::annotation_block(
                annotation,
                width,
                focused == Some(annotation.id.as_str()),
                revision.get(&annotation.id).copied().unwrap_or_default(),
            )?;
            source_end.insert(
                row_id.clone(),
                BlockAnchor {
                    block: comment.id.clone(),
                    position: TextPosition {
                        row: comment.text.row_count(),
                        column: 0,
                    },
                },
            );
            block.push(comment);
        }
    }
    for item in &mut block {
        for fold in &mut item.metadata.fold {
            if fold.end.position.row > 0
                && let Some(end) = source_end.get(&fold.end.block)
            {
                fold.end = end.clone();
            }
        }
    }
    block = attach_walkthrough_documentation(
        block,
        &source_end,
        walkthrough.documentation,
        &mut hidden,
    )?;
    if filter.public_only || !hidden.is_empty() {
        block = filtered_blocks(block, &hidden, body.is_none(), filter.public_only)?;
        let visible_target = block
            .iter()
            .flat_map(|block| &block.metadata.target)
            .map(|range| range.id.clone())
            .collect::<HashSet<_>>();
        target.retain(|id, _| visible_target.contains(id));
    }
    Ok((block, target))
}

/// Move detached context rows after assigning their canonical review positions.
fn attach_walkthrough_documentation(
    mut blocks: Vec<BufferBlock>,
    source_end: &HashMap<BlockId, BlockAnchor>,
    mut attachments: HashMap<BlockId, Vec<BlockId>>,
    hidden: &mut HashSet<BlockId>,
) -> Result<Vec<BufferBlock>> {
    if attachments.is_empty() {
        return Ok(blocks);
    }
    let positions: HashMap<_, _> = blocks
        .iter()
        .enumerate()
        .map(|(index, block)| (block.id.clone(), index))
        .collect();
    let mut moved = HashSet::new();
    for docs in attachments.values_mut() {
        let mut span = Vec::new();
        for doc in docs.drain(..) {
            let first = positions[&doc];
            let last = positions[&source_end[&doc].block];
            span.extend(blocks[first..=last].iter().map(|block| block.id.clone()));
        }
        moved.extend(span.iter().cloned());
        *docs = span;
    }

    // Trim the old folds before moving their endpoints to the destination hunk.
    let mut preceding = None;
    let mut replacement = HashMap::new();
    for block in &blocks {
        if moved.contains(&block.id) {
            replacement.insert(block.id.clone(), preceding.clone());
        } else {
            preceding = Some(BlockAnchor {
                block: block.id.clone(),
                position: TextPosition {
                    row: block.text.row_count(),
                    column: 0,
                },
            });
        }
    }
    for block in &mut blocks {
        let owner = positions[&block.id];
        block.metadata.fold.retain_mut(|fold| {
            let Some(end) = replacement.get(&fold.end.block) else {
                return true;
            };
            if let Some(end) = end
                && positions[&end.block] > owner
            {
                fold.end = end.clone();
                return true;
            }
            if fold.id.0 == block.id.0 && block.id.0.contains(":hunk:") {
                hidden.insert(block.id.clone());
            }
            false
        });
    }
    let (detached, remaining): (Vec<_>, Vec<_>) = blocks
        .into_iter()
        .partition(|block| moved.contains(&block.id));
    let mut detached: HashMap<_, _> = detached
        .into_iter()
        .map(|block| (block.id.clone(), block))
        .collect();
    let mut projected = Vec::with_capacity(remaining.len() + detached.len());
    for block in remaining {
        for doc in attachments.remove(&block.id).into_iter().flatten() {
            projected.push(
                detached
                    .remove(&doc)
                    .context("walkthrough documentation row is missing")?,
            );
        }
        projected.push(block);
    }
    anyhow::ensure!(
        detached.is_empty(),
        "walkthrough documentation destination is missing"
    );
    Ok(projected)
}

/// Wrap a read-only comment and decorate its rounded frame in display cells.
fn walkthrough_frame(
    index: usize,
    design: &super::DeclarationDesign,
    width: &WidthProfile,
) -> Result<BufferBlock> {
    let step = &design.document.walkthrough[index];
    let count = format!("{}/{}", index + 1, design.document.walkthrough.len());
    let title = step
        .title
        .as_deref()
        .unwrap_or_default()
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ");
    let title = if title.is_empty() {
        "Walkthrough"
    } else {
        &title
    };
    let heading = format!("╭─ {count} ");
    let heading_overhead = width.cells(&heading, 0)? + width.cells(" ╮", 0)?;
    let prefix = "│ ";
    let suffix = " │";
    let left = width.cells(prefix, 0)?;
    let right = width.cells(suffix, 0)?;
    let mut interior = width.clone();
    let framed = width.columns >= (left + right + 2).max(heading_overhead + 1);
    if framed {
        interior.columns -= left + right;
    }
    let mut block = forge_buffer::markdown::MarkdownRenderer::render(
        walkthrough_block(index),
        &step.comment,
        &interior,
    )?
    .block;
    block.metadata.target.clear();
    if !framed {
        let mut rows = width.wrap_plain(&format!("{count} {title}"), 0)?;
        for decoration in &mut block.metadata.decoration {
            decoration.range.start.row += rows.len();
            decoration.range.end.row += rows.len();
        }
        rows.extend(block.text.rows().map(str::to_owned));
        block.text = BufferText::from_rows(rows)?;
        return Ok(block);
    }
    let edge = |left: &str, right: &str| -> Result<String> {
        let span = width
            .columns
            .saturating_sub(width.cells(left, 0)? + width.cells(right, 0)?);
        let unit = width.cells("─", 0)?;
        Ok(format!(
            "{left}{}{right}",
            "─".repeat(span / unit) + &" ".repeat(span % unit)
        ))
    };
    let title_width = WidthProfile {
        columns: width.columns - heading_overhead,
        ..width.clone()
    };
    let title = title_width.clip_plain(title, "…")?;
    let mut rows = vec![edge(&format!("{heading}{title} "), "╮")?];
    let original = std::mem::take(&mut block.metadata.decoration);
    let mut pending = original.as_slice();
    let wrap_width = WidthProfile {
        columns: width.columns - right,
        ..width.clone()
    };
    for (source_row, text) in block.text.rows().enumerate() {
        let end = pending.partition_point(|decoration| decoration.range.start.row == source_row);
        let decorations;
        (decorations, pending) = pending.split_at(end);
        let wrapped;
        let segments: Vec<&str> = if width.cells(text, left)? <= interior.columns {
            vec![text]
        } else {
            // Markdown keeps preformatted rows whole. Wrap their presentation without losing
            // the syntax spans or counting tab stops from the wrong side of the frame.
            wrapped = wrap_width.wrap_plain(&format!("{}{text}", " ".repeat(left)), left)?;
            wrapped
                .iter()
                .map(|row| &row[left.min(row.len())..])
                .filter(|row| !row.is_empty())
                .collect()
        };
        let mut offset = 0;
        for segment in segments {
            let row = rows.len();
            let end = offset + segment.len();
            for decoration in decorations {
                let start_column = decoration.range.start.column.max(offset);
                let end_column = decoration.range.end.column.min(end);
                if start_column < end_column {
                    let mut decoration = decoration.clone();
                    decoration.range = TextRange {
                        start: TextPosition {
                            row,
                            column: prefix.len() + start_column - offset,
                        },
                        end: TextPosition {
                            row,
                            column: prefix.len() + end_column - offset,
                        },
                    };
                    block.metadata.decoration.push(decoration);
                }
            }
            let padding = interior.columns.saturating_sub(width.cells(segment, left)?);
            rows.push(format!("{prefix}{segment}{}{suffix}", " ".repeat(padding)));
            offset = end;
        }
    }
    rows.push(edge("╰", "╯")?);
    let style = |row, start, end| Decoration {
        range: TextRange {
            start: TextPosition { row, column: start },
            end: TextPosition { row, column: end },
        },
        capture: "ForgePlanWalkthroughFrame".into(),
        priority: 160,
    };
    for (row, text) in rows.iter().enumerate() {
        if row == 0 || row + 1 == rows.len() {
            block.metadata.decoration.push(style(row, 0, text.len()));
        } else {
            block.metadata.decoration.push(style(row, 0, "│".len()));
            block
                .metadata
                .decoration
                .push(style(row, text.len() - "│".len(), text.len()));
        }
    }
    block.text = BufferText::from_rows(rows)?;
    Ok(block)
}

struct WalkthroughAnchor {
    index: usize,
    heading: usize,
    signature: std::ops::RangeInclusive<usize>,
}

#[derive(Default)]
struct WalkthroughPositions {
    steps: HashMap<usize, Vec<usize>>,
    documentation: std::collections::BTreeMap<usize, usize>,
}

/// Locate a file's symbol steps in its rendered declaration coordinates.
fn walkthrough_rows(
    path: &str,
    source: &super::calls::CallPresentation,
    steps: &[(usize, &super::DesignWalkthroughComment)],
) -> Result<(Vec<WalkthroughAnchor>, HashSet<usize>)> {
    if !steps.iter().any(|(_, step)| step.symbol.is_some()) {
        return Ok(Default::default());
    }
    let inventory = DeclarationOverview::review_symbols(path, &source.plain)
        .context("walkthrough declarations are unavailable")?;
    let mut anchors = Vec::new();
    let mut visible = HashSet::new();
    for &(index, step) in steps {
        let Some(name) = &step.symbol else {
            continue;
        };
        let symbol = super::design_artifact::resolve_walkthrough_symbol(&inventory, name)
            .with_context(|| {
                format!("walkthrough declaration `{name}` is unavailable in {path}")
            })?;
        anchors.push(WalkthroughAnchor {
            index,
            heading: source.declaration_row[&(symbol.start - 1)],
            signature: source.declaration_row[&(symbol.signature - 1)]
                ..=source.declaration_row[&(symbol.end - 1)],
        });
        let mut declaration = (symbol.start - 1..symbol.end).collect::<HashSet<_>>();
        for child in &inventory {
            if symbol.start < child.start && child.end < symbol.end {
                for row in child.start - 1..child.end {
                    declaration.remove(&row);
                }
            }
        }
        visible.extend(
            source
                .plain_row
                .iter()
                .enumerate()
                .filter_map(|(row, plain)| declaration.contains(plain).then_some(row)),
        );
    }
    Ok((anchors, visible))
}

/// Select frame positions and relocate unchanged documentation split from its signature.
fn walkthrough_positions(
    diff: &mut super::review_diff::FileDiff,
    side: SourceSide,
    source: &super::calls::CallPresentation,
    anchors: Vec<WalkthroughAnchor>,
) -> WalkthroughPositions {
    if anchors.is_empty() {
        return WalkthroughPositions::default();
    }
    let mut hunk_by_row = HashMap::new();
    for (index, hunk) in diff.hunk.iter().enumerate() {
        for row in &hunk.row {
            if let Some(coordinate) = row.new_coordinate().or_else(|| row.old_coordinate())
                && coordinate.side == side
            {
                hunk_by_row
                    .entry(coordinate.line)
                    .or_insert((index, row.kind()));
            }
        }
    }
    let mut positions = WalkthroughPositions::default();
    let mut missing = Vec::new();
    for anchor in anchors {
        let first = *anchor.signature.start();
        let docs = anchor.heading..first;
        let signature_hunk = hunk_by_row.get(&first).map(|&(hunk, _)| hunk);
        let attached = docs.clone().all(|row| {
            hunk_by_row
                .get(&row)
                .is_some_and(|&(hunk, _)| Some(hunk) == signature_hunk)
        });
        let changed = docs.clone().any(|row| {
            hunk_by_row
                .get(&row)
                .is_some_and(|&(_, kind)| kind != RowKind::Context)
        });
        let row = if attached || changed {
            if !hunk_by_row.contains_key(&anchor.heading) {
                missing.push(anchor.heading);
            }
            anchor.heading
        } else {
            for doc in docs {
                positions.documentation.entry(doc).or_insert(first);
                if !hunk_by_row.contains_key(&doc) {
                    missing.push(doc);
                }
            }
            first
        };
        if signature_hunk.is_none() {
            missing.extend(anchor.signature);
        }
        positions.steps.entry(row).or_default().push(anchor.index);
    }
    diff.reveal_walkthrough(side, source, &missing);
    positions
}

fn rows(
    document: &PlanDocument,
    syntax: &HashMap<(String, String), super::review_source::ReviewSyntax>,
    filter: DeclarationFilter,
    inspection: bool,
    trace: Option<&ReviewTrace>,
    revealed: &HashSet<(String, String, u32)>,
    files: Option<&super::review_files::ReviewFiles>,
    body: Option<FileBody<'_>>,
) -> Result<DesignReviewRows> {
    let design = document
        .design
        .as_ref()
        .context("declaration review has no design")?;
    let parsed = design.parsed()?;
    let mut block = Vec::new();
    let mut display_row = HashMap::<(String, String, usize), usize>::new();
    let mut navigation = PlanNavigationIndex {
        plan_id: document.plan_id.clone(),
        plan_version: document.version,
        anchor: Vec::new(),
        file_visibility: Default::default(),
    };
    let mut skipped = HashMap::new();
    let mut walkthrough = WalkthroughPlacement::default();
    let mut hidden = HashSet::new();
    let mut contextual_path = HashSet::new();
    let mut contextual_block = HashSet::new();
    let mut visibility = HashMap::<(String, String), DeclarationVisibility>::new();
    let mut presentation =
        HashMap::<(String, String), std::sync::Arc<super::calls::CallPresentation>>::new();
    let started = Instant::now();
    let included: HashSet<_> = revealed.iter().map(|(path, _, _)| path.clone()).collect();
    if let Some(body) = body.filter(|body| body.id.starts_with("plan:tests:file:")) {
        let (block, anchors) =
            test_inventory(design, inspection, files, &mut skipped, Some(body.path))?;
        navigation.anchor = anchors;
        return Ok(DesignReviewRows {
            block,
            navigation,
            hidden,
            contextual: contextual_block,
            skipped,
            walkthrough,
        });
    }
    let ordered_files =
        if let Some(body) = body {
            let heading = files.and_then(|files| files.for_path(body.path));
            vec![super::review_file_layout::ReviewFile {
                baseline: heading
                    .map_or(body.path, |heading| &heading.before)
                    .to_owned(),
                proposed: heading
                    .map_or(body.path, |heading| &heading.after)
                    .to_owned(),
            }]
        } else if let Some(inventory) = files {
            let mut ordered: Vec<_> = inventory
                .files
                .iter()
                .map(|file| super::review_file_layout::ReviewFile {
                    baseline: file.before.clone(),
                    proposed: file.after.clone(),
                })
                .collect();
            // A jump may introduce unchanged context. Its lines have no canonical source offset.
            let mut contextual: Vec<_> = included
                .iter()
                .filter(|path| inventory.for_path(path).is_none())
                .collect();
            contextual.sort();
            ordered.extend(contextual.into_iter().map(|path| {
                super::review_file_layout::ReviewFile {
                    baseline: path.clone(),
                    proposed: path.clone(),
                }
            }));
            ordered
        } else {
            super::review_file_layout::order(design, &included)?
        };
    if let Some(trace) = trace {
        trace.record(
            "plan.review.file_order",
            started.elapsed(),
            ordered_files.len(),
        );
    }
    let mut steps_by_path = HashMap::<_, Vec<_>>::new();
    for (index, step) in design.document.walkthrough.iter().enumerate() {
        steps_by_path
            .entry(step.path.as_str())
            .or_default()
            .push((index, step));
    }
    for file in ordered_files {
        let path = file.baseline;
        let destination = file.proposed;
        let steps = steps_by_path
            .get(destination.as_str())
            .map(Vec::as_slice)
            .unwrap_or_default();
        if let Some((inventory, heading)) =
            files.and_then(|inventory| Some((inventory, inventory.for_path(&destination)?)))
            && body.is_none()
            && (!inventory.loaded.contains(&heading.id)
                || (steps.is_empty() && !heading.visibility.visible(filter)))
            && !revealed
                .iter()
                .any(|(file, _, _)| file == &path || file == &destination)
        {
            let start = block.len();
            let change = if path != destination {
                FileChange::Renamed
            } else if !design.baseline.contains_key(&path) {
                FileChange::Added
            } else if !parsed.proposed.contains_key(&destination) {
                FileChange::Deleted
            } else {
                FileChange::Modified
            };
            let label = if path != destination {
                format!("{path} → {destination}")
            } else {
                destination.clone()
            };
            block.push(file_heading(
                &forge_diff::presentation::DiffPresenter::new(Default::default()),
                BlockId(heading.id.clone()),
                change,
                &label,
                heading.delta,
            )?);
            navigation.anchor.push(PlanNavigationAnchor {
                columns: None,
                line: block.len() as u32,
                target: PlanReviewTarget::File {
                    path: destination.clone(),
                },
                json_path: format!("/design/proposed/{}", pointer(&destination)),
                path: Some(destination),
                label,
            });
            let deferred = BlockId(format!("{}:hunk:deferred", heading.id));
            if steps.is_empty() && !heading.visibility.visible(filter) {
                hidden.insert(BlockId(heading.id.clone()));
                hidden.insert(deferred.clone());
            }
            skipped.insert(deferred.clone(), i64::from(heading.rows) - 2);
            block.push(BufferBlock {
                id: deferred,
                text: BufferText::from_rows([""])?,
                metadata: Default::default(),
            });
            fold(&mut block, start, &heading.id, FILE_FOLD)?;
            continue;
        }
        let sides = super::review_diff::ReviewSides::new(design, &path, &destination)?;
        let (walkthrough_side, walkthrough_source) = sides
            .after
            .as_deref()
            .map_or((SourceSide::Old, sides.before.as_deref()), |source| {
                (SourceSide::New, Some(source))
            });
        let (walkthrough_anchors, walkthrough_visible) = match walkthrough_source {
            Some(source) => walkthrough_rows(&destination, source, steps)?,
            None => Default::default(),
        };
        let mut walkthrough_context: Vec<_> = walkthrough_anchors
            .iter()
            .map(|anchor| (walkthrough_side, anchor.heading))
            .collect();
        walkthrough_context.sort_unstable_by_key(|&(_, row)| row);
        walkthrough_context.dedup();
        let explicitly_revealed = revealed
            .iter()
            .any(|(file, side, _)| file == &path && side == "baseline")
            || revealed
                .iter()
                .any(|(file, side, _)| file == &destination && side == "proposed");
        let reference_context = revealed
            .iter()
            .filter_map(|(file, side, line)| {
                if file == &path && side == "baseline" {
                    Some((forge_diff::source::SourceSide::Old, *line))
                } else if file == &destination && side == "proposed" {
                    Some((forge_diff::source::SourceSide::New, *line))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        let mut diff = super::review_diff::FileDiff::new(
            &path,
            &destination,
            sides.before.as_deref(),
            sides.after.as_deref(),
            &walkthrough_context,
            &reference_context,
        )?;
        let mut locations = match walkthrough_source.filter(|_| inspection) {
            Some(source) => {
                walkthrough_positions(&mut diff, walkthrough_side, source, walkthrough_anchors)
            }
            None => WalkthroughPositions::default(),
        };
        if diff.hunk.is_empty()
            && sides.before.is_some()
            && sides.after.is_some()
            && path == destination
            && steps.is_empty()
        {
            continue;
        }
        let file_visibility = match files.and_then(|inventory| inventory.for_path(&destination)) {
            Some(heading) => heading.visibility,
            None => diff.visibility(&sides)?,
        };
        if !inspection {
            navigation
                .file_visibility
                .insert(destination.clone(), file_visibility);
        }
        let hide_file = inspection
            && steps.is_empty()
            && !explicitly_revealed
            && !file_visibility.visible(filter);
        let super::review_diff::ReviewSides { before, after } = sides;
        let signature_replacements = if inspection {
            super::review_alignment::signature_replacements(
                before.as_deref(),
                after.as_deref(),
                &diff,
            )?
        } else {
            HashMap::new()
        };
        if before.as_ref().map(|source| &source.declaration.text)
            == after.as_ref().map(|source| &source.declaration.text)
            && path == destination
            && explicitly_revealed
        {
            contextual_path.insert(path.clone());
        }
        if let Some(before) = before {
            if inspection {
                visibility.insert(
                    (path.clone(), "baseline".into()),
                    super::calls::visibility(
                        &path,
                        &before,
                        if revealed
                            .iter()
                            .any(|(file, side, _)| file == &path && side == "baseline")
                        {
                            DeclarationFilter::ALL
                        } else {
                            filter
                        },
                    )?
                    .clone(),
                );
            }
            presentation.insert((path.clone(), "baseline".into()), before);
        }
        if let Some(after) = after {
            if inspection {
                visibility.insert(
                    (destination.clone(), "proposed".into()),
                    super::calls::visibility(
                        &destination,
                        &after,
                        if revealed
                            .iter()
                            .any(|(file, side, _)| file == &destination && side == "proposed")
                        {
                            DeclarationFilter::ALL
                        } else {
                            filter
                        },
                    )?
                    .clone(),
                );
            }
            presentation.insert((destination, "proposed".into()), after);
        }
        let file = &diff;
        let hunks = &file.hunk;
        let mut declaration_rows = Vec::new();
        let mut walkthrough_targets = Vec::new();
        let presenter = forge_diff::presentation::DiffPresenter::new(Default::default());
        let start = block.len();
        let path = file
            .new_path
            .as_ref()
            .or(file.old_path.as_ref())
            .context("diff file has no path")?;
        let status = if file.old_path.is_none() {
            FileChange::Added
        } else if file.new_path.is_none() {
            FileChange::Deleted
        } else if file.old_path != file.new_path {
            FileChange::Renamed
        } else {
            FileChange::Modified
        };
        let display = if matches!(status, FileChange::Renamed) {
            format!("{} → {path}", file.old_path.as_deref().unwrap())
        } else {
            path.clone()
        };
        let identity = file.old_path.as_deref().unwrap_or(path);
        let id = format!("plan:design:file:{}", super::digest(identity.as_bytes()));
        let delta = match files.and_then(|inventory| inventory.for_path(path)) {
            Some(heading) => heading.delta,
            None => parsed.review_delta(design, identity, path),
        };
        block.push(file_heading(
            &presenter,
            BlockId(id.clone()),
            status,
            &display,
            delta,
        )?);
        if inspection
            && let Some(&(index, _)) = steps.iter().find(|(_, step)| step.symbol.is_none())
        {
            walkthrough.after_heading.insert(BlockId(id.clone()), index);
        }
        navigation.anchor.push(PlanNavigationAnchor {
            columns: None,
            line: block.len() as u32,
            target: PlanReviewTarget::File { path: path.clone() },
            json_path: format!("/design/proposed/{}", pointer(path)),
            path: Some(path.clone()),
            label: display,
        });
        for (hunk_index, hunk) in hunks.iter().enumerate() {
            let hunk_start = block.len();
            let hunk_id = format!("{id}:hunk:{hunk_index}");
            let counts = forge_diff::presentation::counts(&hunk.row);
            use forge_diff::presentation::HunkHeading;
            let (added, removed) = (counts.0 as usize, counts.1 as usize);
            block.push(presenter.hunk(
                BlockId(hunk_id.clone()),
                &hunk.group,
                HunkHeading::Counts { added, removed },
            )?);
            for (row_index, row) in hunk.row.iter().enumerate() {
                let id = BlockId(format!("{hunk_id}:row:{row_index}"));
                let Some(coordinate) = row.new_coordinate().or_else(|| row.old_coordinate()) else {
                    let metadata = presenter
                        .rows(id, std::slice::from_ref(row), &hunk.group, None, None)?
                        .metadata;
                    block.push(BufferBlock {
                        id: BlockId(format!("{hunk_id}:row:{row_index}")),
                        text: BufferText::from_rows([row.text.as_str()])?,
                        metadata,
                    });
                    continue;
                };
                let (side, source_path) = match coordinate.side {
                    forge_diff::source::SourceSide::Old => (
                        "baseline",
                        file.old_path
                            .as_ref()
                            .context("baseline source path is missing")?,
                    ),
                    forge_diff::source::SourceSide::New => ("proposed", path),
                };
                let line = coordinate.line;
                let walkthrough_target = coordinate.side == walkthrough_side;
                let walkthrough_reveal = walkthrough_target && walkthrough_visible.contains(&line);
                if inspection
                    && walkthrough_target
                    && let Some(steps) = locations.steps.remove(&line)
                {
                    walkthrough
                        .before
                        .entry(id.clone())
                        .or_default()
                        .extend(steps);
                    walkthrough_targets.push(block.len());
                }
                let source = &presentation[&(source_path.clone(), side.into())];
                let syntax_row = source
                    .plain_row
                    .get(line)
                    .copied()
                    .filter(|original| source.declaration_row.get(original) == Some(&line));
                if inspection && syntax_row.is_some() {
                    declaration_rows.push(block.len());
                }
                let behavior = source.behavior_row.get(&line);
                let change_line = behavior
                    .filter(|_| inspection)
                    .and_then(|behavior| behavior.change_line(&row.text))
                    .map(classify_algorithm_diff_line);
                let prefix_width = change_line.map_or(0, |line| line.prefix_width);
                let mut display = if inspection {
                    inspection_row(row, source, line)
                } else {
                    std::borrow::Cow::Borrowed(row)
                };
                if let Some(behavior) = behavior.filter(|_| prefix_width > 0) {
                    display
                        .to_mut()
                        .text
                        .replace_range(behavior.column..behavior.column + prefix_width, "");
                }
                let baseline = signature_replacements.get(&(coordinate.side, line));
                let mut metadata = if let Some(old) = baseline.filter(|_| side == "proposed") {
                    let old_path = file
                        .old_path
                        .as_ref()
                        .context("signature pair lacks baseline path")?;
                    let old_source = &presentation[&(old_path.clone(), "baseline".into())];
                    let old_line = old
                        .old_coordinate()
                        .context("signature pair lacks baseline row")?
                        .line;
                    let old = inspection_row(old, old_source, old_line);
                    let change = if old.text == display.text {
                        RowChange::Context
                    } else {
                        RowChange::Modified
                    };
                    let paired = paired_signature_row(&old, &display)
                        .context("signature pair lacks coordinates")?;
                    presenter.row(id, &paired, &hunk.group, change)?
                } else if let Some(change_line) = change_line {
                    let change = match (row.kind(), change_line.kind) {
                        (RowKind::Removed, _) | (_, AlgorithmDiffLineKind::Removed) => {
                            RowChange::Removed
                        }
                        (_, AlgorithmDiffLineKind::Added) => RowChange::Added,
                        (RowKind::Added, AlgorithmDiffLineKind::Invalid) => RowChange::Modified,
                        (
                            _,
                            AlgorithmDiffLineKind::Context
                            | AlgorithmDiffLineKind::Separator
                            | AlgorithmDiffLineKind::Comment
                            | AlgorithmDiffLineKind::Blank
                            | AlgorithmDiffLineKind::Invalid,
                        ) => RowChange::Context,
                    };
                    presenter.row(id, &display, &hunk.group, change)?
                } else {
                    presenter.rows(id, std::slice::from_ref(&display), &hunk.group, None, None)?
                }
                .metadata;
                // Preserve saved comment positions on rows omitted from the inspection view.
                let scaffold = inspection
                    && syntax_row.is_none()
                    && behavior.is_none_or(|behavior| behavior.offset == 0);
                if scaffold
                    || (row.kind() == RowKind::Removed
                        && baseline
                            .and_then(|row| row.new_coordinate())
                            .is_some_and(|proposed| {
                                visibility
                                    .get(&(path.clone(), "proposed".into()))
                                    .is_some_and(|visible| visible.rows[proposed.line])
                            }))
                {
                    metadata.conceal.push(forge_buffer::block::Conceal {
                        range: TextRange {
                            start: TextPosition { row: 0, column: 0 },
                            end: TextPosition { row: 1, column: 0 },
                        },
                        replacement: String::new(),
                        line: true,
                        priority: 100,
                    });
                }
                let row_text = visibility
                    .get(&(source_path.clone(), side.into()))
                    .filter(|_| !walkthrough_reveal)
                    .and_then(|visibility| visibility.replacement.get(&line))
                    .map(String::as_str)
                    .unwrap_or(&display.text);
                if let Some(source_row) = syntax_row
                    && let Some(handle) = syntax.get(&(source_path.clone(), side.into()))
                {
                    forge_diff::projection::append_syntax_row(
                        &mut metadata,
                        &handle.declaration,
                        source_row,
                        0,
                        row_text,
                    )?;
                }
                if let Some(behavior) = behavior.filter(|behavior| behavior.offset > 0)
                    && let Some(handle) = syntax
                        .get(&(source_path.clone(), side.into()))
                        .and_then(|syntax| syntax.algorithm.as_ref())
                {
                    let algorithm_start = metadata.visible_decoration.len();
                    forge_diff::projection::append_syntax_fragment(
                        &mut metadata,
                        handle,
                        forge_diff::syntax::SyntaxPoint {
                            row: line,
                            // Syntax uses a space-masked prefix in the authored byte positions.
                            column: prefix_width,
                        },
                        0,
                        row_text,
                    )?;
                    // Abstract operations and repository calls share identifier coloring.
                    let mut index = 0;
                    metadata.visible_decoration.retain(|decoration| {
                        index += 1;
                        index <= algorithm_start
                            || decoration.capture != "@function.call.algorithm"
                                && (behavior.category != "Change"
                                    || decoration.capture != "@error.algorithm")
                    });
                }
                if let Some(change_line) = change_line
                    && let Some(behavior) = behavior
                    && let Some(column) = change_line.elision_column
                    && let Some(elision) = row_text.get(behavior.column + column - prefix_width..)
                    && elision.starts_with("...")
                {
                    metadata.visible_decoration.push(Decoration {
                        range: TextRange {
                            start: TextPosition {
                                row: 0,
                                column: row_text.len() - elision.len(),
                            },
                            end: TextPosition {
                                row: 0,
                                column: row_text.len(),
                            },
                        },
                        capture: "Comment".into(),
                        priority: 100,
                    });
                }
                if inspection
                    && !walkthrough_reveal
                    && !visibility
                        .get(&(source_path.clone(), side.into()))
                        .and_then(|visibility| visibility.rows.get(line))
                        .copied()
                        .unwrap_or(false)
                {
                    hidden.insert(BlockId(format!("{hunk_id}:row:{row_index}")));
                }
                if row.kind() == RowKind::Context
                    && let Some(old_line) = row.old_coordinate().map(|coordinate| coordinate.line)
                {
                    display_row.insert(
                        (
                            file.old_path
                                .as_ref()
                                .context("context row has no baseline file")?
                                .clone(),
                            "baseline".into(),
                            old_line,
                        ),
                        block.len(),
                    );
                }
                display_row.insert((source_path.clone(), side.into(), line), block.len());
                block.push(BufferBlock {
                    id: BlockId(format!("{hunk_id}:row:{row_index}")),
                    text: BufferText::from_rows([row_text])?,
                    metadata,
                });
                if row.kind() == RowKind::Context
                    && row
                        .old_coordinate()
                        .map(|coordinate| coordinate.line)
                        .is_some()
                {
                    let baseline_path = file
                        .old_path
                        .as_ref()
                        .context("context row has no baseline file")?;
                    if let Some(position) = presentation
                        .get(&(baseline_path.clone(), "baseline".into()))
                        .and_then(|file| {
                            row.old_coordinate()
                                .map(|coordinate| coordinate.line)
                                .and_then(|line| file.declaration.source.get(line))
                        })
                        .copied()
                        .flatten()
                    {
                        navigation.anchor.push(declaration_anchor(
                            block.len() as u32,
                            baseline_path,
                            "baseline",
                            position,
                            row_text,
                        ));
                    }
                }
                let position = presentation
                    .get(&(source_path.clone(), side.into()))
                    .and_then(|file| file.declaration.source.get(line))
                    .copied()
                    .flatten();
                if let Some(position) = position {
                    navigation.anchor.push(declaration_anchor(
                        block.len() as u32,
                        source_path,
                        side,
                        position,
                        row_text,
                    ));
                }
                if let Some(behavior) = behavior {
                    let (owner, offset, category) =
                        (&behavior.owner, behavior.offset, behavior.category);
                    navigation.anchor.push(PlanNavigationAnchor {
                        columns: None,
                        line: block.len() as u32,
                        target: PlanReviewTarget::Behavior {
                            path: source_path.clone(),
                            side: side.into(),
                            owner: owner.clone(),
                            offset,
                        },
                        json_path: format!(
                            "/design/{side}_calls/{}/{}/behavior/{offset}",
                            pointer(source_path),
                            pointer(owner)
                        ),
                        path: Some(source_path.clone()),
                        label: format!("{source_path}: {owner}: {category}"),
                    });
                }
            }
            fold(&mut block, hunk_start, &hunk_id, FoldPolicy::default())?;
            if file.hunk[hunk_index].contextual {
                contextual_block.extend(block[hunk_start..].iter().map(|row| row.id.clone()));
            }
        }
        if inspection {
            anyhow::ensure!(
                locations.steps.is_empty(),
                "walkthrough declaration rows are unavailable in {path}"
            );
            declaration_folds(
                &mut block,
                [file.old_path.as_ref(), file.new_path.as_ref()],
                &presentation,
                &display_row,
                &walkthrough_targets,
            )?;
            behavior_folds(
                &mut block,
                [file.old_path.as_ref(), file.new_path.as_ref()],
                &presentation,
                &display_row,
                &declaration_rows,
            )?;
            let side = match walkthrough_side {
                SourceSide::Old => "baseline",
                SourceSide::New => "proposed",
            };
            let block_for = |row| -> Result<BlockId> {
                let index = display_row
                    .get(&(path.clone(), side.into(), row))
                    .context("walkthrough documentation has no displayed declaration")?;
                Ok(block[*index].id.clone())
            };
            for (doc, signature) in locations.documentation {
                walkthrough
                    .documentation
                    .entry(block_for(signature)?)
                    .or_default()
                    .push(block_for(doc)?);
            }
        }
        fold(&mut block, start, &id, FILE_FOLD)?;
        if hide_file {
            hidden.extend(block[start..].iter().map(|row| row.id.clone()));
        }
        if contextual_path.contains(path) {
            contextual_block.extend(block[start..].iter().map(|row| row.id.clone()));
        }
    }
    if body.is_some() {
        navigation.anchor.sort_by_key(|anchor| anchor.line);
        return Ok(DesignReviewRows {
            block,
            navigation,
            hidden,
            contextual: contextual_block,
            skipped,
            walkthrough,
        });
    }
    if block.is_empty() {
        block.push(forge_diff::projection::header(
            BlockId("plan:design:empty".into()),
            vec![TextChunk {
                text: "No declaration changes. Implementation bodies are outside this design view."
                    .into(),
                capture: "Comment".into(),
            }],
            0,
        )?);
        navigation.anchor.push(PlanNavigationAnchor {
            columns: None,
            line: 1,
            target: PlanReviewTarget::Section {
                section: super::PlanSection::Files,
            },
            json_path: "/design".into(),
            path: None,
            label: "No declaration changes".into(),
        });
    }
    let mut overview = Vec::new();
    let mut section_anchor = Vec::new();
    for metadata in design.document.sections() {
        let name = metadata.path;
        let section = metadata.section;
        if matches!(
            section,
            super::PlanSection::Usage
                | super::PlanSection::Requirements
                | super::PlanSection::Decisions
                | super::PlanSection::Flows
        ) && metadata.text.trim().is_empty()
        {
            continue;
        }
        if section == super::PlanSection::Tests {
            let changes_start = overview.len();
            overview.push(section_header(
                BlockId("plan:section:changes".into()),
                "Changes",
            )?);
            overview.push(BufferBlock {
                id: BlockId("plan:section:changes:spacing".into()),
                text: BufferText::from_rows([""])?,
                metadata: BlockMetadata::default(),
            });
            let offset = overview.len() as u32;
            for anchor in &mut navigation.anchor {
                anchor.line += offset;
            }
            section_anchor.push(PlanNavigationAnchor {
                columns: None,
                line: changes_start as u32 + 1,
                target: PlanReviewTarget::Section {
                    section: super::PlanSection::Files,
                },
                json_path: "/design".into(),
                path: None,
                label: "Changes".into(),
            });
            overview.append(&mut block);
            fold(
                &mut overview,
                changes_start,
                "plan:section:changes",
                FoldPolicy::default(),
            )?;
            overview.push(BufferBlock {
                id: BlockId("plan:section:changes:separator".into()),
                text: BufferText::from_rows([""])?,
                metadata: BlockMetadata::default(),
            });
            let offset = overview.len() as u32;
            let (mut tests, mut anchors) =
                test_inventory(design, inspection, files, &mut skipped, None)?;
            if inspection && !filter.show_tests {
                hidden.extend(tests.iter().map(|row| row.id.clone()));
                hidden.insert(BlockId("plan:section:tests:separator".into()));
            }
            for anchor in &mut anchors {
                anchor.line += offset;
            }
            overview.append(&mut tests);
            section_anchor.append(&mut anchors);
            overview.push(BufferBlock {
                id: BlockId("plan:section:tests:separator".into()),
                text: BufferText::from_rows([""])?,
                metadata: BlockMetadata::default(),
            });
            append_checks(&mut overview, &mut section_anchor, design)?;
            continue;
        }
        let start = overview.len();
        let fold_id = format!("plan:section:{name}");
        let authored_overview = section == super::PlanSection::Overview;
        if !authored_overview {
            overview.push(section_header(BlockId(fold_id.clone()), metadata.title)?);
            overview.push(BufferBlock {
                id: BlockId(format!("{fold_id}:spacing")),
                text: BufferText::from_rows([""])?,
                metadata: BlockMetadata::default(),
            });
        }
        let text = if !authored_overview && metadata.text.trim().is_empty() {
            "None specified."
        } else {
            metadata.text.as_str()
        };
        for (index, line) in text.lines().enumerate() {
            overview.push(BufferBlock {
                id: BlockId(format!("plan:metadata:{name}:{index}")),
                text: BufferText::from_rows([line])?,
                metadata: BlockMetadata {
                    markdown: authored_overview || section == super::PlanSection::Flows,
                    ..BlockMetadata::default()
                },
            });
        }
        if !authored_overview {
            fold(&mut overview, start, &fold_id, FoldPolicy::default())?;
        }
        for line in start as u32 + 1..=overview.len() as u32 {
            section_anchor.push(PlanNavigationAnchor {
                columns: None,
                line,
                target: PlanReviewTarget::Section { section },
                json_path: format!("/design/document/{name}"),
                path: None,
                label: metadata.title.into(),
            });
        }
        overview.push(BufferBlock {
            id: BlockId(format!("plan:section:{name}:separator")),
            text: BufferText::from_rows([""])?,
            metadata: BlockMetadata::default(),
        });
    }
    navigation.anchor.extend(section_anchor);
    block = overview;
    navigation.anchor.sort_by_key(|anchor| anchor.line);
    Ok(DesignReviewRows {
        block,
        navigation,
        hidden,
        contextual: contextual_block,
        skipped,
        walkthrough,
    })
}

/// Compare declaration text and omit token emphasis from synthetic behavior rows.
fn inspection_row<'row>(
    row: &'row DisplayRow,
    source: &super::calls::CallPresentation,
    line: usize,
) -> std::borrow::Cow<'row, DisplayRow> {
    let mut display = std::borrow::Cow::Borrowed(row);
    if let Some(body) = source.body.get(&line) {
        let display = display.to_mut();
        display.text.clone_from(&body.declaration);
        display.emphasis.clear();
    } else if !row.emphasis.is_empty() && source.behavior_row.contains_key(&line) {
        display.to_mut().emphasis.clear();
    }
    display
}

/// Paint the proposed signature with both gutters and its own changed byte ranges.
fn paired_signature_row(baseline: &DisplayRow, proposed: &DisplayRow) -> Option<DisplayRow> {
    let old_line = baseline.old_coordinate()?.line;
    let new_line = proposed.new_coordinate()?.line;
    let mut row = proposed.clone();
    row.source = DisplaySource::Diff(RowCoordinates::Context { old_line, new_line });
    match compare_replacement(Default::default(), &[&baseline.text], &[&proposed.text]) {
        IntralineResult::Emphasis(lines) => {
            row.emphasis = lines.into_iter().flat_map(|line| line.new).collect();
            row.emphasis_fallback = None;
        }
        IntralineResult::LineStyle(reason) => {
            row.emphasis.clear();
            row.emphasis_fallback = Some(reason);
        }
    }
    Some(row)
}

/// Append Harness's automated-check summary after the test inventory.
fn append_checks(
    overview: &mut Vec<BufferBlock>,
    section_anchor: &mut Vec<PlanNavigationAnchor>,
    design: &super::DeclarationDesign,
) -> Result<()> {
    let start = overview.len();
    overview.push(section_header(
        BlockId("plan:section:checks".into()),
        "Checks",
    )?);
    overview.push(BufferBlock {
        id: BlockId("plan:section:checks:spacing".into()),
        text: BufferText::from_rows([""])?,
        metadata: BlockMetadata::default(),
    });
    let lines = review_checks(Some(design), None)?;
    let check_ids = design
        .check_preview
        .iter()
        .filter(|preview| preview.selected || preview.pending)
        .map(|preview| &preview.id)
        .collect::<Vec<_>>();
    for (index, row) in lines.into_iter().enumerate() {
        overview.push(row);
        if design.check_preview_current()?
            && let Some(id) = index.checked_sub(2).and_then(|index| check_ids.get(index))
        {
            section_anchor.push(PlanNavigationAnchor {
                columns: None,
                line: overview.len() as u32,
                target: PlanReviewTarget::Check { id: (*id).clone() },
                json_path: "/design/document/checks".into(),
                path: None,
                label: format!("Check: {id}"),
            });
        }
    }
    fold(
        overview,
        start,
        "plan:section:checks",
        FoldPolicy::default(),
    )?;
    overview.push(BufferBlock {
        id: BlockId("plan:section:checks:separator".into()),
        text: BufferText::from_rows([""])?,
        metadata: BlockMetadata::default(),
    });
    Ok(())
}

fn review_checks(
    design: Option<&super::DeclarationDesign>,
    width: Option<&WidthProfile>,
) -> Result<Vec<BufferBlock>> {
    if let Some(design) = design
        && design.check_preview_current()?
    {
        let mut table = vec![["Stage", "Command", "Scope", "Why", "Status"].map(str::to_owned)];
        for preview in design
            .check_preview
            .iter()
            .filter(|preview| preview.selected || preview.pending)
        {
            table.push([
                format!("{:?}", preview.category),
                preview.command.clone(),
                format!("{:?}", preview.scope),
                preview.reason.clone(),
                (if preview.pending {
                    "Pending validation"
                } else if preview.selected {
                    "Will run"
                } else {
                    "Not selected"
                })
                .into(),
            ]);
        }
        return render_check_table(table, width);
    }
    configured_checks(
        design.and_then(|design| design.check_configuration.as_ref()),
        width,
    )
}

fn configured_checks(
    snapshot: Option<&crate::checks::ConfigSnapshot>,
    width: Option<&WidthProfile>,
) -> Result<Vec<BufferBlock>> {
    use crate::checks::{CheckScope, CheckTrigger};

    let message = match snapshot {
        None => Some("Unconfigured · enable /checks edit on and request configuration in chat."),
        Some(snapshot) if snapshot.config.definitions.is_empty() => {
            Some("No automated checks configured.")
        }
        _ => None,
    };
    if let Some(message) = message {
        return Ok(vec![BufferBlock {
            id: BlockId("plan:configured-checks:0".into()),
            text: BufferText::from_rows([message])?,
            metadata: BlockMetadata::default(),
        }]);
    }
    let snapshot = snapshot.context("configured checks have no snapshot")?;
    let mut commands = HashMap::new();
    for check in &snapshot.config.definitions {
        commands.insert(check.id.as_str(), check.runner.display());
    }
    let mut table = vec![["Stage", "Command", "Scope", "Triggers", "Requires"].map(str::to_owned)];
    for index in snapshot.config.order()? {
        let check = &snapshot.config.definitions[index];
        let scope = match check.scope {
            CheckScope::All => "All",
            CheckScope::Paths => "Changed paths",
            CheckScope::Tests => "Affected tests",
        };
        let trigger = if !check.auto {
            "Explicit runs only".into()
        } else {
            match &check.trigger {
                CheckTrigger::Paths { include, .. }
                | CheckTrigger::FilesChanged { include, .. } => include.join(", "),
                _ => check.trigger.description(),
            }
        };
        let requires = check
            .depends_on
            .iter()
            .map(|dependency| {
                commands
                    .get(dependency.as_str())
                    .map(String::as_str)
                    .context("check prerequisite has no command")
            })
            .collect::<Result<Vec<_>>>()?;
        table.push([
            format!(
                "{:?}{}",
                check.category,
                if check.enabled { "" } else { " (disabled)" }
            ),
            commands[check.id.as_str()].to_owned(),
            scope.to_owned(),
            trigger,
            if requires.is_empty() {
                "—".into()
            } else {
                requires.join(", ")
            },
        ]);
    }
    render_check_table(table, width)
}

fn render_check_table(
    table: Vec<[String; 5]>,
    width: Option<&WidthProfile>,
) -> Result<Vec<BufferBlock>> {
    let profile = width.cloned().unwrap_or_default();
    let mut widths = [0; 5];
    for row in &table {
        for (column, cell) in row.iter().enumerate() {
            for line in cell.split('\n') {
                widths[column] = widths[column].max(profile.cells(line, 0)?);
            }
        }
    }
    let mut lines = Vec::new();
    if width.is_some() {
        let minimum = [5, 7, 5, 8, 8];
        while widths.iter().sum::<usize>() + 12 >= profile.columns {
            let Some(column) = (0..5)
                .filter(|column| widths[*column] > minimum[*column])
                .max_by_key(|column| widths[*column])
            else {
                break;
            };
            widths[column] -= 1;
        }
    }
    for (index, row) in table.iter().enumerate() {
        // Direct check invocations use PowerShell-compatible quoting in their display form.
        let highlights = if index > 0 && width.is_some() {
            crate::permissions::shell::highlights(
                &row[1],
                crate::permissions::shell::CommandShell::PowerShell,
            )
        } else {
            Vec::new()
        };
        let wrapped = row
            .iter()
            .enumerate()
            .map(|(column, cell)| {
                let mut cell_profile = profile.clone();
                cell_profile.columns = widths[column];
                cell_profile.wrap_plain(cell, 0)
            })
            .collect::<Result<Vec<_>, _>>()?;
        let height = wrapped.iter().map(Vec::len).max().unwrap_or(1);
        let mut rendered = Vec::new();
        let mut metadata = BlockMetadata::default();
        let mut command_offset = 0;
        for row_index in 0..height {
            let mut line = String::new();
            for (column, rows) in wrapped.iter().enumerate() {
                if column > 0 {
                    line.push_str(" │ ");
                }
                let cell = rows.get(row_index).map(String::as_str).unwrap_or_default();
                if column == 1 {
                    for highlight in &highlights {
                        let start = highlight.range.start.max(command_offset);
                        let end = highlight.range.end.min(command_offset + cell.len());
                        if start < end {
                            metadata.decoration.push(Decoration {
                                range: TextRange {
                                    start: TextPosition {
                                        row: row_index,
                                        column: line.len() + start - command_offset,
                                    },
                                    end: TextPosition {
                                        row: row_index,
                                        column: line.len() + end - command_offset,
                                    },
                                },
                                capture: highlight.group.clone(),
                                priority: 100u16.saturating_add(
                                    highlight.priority.try_into().unwrap_or(u16::MAX),
                                ),
                            });
                        }
                    }
                    command_offset += cell.len();
                    if row[1].as_bytes().get(command_offset) == Some(&b'\n') {
                        command_offset += 1;
                    }
                }
                line.push_str(cell);
                line.push_str(&" ".repeat(widths[column].saturating_sub(profile.cells(cell, 0)?)));
            }
            rendered.push(line.trim_end().to_owned());
        }
        lines.push(BufferBlock {
            id: BlockId(format!("plan:configured-checks:{}", lines.len())),
            text: BufferText::from_rows(rendered)?,
            metadata,
        });
        if index == 0 {
            lines.push(BufferBlock {
                id: BlockId(format!("plan:configured-checks:{}", lines.len())),
                text: BufferText::from_rows([widths
                    .iter()
                    .map(|width| "─".repeat(*width))
                    .collect::<Vec<_>>()
                    .join("─┼─")])?,
                metadata: BlockMetadata::default(),
            });
        }
    }
    Ok(lines)
}

fn section_header(id: BlockId, title: &str) -> Result<BufferBlock> {
    Ok(forge_diff::projection::header(
        id,
        vec![TextChunk {
            text: format!(" {title} "),
            capture: "RenderMarkdownH1".into(),
        }],
        0,
    )?)
}

/// Classify a planned test file for its review heading.
///
/// A file with no baseline is new even when the plan describes its tests without proposing
/// its source, because nothing exists for the plan to modify.
fn test_file_change(
    has_baseline: bool,
    has_proposal: bool,
    cases: &[super::design_tests::DesignTestCase],
) -> FileChange {
    if !has_baseline {
        FileChange::Added
    } else if !has_proposal {
        FileChange::Deleted
    } else if cases
        .iter()
        .all(|case| case.change == super::DesignTestChange::Reused)
    {
        FileChange::Unchanged
    } else {
        FileChange::Modified
    }
}

pub(super) fn test_inventory(
    design: &super::DeclarationDesign,
    inspection: bool,
    files_loaded: Option<&super::review_files::ReviewFiles>,
    skipped: &mut HashMap<BlockId, i64>,
    only_file: Option<&str>,
) -> Result<(Vec<BufferBlock>, Vec<PlanNavigationAnchor>)> {
    let files = &design.document.tests;
    let parsed = design.parsed()?;
    let presenter = forge_diff::presentation::DiffPresenter::new(Default::default());
    let (mut block, mut anchors) = match only_file {
        Some(_) => (Vec::new(), Vec::new()),
        None => test_section_heading(design, inspection)?,
    };
    for (file_index, file) in files.iter().enumerate() {
        if only_file.is_some_and(|path| path != file.file) {
            continue;
        }
        let start = block.len();
        let id = format!("plan:tests:file:{}", super::digest(file.file.as_bytes()));
        let change = test_file_change(
            design.baseline.contains_key(&file.file),
            parsed.proposed.contains_key(&file.file),
            &file.cases,
        );
        let counts = file.cases.iter().fold((0, 0), |(added, removed), case| {
            (
                added + u64::from(case.change == super::DesignTestChange::New),
                removed + u64::from(case.change == super::DesignTestChange::Removed),
            )
        });
        block.push(presenter.file(BlockId(id.clone()), change, &file.file, Some(counts))?);
        anchors.push(PlanNavigationAnchor {
            columns: None,
            line: block.len() as u32,
            target: PlanReviewTarget::Section {
                section: super::PlanSection::Tests,
            },
            json_path: format!("/design/document/tests/{file_index}"),
            path: Some(file.file.clone()),
            label: file.file.clone(),
        });
        if file.cases.is_empty() {
            continue;
        }
        if only_file.is_none()
            && files_loaded.is_some_and(|inventory| !inventory.loaded.contains(&id))
        {
            let rows: usize = file
                .cases
                .iter()
                .map(|case| 1 + case.description.lines().count())
                .sum();
            let deferred = BlockId(format!("{id}:hunk:deferred"));
            skipped.insert(deferred.clone(), rows as i64 - 1);
            block.push(BufferBlock {
                id: deferred,
                text: BufferText::from_rows([""])?,
                metadata: Default::default(),
            });
            fold(&mut block, start, &id, FILE_FOLD)?;
            continue;
        }
        let original_path = design
            .moved
            .iter()
            .find(|(_, destination)| *destination == &file.file)
            .map_or(&file.file, |(original, _)| original);
        let declarations =
            |text: Option<&str>| -> Result<Vec<forge_diff::syntax::DeclarationCallable>> {
                if forge_diff::syntax::DeclarationOverview::language(&file.file).is_none() {
                    return Ok(Vec::new());
                }
                text.map(|text| {
                    forge_diff::syntax::DeclarationCalls::extract(&file.file, text, true)
                        .map_err(|error| anyhow::anyhow!("{error:?}"))
                })
                .transpose()
                .map(Option::unwrap_or_default)
            };
        let before = declarations(
            design
                .baseline
                .get(original_path)
                .map(|file| file.text.as_str()),
        )?;
        let after = declarations(parsed.proposed.get(&file.file).map(String::as_str))?;
        let line_width = before
            .iter()
            .chain(&after)
            .map(|function| function.line)
            .max()
            .unwrap_or(0)
            .to_string()
            .len()
            .max(3);
        for (case_index, case) in file.cases.iter().enumerate() {
            let case_start = block.len();
            let case_id = format!("{id}:{}", super::digest(case.name.as_bytes()));
            let change = match case.change {
                super::DesignTestChange::New => forge_diff::presentation::RowChange::Added,
                super::DesignTestChange::Modified => forge_diff::presentation::RowChange::Modified,
                super::DesignTestChange::Removed => forge_diff::presentation::RowChange::Removed,
                super::DesignTestChange::Reused => forge_diff::presentation::RowChange::Context,
            };
            let name = if file.file.ends_with(".rs") {
                case.name.rsplit("::").next().unwrap()
            } else {
                &case.name
            };
            let lines = [
                (case.change != super::DesignTestChange::New)
                    .then(|| super::design_tests::declaration_line(&before, &case.name))
                    .flatten(),
                (case.change != super::DesignTestChange::Removed)
                    .then(|| super::design_tests::declaration_line(&after, &case.name))
                    .flatten(),
            ];
            let mut heading =
                presenter.summary(BlockId(case_id.clone()), name, change, lines, line_width)?;
            heading
                .metadata
                .visible_decoration
                .push(forge_buffer::block::Decoration {
                    range: forge_buffer::block::TextRange {
                        start: TextPosition { row: 0, column: 0 },
                        end: TextPosition {
                            row: 0,
                            column: name.len(),
                        },
                    },
                    capture: "@function".into(),
                    priority: 100,
                });
            block.push(heading);
            for (line_index, line) in case.description.lines().enumerate() {
                let mut description = presenter.summary(
                    BlockId(format!("{case_id}:description:{line_index}")),
                    &format!("  {line}"),
                    change,
                    [None, None],
                    line_width,
                )?;
                description
                    .metadata
                    .visible_decoration
                    .push(forge_buffer::block::Decoration {
                        range: forge_buffer::block::TextRange {
                            start: TextPosition { row: 0, column: 0 },
                            end: TextPosition {
                                row: 0,
                                column: description.text.row(0).unwrap().len(),
                            },
                        },
                        capture: "Comment".into(),
                        priority: 100,
                    });
                block.push(description);
            }
            for line in case_start + 1..=block.len() {
                anchors.push(PlanNavigationAnchor {
                    columns: None,
                    line: line as u32,
                    target: PlanReviewTarget::TestCase {
                        path: file.file.clone(),
                        name: case.name.clone(),
                    },
                    json_path: format!("/design/document/tests/{file_index}/cases/{case_index}"),
                    path: Some(file.file.clone()),
                    label: format!("{}: {}", file.file, case.name),
                });
            }
        }
        fold(&mut block, start, &id, FILE_FOLD)?;
    }
    if only_file.is_none() {
        fold(&mut block, 0, "plan:section:tests", FoldPolicy::default())?;
    }
    Ok((block, anchors))
}

/// Recolor a value or construction reference by the narrative symbol it names, so types and
/// factories keep the coloring their declarations use elsewhere in the review.
fn test_section_heading(
    design: &super::DeclarationDesign,
    inspection: bool,
) -> Result<(Vec<BufferBlock>, Vec<PlanNavigationAnchor>)> {
    let mut heading = vec![
        TextChunk {
            text: " Tests ".into(),
            capture: "RenderMarkdownH1".into(),
        },
        TextChunk {
            text: "· ".into(),
            capture: "Comment".into(),
        },
    ];
    heading.extend(super::design_tests::summary(&design.document.tests));
    heading.push(TextChunk {
        text: " ".into(),
        capture: "Normal".into(),
    });
    let mut block = vec![forge_diff::projection::header(
        BlockId("plan:section:tests".into()),
        heading,
        0,
    )?];
    let mut anchors = vec![PlanNavigationAnchor {
        columns: None,
        line: 1,
        target: PlanReviewTarget::Section {
            section: super::PlanSection::Tests,
        },
        json_path: "/design/document/tests".into(),
        path: None,
        label: "Tests".into(),
    }];
    block.push(BufferBlock {
        id: BlockId("plan:section:tests:spacing".into()),
        text: BufferText::from_rows([""])?,
        metadata: BlockMetadata::default(),
    });
    if let Some(report) = &design.submission_warnings
        && !report.warnings().is_empty()
    {
        let warning_start = block.len();
        let matches = report.matches_plan(design)?;
        block.push(BufferBlock {
            id: BlockId("plan:tests:warnings:status".into()),
            // Decisions compare the canonical Markdown with its saved artifact.
            // The editor can label the observations without changing those bytes.
            text: BufferText::from_rows([match (inspection, matches) {
                (true, true) => "Warnings recorded at plan submission",
                (true, false) => "Warnings from a previous submission",
                (false, true) => "Submission warnings — captured observations; live inputs not revalidated",
                (false, false) => "Stale submission warnings — authored tests or reviewer choices changed; resubmit to replace them",
            }])?,
            metadata: BlockMetadata::default(),
        });
        for (index, warning) in report.warnings().iter().enumerate() {
            let location = if matches {
                format!("docs/tests.md:{}", warning.line)
            } else {
                "previous submission".into()
            };
            block.push(BufferBlock {
                id: BlockId(format!("plan:tests:warnings:{index}")),
                text: BufferText::from_rows([format!(
                    "{location}: warning: {}",
                    warning
                        .message
                        .lines()
                        .collect::<Vec<_>>()
                        .join(" ")
                        .replace('\0', "�")
                )])?,
                metadata: BlockMetadata::default(),
            });
        }
        for line in warning_start + 1..=block.len() {
            anchors.push(PlanNavigationAnchor {
                columns: None,
                line: u32::try_from(line)?,
                target: PlanReviewTarget::Section {
                    section: super::PlanSection::Tests,
                },
                json_path: "/design/submission_warnings".into(),
                path: None,
                label: "Submission warning".into(),
            });
        }
    }
    Ok((block, anchors))
}

fn restyle_narrative_reference(
    metadata: &mut forge_buffer::block::BlockMetadata,
    anchor: &PlanReviewTarget,
    narrative_symbols: &super::review_symbols::NarrativeSymbols,
) {
    if let PlanReviewTarget::Call {
        path,
        side,
        name,
        kind,
        ..
    } = anchor
        && matches!(kind, super::CallKind::Value | super::CallKind::Construct)
        && let Some(reference) = metadata.visible_decoration.last_mut()
    {
        match narrative_symbols.capture(path, side, name) {
            Some("@type") => reference.capture = "ForgeWalkthroughType".into(),
            Some("@function") if *kind == super::CallKind::Construct => {
                reference.capture = if name.contains([':', '.']) {
                    "@function.method.call"
                } else {
                    "@function.call"
                }
                .into();
            }
            _ => (),
        }
    }
}

fn declaration_folds(
    block: &mut [BufferBlock],
    paths: [Option<&String>; 2],
    presentation: &HashMap<(String, String), std::sync::Arc<super::calls::CallPresentation>>,
    display_row: &HashMap<(String, String, usize), usize>,
    walkthrough_targets: &[usize],
) -> Result<()> {
    let mut candidates = Vec::new();
    for (source_path, side) in [(paths[0], "baseline"), (paths[1], "proposed")] {
        let Some(source_path) = source_path else {
            continue;
        };
        let Some(source) = presentation.get(&(source_path.clone(), side.into())) else {
            continue;
        };
        for declaration in source.folds(source_path)? {
            let mapped = |line| {
                source.declaration_row.get(&line).and_then(|row| {
                    display_row
                        .get(&(source_path.clone(), side.into(), *row))
                        .copied()
                })
            };
            let (Some(opening), Some(closing)) =
                (mapped(declaration.start), mapped(declaration.end))
            else {
                continue;
            };
            if opening >= closing || !block[opening].text.wire_rows()[0].trim_end().ends_with('{') {
                continue;
            }
            candidates.push((
                opening,
                closing,
                mapped(declaration.heading)
                    .filter(|heading| *heading <= opening)
                    .unwrap_or(opening),
                declaration.collapsed_suffix.clone(),
                declaration.closed,
            ));
        }
    }
    candidates.sort_by_key(|(opening, closing, _, _, _)| (std::cmp::Reverse(*opening), *closing));
    let mut installed = Vec::new();
    for (opening, closing, heading, suffix, closed) in candidates {
        if installed.iter().any(|&(start, end)| {
            end == closing
                || (opening < start && start <= closing && closing < end)
                || (start < opening && opening <= end && end < closing)
        }) {
            continue;
        }
        let fold_id = FoldId(format!("{}:declaration", block[opening].id.0));
        forge_diff::presentation::DiffPresenter::declaration_fold(
            block,
            forge_diff::presentation::DeclarationFoldLayout {
                id: fold_id,
                heading,
                opening,
                closing,
                closed,
                summary: suffix,
            },
        )?;
        // Retain annotated declarations in the native fold model so jumps can open ancestors.
        if walkthrough_targets
            .iter()
            .any(|&target| opening < target && target <= closing)
        {
            block[opening]
                .metadata
                .fold
                .last_mut()
                .unwrap()
                .collapsed_suffix = None;
        }
        installed.push((opening, closing));
    }
    Ok(())
}

/// Attach an open fold to the signature of each rendered behavior body.
fn behavior_folds(
    block: &mut [BufferBlock],
    paths: [Option<&String>; 2],
    presentation: &HashMap<(String, String), std::sync::Arc<super::calls::CallPresentation>>,
    display_row: &HashMap<(String, String, usize), usize>,
    declaration_rows: &[usize],
) -> Result<()> {
    let mut folds = std::collections::BTreeMap::<usize, (usize, usize, FoldId)>::new();
    for (path, side) in [(paths[1], "proposed"), (paths[0], "baseline")] {
        let Some(path) = path else { continue };
        let Some(source) = presentation.get(&(path.clone(), side.into())) else {
            continue;
        };
        let mapped = |line| display_row.get(&(path.clone(), side.into(), line)).copied();
        for (&signature, body) in &source.body {
            let (Some(opening), Some(behavior)) =
                (mapped(signature), source.behavior_row.get(&(signature + 1)))
            else {
                continue;
            };
            // A revision can interleave old pseudocode with lines of the proposed signature.
            let next = declaration_rows.partition_point(|row| *row <= opening);
            let boundary = declaration_rows.get(next).copied().unwrap_or(block.len());
            let Some(closing) = (signature + 2..body.end)
                .rev()
                .filter_map(mapped)
                .find(|row| opening < *row && *row < boundary)
            else {
                continue;
            };
            folds
                .entry(opening)
                .and_modify(|(_, last, _)| *last = (*last).max(closing))
                .or_insert_with(|| {
                    let id = FoldId(format!(
                        "plan:calls:{}:{side}:{}",
                        super::digest(path.as_bytes()),
                        super::digest(behavior.owner.as_bytes())
                    ));
                    (
                        mapped(body.heading)
                            .filter(|heading| *heading <= opening)
                            .unwrap_or(opening),
                        closing,
                        id,
                    )
                });
        }
    }
    for (opening, (heading, closing, id)) in folds {
        forge_diff::presentation::DiffPresenter::declaration_fold(
            block,
            forge_diff::presentation::DeclarationFoldLayout {
                id,
                heading,
                opening,
                closing,
                closed: false,
                summary: String::new(),
            },
        )?;
    }
    Ok(())
}

fn filtered_blocks(
    mut block: Vec<BufferBlock>,
    hidden: &HashSet<BlockId>,
    whole_document: bool,
    public_only: bool,
) -> Result<Vec<BufferBlock>> {
    let index: HashMap<_, _> = block
        .iter()
        .enumerate()
        .map(|(index, block)| (block.id.clone(), index))
        .collect();
    let mut retained: Vec<_> = block
        .iter()
        .map(|block| !hidden.contains(&block.id))
        .collect();
    let mut visible_count = vec![0usize];
    for (index, block) in block.iter().enumerate() {
        let declaration = retained[index]
            && !block
                .metadata
                .visible_decoration
                .iter()
                .any(|decoration| decoration.capture == "ForgeHunkBoundary")
            && (block.metadata.fold.is_empty()
                || block
                    .metadata
                    .fold
                    .iter()
                    .any(|fold| fold.collapsed_suffix.is_some()))
            && !block.id.0.starts_with("plan:annotation:")
            && block
                .text
                .wire_rows()
                .into_iter()
                .any(|row| !row.trim().is_empty());
        visible_count.push(visible_count.last().unwrap() + usize::from(declaration));
    }
    for (start, header) in block.iter().enumerate() {
        if header.id.0.starts_with("plan:section:")
            || super::review_projection::change_file(&header.id) == Some(header.id.0.as_str())
            || header
                .id
                .0
                .strip_prefix("plan:tests:file:")
                .is_some_and(|key| !key.contains(':'))
        {
            continue;
        }
        for fold in &header.metadata.fold {
            let end = *index
                .get(&fold.end.block)
                .context("filtered fold endpoint is missing")?;
            let boundary = end + usize::from(fold.end.position.row > 0);
            if !fold.id.0.starts_with("plan:calls:")
                && visible_count[boundary] == visible_count[start + 1]
            {
                retained[start] = false;
            }
        }
    }
    for (start, header) in block.iter().enumerate() {
        if header.id.0.contains(":hunk:") && !header.id.0.contains(":row:") {
            for fold in &header.metadata.fold {
                let end = index[&fold.end.block] + usize::from(fold.end.position.row > 0);
                if !retained[start] {
                    retained[start..end].fill(false);
                    continue;
                }
                let mut preceding = false;
                let mut pending = None;
                for position in start + 1..end {
                    if !retained[position] {
                        continue;
                    }
                    if block[position]
                        .metadata
                        .visible_decoration
                        .iter()
                        .any(|decoration| decoration.capture == "ForgeHunkBoundary")
                    {
                        if !preceding || pending.is_some() {
                            retained[position] = false;
                        } else {
                            pending = Some(position);
                        }
                    } else if block[position]
                        .text
                        .wire_rows()
                        .iter()
                        .any(|text| !text.trim().is_empty())
                    {
                        preceding = true;
                        pending = None;
                    }
                }
                if let Some(position) = pending {
                    retained[position] = false;
                }
            }
        }
    }
    let mut owner = None;
    for (position, item) in block.iter().enumerate() {
        if item.id.0.starts_with("plan:annotation:") {
            retained[position] &= owner.is_some_and(|owner: usize| retained[owner]);
        } else {
            owner = Some(position);
        }
    }
    let mut blank = false;
    for (position, item) in block.iter().enumerate() {
        if !retained[position] {
            continue;
        }
        let empty = item
            .text
            .wire_rows()
            .iter()
            .all(|row| row.trim().is_empty());
        // An empty declaration row cannot replace the unhighlighted section spacer.
        let section_separator =
            item.id.0.starts_with("plan:section:") && item.id.0.ends_with(":separator");
        if empty && blank && !section_separator && !item.id.0.ends_with(":hunk:deferred") {
            retained[position] = false;
        }
        blank = empty;
    }
    for (position, item) in block
        .iter()
        .enumerate()
        .rev()
        .take_while(|_| whole_document)
    {
        if !retained[position] {
            continue;
        }
        if item
            .text
            .wire_rows()
            .iter()
            .all(|row| row.trim().is_empty())
            && !item.id.0.ends_with(":hunk:deferred")
        {
            retained[position] = false;
        } else {
            break;
        }
    }
    let mut previous = Vec::with_capacity(block.len());
    let mut last = None;
    for (index, keep) in retained.iter().enumerate() {
        if *keep {
            last = Some(index);
        }
        previous.push(last);
    }
    let endpoint: Vec<_> = block
        .iter()
        .map(|block| BlockAnchor {
            block: block.id.clone(),
            position: TextPosition {
                row: block.text.row_count(),
                column: 0,
            },
        })
        .collect();
    for (start, header) in block.iter_mut().enumerate() {
        if !retained[start] {
            continue;
        }
        for fold in &mut header.metadata.fold {
            if fold
                .heading_start
                .as_ref()
                .is_some_and(|heading| !retained[index[&heading.block]])
            {
                fold.heading_start = None;
            }
            let end = index[&fold.end.block];
            let boundary = end + usize::from(fold.end.position.row > 0);
            let last = boundary
                .checked_sub(1)
                .and_then(|end| previous[end])
                .context("filtered fold has no retained endpoint")?;
            fold.end = endpoint[last].clone();
        }
    }
    let mut output: Vec<_> = block
        .into_iter()
        .zip(retained)
        .filter_map(|(block, keep)| keep.then_some(block))
        .collect();
    if !whole_document {
        return Ok(output);
    }
    let changes = output
        .iter()
        .position(|block| block.id.0 == "plan:section:changes")
        .context("changes section is missing")?;
    let mut changes_end = output
        .iter()
        .position(|block| block.id.0 == "plan:section:changes:separator")
        .context("changes section separator is missing")?;
    if output[changes + 1..changes_end].iter().all(|block| {
        block.id.0.starts_with("plan:annotation:")
            || block
                .text
                .wire_rows()
                .iter()
                .all(|row| row.trim().is_empty())
    }) {
        output.insert(
            changes + 1,
            forge_diff::projection::header(
                BlockId("plan:design:public:empty".into()),
                vec![TextChunk {
                    text: if public_only {
                        "No public declaration changes."
                    } else {
                        "No visible changes."
                    }
                    .into(),
                    capture: "Comment".into(),
                }],
                0,
            )?,
        );
        changes_end += 1;
    }
    let mut trailing = output.split_off(changes_end);
    output[changes].metadata.fold.clear();
    fold(
        &mut output,
        changes,
        "plan:section:changes",
        FoldPolicy::default(),
    )?;
    output.append(&mut trailing);
    Ok(output)
}

fn declaration_anchor(
    display_line: u32,
    path: &str,
    side: &str,
    position: forge_diff::syntax::DeclarationPosition,
    text: &str,
) -> PlanNavigationAnchor {
    PlanNavigationAnchor {
        columns: None,
        line: display_line,
        target: PlanReviewTarget::Declaration {
            path: path.into(),
            side: side.into(),
            line: position.line,
            column: Some(position.column),
        },
        json_path: format!(
            "/design/{side}/{}/lines/{}/columns/{}",
            pointer(path),
            position.line - 1,
            position.column
        ),
        path: Some(path.into()),
        label: format!(
            "{path} {side}:{}:{}: {text}",
            position.line, position.column
        ),
    }
}

pub(super) fn fold(
    block: &mut [BufferBlock],
    start: usize,
    id: &str,
    policy: FoldPolicy,
) -> Result<()> {
    if block.len() <= start + 1 {
        return Ok(());
    }
    let last = block.last().context("diff fold has no rows")?;
    let end = BlockAnchor {
        block: last.id.clone(),
        position: TextPosition {
            row: last.text.row_count(),
            column: 0,
        },
    };
    forge_diff::presentation::DiffPresenter::fold(
        &mut block[start],
        FoldId(id.into()),
        end,
        policy,
    );
    Ok(())
}

fn pointer(path: &str) -> String {
    path.replace('~', "~0").replace('/', "~1")
}

#[cfg(test)]
mod tests {
    use super::{FileChange, test_file_change};
    use crate::plan::DesignTestChange;
    use crate::plan::design_tests::DesignTestCase;

    #[test]
    fn walkthrough_reunites_a_signature_with_its_detached_doc_comment() -> anyhow::Result<()> {
        use forge_diff::display::{DisplayHunk, DisplayRow, DisplaySource};
        use forge_diff::source::{SourceCoordinate, SourceSide};

        let path = "worker.rs";
        let source = super::super::calls::insert(
            path,
            "impl Worker {\n    /// Opens pending work.\n    fn open(\n        &self,\n        queue: Queue,\n    ) -> Result<()>;\n}\n",
            &[],
            true,
        )?;
        let step = crate::plan::DesignWalkthroughComment {
            path: path.into(),
            symbol: Some("Worker::open".into()),
            title: None,
            comment: "Acquire the lease first.".into(),
        };
        let anchors = || super::walkthrough_rows(path, &source, &[(0, &step)]);
        let (locations, _) = anchors()?;
        let heading = locations[0].heading;
        let signature = locations[0].signature.clone();
        assert!(heading < *signature.start());
        assert!(signature.start() < signature.end());
        let lines: Vec<_> = source.declaration.text.lines().collect();
        let excerpt =
            |range: std::ops::Range<usize>, group_index| super::super::review_diff::ReviewHunk {
                group: DisplayHunk::context(0..0, range.clone()),
                row: range
                    .map(|line| DisplayRow {
                        group_index,
                        text: lines[line].into(),
                        source: DisplaySource::Context(SourceCoordinate {
                            side: SourceSide::New,
                            line,
                            byte_column: 0,
                        }),
                        raw_id: None,
                        emphasis: Vec::new(),
                        emphasis_fallback: None,
                    })
                    .collect(),
                contextual: false,
            };
        let mut diff = super::super::review_diff::FileDiff {
            old_path: None,
            new_path: Some(path.into()),
            hunk: vec![
                excerpt(*signature.start()..*signature.end() + 1, 0),
                excerpt(heading..heading + 1, 1),
            ],
            alignment: Default::default(),
        };
        let positions =
            super::walkthrough_positions(&mut diff, SourceSide::New, &source, locations);
        assert_eq!(positions.steps[signature.start()], [0]);
        assert_eq!(positions.documentation[&heading], *signature.start());
        assert_eq!(diff.hunk.len(), 2);

        let block = |id: &str, text: &str| -> anyhow::Result<_> {
            Ok(super::BufferBlock {
                id: super::BlockId(id.into()),
                text: super::BufferText::from_rows([text])?,
                metadata: Default::default(),
            })
        };
        let mut blocks = vec![
            block("file:hunk:0", "@@")?,
            block("plan:walkthrough:1", "Walkthrough 1/1")?,
            block("file:hunk:0:row:0", "fn open(")?,
        ];
        super::fold(&mut blocks, 0, "file:hunk:0", Default::default())?;
        blocks.push(block("file:hunk:1", "@@")?);
        blocks.push(block("file:hunk:1:row:0", "/// Opens pending work.")?);
        super::fold(&mut blocks, 3, "file:hunk:1", Default::default())?;
        let docs = blocks[4].id.clone();
        let declaration = blocks[2].id.clone();
        let detached_hunk = blocks[3].id.clone();
        let end = super::BlockAnchor {
            block: docs.clone(),
            position: super::TextPosition { row: 1, column: 0 },
        };
        let mut hidden = super::HashSet::new();
        let projected = super::attach_walkthrough_documentation(
            blocks,
            &[(docs.clone(), end)].into(),
            [(declaration, vec![docs])].into(),
            &mut hidden,
        )?;
        assert!(hidden.contains(&detached_hunk));
        let projected = super::filtered_blocks(projected, &hidden, false, false)?;
        let document = forge_buffer::document::BufferDocument::new(
            forge_buffer::identity::DocumentId("plan:review".into()),
            projected,
        )?;
        assert_eq!(
            (&document)
                .into_iter()
                .filter_map(|block| block.text.row(0))
                .collect::<Vec<_>>(),
            [
                "@@",
                "Walkthrough 1/1",
                "/// Opens pending work.",
                "fn open("
            ],
        );

        diff.hunk.remove(0);
        let (locations, _) = anchors()?;
        let positions =
            super::walkthrough_positions(&mut diff, SourceSide::New, &source, locations);
        assert_eq!(positions.steps[signature.start()], [0]);
        assert_eq!(positions.documentation[&heading], *signature.start());
        let supplemental = &diff.hunk[1];
        assert!(supplemental.contextual);
        let visible: Vec<_> = supplemental
            .row
            .iter()
            .filter_map(DisplayRow::new_coordinate)
            .map(|coordinate| coordinate.line)
            .collect();
        assert!(signature.into_iter().all(|row| visible.contains(&row)));
        Ok(())
    }

    #[test]
    fn file_headings_show_only_declaration_deltas_and_read_legacy_summaries() -> anyhow::Result<()>
    {
        use crate::plan::design::{DeclarationDelta, PlanFileSummary};
        use crate::plan::{DeclarationDesign, ParsedDeclarations};
        let mut plan = crate::plan::references::tests::scoring_plan();
        let mut design = DeclarationDesign::scratch(
            [
                ("app.rs".into(), "pub fn app();\n".into()),
                ("legacy.rs".into(), "pub fn legacy();\n".into()),
                ("missing.rs".into(), "pub fn missing();\n".into()),
                ("imports.rs".into(), "use std::path::Path;\n".into()),
                (
                    "Cargo.toml".into(),
                    "[package]\nname = \"example\"\nversion = \"0.1.0\"\n".into(),
                ),
            ]
            .into(),
        )?;
        let saved = serde_json::to_value(design.parsed()?)?;
        assert!(saved.get("file_summary").is_none());
        design.parsed = Some(serde_json::from_value::<ParsedDeclarations>(saved)?);
        plan.design = Some(design.clone());
        let original = super::render(&plan)?.markdown;
        assert!(original.contains("app.rs\n@@ +1 -0\n"));
        let counts = &mut design.parsed_mut()?.file_summary;
        counts.insert(
            "app.rs".into(),
            PlanFileSummary::Delta(DeclarationDelta {
                added: 1,
                modified: 0,
                removed: 0,
            }),
        );
        counts.insert("Cargo.toml".into(), PlanFileSummary::Lines);
        counts.insert(
            "legacy.rs".into(),
            serde_json::from_str(r#"{"unit":"lines"}"#)?,
        );
        counts.insert(
            "imports.rs".into(),
            serde_json::from_str(r#"{"unit":"declarations","added":0,"modified":1,"removed":0}"#)?,
        );
        plan.design = Some(design);
        let rendered = super::render(&plan)?.markdown;
        assert!(rendered.contains("app.rs +1\n@@ +1 -0\n"));
        assert!(rendered.contains("legacy.rs +1\n@@ +1 -0\n"));
        assert!(rendered.contains("missing.rs\n@@ +1 -0\n"));
        assert!(rendered.contains("imports.rs\n@@ +1 -0\n"));
        assert!(rendered.contains("Cargo.toml\n@@ +3 -0\n"));
        Ok(())
    }

    #[test]
    fn file_heading_omits_each_zero_count() -> anyhow::Result<()> {
        use crate::plan::design::DeclarationDelta;
        let presenter = forge_diff::presentation::DiffPresenter::new(Default::default());
        for ([added, modified, removed], expected) in [
            ([0, 1, 0], "Modified lib.rs ~1"),
            ([0, 0, 2], "Modified lib.rs -2"),
            ([3, 0, 2], "Modified lib.rs +3 -2"),
            ([0, 0, 0], "Modified lib.rs"),
        ] {
            let heading = super::file_heading(
                &presenter,
                super::BlockId("file".into()),
                FileChange::Modified,
                "lib.rs",
                Some(DeclarationDelta {
                    added,
                    modified,
                    removed,
                }),
            )?;
            assert_eq!(heading.text.row(0), Some(expected));
        }
        Ok(())
    }

    fn case(change: DesignTestChange) -> DesignTestCase {
        DesignTestCase {
            name: "refund_rejects_unknown_products".into(),
            target: None,
            change,
            description: "Rejects a product absent from the order.".into(),
        }
    }

    /// A test file that exists nowhere yet is new, whether or not the plan proposes its source.
    #[test]
    fn test_file_without_baseline_is_added() {
        assert!(matches!(
            test_file_change(false, false, &[case(DesignTestChange::New)]),
            FileChange::Added
        ));
        assert!(matches!(
            test_file_change(false, true, &[case(DesignTestChange::New)]),
            FileChange::Added
        ));
        assert!(matches!(
            test_file_change(true, true, &[case(DesignTestChange::New)]),
            FileChange::Modified
        ));
        assert!(matches!(
            test_file_change(true, true, &[case(DesignTestChange::Reused)]),
            FileChange::Unchanged
        ));
    }
}
