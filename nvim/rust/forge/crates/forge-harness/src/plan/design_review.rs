use std::collections::{HashMap, HashSet};
use std::time::Instant;

use anyhow::{Context, Result, ensure};
use forge_buffer::block::{
    BlockAnchor, BlockMetadata, BufferBlock, Decoration, TargetRange, TextChunk, TextPosition,
    TextRange,
};
use forge_buffer::identity::{BlockId, EditSequence, FoldId, RegionRevision, TargetId};
use forge_buffer::text::BufferText;
use forge_buffer::width::WidthProfile;
use forge_diff::display::RowKind;
use forge_diff::file_header::FileChange;
use forge_diff::syntax::DeclarationVisibility;

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
}

pub(super) fn render(document: &PlanDocument) -> Result<RenderedPlan> {
    let DesignReviewRows {
        block, navigation, ..
    } = rows(
        document,
        &HashMap::new(),
        false,
        false,
        None,
        &HashSet::new(),
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

/// Replaces only the test inventory and shifts later canonical source anchors.
pub(super) fn replace_test_inventory(
    previous: &RenderedPlan,
    document: &PlanDocument,
) -> Result<RenderedPlan> {
    let (start, end) = test_source_range(previous)?;
    let design = document
        .design
        .as_ref()
        .context("test edits require a declaration plan")?;
    let (block, mut anchors) = test_inventory(design)?;
    let rows = block
        .iter()
        .flat_map(|block| block.text.wire_rows())
        .collect::<Vec<_>>();
    let previous_rows = previous.markdown.lines().collect::<Vec<_>>();
    ensure!(
        end <= previous_rows.len(),
        "test inventory exceeds saved source"
    );
    let mut output = previous_rows[..start]
        .iter()
        .map(|line| (*line).to_owned())
        .collect::<Vec<_>>();
    output.extend(rows.iter().map(|row| row.to_string()));
    output.extend(previous_rows[end..].iter().map(|line| (*line).to_owned()));
    let shift = rows.len() as i64 - (end - start) as i64;
    let mut navigation = previous.navigation.clone();
    navigation.plan_version = document.version;
    navigation
        .anchor
        .retain(|anchor| (anchor.line as usize) <= start || anchor.line as usize > end);
    for anchor in &mut navigation.anchor {
        if anchor.line as usize > end {
            anchor.line = (anchor.line as i64 + shift) as u32;
        }
    }
    for anchor in &mut anchors {
        anchor.line += start as u32;
    }
    navigation.anchor.extend(anchors);
    navigation.anchor.sort_by_key(|anchor| anchor.line);
    Ok(RenderedPlan {
        markdown: output.join("\n") + "\n",
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

pub(super) fn project(
    document: &PlanDocument,
    width: &WidthProfile,
    annotations: ReviewAnnotationView<'_>,
    syntax: &HashMap<(String, String), super::review_source::ReviewSyntax>,
    public_only: bool,
    trace: Option<&ReviewTrace>,
    revealed: &HashSet<(String, String, u32)>,
) -> Result<(Vec<BufferBlock>, HashMap<TargetId, PlanNavigationAnchor>)> {
    let ReviewAnnotationView {
        annotation,
        revision,
        focused,
    } = annotations;
    let DesignReviewRows {
        block: source,
        navigation,
        hidden,
        contextual,
    } = rows(document, syntax, public_only, true, trace, revealed)?;
    // Render the complete flows section once so each row keeps its Mermaid fence context.
    let flow_text = document
        .design
        .as_ref()
        .context("declaration review has no design")?
        .document
        .flows
        .trim_end_matches('\n')
        .to_owned();
    let flow_markdown = forge_buffer::markdown::MarkdownRenderer::source(
        BlockId("plan:flow-markdown".into()),
        &flow_text,
        width,
    )?;
    let check_rows = review_checks(document.design.as_ref(), Some(width))?;
    let mut target = HashMap::new();
    let design = document
        .design
        .as_ref()
        .context("declaration review has no design")?;
    let narrative_symbols =
        super::review_symbols::NarrativeSymbols::new(design, std::path::Path::new(""))?;
    let narrative_spans = super::review_symbols::spans(design);
    let mut block = Vec::new();
    let mut source_end = HashMap::new();
    let mut omitted = 0;
    for (index, mut row) in source.into_iter().enumerate() {
        if let Some(index) = row
            .id
            .0
            .strip_prefix("plan:metadata:flows:")
            .and_then(|index| index.parse::<usize>().ok())
        {
            row.metadata.markdown = true;
            for decoration in &flow_markdown.block.metadata.decoration {
                if decoration.range.start.row == index {
                    let mut decoration = decoration.clone();
                    decoration.range.start.row = 0;
                    decoration.range.end.row = 0;
                    row.metadata.decoration.push(decoration);
                }
            }
        } else if let Some(index) = row
            .id
            .0
            .strip_prefix("plan:configured-checks:")
            .and_then(|index| index.parse::<usize>().ok())
        {
            row.text = check_rows[index].text.clone();
            row.metadata.decoration = check_rows[index].metadata.decoration.clone();
        } else if row.id.0.starts_with("plan:metadata:")
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
        let context = contextual.contains(&row.id);
        if context {
            omitted += 1;
        }
        let line_anchor = navigation
            .anchor
            .iter()
            .filter(|anchor| anchor.line == index as u32 + 1)
            .collect::<Vec<_>>();
        if let Some(anchor) = line_anchor.last().copied() {
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
                        &narrative_symbols,
                    );
                } else {
                    for (entry, start, end) in entry_span
                        .iter()
                        .filter(|(entry, _, _)| line_anchor.iter().any(|anchor| **anchor == *entry))
                    {
                        super::review_projection::append_semantic_span(
                            &mut row.metadata,
                            &entry.target,
                            &text,
                            *start..*end,
                        );
                        restyle_narrative_reference(
                            &mut row.metadata,
                            &entry.target,
                            &narrative_symbols,
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
                entry.line = if context { 0 } else { entry.line - omitted };
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
            anchor.line = if context { 0 } else { anchor.line - omitted };
            super::review_symbols::decorate(
                &mut row,
                &anchor,
                &narrative_spans,
                &narrative_symbols,
                &mut target,
            );
            target.insert(id, anchor);
        }
        let row_id = row.id.clone();
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
        for annotation in annotation
            .iter()
            .filter(|annotation| annotation.source.end_line == index as u32 + 1)
        {
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
    if public_only || !hidden.is_empty() {
        block = public_blocks(block, &hidden)?;
        let visible_target = block
            .iter()
            .flat_map(|block| &block.metadata.target)
            .map(|range| range.id.clone())
            .collect::<HashSet<_>>();
        target.retain(|id, _| visible_target.contains(id));
    }
    Ok((block, target))
}

fn rows(
    document: &PlanDocument,
    syntax: &HashMap<(String, String), super::review_source::ReviewSyntax>,
    public_only: bool,
    inspection: bool,
    trace: Option<&ReviewTrace>,
    revealed: &HashSet<(String, String, u32)>,
) -> Result<DesignReviewRows> {
    let design = document
        .design
        .as_ref()
        .context("declaration review has no design")?;
    let parsed = design.parsed()?;
    let mut file_diff = Vec::new();
    let mut hidden = HashSet::new();
    let mut contextual_path = HashSet::new();
    let mut contextual_block = HashSet::new();
    let mut visibility = HashMap::<(String, String), DeclarationVisibility>::new();
    let mut presentation =
        HashMap::<(String, String), std::sync::Arc<super::calls::CallPresentation>>::new();
    let started = Instant::now();
    let included = revealed.iter().map(|(path, _, _)| path.clone()).collect();
    let ordered_files = super::review_file_layout::order(design, &included)?;
    if let Some(trace) = trace {
        trace.record(
            "plan.review.file_order",
            started.elapsed(),
            ordered_files.len(),
        );
    }
    for file in ordered_files {
        let path = file.baseline;
        let destination = file.proposed;
        let before = design
            .baseline
            .get(&path)
            .map(|file| {
                super::calls::present(
                    &path,
                    &file.text,
                    design
                        .baseline_calls
                        .get(&path)
                        .map(Vec::as_slice)
                        .unwrap_or_default(),
                )
            })
            .transpose()
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
        let after = parsed
            .proposed
            .get(&destination)
            .map(|text| {
                super::calls::present(
                    &destination,
                    text,
                    parsed
                        .proposed_calls
                        .get(&destination)
                        .map(Vec::as_slice)
                        .unwrap_or_default(),
                )
            })
            .transpose()
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
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
        let diff = super::review_diff::FileDiff::new(
            &path,
            &destination,
            before.as_deref(),
            after.as_deref(),
            &reference_context,
        )?;
        if diff.hunk.is_empty() && before.is_some() && after.is_some() && path == destination {
            continue;
        }
        if before.as_ref().map(|source| &source.declaration.text)
            == after.as_ref().map(|source| &source.declaration.text)
            && path == destination
            && explicitly_revealed
        {
            contextual_path.insert(path.clone());
        }
        file_diff.push(diff);
        if let Some(before) = before {
            if inspection {
                visibility.insert(
                    (path.clone(), "baseline".into()),
                    super::calls::visibility(
                        &path,
                        &before,
                        public_only
                            && !revealed
                                .iter()
                                .any(|(file, side, _)| file == &path && side == "baseline"),
                    )?,
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
                        public_only
                            && !revealed
                                .iter()
                                .any(|(file, side, _)| file == &destination && side == "proposed"),
                    )?,
                );
            }
            presentation.insert((destination, "proposed".into()), after);
        }
    }
    let mut block = Vec::new();
    let mut display_row = HashMap::<(String, String, usize), usize>::new();
    let mut navigation = PlanNavigationIndex {
        plan_id: document.plan_id.clone(),
        plan_version: document.version,
        anchor: Vec::new(),
    };
    for file in &file_diff {
        let hunks = &file.hunk;
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
        let added = hunks
            .iter()
            .flat_map(|hunk| &hunk.row)
            .filter(|row| row.kind() == RowKind::Added)
            .count() as u64;
        let removed = hunks
            .iter()
            .flat_map(|hunk| &hunk.row)
            .filter(|row| row.kind() == RowKind::Removed)
            .count() as u64;
        let identity = file.old_path.as_deref().unwrap_or(path);
        let id = format!("plan:design:file:{}", super::digest(identity.as_bytes()));
        block.push(presenter.file(
            BlockId(id.clone()),
            status,
            &display,
            Some((added, removed)),
        )?);
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
            block.push(presenter.hunk(
                BlockId(hunk_id.clone()),
                &hunk.group,
                forge_diff::presentation::HunkHeading::Counts {
                    added: counts.0 as usize,
                    removed: counts.1 as usize,
                },
            )?);
            for (row_index, row) in hunk.row.iter().enumerate() {
                let id = BlockId(format!("{hunk_id}:row:{row_index}"));
                let mut metadata = if file.alignment.modified(row) {
                    presenter
                        .row(
                            id,
                            row,
                            &hunk.group,
                            forge_diff::presentation::RowChange::Modified,
                        )?
                        .metadata
                } else {
                    presenter
                        .rows(id, std::slice::from_ref(row), &hunk.group, None, None)?
                        .metadata
                };
                let Some(coordinate) = row.new_coordinate().or_else(|| row.old_coordinate()) else {
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
                let row_text = visibility
                    .get(&(source_path.clone(), side.into()))
                    .and_then(|visibility| visibility.replacement.get(&line))
                    .map(String::as_str)
                    .unwrap_or(row.text.as_str());
                let syntax_row = presentation
                    .get(&(source_path.clone(), side.into()))
                    .and_then(|file| {
                        let original = *file.plain_row.get(line)?;
                        (file.declaration_row.get(&original) == Some(&line)).then_some(original)
                    });
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
                if presentation
                    .get(&(source_path.clone(), side.into()))
                    .and_then(|file| file.behavior_row.get(&line))
                    .is_some_and(|(_, _, category)| *category == "Algorithm")
                    && let Some(handle) = syntax
                        .get(&(source_path.clone(), side.into()))
                        .and_then(|syntax| syntax.algorithm.as_ref())
                {
                    let algorithm_start = metadata.visible_decoration.len();
                    forge_diff::projection::append_syntax_row(
                        &mut metadata,
                        handle,
                        line,
                        0,
                        row_text,
                    )?;
                    // Plans record no references, so no Algorithm call names a known declaration
                    // and none takes call coloring.
                    let mut index = 0;
                    metadata.visible_decoration.retain(|decoration| {
                        index += 1;
                        index <= algorithm_start || decoration.capture != "@function.call.algorithm"
                    });
                }
                if inspection
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
                if let Some((owner, offset, category)) = presentation
                    .get(&(source_path.clone(), side.into()))
                    .and_then(|file| file.behavior_row.get(&line))
                {
                    navigation.anchor.push(PlanNavigationAnchor {
                        columns: None,
                        line: block.len() as u32,
                        target: PlanReviewTarget::Behavior {
                            path: source_path.clone(),
                            side: side.into(),
                            owner: owner.clone(),
                            offset: *offset,
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
            fold(&mut block, hunk_start, &hunk_id)?;
            if file.hunk[hunk_index].contextual {
                contextual_block.extend(block[hunk_start..].iter().map(|row| row.id.clone()));
            }
        }
        if inspection {
            declaration_folds(
                &mut block,
                [file.old_path.as_ref(), file.new_path.as_ref()],
                &presentation,
                &display_row,
            )?;
            call_folds(
                &mut block,
                [file.old_path.as_ref(), file.new_path.as_ref()],
                &presentation,
                &display_row,
                &file.hunk,
            )?;
            section_folds(
                &mut block,
                [file.old_path.as_ref(), file.new_path.as_ref()],
                &presentation,
                &display_row,
            );
        }
        fold(&mut block, start, &id)?;
        if contextual_path.contains(path) {
            contextual_block.extend(block[start..].iter().map(|row| row.id.clone()));
        }
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
        if section == super::PlanSection::ManualVerification {
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
            fold(&mut overview, start, "plan:section:checks")?;
            overview.push(BufferBlock {
                id: BlockId("plan:section:checks:separator".into()),
                text: BufferText::from_rows([""])?,
                metadata: BlockMetadata::default(),
            });
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
            fold(&mut overview, changes_start, "plan:section:changes")?;
            overview.push(BufferBlock {
                id: BlockId("plan:section:changes:separator".into()),
                text: BufferText::from_rows([""])?,
                metadata: BlockMetadata::default(),
            });
            let offset = overview.len() as u32;
            let (mut tests, mut anchors) = test_inventory(design)?;
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
            continue;
        }
        let start = overview.len();
        let fold_id = format!("plan:section:{name}");
        let title = metadata.title;
        overview.push(section_header(BlockId(fold_id.clone()), title)?);
        overview.push(BufferBlock {
            id: BlockId(format!("{fold_id}:spacing")),
            text: BufferText::from_rows([""])?,
            metadata: BlockMetadata::default(),
        });
        let text = if metadata.text.trim().is_empty() {
            "None specified."
        } else {
            metadata.text.as_str()
        };
        for (index, line) in text.lines().enumerate() {
            overview.push(BufferBlock {
                id: BlockId(format!("plan:metadata:{name}:{index}")),
                text: BufferText::from_rows([line.to_owned()])?,
                metadata: BlockMetadata::default(),
            });
        }
        fold(&mut overview, start, &fold_id)?;
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
    })
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
        let operation = match check.scope {
            CheckScope::All => Some(&check.all),
            CheckScope::ChangedFiles => check.files.as_ref(),
            CheckScope::ChangedCases | CheckScope::NewCases => check.cases.as_ref(),
        }
        .with_context(|| format!("{} has no command for its configured scope", check.name))?;
        commands.insert(check.id.as_str(), operation.display());
    }
    let mut table = vec![["Stage", "Command", "Scope", "Triggers", "Requires"].map(str::to_owned)];
    for index in snapshot.config.order()? {
        let check = &snapshot.config.definitions[index];
        let scope = match check.scope {
            CheckScope::All => "All",
            CheckScope::ChangedFiles => "Changed files",
            CheckScope::ChangedCases => "Changed cases",
            CheckScope::NewCases => "New cases",
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

pub(super) fn test_inventory(
    design: &super::DeclarationDesign,
) -> Result<(Vec<BufferBlock>, Vec<PlanNavigationAnchor>)> {
    let files = &design.document.tests;
    let parsed = design.parsed()?;
    let presenter = forge_diff::presentation::DiffPresenter::new(Default::default());
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
    heading.extend(super::design_tests::summary(files));
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
    for (file_index, file) in files.iter().enumerate() {
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
        let start = block.len();
        let id = format!("plan:tests:file:{}", super::digest(file.file.as_bytes()));
        let change = if !design.baseline.contains_key(&file.file)
            && parsed.proposed.contains_key(&file.file)
        {
            FileChange::Added
        } else if design.baseline.contains_key(&file.file)
            && !parsed.proposed.contains_key(&file.file)
        {
            FileChange::Deleted
        } else if file
            .cases
            .iter()
            .all(|case| case.change == super::DesignTestChange::Reused)
        {
            FileChange::Unchanged
        } else {
            FileChange::Modified
        };
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
        fold(&mut block, start, &id)?;
    }
    fold(&mut block, 0, "plan:section:tests")?;
    Ok((block, anchors))
}

/// Recolor a value or construction reference by the narrative symbol it names, so types and
/// factories keep the coloring their declarations use elsewhere in the review.
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
                mapped(declaration.heading).unwrap_or(opening),
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
        installed.push((opening, closing));
    }
    Ok(())
}

/// Fold each Algorithm or Change block below its heading, marking the heading with its open state.
/// Every block starts open because it carries the callable's planned behavior.
fn section_folds(
    block: &mut [BufferBlock],
    paths: [Option<&String>; 2],
    presentation: &HashMap<(String, String), std::sync::Arc<super::calls::CallPresentation>>,
    display_row: &HashMap<(String, String, usize), usize>,
) {
    for (path, side) in [(paths[1], "proposed"), (paths[0], "baseline")] {
        let Some(path) = path else { continue };
        let Some(source) = presentation.get(&(path.clone(), side.into())) else {
            continue;
        };
        for (heading_row, (end, _)) in &source.section {
            let Some(opening) = display_row
                .get(&(path.clone(), side.into(), *heading_row))
                .copied()
            else {
                continue;
            };
            let Some(closing) = end
                .checked_sub(1)
                .and_then(|last| display_row.get(&(path.clone(), side.into(), last)))
                .copied()
            else {
                continue;
            };
            if closing <= opening
                || block[opening]
                    .metadata
                    .fold
                    .iter()
                    .any(|fold| fold.id.0.starts_with("plan:section:"))
            {
                continue;
            }
            let end = BlockAnchor {
                block: block[closing].id.clone(),
                position: TextPosition {
                    row: block[closing].text.row_count(),
                    column: 0,
                },
            };
            block[opening]
                .metadata
                .fold
                .push(forge_buffer::block::FoldRange {
                    collapsed_suffix: None,
                    id: FoldId(format!(
                        "plan:section:{}:{side}:{heading_row}",
                        super::digest(path.as_bytes())
                    )),
                    heading_start: None,
                    start: TextPosition { row: 0, column: 0 },
                    end,
                    closed: false,
                    collapse_children: false,
                    expand_children: false,
                    heading_marker: true,
                });
        }
    }
}

fn call_folds(
    block: &mut [BufferBlock],
    paths: [Option<&String>; 2],
    presentation: &HashMap<(String, String), std::sync::Arc<super::calls::CallPresentation>>,
    display_row: &HashMap<(String, String, usize), usize>,
    hunk: &[super::review_diff::ReviewHunk],
) -> Result<()> {
    let mut regions = HashMap::<(bool, usize), HashSet<forge_diff::raw::RawHunkId>>::new();
    let mut owner = HashMap::<(bool, &str), Vec<usize>>::new();
    let mut declaration_heading = Vec::<(usize, Option<&str>)>::new();
    for (old_side, path, side) in [(true, paths[0], "baseline"), (false, paths[1], "proposed")] {
        let Some(path) = path else { continue };
        let Some(source) = presentation.get(&(path.clone(), side.into())) else {
            continue;
        };
        for (line, body) in &source.body {
            let identity = body
                .owner
                .rsplit_once('#')
                .map_or(body.owner.as_str(), |(owner, _)| owner);
            owner.entry((old_side, identity)).or_default().push(*line);
            if let Some(position) = display_row.get(&(path.clone(), side.into(), body.heading)) {
                declaration_heading.push((*position, Some(identity)));
            }
        }
        let callable_heading = source
            .body
            .values()
            .map(|body| body.heading)
            .collect::<HashSet<_>>();
        for declaration in source.folds(path)? {
            if let Some(row) = source.declaration_row.get(&declaration.heading)
                && !callable_heading.contains(row)
                && let Some(position) = display_row.get(&(path.clone(), side.into(), *row))
            {
                declaration_heading.push((*position, None));
            }
        }
        for row in hunk.iter().flat_map(|hunk| &hunk.row) {
            let Some(raw) = row.raw_id else { continue };
            let coordinate = if old_side {
                row.old_coordinate()
            } else {
                row.new_coordinate()
            };
            let Some(coordinate) = coordinate else {
                continue;
            };
            if let Some((line, body)) = source.body.range(..coordinate.line).next_back()
                && coordinate.line < body.end
            {
                regions.entry((old_side, *line)).or_default().insert(raw);
            }
        }
    }
    declaration_heading.sort_unstable();
    declaration_heading.dedup();
    let mut next_foreign = vec![declaration_heading.len(); declaration_heading.len()];
    for index in (0..declaration_heading.len().saturating_sub(1)).rev() {
        next_foreign[index] = if declaration_heading[index].1 == declaration_heading[index + 1].1 {
            next_foreign[index + 1]
        } else {
            index + 1
        };
    }
    let mut installed = HashSet::new();
    for (path, side) in [(paths[1], "proposed"), (paths[0], "baseline")] {
        let Some(path) = path else { continue };
        let Some(presentation) = presentation.get(&(path.clone(), side.into())) else {
            continue;
        };
        for (line, body) in &presentation.body {
            let line = *line;
            let last = body.end;
            let Some(opening) = display_row.get(&(path.clone(), side.into(), line)).copied() else {
                continue;
            };
            let Some(mut closing) = display_row.get(&(path.clone(), side.into(), last)).copied()
            else {
                continue;
            };
            let identity = body
                .owner
                .rsplit_once('#')
                .map_or(body.owner.as_str(), |(owner, _)| owner);
            let mut boundary =
                declaration_heading.partition_point(|(position, _)| *position <= opening);
            if declaration_heading
                .get(boundary)
                .is_some_and(|(_, owner)| *owner == Some(identity))
            {
                boundary = next_foreign[boundary];
            }
            if let Some((position, _)) = declaration_heading
                .get(boundary)
                .filter(|(position, _)| *position <= closing)
            {
                closing = position - 1;
            }
            if closing <= opening {
                continue;
            }
            if !installed.insert(opening) {
                continue;
            }
            let heading = display_row
                .get(&(path.clone(), side.into(), body.heading))
                .copied()
                .unwrap_or(opening);
            let id = FoldId(format!(
                "plan:calls:{}:{side}:{}",
                super::digest(path.as_bytes()),
                super::digest(body.owner.as_bytes())
            ));
            let old_side = side == "baseline";
            let mut changed = regions.get(&(old_side, line)).cloned().unwrap_or_default();
            if owner
                .get(&(old_side, identity))
                .is_some_and(|positions| positions.len() == 1)
                && let Some(other) = owner
                    .get(&(!old_side, identity))
                    .filter(|positions| positions.len() == 1)
                && let Some(other) = regions.get(&(!old_side, other[0]))
            {
                changed.extend(other);
            }
            forge_diff::presentation::DiffPresenter::declaration_fold(
                block,
                forge_diff::presentation::DeclarationFoldLayout {
                    id,
                    heading,
                    opening,
                    closing,
                    closed: true,
                    summary: forge_diff::presentation::declaration_body_summary(
                        changed.len(),
                        if body.collapsed_suffix.ends_with("end") {
                            "end"
                        } else {
                            "}"
                        },
                    ),
                },
            )?;
        }
    }
    Ok(())
}

fn public_blocks(
    mut block: Vec<BufferBlock>,
    hidden: &HashSet<BlockId>,
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
    let mut public_count = vec![0usize];
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
        public_count.push(public_count.last().unwrap() + usize::from(declaration));
    }
    for (start, header) in block.iter().enumerate() {
        if header.id.0.starts_with("plan:section:") {
            continue;
        }
        for fold in &header.metadata.fold {
            let end = *index
                .get(&fold.end.block)
                .context("public fold endpoint is missing")?;
            let boundary = end + usize::from(fold.end.position.row > 0);
            if public_count[boundary] == public_count[start + 1] {
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
        if empty && blank && !section_separator {
            retained[position] = false;
        }
        blank = empty;
    }
    for (position, item) in block.iter().enumerate().rev() {
        if !retained[position] {
            continue;
        }
        if item
            .text
            .wire_rows()
            .iter()
            .all(|row| row.trim().is_empty())
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
                .context("public fold has no retained endpoint")?;
            fold.end = endpoint[last].clone();
        }
    }
    let mut output: Vec<_> = block
        .into_iter()
        .zip(retained)
        .filter_map(|(block, keep)| keep.then_some(block))
        .collect();
    let changes = output
        .iter()
        .position(|block| block.id.0 == "plan:section:changes")
        .context("changes section is missing")?;
    let mut tests = output
        .iter()
        .position(|block| block.id.0 == "plan:section:tests")
        .context("tests section is missing")?;
    if output[changes + 1..tests].iter().all(|block| {
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
                    text: "No public declaration changes.".into(),
                    capture: "Comment".into(),
                }],
                0,
            )?,
        );
        tests += 1;
    }
    // Section spacing must remain visible when the preceding Changes fold is closed.
    let mut changes_end = tests;
    while changes_end > changes + 1
        && output[changes_end - 1].id.0 == "plan:section:changes:separator"
    {
        changes_end -= 1;
    }
    let mut trailing = output.split_off(changes_end);
    output[changes].metadata.fold.clear();
    fold(&mut output, changes, "plan:section:changes")?;
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

fn fold(block: &mut [BufferBlock], start: usize, id: &str) -> Result<()> {
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
        Default::default(),
    );
    Ok(())
}

fn pointer(path: &str) -> String {
    path.replace('~', "~0").replace('/', "~1")
}
