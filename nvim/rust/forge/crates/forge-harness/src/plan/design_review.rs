use std::collections::{HashMap, HashSet};
use std::time::Instant;

use anyhow::{Context, Result, ensure};
use forge_buffer::block::{
    BlockAnchor, BlockMetadata, BufferBlock, Decoration, FoldRange, TargetRange, TextChunk, TextPosition, TextRange,
};
use forge_buffer::identity::{BlockId, EditSequence, FoldId, RegionRevision, TargetId};
use forge_buffer::text::BufferText;
use forge_buffer::width::WidthProfile;
use forge_diff::display::RowKind;
use forge_diff::file_header::FileChange;
#[cfg(test)]
use forge_diff::source::{Representation, SourceVersion};
use forge_diff::syntax::DeclarationVisibility;

use super::review_annotation::ReviewAnnotation;
use super::review_source::ReviewTrace;
use super::{
    PlanDocument, PlanNavigationAnchor, PlanNavigationIndex, PlanReviewTarget, RenderedPlan,
};

pub(super) fn render(document: &PlanDocument) -> Result<RenderedPlan> {
    let (block, navigation, _, _) = rows(document, &HashMap::new(), false, false, None, &HashSet::new())?;
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
pub(super) fn replace_test_inventory(previous: &RenderedPlan, document: &PlanDocument) -> Result<RenderedPlan> {
    let (start, end) = test_source_range(previous)?;
    let design = document.design.as_ref().context("test edits require a declaration plan")?;
    let (block, mut anchors) = test_inventory(design)?;
    let rows = block.iter().flat_map(|block| block.text.wire_rows()).collect::<Vec<_>>();
    let previous_rows = previous.markdown.lines().collect::<Vec<_>>();
    ensure!(end <= previous_rows.len(), "test inventory exceeds saved source");
    let mut output = previous_rows[..start].iter().map(|line| (*line).to_owned()).collect::<Vec<_>>();
    output.extend(rows.iter().map(|row| row.to_string()));
    output.extend(previous_rows[end..].iter().map(|line| (*line).to_owned()));
    let shift = rows.len() as i64 - (end - start) as i64;
    let mut navigation = previous.navigation.clone();
    navigation.plan_version = document.version;
    navigation.anchor.retain(|anchor| (anchor.line as usize) <= start || anchor.line as usize > end);
    for anchor in &mut navigation.anchor {
        if anchor.line as usize > end { anchor.line = (anchor.line as i64 + shift) as u32; }
    }
    for anchor in &mut anchors { anchor.line += start as u32; }
    navigation.anchor.extend(anchors);
    navigation.anchor.sort_by_key(|anchor| anchor.line);
    Ok(RenderedPlan { markdown: output.join("\n") + "\n", navigation })
}

/// Locates the canonical rows owned by the test section, excluding its separator.
pub(super) fn test_source_range(rendered: &RenderedPlan) -> Result<(usize, usize)> {
    let mut lines = rendered.navigation.anchor.iter().filter(|anchor|
        matches!(anchor.target, PlanReviewTarget::TestCase { .. }
            | PlanReviewTarget::Section { section: super::PlanSection::Tests }))
        .map(|anchor| anchor.line as usize);
    let first = lines.next().context("saved plan has no test section")?;
    let (mut start, mut end) = (first, first);
    for line in lines { start = start.min(line); end = end.max(line); }
    Ok((start - 1, end))
}

pub(super) fn project(
    document: &PlanDocument,
    width: &WidthProfile,
    annotation: &[ReviewAnnotation],
    revision: &HashMap<String, (RegionRevision, EditSequence)>,
    focused: Option<&str>,
    syntax: &HashMap<(String, String), super::review_source::ReviewSyntax>,
    public_only: bool,
    trace: Option<&ReviewTrace>,
    revealed: &HashSet<(String, String, u32)>,
) -> Result<(Vec<BufferBlock>, HashMap<TargetId, PlanNavigationAnchor>)> {
    let (source, navigation, mut hidden, contextual) = rows(document, syntax, public_only, true, trace, revealed)?;
    // Parse the complete generated section so code-fence context survives per-row projection.
    let flow_rows = super::design_flows::rows(&document.design.as_ref().context("declaration review has no design")?.document.flows);
    let flow_text = flow_rows.iter().map(|row| row.text.as_str()).collect::<Vec<_>>().join("\n");
    let flow_markdown = forge_buffer::markdown::MarkdownRenderer::source(
        BlockId("plan:flow-markdown".into()), &flow_text, width,
    )?;
    let check_rows = review_checks(document.design.as_ref(), Some(width))?;
    let mut target = HashMap::new();
    let design = document.design.as_ref().context("declaration review has no design")?;
    let narrative_symbols = super::review_symbols::NarrativeSymbols::new(design, std::path::Path::new(""));
    let narrative_spans = super::review_symbols::spans(design, &narrative_symbols);
    let mut block = Vec::new();
    let mut source_end = HashMap::new();
    let mut omitted = 0;
    for (index, mut row) in source.into_iter().enumerate() {
        if let Some(index) = row.id.0.strip_prefix("plan:metadata:flows:").and_then(|index| index.parse::<usize>().ok()) {
            match flow_rows[index].kind {
                super::design_flows::FlowRowKind::Fence => { hidden.insert(row.id.clone()); }
                super::design_flows::FlowRowKind::Diagram => {
                    row.metadata.decoration.push(forge_buffer::block::Decoration {
                        range: forge_buffer::block::TextRange {
                            start: TextPosition { row: 0, column: 0 },
                            end: TextPosition { row: 0, column: row.text.row(0).unwrap_or_default().len() },
                        },
                        capture: "Comment".into(), priority: 100,
                    });
                }
                super::design_flows::FlowRowKind::Narrative => {
                    row.metadata.markdown = true;
                    for decoration in &flow_markdown.block.metadata.decoration {
                        if decoration.range.start.row == index {
                            let mut decoration = decoration.clone();
                            decoration.range.start.row = 0;
                            decoration.range.end.row = 0;
                            row.metadata.decoration.push(decoration);
                        }
                    }
                }
            }
        } else if let Some(index) = row.id.0.strip_prefix("plan:configured-checks:").and_then(|index| index.parse::<usize>().ok()) {
            row.text = check_rows[index].text.clone();
            row.metadata.decoration = check_rows[index].metadata.decoration.clone();
        } else if row.id.0.starts_with("plan:metadata:")
            && !row.id.0.starts_with("plan:metadata:checks:") {
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
        if context { omitted += 1; }
        if let Some(anchor) = navigation.resolve_line(index as u32 + 1) {
            if matches!(anchor.target, PlanReviewTarget::Call { .. } | PlanReviewTarget::Behavior { .. })
                && !row.metadata.visible_decoration.iter().any(|decoration| decoration.capture.ends_with(".algorithm")) {
                super::review_projection::append_semantic_style(&mut row.metadata, &anchor.target, row.text.row(0).unwrap_or_default());
                if let PlanReviewTarget::Call { path, side, name, kind, .. } = &anchor.target
                    && matches!(kind, super::CallKind::Value | super::CallKind::Construct)
                    && let Some(reference) = row.metadata.visible_decoration.last_mut()
                {
                    match narrative_symbols.capture(path, side, name) {
                        Some("@type") => reference.capture = "ForgeWalkthroughType".into(),
                        Some("@function") if *kind == super::CallKind::Construct => {
                            reference.capture = if name.contains([':', '.']) { "@function.method.call" } else { "@function.call" }.into();
                        }
                        _ => (),
                    }
                }
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
            super::review_symbols::decorate(&mut row, &anchor, &narrative_spans, &narrative_symbols, &mut target);
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
) -> Result<(Vec<BufferBlock>, PlanNavigationIndex, HashSet<BlockId>, HashSet<BlockId>)> {
    let design = document
        .design
        .as_ref()
        .context("declaration review has no design")?;
    let mut file_diff = Vec::new();
    let mut hidden = HashSet::new();
    let mut contextual_path = HashSet::new();
    let mut contextual_block = HashSet::new();
    let mut visibility = HashMap::<(String, String), DeclarationVisibility>::new();
    let mut presentation = HashMap::<(String, String), std::sync::Arc<super::calls::CallPresentation>>::new();
    let started = Instant::now();
    let included = revealed.iter().map(|(path, _, _)| path.clone()).collect();
    let ordered_files = super::review_file_layout::order(design, &included);
    if let Some(trace) = trace {
        trace.record("plan.review.file_order", started.elapsed(), ordered_files.len());
    }
    for file in ordered_files {
        let path = file.baseline;
        let destination = file.proposed;
        let before = design
            .baseline
            .get(&path)
            .map(|file| super::calls::present(&path, &file.text, design.baseline_calls.get(&path).map(Vec::as_slice).unwrap_or_default()))
            .transpose()
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
        let after = design
            .proposed
            .get(&destination)
            .map(|text| super::calls::present(&destination, text, design.proposed_calls.get(&destination).map(Vec::as_slice).unwrap_or_default()))
            .transpose()
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
        let explicitly_revealed = revealed.iter().any(|(file, side, _)| file == &path && side == "baseline")
            || revealed.iter().any(|(file, side, _)| file == &destination && side == "proposed");
        let reference_context = revealed.iter().filter_map(|(file, side, line)| {
            if file == &path && side == "baseline" { Some((forge_diff::source::SourceSide::Old, *line)) }
            else if file == &destination && side == "proposed" { Some((forge_diff::source::SourceSide::New, *line)) }
            else { None }
        }).collect::<Vec<_>>();
        let diff = super::review_diff::FileDiff::new(&path, &destination, before.as_deref(), after.as_deref(), &reference_context)?;
        if diff.hunk.is_empty() && before.is_some() && after.is_some() && path == destination {
            continue;
        }
        if before.as_ref().map(|source| &source.declaration.text) == after.as_ref().map(|source| &source.declaration.text)
            && path == destination && explicitly_revealed {
            contextual_path.insert(path.clone());
        }
        file_diff.push(diff);
        if let Some(before) = before {
            if inspection {
                visibility.insert(
                    (path.clone(), "baseline".into()),
                    super::calls::visibility(&path, &before, public_only && !revealed.iter().any(|(file, side, _)| file == &path && side == "baseline"))?,
                );
            }
            presentation.insert((path.clone(), "baseline".into()), before);
        }
        if let Some(after) = after {
            if inspection {
                visibility.insert(
                    (destination.clone(), "proposed".into()),
                    super::calls::visibility(&destination, &after, public_only && !revealed.iter().any(|(file, side, _)| file == &destination && side == "proposed"))?,
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
            .filter(|row| row.kind == RowKind::Added)
            .count() as u64;
        let removed = hunks
            .iter()
            .flat_map(|hunk| &hunk.row)
            .filter(|row| row.kind == RowKind::Removed)
            .count() as u64;
        let identity = file.old_path.as_deref().unwrap_or(path);
        let id = format!("plan:design:file:{}", super::digest(identity.as_bytes()));
        block.push(presenter.file(BlockId(id.clone()), status, &display, Some((added, removed)))?);
        navigation.anchor.push(PlanNavigationAnchor {
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
            block.push(presenter.hunk(BlockId(hunk_id.clone()), &hunk.group,
                forge_diff::presentation::HunkHeading::Counts { added: counts.0 as usize, removed: counts.1 as usize })?);
            for (row_index, row) in hunk.row.iter().enumerate() {
                let id = BlockId(format!("{hunk_id}:row:{row_index}"));
                let mut metadata = if file.alignment.modified(row) {
                    presenter.row(id, row, &hunk.group, forge_diff::presentation::RowChange::Modified)?.metadata
                } else {
                    presenter.rows(id, std::slice::from_ref(row), &hunk.group, None, None)?.metadata
                };
                if row.old.map(|coordinate| coordinate.line).is_none() && row.new.map(|coordinate| coordinate.line).is_none() {
                    block.push(BufferBlock {
                        id: BlockId(format!("{hunk_id}:row:{row_index}")),
                        text: BufferText::from_rows([row.text.as_str()])?, metadata,
                    });
                    continue;
                }
                let (side, source_path, line) = if row.kind == RowKind::Removed
                    || row.new.map(|coordinate| coordinate.line).is_none() {
                    (
                        "baseline",
                        file.old_path.as_ref().unwrap(),
                        row.old.map(|coordinate| coordinate.line).unwrap(),
                    )
                } else {
                    ("proposed", path, row.new.map(|coordinate| coordinate.line).unwrap())
                };
                let row_text = visibility
                    .get(&(source_path.clone(), side.into()))
                    .and_then(|visibility| visibility.replacement.get(&line))
                    .map(String::as_str)
                    .unwrap_or(row.text.as_str());
                let syntax_row = presentation.get(&(source_path.clone(), side.into())).and_then(|file| {
                    let original = *file.plain_row.get(line)?;
                    (file.declaration_row.get(&original) == Some(&line)).then_some(original)
                });
                if let Some(source_row) = syntax_row
                    && let Some(handle) = syntax.get(&(source_path.clone(), side.into())) {
                    forge_diff::projection::append_syntax_row(
                        &mut metadata,
                        &handle.declaration,
                        source_row,
                        0,
                        row_text,
                    )?;
                }
                if presentation.get(&(source_path.clone(), side.into()))
                    .and_then(|file| file.behavior_row.get(&line))
                    .is_some_and(|(_, _, category)| *category == "Algorithm")
                    && let Some(handle) = syntax.get(&(source_path.clone(), side.into())).and_then(|syntax| syntax.algorithm.as_ref()) {
                    forge_diff::projection::append_syntax_row(&mut metadata, handle, line, 0, row_text)?;
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
                if row.kind == RowKind::Context && let Some(old_line) = row.old.map(|coordinate| coordinate.line) {
                    display_row.insert((file.old_path.as_ref().unwrap().clone(), "baseline".into(), old_line), block.len());
                }
                display_row.insert((source_path.clone(), side.into(), line), block.len());
                block.push(BufferBlock {
                    id: BlockId(format!("{hunk_id}:row:{row_index}")),
                    text: BufferText::from_rows([row_text])?,
                    metadata,
                });
                if row.kind == RowKind::Context && row.old.map(|coordinate| coordinate.line).is_some() {
                    let baseline_path = file
                        .old_path
                        .as_ref()
                        .context("context row has no baseline file")?;
                    if let Some(position) = presentation
                        .get(&(baseline_path.clone(), "baseline".into()))
                        .and_then(|file| row.old.map(|coordinate| coordinate.line).and_then(|line| file.declaration.source.get(line)))
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
                if let Some((owner, name, kind)) = presentation.get(&(source_path.clone(), side.into())).and_then(|file| file.call_row.get(&line)) {
                    navigation.anchor.push(PlanNavigationAnchor {
                        line: block.len() as u32,
                        target: PlanReviewTarget::Call { kind: *kind, path: source_path.clone(), side: side.into(), owner: owner.clone(), name: name.clone() },
                        json_path: format!("/design/{side}_calls/{}/{}/{}", pointer(source_path), pointer(owner), if *kind == super::CallKind::Call { pointer(name) } else { format!("{}/{}", kind.label(), pointer(name)) }),
                        path: Some(source_path.clone()), label: format!("{source_path}: {owner}: {} {name}", kind.label()),
                    });
                }
                if let Some((owner, offset, category)) = presentation.get(&(source_path.clone(), side.into())).and_then(|file| file.behavior_row.get(&line)) {
                    navigation.anchor.push(PlanNavigationAnchor {
                        line: block.len() as u32,
                        target: PlanReviewTarget::Behavior { path: source_path.clone(), side: side.into(), owner: owner.clone(), offset: *offset },
                        json_path: format!("/design/{side}_calls/{}/{}/behavior/{offset}", pointer(source_path), pointer(owner)),
                        path: Some(source_path.clone()), label: format!("{source_path}: {owner}: {category}"),
                    });
                }
            }
            fold(&mut block, hunk_start, &hunk_id)?;
            if file.hunk[hunk_index].contextual {
                contextual_block.extend(block[hunk_start..].iter().map(|row| row.id.clone()));
            }
        }
        if inspection {
            declaration_folds(&mut block, [file.old_path.as_ref(), file.new_path.as_ref()], &presentation, &display_row)?;
            call_folds(&mut block, [file.old_path.as_ref(), file.new_path.as_ref()], &presentation, &display_row);
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
        if matches!(section, super::PlanSection::Usage | super::PlanSection::Requirements | super::PlanSection::Decisions | super::PlanSection::Flows)
            && metadata.text.trim().is_empty() {
            continue;
        }
        if section == super::PlanSection::ManualVerification {
            let start = overview.len();
            overview.push(section_header(BlockId("plan:section:checks".into()), "Checks")?);
            let lines = review_checks(Some(design), None)?;
            let check_ids = design.check_preview.iter().filter(|preview| preview.selected || preview.pending).map(|preview| &preview.id).collect::<Vec<_>>();
            for (index, row) in lines.into_iter().enumerate() {
                overview.push(row);
                if design.check_preview_current()? && let Some(id) = index.checked_sub(2).and_then(|index| check_ids.get(index)) {
                    section_anchor.push(PlanNavigationAnchor { line: overview.len() as u32,
                        target: PlanReviewTarget::Check { id: (*id).clone() },
                        json_path: "/design/document/checks".into(), path: None, label: format!("Check: {id}") });
                }
            }
            fold(&mut overview, start, "plan:section:checks")?;
            overview.push(BufferBlock {
                id: BlockId("plan:section:checks:separator".into()),
                text: BufferText::from_rows([""])?, metadata: BlockMetadata::default(),
            });
        }
        if section == super::PlanSection::Tests {
            let changes_start = overview.len();
            overview.push(section_header(
                BlockId("plan:section:changes".into()),
                "Changes",
            )?);
            let offset = overview.len() as u32;
            for anchor in &mut navigation.anchor { anchor.line += offset; }
            section_anchor.push(PlanNavigationAnchor {
                line: offset, target: PlanReviewTarget::Section { section: super::PlanSection::Files },
                json_path: "/design".into(), path: None, label: "Changes".into(),
            });
            overview.append(&mut block);
            fold(&mut overview, changes_start, "plan:section:changes")?;
            overview.push(BufferBlock {
                id: BlockId("plan:section:changes:separator".into()),
                text: BufferText::from_rows([""])?, metadata: BlockMetadata::default(),
            });
            let offset = overview.len() as u32;
            let (mut tests, mut anchors) = test_inventory(design)?;
            for anchor in &mut anchors { anchor.line += offset; }
            overview.append(&mut tests);
            section_anchor.append(&mut anchors);
            overview.push(BufferBlock {
                id: BlockId("plan:section:tests:separator".into()),
                text: BufferText::from_rows([""])?, metadata: BlockMetadata::default(),
            });
            continue;
        }
        let start = overview.len();
        let fold_id = format!("plan:section:{name}");
        let title = metadata.title;
        overview.push(section_header(BlockId(fold_id.clone()),title)?);
        let text = if metadata.text.trim().is_empty() { "None specified." } else { metadata.text.as_str() };
        for (index, line) in text.lines().enumerate() {
            overview.push(BufferBlock {
                id: BlockId(format!("plan:metadata:{name}:{index}")),
                text: BufferText::from_rows([line.to_owned()])?, metadata: BlockMetadata::default(),
            });
        }
        fold(&mut overview, start, &fold_id)?;
        for line in start as u32 + 1..=overview.len() as u32 {
            section_anchor.push(PlanNavigationAnchor {
                line, target: PlanReviewTarget::Section { section },
                json_path: format!("/design/document/{name}"), path: None, label: metadata.title.into(),
            });
        }
        overview.push(BufferBlock {
            id: BlockId(format!("plan:section:{name}:separator")),
            text: BufferText::from_rows([""])?, metadata: BlockMetadata::default(),
        });
    }
    navigation.anchor.extend(section_anchor);
    block = overview;
    navigation.anchor.sort_by_key(|anchor| anchor.line);
    Ok((block, navigation, hidden, contextual_block))
}

fn review_checks(design: Option<&super::DeclarationDesign>, width: Option<&WidthProfile>) -> Result<Vec<BufferBlock>> {
    if let Some(design) = design && design.check_preview_current()? {
        let mut table = vec![["Stage", "Command", "Scope", "Why", "Status"].map(str::to_owned)];
        for preview in design.check_preview.iter().filter(|preview| preview.selected || preview.pending) {
            table.push([
                format!("{:?}", preview.category), preview.command.clone(),
                format!("{:?}", preview.scope), preview.reason.clone(),
                (if preview.pending { "Pending validation" } else if preview.selected { "Will run" } else { "Not selected" }).into(),
            ]);
        }
        return render_check_table(table, width);
    }
    configured_checks(design.and_then(|design| design.check_configuration.as_ref()), width)
}

fn configured_checks(
    snapshot: Option<&crate::checks::ConfigSnapshot>,
    width: Option<&WidthProfile>,
) -> Result<Vec<BufferBlock>> {
    use crate::checks::{CheckScope, CheckTrigger};

    let message = match snapshot {
        None => Some("Unconfigured · use /checks setup before submission."),
        Some(snapshot) if snapshot.config.definitions.is_empty() => Some("No automated checks configured."),
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
        let trigger = match &check.trigger {
            CheckTrigger::Paths { include, .. } | CheckTrigger::FilesChanged { include, .. } => include.join(", "),
            _ => check.trigger.description(),
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

fn render_check_table(table: Vec<[String; 5]>, width: Option<&WidthProfile>) -> Result<Vec<BufferBlock>> {
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
            crate::permissions::shell::highlights(&row[1], crate::permissions::shell::CommandShell::PowerShell)
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
                if column > 0 { line.push_str(" │ "); }
                let cell = rows.get(row_index).map(String::as_str).unwrap_or_default();
                if column == 1 {
                    for highlight in &highlights {
                        let start = highlight.range.start.max(command_offset);
                        let end = highlight.range.end.min(command_offset + cell.len());
                        if start < end {
                            metadata.decoration.push(Decoration {
                                range: TextRange {
                                    start: TextPosition { row: row_index, column: line.len() + start - command_offset },
                                    end: TextPosition { row: row_index, column: line.len() + end - command_offset },
                                },
                                capture: highlight.group.clone(),
                                priority: 100u16.saturating_add(highlight.priority.try_into().unwrap_or(u16::MAX)),
                            });
                        }
                    }
                    command_offset += cell.len();
                    if row[1].as_bytes().get(command_offset) == Some(&b'\n') { command_offset += 1; }
                }
                line.push_str(cell);
                line.push_str(&" ".repeat(widths[column].saturating_sub(profile.cells(cell, 0)?)));
            }
            rendered.push(line.trim_end().to_owned());
        }
        lines.push(BufferBlock { id: BlockId(format!("plan:configured-checks:{}", lines.len())),
            text: BufferText::from_rows(rendered)?, metadata });
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
        vec![TextChunk { text: format!(" {title} "), capture: "RenderMarkdownH1".into() }],
        0,
    )?)
}

pub(super) fn test_inventory(design: &super::DeclarationDesign) -> Result<(Vec<BufferBlock>, Vec<PlanNavigationAnchor>)> {
    let files = &design.document.tests;
    let presenter = forge_diff::presentation::DiffPresenter::new(Default::default());
    let mut heading = vec![TextChunk { text: " Tests ".into(), capture: "RenderMarkdownH1".into() },
        TextChunk { text: "· ".into(), capture: "Comment".into() }];
    heading.extend(super::design_tests::summary(files));
    heading.push(TextChunk { text: " ".into(), capture: "Normal".into() });
    let mut block = vec![forge_diff::projection::header(BlockId("plan:section:tests".into()), heading, 0)?];
    let mut anchors = vec![PlanNavigationAnchor {
        line: 1, target: PlanReviewTarget::Section { section: super::PlanSection::Tests },
        json_path: "/design/document/tests".into(), path: None, label: "Tests".into(),
    }];
    for (file_index, file) in files.iter().enumerate() {
        let original_path = design.moved.iter().find(|(_, destination)| *destination == &file.file)
            .map_or(&file.file, |(original, _)| original);
        let declarations = |text: Option<&str>| -> Result<Vec<forge_diff::syntax::DeclarationCallable>> {
            if forge_diff::syntax::DeclarationOverview::language(&file.file).is_none() { return Ok(Vec::new()); }
            text.map(|text| forge_diff::syntax::DeclarationCalls::extract(&file.file, text, true)
                .map_err(|error| anyhow::anyhow!("{error:?}"))).transpose().map(Option::unwrap_or_default)
        };
        let before = declarations(design.baseline.get(original_path).map(|file| file.text.as_str()))?;
        let after = declarations(design.proposed.get(&file.file).map(String::as_str))?;
        let line_width = before.iter().chain(&after).map(|function| function.line).max().unwrap_or(0).to_string().len().max(3);
        let start = block.len();
        let id = format!("plan:tests:file:{}", super::digest(file.file.as_bytes()));
        let change = if !design.baseline.contains_key(&file.file) && design.proposed.contains_key(&file.file) {
            FileChange::Added
        } else if design.baseline.contains_key(&file.file) && !design.proposed.contains_key(&file.file) {
            FileChange::Deleted
        } else if file.cases.iter().all(|case| case.change == super::DesignTestChange::Reused) {
            FileChange::Unchanged
        } else { FileChange::Modified };
        let counts = file.cases.iter().fold((0, 0), |(added, removed), case| (
            added + u64::from(case.change == super::DesignTestChange::New),
            removed + u64::from(case.change == super::DesignTestChange::Removed),
        ));
        block.push(presenter.file(BlockId(id.clone()), change, &file.file, Some(counts))?);
        anchors.push(PlanNavigationAnchor {
            line: block.len() as u32, target: PlanReviewTarget::Section { section: super::PlanSection::Tests },
            json_path: format!("/design/document/tests/{file_index}"), path: Some(file.file.clone()), label: file.file.clone(),
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
            let name = if file.file.ends_with(".rs") { case.name.rsplit("::").next().unwrap() } else { &case.name };
            let lines = [
                (case.change != super::DesignTestChange::New).then(|| super::design_tests::declaration_line(&before, &case.name)).flatten(),
                (case.change != super::DesignTestChange::Removed).then(|| super::design_tests::declaration_line(&after, &case.name)).flatten(),
            ];
            let mut heading = presenter.summary(BlockId(case_id.clone()), name, change, lines, line_width)?;
            heading.metadata.visible_decoration.push(forge_buffer::block::Decoration {
                range: forge_buffer::block::TextRange { start: TextPosition { row: 0, column: 0 }, end: TextPosition { row: 0, column: name.len() } },
                capture: "@function".into(), priority: 100,
            });
            block.push(heading);
            for (line_index, line) in case.description.lines().enumerate() {
                let mut description = presenter.summary(BlockId(format!("{case_id}:description:{line_index}")), &format!("  {line}"), change, [None, None], line_width)?;
                description.metadata.visible_decoration.push(forge_buffer::block::Decoration {
                    range: forge_buffer::block::TextRange { start: TextPosition { row: 0, column: 0 }, end: TextPosition { row: 0, column: description.text.row(0).unwrap().len() } },
                    capture: "Comment".into(), priority: 100,
                });
                block.push(description);
            }
            for line in case_start + 1..=block.len() {
                anchors.push(PlanNavigationAnchor {
                    line: line as u32, target: PlanReviewTarget::TestCase { path: file.file.clone(), name: case.name.clone() },
                    json_path: format!("/design/document/tests/{file_index}/cases/{case_index}"),
                    path: Some(file.file.clone()), label: format!("{}: {}", file.file, case.name),
                });
            }
        }
        fold(&mut block, start, &id)?;
    }
    fold(&mut block, 0, "plan:section:tests")?;
    Ok((block, anchors))
}

fn declaration_folds(
    block: &mut [BufferBlock],
    paths: [Option<&String>; 2],
    presentation: &HashMap<(String, String), std::sync::Arc<super::calls::CallPresentation>>,
    display_row: &HashMap<(String, String, usize), usize>,
) -> Result<()> {
    let mut candidates = Vec::new();
    for (source_path, side) in [(paths[0], "baseline"), (paths[1], "proposed")] {
        let Some(source_path) = source_path else { continue };
        let Some(source) = presentation.get(&(source_path.clone(), side.into())) else { continue };
        for declaration in source.folds(source_path)? {
            let mapped = |line| source.declaration_row.get(&line).and_then(|row| display_row.get(&(source_path.clone(), side.into(), *row)).copied());
            let (Some(opening), Some(closing)) = (mapped(declaration.start), mapped(declaration.end)) else { continue };
            if opening >= closing || !block[opening].text.wire_rows()[0].trim_end().ends_with('{') { continue; }
            candidates.push((opening, closing, mapped(declaration.heading).unwrap_or(opening), declaration.collapsed_suffix.clone(), declaration.closed));
        }
    }
    candidates.sort_by_key(|(opening, closing, _, _, _)| (std::cmp::Reverse(*opening), *closing));
    let mut installed = Vec::new();
    for (opening, closing, heading, suffix, closed) in candidates {
        if installed.iter().any(|&(start, end)| end == closing
            || (opening < start && start <= closing && closing < end)
            || (start < opening && opening <= end && end < closing)) { continue; }
        let endpoint = BlockAnchor { block: block[closing].id.clone(), position: TextPosition { row: 1, column: 0 } };
        let heading_start = Some(BlockAnchor { block: block[heading].id.clone(), position: TextPosition { row: 0, column: 0 } });
        let fold_id = FoldId(format!("{}:declaration", block[opening].id.0));
        block[opening].metadata.fold.push(FoldRange { collapsed_suffix: Some(suffix), heading_start,
            id: fold_id, start: TextPosition { row: 0, column: 0 }, end: endpoint,
            closed, collapse_children: false, expand_children: false });
        installed.push((opening, closing));
    }
    Ok(())
}

fn call_folds(block: &mut [BufferBlock], paths: [Option<&String>; 2], presentation: &HashMap<(String, String), std::sync::Arc<super::calls::CallPresentation>>, display_row: &HashMap<(String, String, usize), usize>) {
    let mut installed = HashSet::new();
    for (path, side) in [(paths[1], "proposed"), (paths[0], "baseline")] {
        let Some(path) = path else { continue };
        let Some(presentation) = presentation.get(&(path.clone(), side.into())) else { continue };
        for (line, body) in &presentation.body {
            let line = *line;
            let last = body.end;
            let Some(opening) = display_row.get(&(path.clone(), side.into(), line)).copied() else { continue };
            let Some(closing) = display_row.get(&(path.clone(), side.into(), last)).copied() else { continue };
            if !installed.insert(opening) { continue; }
            let heading = display_row.get(&(path.clone(), side.into(), body.heading)).copied().unwrap_or(opening);
            let id = FoldId(format!("plan:calls:{}:{side}:{}", super::digest(path.as_bytes()), super::digest(body.owner.as_bytes())));
            block[opening].metadata.fold.push(FoldRange {
                id, start: TextPosition { row: 0, column: 0 },
                end: BlockAnchor { block: block[closing].id.clone(), position: TextPosition { row: 1, column: 0 } },
                heading_start: Some(BlockAnchor { block: block[heading].id.clone(), position: TextPosition { row: 0, column: 0 } }),
                collapsed_suffix: Some(body.collapsed_suffix.into()), closed: true, collapse_children: false, expand_children: false,
            });
        }
    }
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
            && !block.metadata.visible_decoration.iter().any(|decoration| decoration.capture == "ForgeHunkBoundary")
            && (block.metadata.fold.is_empty() || block.metadata.fold.iter().any(|fold| fold.collapsed_suffix.is_some()))
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
                    if !retained[position] { continue; }
                    if block[position].metadata.visible_decoration.iter().any(|decoration| decoration.capture == "ForgeHunkBoundary") {
                        if !preceding || pending.is_some() { retained[position] = false; }
                        else { pending = Some(position); }
                    } else if block[position].text.wire_rows().iter().any(|text| !text.trim().is_empty()) {
                        preceding = true;
                        pending = None;
                    }
                }
                if let Some(position) = pending { retained[position] = false; }
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
        let section_separator = item.id.0.starts_with("plan:section:") && item.id.0.ends_with(":separator");
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
            if fold.heading_start.as_ref().is_some_and(|heading| !retained[index[&heading.block]]) {
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
    if output[changes + 1..tests]
        .iter()
        .all(|block| block.id.0.starts_with("plan:annotation:") || block.text.wire_rows().iter().all(|row| row.trim().is_empty()))
    {
        output.insert(changes + 1, forge_diff::projection::header(
            BlockId("plan:design:public:empty".into()),
            vec![TextChunk {
                text: "No public declaration changes.".into(),
                capture: "Comment".into(),
            }],
            0,
        )?);
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
    forge_diff::presentation::DiffPresenter::fold(&mut block[start], FoldId(id.into()), end, Default::default());
    Ok(())
}

fn pointer(path: &str) -> String {
    path.replace('~', "~0").replace('/', "~1")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn algorithm_syntax_preserves_native_rows_and_behavior_anchors() {
        use forge_diff::syntax::{SyntaxEngine, SyntaxLanguage, SyntaxRequest};
        use forge_diff::workers::{AnalysisPool, PoolLimits, WorkPriority};
        let engine = SyntaxEngine::new(std::sync::Arc::new(AnalysisPool::new(PoolLimits { workers: 1, jobs: 2 })));
        for (path, text) in [
            ("main.rs", "pub fn run();\npub struct Following;\n"),
            ("main.ts", "export function run(): void;\nexport class Following {}\n"),
            ("main.lua", "function run()\nend\nfunction following()\nend\n"),
        ] {
            let algorithm = "result ← ProbeResult(value = inspect(value))\nDecrease cooldown_remaining and weapon.cooldown by elapsed seconds\nif result ≠ none\n  return result\nreturn none";
            let calls = vec![super::super::FunctionBody { owner: "run".into(),
                behavior: Some(super::super::FunctionBehavior::Algorithm(algorithm.into())), evidence: None, call: None }];
            let presentation = super::super::calls::present(path, text, &calls).unwrap();
            let mut syntax = HashMap::new();
            let mut handles = Vec::new();
            for (language, source) in [
                (SyntaxLanguage::for_path(path).unwrap(), presentation.plain.clone()),
                (SyntaxLanguage::Algorithm, presentation.algorithm_source().unwrap()),
            ] {
                handles.push(engine.analyze(SyntaxRequest {
                    source: SourceVersion::new(source.into_bytes(), Representation::DisplayOnly).unwrap(),
                    language, priority: WorkPriority::Foreground, deadline: None,
                }).await.unwrap());
            }
            syntax.insert((path.into(), "proposed".into()), super::super::review_source::ReviewSyntax {
                declaration: handles.remove(0), algorithm: Some(handles.remove(0)),
            });
            let mut document = crate::plan::document::test_fixture("algorithm-syntax", "Algorithm syntax");
            let mut design = super::super::DeclarationDesign::default();
            design.proposed.insert(path.into(), text.into());
            design.proposed_calls.insert(path.into(), calls);
            document.design = Some(design);
            let original = document.clone();
            let (blocks, targets) = project(&document, &Default::default(), &[], &HashMap::new(), None, &syntax, false, None, &HashSet::new()).unwrap();
            for (fragment, token, capture) in [
                ("result ← ProbeResult", "←", "@operator.algorithm"),
                ("result ← ProbeResult", "ProbeResult", "@type.algorithm"),
                ("result ← ProbeResult", "value", "@variable.member.algorithm"),
                ("if result", "if", "@keyword.conditional.algorithm"),
                ("return result", "return", "@keyword.return.algorithm"),
                ("Decrease cooldown_remaining", "cooldown_remaining", "@variable.algorithm"),
                ("Decrease cooldown_remaining", "weapon.cooldown", "@variable.algorithm"),
            ] {
                let block = blocks.iter().find(|block| block.text.row(0).unwrap_or_default().contains(fragment)).unwrap();
                let text = block.text.row(0).unwrap();
                assert!(block.metadata.visible_decoration.iter().any(|span| span.capture == capture
                    && &text[span.range.start.column..span.range.end.column] == token), "{path}: {fragment}");
                assert!(!block.metadata.visible_decoration.iter().any(|span| span.capture == "@comment"));
                assert!(block.metadata.target.iter().any(|range| targets.get(&range.id)
                    .is_some_and(|anchor| matches!(anchor.target, PlanReviewTarget::Behavior { .. }))));
            }
            let prose = blocks.iter().find(|block| block.text.row(0).unwrap_or_default().contains("Decrease cooldown_remaining")).unwrap();
            let prose_text = prose.text.row(0).unwrap();
            assert!(prose.metadata.visible_decoration.iter().any(|span| span.capture == "@comment.algorithm"
                && prose_text[span.range.start.column..span.range.end.column].starts_with("Decrease")));
            assert!(!prose.metadata.visible_decoration.iter().any(|span| span.capture == "@variable.algorithm"
                && &prose_text[span.range.start.column..span.range.end.column] == "Decrease"));
            assert!(blocks.iter().any(|block| block.text.row(0).unwrap_or_default().contains("following")
                || block.text.row(0).unwrap_or_default().contains("Following")));
            assert_eq!(document, original);
        }
    }

    #[test]
    fn check_table_preserves_multiline_resolution_errors() {
        let table = vec![
            ["Stage", "Command", "Scope", "Why", "Status"].map(str::to_owned),
            ["Test", "cargo test", "Changed files", "Pending validation: cargo metadata failed\nerror: no toolchain selected", "Pending validation"].map(str::to_owned),
        ];
        for columns in [60, 100, 170] {
            let profile = WidthProfile { columns, ..Default::default() };
            let rows = render_check_table(table.clone(), Some(&profile)).unwrap();
            let text = rows.iter().flat_map(|row| row.text.wire_rows()).collect::<Vec<_>>().join("\n");
            assert!(text.contains("error:"), "{text}");
            for line in rows.iter().flat_map(|row| row.text.wire_rows()) {
                assert!(profile.cells(line, 0).unwrap() < columns, "{columns}: {line}");
            }
        }
        render_check_table(table, None).unwrap();
    }

    #[test]
    fn check_table_projects_command_syntax_across_wrapped_quotes_and_unicode() {
        let toolchain = "\"+nightly-2026-04-01-x86_64-pc-windows-msvc\"";
        let command = format!("cargo {toolchain} fmt --all -- --check\nnu -c \"print 日本語\"");
        for columns in [60, 100, 170] {
            let profile = WidthProfile { columns, ..Default::default() };
            let table = vec![
                ["Stage", "Command", "Scope", "Triggers", "Requires"].map(str::to_owned),
                ["Format", &command, "All", "src/**", "—"].map(str::to_owned),
            ];
            let blocks = render_check_table(table, Some(&profile)).unwrap();
            let row = &blocks[2];
            let captured = |capture: &str| row.metadata.decoration.iter()
                .filter(|span| span.capture == capture)
                .map(|span| {
                    assert_eq!(span.range.start.row, span.range.end.row);
                    let line = row.text.row(span.range.start.row).unwrap();
                    let cells = line.split(" │ ").collect::<Vec<_>>();
                    let command_start = cells[0].len() + " │ ".len();
                    assert!(span.range.start.column >= command_start);
                    assert!(span.range.end.column <= command_start + cells[1].len());
                    &line[span.range.start.column..span.range.end.column]
                }).collect::<String>();
            assert_eq!(captured("ForgeHarnessCommand"), "cargonu");
            assert_eq!(captured("String"), format!("{toolchain}\"print 日本語\""));
            let options = captured("ForgeHarnessOption");
            assert!(options.contains("--all") && options.contains("--check") && options.contains("-c"), "{options}");
            assert!(blocks[..2].iter().all(|row| row.metadata.decoration.is_empty()));
        }
    }

    #[test]
    fn configured_checks_show_scoped_commands_and_keep_repeated_stages() {
        let mut document = crate::plan::document::test_fixture("checks", "Checks");
        let mut design = super::super::DeclarationDesign::default();
        design.check_configuration = Some(serde_json::from_value(serde_json::json!({
            "digest": "checks",
            "helper_digest": {},
            "config": { "version": 2, "definitions": [
                { "id": "format-internal", "name": "Format", "area": "format", "category": "format",
                  "enabled": true, "scope": "all", "all": { "command": "format-all" },
                  "trigger": { "kind": "paths", "include": ["src/**"], "exclude": ["private/**"] } },
                { "id": "unit-internal", "name": "Unit", "area": "unit", "category": "test",
                  "enabled": true, "scope": "changed_cases", "all": { "command": "unit-all" },
                  "cases": { "command": "unit-cases" }, "depends_on": ["format-internal"],
                  "trigger": { "kind": "any_change" } },
                { "id": "integration-internal", "name": "Integration", "area": "integration", "category": "test",
                  "enabled": true, "scope": "changed_files", "all": { "command": "integration-all" },
                  "files": { "command": "integration-files" }, "depends_on": ["unit-internal"],
                  "trigger": { "kind": "manual" } },
                { "id": "optional-internal", "name": "Optional", "area": "optional", "category": "test",
                  "enabled": false, "scope": "all", "all": { "command": "optional-all" },
                  "trigger": { "kind": "manual" } }
            ] }
        })).unwrap());
        document.design = Some(design);
        let original = serde_json::to_vec(&document).unwrap();
        let rendered = render(&document).unwrap();
        let cells = rendered
            .markdown
            .lines()
            .filter(|line| line.contains(" │ "))
            .map(|line| line.split(" │ ").map(str::trim).collect::<Vec<_>>())
            .collect::<Vec<_>>();
        assert_eq!(
            cells,
            vec![
                vec!["Stage", "Command", "Scope", "Triggers", "Requires"],
                vec!["Format", "format-all", "All", "src/**", "—"],
                vec![
                    "Test",
                    "unit-cases",
                    "Changed cases",
                    "Any changed file",
                    "format-all"
                ],
                vec![
                    "Test",
                    "integration-files",
                    "Changed files",
                    "Explicit runs only",
                    "unit-cases"
                ],
                vec![
                    "Test (disabled)",
                    "optional-all",
                    "All",
                    "Explicit runs only",
                    "—"
                ],
            ]
        );
        assert!(!rendered.markdown.contains("private/**"));
        assert!(!rendered.markdown.contains("-internal"));
        assert_eq!(serde_json::to_vec(&document).unwrap(), original);
        let (blocks, _) = project(
            &document,
            &WidthProfile::default(),
            &[],
            &HashMap::new(),
            None,
            &HashMap::new(),
            false,
            None,
            &HashSet::new(),
        )
        .unwrap();
        let header = blocks
            .iter()
            .find(|block| block.id.0 == "plan:section:checks")
            .unwrap();
        let ending = blocks
            .iter()
            .find(|block| block.id == header.metadata.fold[0].end.block)
            .unwrap();
        assert!(ending.text.row(0).unwrap().contains("optional-all"));
        for columns in [60, 100, 160] {
            let profile = WidthProfile {
                columns,
                ..Default::default()
            };
            let (blocks, _) = project(
                &document,
                &profile,
                &[],
                &HashMap::new(),
                None,
                &HashMap::new(),
                false,
                None,
                &HashSet::new(),
            )
            .unwrap();
            let checks = blocks
                .iter()
                .filter(|block| block.id.0.starts_with("plan:configured-checks:"))
                .collect::<Vec<_>>();
            for block in &checks {
                for line in block.text.wire_rows() {
                    assert!(
                        profile.cells(line, 0).unwrap() < columns,
                        "{columns}: {line}"
                    );
                }
            }
            if columns == 60 {
                assert!(checks.iter().any(|block| block.text.row_count() > 1));
            }
        }
    }

    #[test]
    fn planned_check_commands_are_invalidated_when_plan_inputs_change() {
        let mut document = crate::plan::document::test_fixture("preview", "Preview");
        let design = document.design.get_or_insert_with(Default::default);
        design.check_preview = vec![crate::checks::CheckPreview {
            id: "package-tests".into(), category: crate::checks::CheckCategory::Test,
            command: "cargo test --package combat --tests".into(), scope: crate::checks::CheckScope::ChangedFiles,
            reason: "Changed: crates/combat/src/lib.rs".into(), pending: false, selected: true,
        }];
        design.check_preview_source = design.check_preview_fingerprint().unwrap();
        let rendered = render(&document).unwrap();
        assert!(rendered.markdown.find(" Checks ").unwrap() < rendered.markdown.find(" Verification ").unwrap());
        assert!(rendered.markdown.contains("cargo test --package combat --tests"));
        assert!(rendered.markdown.contains("Will run"));
        document.design.as_mut().unwrap().proposed.insert("new.rs".into(), "fn added();".into());
        let rendered = render(&document).unwrap();
        assert!(!rendered.markdown.contains("cargo test --package combat --tests"));
        assert!(rendered.markdown.find(" Checks ").unwrap() < rendered.markdown.find(" Verification ").unwrap());
    }

    #[test]
    fn public_filter_removes_hunks_containing_only_private_context_and_gaps() {
        let mut baseline = String::from("pub fn visible() -> u64;\nstruct Hidden;\nimpl Hidden {\n");
        for index in 0..40 {
            baseline.push_str(&format!("  fn method_{index:02}(&self) -> u64;\n"));
        }
        baseline.push_str("}\n");
        let proposed = baseline.replace("visible() -> u64", "visible() -> String")
            .replace("method_15(&self) -> u64", "method_15(&self) -> String");
        let mut design = super::super::DeclarationDesign::default();
        design.baseline.insert("lib.rs".into(), super::super::DeclarationFile {
            text: baseline, source_digest: String::new(),
        });
        design.proposed.insert("lib.rs".into(), proposed);
        let mut document = crate::plan::document::test_fixture("private-hunks", "Visibility");
        document.design = Some(design);
        let (blocks, _) = project(&document, &Default::default(), &[], &HashMap::new(), None,
            &HashMap::new(), true, None, &HashSet::new()).unwrap();
        let rows = blocks.iter().filter_map(|block| block.text.row(0)).collect::<Vec<_>>();
        assert_eq!(rows.iter().filter(|row| row.starts_with("@@")).count(), 1, "{rows:?}");
        assert!(!rows.iter().any(|row| row.trim() == "..."), "private omission survived: {rows:?}");
        assert!(rows.iter().any(|row| row.contains("visible")));
        assert!(!rows.iter().any(|row| row.contains("method_")));
    }

    #[test]
    fn modified_files_show_bounded_context_until_reference_navigation() {
        let mut document = crate::plan::document::test_fixture("context", "Review changes");
        let mut baseline = String::from("pub struct State {\n");
        for index in 0..32 {
            baseline.push_str(&format!("  pub field_{index:02}: u64,\n"));
        }
        baseline.push_str("}\n");
        let proposed = baseline.replace("field_06: u64", "field_06: String")
            .replace("field_25: u64", "field_25: String");
        let mut design = super::super::DeclarationDesign::default();
        design.baseline.insert("state.rs".into(), super::super::DeclarationFile {
            text: baseline, source_digest: String::new(),
        });
        design.proposed.insert("state.rs".into(), proposed);
        document.design = Some(design);

        for public_only in [false, true] {
            let (blocks, _) = project(&document, &Default::default(), &[], &HashMap::new(), None,
                &HashMap::new(), public_only, None, &HashSet::new()).unwrap();
            let rows = blocks.iter().filter_map(|block| block.text.row(0)).collect::<Vec<_>>();
            for index in [6, 25] {
                assert!(rows.iter().any(|row| row.contains(&format!("field_{index:02}:"))));
            }
            for index in [2, 3, 9, 10, 16, 21, 22, 28, 29] {
                assert!(!rows.iter().any(|row| row.contains(&format!("field_{index:02}:"))));
            }
            assert_eq!(rows.iter().filter(|row| row.starts_with("@@")).count(), 2);

            let revealed = HashSet::from([("state.rs".into(), "proposed".into(), 18)]);
            let (blocks, _) = project(&document, &Default::default(), &[], &HashMap::new(), None,
                &HashMap::new(), public_only, None, &revealed).unwrap();
            assert!(blocks.iter().any(|block| block.text.row(0).is_some_and(|row| row.contains("field_16:"))));
        }
    }

    #[tokio::test]
    async fn implementation_context_uses_source_syntax_and_omissions_have_no_targets() {
        use forge_diff::syntax::{SyntaxEngine, SyntaxLanguage, SyntaxRequest};
        use forge_diff::workers::{AnalysisPool, PoolLimits, WorkPriority};
        let mut baseline = String::from("pub struct State;\nimpl State {\n");
        for index in 0..40 { baseline.push_str(&format!("  pub fn method_{index:02}(&self) -> u64;\n")); }
        baseline.push_str("}\n");
        let proposed = baseline.replace("method_15(&self) -> u64", "method_15(&self) -> String");
        let mut design = super::super::DeclarationDesign::default();
        design.baseline.insert("state.rs".into(), super::super::DeclarationFile { text: baseline.clone(), source_digest: String::new() });
        design.proposed.insert("state.rs".into(), proposed.clone());
        let mut document = crate::plan::document::test_fixture("context-syntax", "Syntax context");
        document.design = Some(design);
        let engine = SyntaxEngine::new(std::sync::Arc::new(AnalysisPool::new(PoolLimits { workers: 1, jobs: 2 })));
        let mut syntax = HashMap::new();
        for (side, text) in [("baseline", baseline), ("proposed", proposed)] {
            let presentation = super::super::calls::present("state.rs", &text, &[]).unwrap();
            let handle = engine.analyze(SyntaxRequest {
                source: SourceVersion::new(presentation.plain.as_bytes().to_vec(), Representation::DisplayOnly).unwrap(),
                language: SyntaxLanguage::Rust, priority: WorkPriority::Foreground, deadline: None,
            }).await.unwrap();
            syntax.insert(("state.rs".into(), side.into()), super::super::review_source::ReviewSyntax { declaration: handle, algorithm: None });
        }
        for public_only in [false, true] {
            let (blocks, _) = project(&document, &Default::default(), &[], &HashMap::new(), None, &syntax, public_only, None, &HashSet::new()).unwrap();
            let implementation = blocks.iter().filter(|block| block.text.row(0) == Some("impl State {")).collect::<Vec<_>>();
            assert_eq!(implementation.len(), 1);
            assert!(implementation[0].metadata.visible_decoration.iter().any(|capture| capture.capture.starts_with("@keyword") && capture.capture.ends_with(".rust")));
            let omissions = blocks.iter().filter(|block| block.text.row(0).is_some_and(|text| text.trim() == "...")).collect::<Vec<_>>();
            assert!(!omissions.is_empty());
            assert!(omissions.iter().all(|block| block.metadata.target.is_empty()));
            assert!(!blocks.iter().any(|block| block.text.row(0).is_some_and(|text| text.contains("method_30"))));
        }
    }

    #[test]
    fn change_only_edits_have_semantic_targets_and_closed_function_folds() {
        let mut document = crate::plan::document::test_fixture("change", "Behavior change");
        let mut design = super::super::DeclarationDesign::default();
        let declaration = "pub fn run();\n";
        design.baseline.insert("main.rs".into(), super::super::DeclarationFile { text: declaration.into(), source_digest: String::new() });
        design.proposed.insert("main.rs".into(), declaration.into());
        design.proposed_calls.insert("main.rs".into(), vec![super::super::FunctionBody { evidence: None, owner: "run".into(), call: None, behavior: Some(crate::plan::FunctionBehavior::Change("Stop retrying authentication failures.\nRecord the final attempt.".into())) }]);
        document.design = Some(design);
        let original = document.clone();
        let rendered = render(&document).unwrap();
        assert!(rendered.markdown.contains("Change") && rendered.markdown.contains("Stop retrying"));
        assert!(!rendered.markdown.contains("Calls"));
        for public_only in [false, true] {
            let (blocks, targets) = project(&document, &Default::default(), &[], &HashMap::new(), None, &HashMap::new(), public_only, None, &HashSet::new()).unwrap();
            assert_eq!(targets.values().filter(|anchor| matches!(anchor.target, PlanReviewTarget::Behavior { .. })).count(), 3);
            assert!(blocks.iter().filter(|block| block.text.row(0).unwrap().contains("pub fn run")).any(|heading| heading.metadata.fold.iter().any(|fold| fold.closed && fold.collapsed_suffix.as_ref().is_some_and(|suffix| !suffix.contains("[changed]")))));
            for block in blocks.iter().filter(|block| block.text.row(0).unwrap().contains("Stop retrying")) {
                assert!(block.metadata.visible_decoration.iter().any(|capture| capture.capture == "@comment"));
            }
        }
        assert_eq!(document, original);
    }

    #[tokio::test]
    async fn synthetic_function_bodies_preserve_following_declaration_syntax() {
        use std::sync::Arc;
        use forge_diff::syntax::{SyntaxEngine, SyntaxLanguage, SyntaxRequest};
        use forge_diff::workers::{AnalysisPool, PoolLimits, WorkPriority};

        let mut document = crate::plan::document::test_fixture("syntax-rows", "Syntax rows");
        let mut design = super::super::DeclarationDesign::default();
        let before = "pub struct ArenaPlugin;\nimpl ArenaPlugin { pub fn new(); }\n/// Registers the old arena systems.\nimpl Plugin for ArenaPlugin {}\n";
        let after = before.replace("old", "shared");
        design.baseline.insert("arena.rs".into(), super::super::DeclarationFile { text: before.into(), source_digest: String::new() });
        design.proposed.insert("arena.rs".into(), after.clone());
        let calls = vec![super::super::FunctionBody { evidence: None, behavior: None, owner: "ArenaPlugin::new".into(), call: Some(vec![
            super::super::CallSite { kind: super::super::CallKind::Call, name: "ArenaConfig::validate".into(), source: None, unresolved: false },
            super::super::CallSite { kind: super::super::CallKind::Property, name: "ArenaConfig::enabled".into(), source: None, unresolved: false },
        ]) }];
        design.baseline_calls.insert("arena.rs".into(), calls.clone());
        let mut proposed = calls;
        proposed[0].behavior = Some(crate::plan::FunctionBehavior::Change("Reject invalid configuration before registration.".into()));
        design.proposed_calls.insert("arena.rs".into(), proposed);
        document.design = Some(design);
        let original = document.clone();
        let engine = SyntaxEngine::new(Arc::new(AnalysisPool::new(PoolLimits { workers: 1, jobs: 2 })));
        let mut syntax = HashMap::new();
        for (side, text) in [("baseline", before), ("proposed", after.as_str())] {
            let plain = forge_diff::syntax::DeclarationOverview::present("arena.rs", text).unwrap().text;
            let handle = engine.analyze(SyntaxRequest {
                source: SourceVersion::new(plain.into_bytes(), Representation::DisplayOnly).unwrap(),
                language: SyntaxLanguage::Rust, priority: WorkPriority::Foreground, deadline: None,
            }).await.unwrap();
            syntax.insert(("arena.rs".into(), side.into()), super::super::review_source::ReviewSyntax { declaration: handle, algorithm: None });
        }
        for public_only in [false, true] {
            let design = document.design.as_ref().unwrap();
            let mut revealed = HashSet::new();
            for (side, text, calls) in [
                ("baseline", before, design.baseline_calls["arena.rs"].as_slice()),
                ("proposed", after.as_str(), design.proposed_calls["arena.rs"].as_slice()),
            ] {
                let presentation = super::super::calls::present("arena.rs", text, calls).unwrap();
                let row = presentation.declaration.text.lines().position(|line| line == "impl Plugin for ArenaPlugin {}").unwrap();
                revealed.insert(("arena.rs".into(), side.into(), row as u32));
            }
            let (blocks, _) = project(&document, &Default::default(), &[], &HashMap::new(), None, &syntax, public_only, None, &revealed).unwrap();
            let mut comments = 0;
            let mut implementations = 0;
            let mut synthetic_rows = 0;
            for block in blocks {
                let text = block.text.row(0).unwrap();
                let captures = &block.metadata.visible_decoration;
                if text.starts_with("/// Registers") {
                    comments += 1;
                    assert!(captures.iter().any(|capture| capture.capture.starts_with("@comment")), "{text}: {captures:?}");
                    assert!(captures.iter().any(|capture| capture.capture.starts_with("@comment.documentation") && capture.range.start.column == 0 && capture.range.end.column == text.len()), "{text}: {captures:?}");
                    assert!(captures.iter().all(|capture| !capture.capture.starts_with("@keyword") && !capture.capture.starts_with("@type") && !capture.capture.starts_with("@function")), "{text}: {captures:?}");
                }
                if text == "impl Plugin for ArenaPlugin {}" {
                    implementations += 1;
                    for name in ["Plugin", "ArenaPlugin"] {
                        assert!(captures.iter().any(|capture| capture.capture.starts_with("@type") && &text[capture.range.start.column..capture.range.end.column] == name), "{text}: {captures:?}");
                    }
                }
                if matches!(text.trim(), "Change" | "Reject invalid configuration before registration." | "Calls" | "Accesses" | "ArenaConfig::validate" | "ArenaConfig::enabled") {
                    synthetic_rows += 1;
                    assert!(captures.iter().all(|capture| !capture.capture.ends_with(".rust")), "synthetic row received declaration syntax: {text}");
                }
            }
            assert_eq!(comments, 2);
            assert!(implementations > 0);
            assert!(synthetic_rows >= 2);
        }
        assert_eq!(document, original);
    }

    #[test]
    fn matched_function_fold_keeps_both_revisions_and_excludes_new_declarations() {
        let mut document = crate::plan::document::test_fixture("replacement", "Replace main");
        let mut design = super::super::DeclarationDesign::default();
        design.baseline.insert("main.rs".into(), super::super::DeclarationFile {
            text: "/// Creates the original application.\nfn main();\n\nfn setup();\n".into(),
            source_digest: String::new(),
        });
        design.proposed.insert("main.rs".into(),
            "pub struct PlayerAbilities { pub health: u32 }\n\n/// Creates the configured application.\nfn main();\n\nfn configure_game();\n\nfn setup();\n".into());
        let body = |owner: &str, target: &str, change: Option<&str>| super::super::FunctionBody {
            evidence: None, owner: owner.into(), behavior: change.map(|text| crate::plan::FunctionBehavior::Change(text.into())),
            call: Some(vec![super::super::CallSite {
                kind: super::super::CallKind::Call, name: target.into(), source: None, unresolved: false,
            }]),
        };
        let mut original = body("main", "setup", None);
        for name in ["restart_round", "move_player", "chase_player", "resolve_collisions", "update_hud"] {
            original.call.as_mut().unwrap().push(super::super::CallSite {
                kind: super::super::CallKind::Call, name: name.into(), source: None, unresolved: false,
            });
        }
        design.baseline_calls.insert("main.rs".into(), vec![original.clone()]);
        original.owner = "configure_game".into();
        design.proposed_calls.insert("main.rs".into(), vec![
            body("main", "configure_game", Some("Use the shared gameplay configuration.")),
            original,
        ]);
        document.design = Some(design);
        for public_only in [false, true] {
            let (blocks, _) = project(&document, &Default::default(), &[], &HashMap::new(), None,
                &HashMap::new(), public_only, None, &HashSet::new()).unwrap();
            let collapsed = forge_buffer::collapse::project(blocks.clone(), &HashMap::new()).unwrap();
            let text = collapsed.iter().flat_map(|block| block.text.wire_rows()).collect::<Vec<_>>().join("\n");
            assert!(text.contains("pub struct PlayerAbilities"), "{text}");
            assert!(text.contains("Creates the original application"), "{text}");
            assert!(text.contains("Creates the configured application"), "{text}");
            assert_eq!(text.matches("fn main()").count(), 1, "{text}");
            assert!(!text.contains("restart_round"), "{text}");
            let new = blocks.iter().find(|block| block.metadata.fold.iter()
                .any(|fold| fold.id.0.contains(":proposed:") && block.text.row(0) == Some("fn main() {"))).unwrap();
            let new_fold = new.metadata.fold.iter().find(|fold| fold.id.0.contains(":proposed:")).unwrap().id.clone();
            assert!(new.metadata.gutter.iter().flat_map(|gutter| &gutter.chunk).any(|chunk| chunk.text == "~"));
            let expanded = forge_buffer::collapse::project(blocks.clone(),
                &HashMap::from([(new_fold.clone(), false)])).unwrap();
            let text = expanded.iter().flat_map(|block| block.text.wire_rows()).collect::<Vec<_>>().join("\n");
            assert_eq!(text.matches("fn main()").count(), 1, "{text}");
            assert!(text.contains("pub struct PlayerAbilities"), "{text}");
            assert!(text.contains("restart_round"), "{text}");
            assert!(text.contains("Use the shared gameplay configuration."), "{text}");
            if !public_only {
                assert!(text.contains("fn configure_game()"), "{text}");
            }
        }
    }

    #[test]
    fn call_rows_highlight_qualified_types_methods_and_properties() {
        let mut document = crate::plan::document::test_fixture("call-colors", "Call colors");
        let mut design = super::super::DeclarationDesign::default();
        design.proposed.insert("lib.rs".into(), "pub fn main();\npub struct Projectile;\npub type projectile_alias = Projectile;\npub const MAX_SHOTS: u32;\n".into());
        let cases = [
            ("App::add_plugins", super::super::CallKind::Call, vec![("App", "@type"), ("add_plugins", "@function.method.call")]),
            ("std::time::Instant::now", super::super::CallKind::Call, vec![("std", "@module"), ("time", "@module"), ("Instant", "@type"), ("now", "@function.method.call")]),
            ("validate_policy", super::super::CallKind::Call, vec![("validate_policy", "@function.call")]),
            ("client.send", super::super::CallKind::Call, vec![("client", "@variable"), ("send", "@function.method.call")]),
            ("État::更新", super::super::CallKind::Call, vec![("État", "@type"), ("更新", "@function.method.call")]),
            ("Client::count", super::super::CallKind::Property, vec![("Client", "@type"), ("count", "@variable.member")]),
            ("Projectile", super::super::CallKind::Value, vec![("Projectile", "ForgeWalkthroughType")]),
            ("projectile_alias", super::super::CallKind::Value, vec![("projectile_alias", "ForgeWalkthroughType")]),
            ("MAX_SHOTS", super::super::CallKind::Value, vec![("MAX_SHOTS", "@variable")]),
            ("construct_alias", super::super::CallKind::Construct, vec![("construct_alias", "ForgeWalkthroughType")]),
            ("Projectile::ready", super::super::CallKind::Construct, vec![("Projectile", "@type"), ("ready", "@function.method.call")]),
        ];
        design.proposed_calls.insert("lib.rs".into(), vec![super::super::FunctionBody { evidence: None,
            owner: "main".into(),
            behavior: None,
            call: Some(cases.iter().map(|(name, kind, _)| super::super::CallSite {
                name: (*name).into(), kind: *kind, source: None, unresolved: false,
            }).collect()),
        }]);
        document.design = Some(design);
        let original = document.clone();
        for public_only in [false, true] {
            let (blocks, _) = project(&document, &Default::default(), &[], &HashMap::new(), None, &HashMap::new(), public_only, None, &HashSet::new()).unwrap();
            if !public_only {
                println!("TYPE_ACCESS_FIXTURE:{}", serde_json::json!({"document":"type-accesses","revision":0,"block":blocks}));
            }
            for (name, _, expected) in &cases {
                let row = blocks.iter().find(|block| block.text.row(0).is_some_and(|text| text.trim() == *name)).unwrap();
                let text = row.text.row(0).unwrap();
                let captures = row.metadata.visible_decoration.iter().map(|decoration| (
                    &text[decoration.range.start.column..decoration.range.end.column], decoration.capture.as_str(),
                )).collect::<Vec<_>>();
                assert_eq!(&captures, expected, "{name}");
                assert!(row.metadata.decoration.iter().any(|decoration| decoration.capture == "ForgeAddBg"));
            }
        }
        assert_eq!(document, original);
    }

    #[test]
    fn rust_tests_are_hidden_only_in_review_projection() {
        let mut document = crate::plan::document::test_fixture("test-filter", "Review production changes");
        let mut design = super::super::DeclarationDesign::default();
        design.baseline.insert("src/lib.rs".into(), super::super::DeclarationFile {
            text: "pub fn production();\n/// Old test helpers.\n#[cfg(test)]\nmod tests { fn old_helper(); #[test] fn old_test(); }\n".into(),
            source_digest: "fixture".into(),
        });
        design.proposed.insert("src/lib.rs".into(), "pub fn production(value: u32);\n/// New test helpers.\n#[cfg(test)]\nmod tests { fn new_helper(); #[test] fn new_test(); }\n".into());
        design.document.tests = serde_json::from_value(serde_json::json!([
            {"file":"src/lib.rs","cases":[{"name":"tests::new_test","change":"new","description":"Checks the new test contract."}]}
        ])).unwrap();
        document.design = Some(design);
        let saved = serde_json::to_string(&document).unwrap();
        assert!(render(&document).unwrap().markdown.contains("new_helper"));
        for public_only in [false, true] {
            let (blocks, targets) = project(&document, &Default::default(), &[], &HashMap::new(), None, &HashMap::new(), public_only, None, &HashSet::new()).unwrap();
            let changes = blocks.iter().take_while(|block| block.id.0 != "plan:section:tests")
                .flat_map(|block| block.text.wire_rows()).collect::<Vec<_>>().join("\n");
            assert!(changes.contains("production"));
            for hidden in ["mod tests", "old_helper", "new_helper", "old_test", "new_test", "test helpers", "cfg(test)"] {
                assert!(!changes.contains(hidden), "{hidden}: {changes}");
            }
            assert!(targets.values().any(|anchor| matches!(&anchor.target, PlanReviewTarget::TestCase { name, .. } if name == "tests::new_test")));
            forge_buffer::document::BufferDocument::new(forge_buffer::identity::DocumentId("test-filter".into()), blocks).unwrap();
        }
        assert_eq!(serde_json::to_string(&document).unwrap(), saved);
    }

    #[test]
    fn test_inventory_uses_leaf_labels_and_declaration_line_numbers() {
        let mut design = super::super::DeclarationDesign::default();
        design.baseline.insert("src/combat.rs".into(), super::super::DeclarationFile {
            text: "mod tests {\n  fn kept();\n  fn gone();\n}\n".into(), source_digest: String::new(),
        });
        design.proposed.insert("src/combat.rs".into(), "mod tests {\n  fn added();\n  fn kept();\n}\n".into());
        design.document.tests = serde_json::from_value(serde_json::json!([
            {"file":"src/combat.rs","cases":[
                {"name":"combat::tests::added","change":"new","description":"New coverage."},
                {"name":"combat::tests::kept","change":"modified","description":"Changed coverage."},
                {"name":"combat::tests::gone","change":"removed","description":"Retired coverage."},
                {"name":"combat::tests::unwritten","change":"new","description":"Not declared yet."}]}
        ])).unwrap();
        let (blocks, anchors) = test_inventory(&design).unwrap();
        for (label, old, new) in [("added", "", "2"), ("kept", "2", "3"), ("gone", "3", ""), ("unwritten", "", "")] {
            let block = blocks.iter().find(|block| block.text.row(0) == Some(label)).unwrap();
            let gutter = &block.metadata.gutter[0].chunk;
            assert_eq!(gutter[0].text.trim(), old, "{label}");
            assert_eq!(gutter[2].text.trim(), new, "{label}");
        }
        assert!(anchors.iter().any(|anchor| matches!(&anchor.target, PlanReviewTarget::TestCase { name, .. } if name == "combat::tests::added")));
    }

    #[test]
    fn test_inventory_preserves_file_folds_targets_and_change_markers() {
        let mut document = crate::plan::document::test_fixture("tests", "Tests");
        let mut design = super::super::DeclarationDesign::default();
        design.proposed.insert("src/tool.rs".into(), "pub fn run();\n\n".into());
        design.document.tests = serde_json::from_value(serde_json::json!([
            {"file":"src/tool.rs", "cases":[
                {"name":"tests::new_case", "change":"new", "description":"A long scenario retains its full expected result when the review window becomes narrow."},
                {"name":"tests::changed_case", "change":"modified", "description":"Updated expectation."},
                {"name":"tests::old_case", "change":"removed", "description":"The supported behavior was removed."}]},
            {"file":"tests/runtime.rs", "cases":[
                {"name":"existing_case", "change":"reused", "description":"Existing coverage remains applicable."}]}
        ])).unwrap();
        document.design = Some(design);
        for public_only in [false, true] {
            let width = WidthProfile { columns: 40, ..Default::default() };
            let (block, target) = project(&document, &width, &[], &HashMap::new(), None, &HashMap::new(), public_only, None, &HashSet::new()).unwrap();
            let rows = block.iter().flat_map(|block| block.text.wire_rows()).collect::<Vec<_>>();
            assert!(rows.contains(&" Tests · 1 new · 1 modified · 1 removed · 1 reused "));
            let tests_start = block.iter().position(|block| block.id.0 == "plan:section:tests").unwrap();
            let heading = &block[tests_start];
            let text = heading.text.row(0).unwrap();
            for (label, capture) in [(" Tests ", "RenderMarkdownH1"), ("1 new", "ForgeAddRange"),
                ("1 modified", "ForgeModifyRange"), ("1 removed", "ForgeDeleteRange"), ("1 reused", "Comment")] {
                let span = heading.metadata.decoration.iter().find(|span| span.capture == capture
                    && &text[span.range.start.column..span.range.end.column] == label);
                assert!(span.is_some(), "missing {capture} for {label}");
            }
            assert!(heading.metadata.decoration.iter().filter(|span| span.capture == "RenderMarkdownH1")
                .all(|span| span.range.end.column == " Tests ".len()));
            assert_eq!(block[tests_start - 1].text.row(0), Some(""));
            assert_eq!(block[tests_start - 1].id.0, "plan:section:changes:separator");
            assert!(block[tests_start - 1].metadata.decoration.is_empty());
            assert!(block[tests_start - 1].metadata.gutter.is_empty());
            let changes = block.iter().find(|block| block.id.0 == "plan:section:changes").unwrap();
            let endpoint = &changes.metadata.fold[0].end;
            let end_index = block.iter().position(|block| block.id == endpoint.block).unwrap();
            assert!(end_index + usize::from(endpoint.position.row > 0) <= tests_start - 1);
            for expected in ["new_case", "changed_case", "old_case", "existing_case"] {
                assert!(rows.contains(&expected));
            }
            let case = block.iter().find(|block| block.text.row(0) == Some("changed_case")).unwrap();
            let name = case.metadata.visible_decoration.iter().find(|decoration| decoration.capture == "@function").unwrap();
            assert_eq!(&case.text.row(0).unwrap()[name.range.start.column..name.range.end.column], "changed_case");
            assert!(case.metadata.target.iter().any(|range| matches!(&target[&range.id].target,
                PlanReviewTarget::TestCase { name, .. } if name == "tests::changed_case")));
            assert_eq!(document.design.as_ref().unwrap().document.tests[0].cases[1].name, "tests::changed_case");
            assert!(case.metadata.decoration.iter().any(|decoration| decoration.capture == "ForgeModifyBg" && decoration.range.end.row == 1));
            assert!(case.metadata.gutter[0].chunk.iter().any(|chunk| chunk.text == "~" && chunk.capture == "ForgeModifyLineNr"));
            let files = block.iter().filter(|block| block.id.0.starts_with("plan:tests:file:") && !block.id.0["plan:tests:file:".len()..].contains(':')).collect::<Vec<_>>();
            assert_eq!(files.len(), 2);
            assert!(files.iter().all(|file| file.metadata.fold.len() == 1 && !file.metadata.fold[0].closed));
            assert!(files[0].text.row(0).unwrap().starts_with("New"));
            assert!(files[1].text.row(0).unwrap().starts_with("Unchanged"));
            assert!(files[0].text.row(0).unwrap().ends_with("src/tool.rs +1 -1"));
            assert!(files[1].text.row(0).unwrap().ends_with("tests/runtime.rs +0 -0"));
            if !public_only {
                println!("TEST_NAME_FIXTURE:{}", serde_json::json!({"document":"test-names","revision":0,"block":block}));
            }
            assert!(!rows.iter().any(|row| row.contains("= existing_case")));
            assert!(target.values().any(|anchor| anchor.json_path == "/design/document/tests/1/cases/0"));
            let tests_start = block.iter().position(|block| block.id.0 == "plan:section:tests").unwrap();
            let checks_start = block.iter().position(|block| block.id.0 == "plan:section:checks").unwrap();
            let verification_start = block.iter().position(|block| block.id.0 == "plan:section:verification").unwrap();
            assert!(tests_start < checks_start && checks_start < verification_start);
            let endpoint = &block[tests_start].metadata.fold[0].end.block;
            assert!(block.iter().position(|block| &block.id == endpoint).unwrap() < verification_start);
            assert_eq!(block[tests_start..checks_start].iter().map(|block| block.metadata.fold.len()).sum::<usize>(), 3);
            let changes = block.iter().find(|block| block.id.0 == "plan:section:changes").unwrap();
            let endpoint = &changes.metadata.fold[0].end.block;
            assert!(block.iter().position(|block| &block.id == endpoint).unwrap() < tests_start);
            forge_buffer::document::BufferDocument::new(forge_buffer::identity::DocumentId("test-inventory".into()), block).unwrap();
        }
        document.design.as_mut().unwrap().document.tests.clear();
        assert!(render(&document).unwrap().markdown.contains("Tests · None planned"));
    }

    #[test]
    fn structured_flows_render_native_symbols_in_both_review_views() {
        let mut document = crate::plan::document::test_fixture("flows", "Cancel work");
        let mut design = super::super::DeclarationDesign::default();
        design.document.design = "Keep the published value while cancelling pending work.".into();
        design.proposed.insert("src/flow.rs".into(), "fn cancel_request();\nstruct PendingWork;\nimpl PendingWork { fn stop(); }\n".into());
        design.document.flows = serde_json::from_value(serde_json::json!([{
            "title":"Cancel pending work", "description":"Retain the published value.",
            "trigger":"cancellation requested", "steps":[
                {"action":"cancel_request"},
                {"action":"PendingWork::stop", "reads":["active handle"],
                 "effects":[{"target":"*literal* published_value", "change":"preserve"}]}
            ]
        }])).unwrap();
        document.design = Some(design);
        for public_only in [false, true] {
            let (block, target) = project(&document, &Default::default(), &[], &HashMap::new(), None, &HashMap::new(), public_only, None, &HashSet::new()).unwrap();
            let heading = block.iter().find(|block| block.text.row(0) == Some(" Flows ")).unwrap();
            assert!(heading.metadata.decoration.iter().any(|style| {
                style.capture == "RenderMarkdownH1"
                    && style.range.start.column == 0
                    && style.range.end.column == " Flows ".len()
            }));
            assert!(!heading.metadata.fold.is_empty());
            let node = block.iter().find(|block| block.text.row(0) == Some("  → cancel_request")).unwrap();
            assert!(!node.metadata.markdown);
            assert!(node.metadata.decoration.iter().any(|style| style.capture == "@function"));
            assert!(!node.metadata.decoration.iter().any(|style| style.capture == "@markup.raw.block"));
            assert!(node.metadata.target.iter().any(|range| matches!(&target[&range.id].target, PlanReviewTarget::InlineSymbol { name } if name == "cancel_request")));
            let member = block.iter().find(|block| block.text.row(0).is_some_and(|text| text.contains("PendingWork::stop"))).unwrap();
            assert!(member.metadata.target.iter().any(|range| matches!(&target[&range.id].target, PlanReviewTarget::InlineSymbol { name } if name == "PendingWork::stop")));
            assert!(block.iter().filter(|block| block.id.0.starts_with("plan:metadata:flows:")).all(|block| !block.text.row(0).unwrap_or_default().starts_with("```")));
            assert!(block.iter().any(|block| block.text.row(0).is_some_and(|row| row.contains("*literal*"))));
            assert!(target.values().any(|anchor| anchor.json_path == "/design/document/flows"));
            forge_buffer::document::BufferDocument::new(forge_buffer::identity::DocumentId("flows".into()), block).unwrap();
        }
    }

    #[test]
    fn validation_evidence_stays_out_of_review_projection() {
        let mut document = crate::plan::document::test_fixture("validation", "Validation");
        let mut design = super::super::DeclarationDesign::default();
        design.proposed.insert("lib.rs".into(), "pub struct State;\n".into());

        design.document.verification = vec!["Resize and confirm `State` remains visible.".into()];
        design.document.decisions.push(super::super::design_document::DesignDecision {
            decision: "Keep state explicit.".into(), rationale: "Callers can inspect it.".into(),
        });
        design.validation = Some(crate::declaration::DeclarationValidation {
            diagnostic: vec![crate::declaration::DeclarationDiagnostic {
                path: "plan".into(), line: 1, column: 0, reference: String::new(), error: false,
                reason: "Cargo source resolution failed:\n  dependency source is unavailable".into(),
            }],
            ..Default::default()
        });
        document.design = Some(design);
        for public_only in [false, true] {
            let (block, target) = project(&document, &Default::default(), &[], &HashMap::new(), None, &HashMap::new(), public_only, None, &HashSet::new()).unwrap();
            assert!(block.iter().flat_map(|block| block.text.wire_rows()).all(|line| !line.contains("Cargo source resolution failed") && !line.contains("dependency source is unavailable")));
            let checks = block.iter().find(|block| block.id.0 == "plan:configured-checks:0").unwrap();
            assert!(!checks.metadata.markdown);
            let decision = block.iter().find(|block| block.id.0 == "plan:metadata:decisions:0").unwrap();
            assert_eq!(decision.text.row(0), Some("- **Keep state explicit.** Callers can inspect it."));
            assert!(decision.metadata.markdown);
            assert!(decision.metadata.decoration.iter().any(|style| style.capture == "ForgeStatusHeader"));
            assert!(block.iter().filter(|block| block.id.0.starts_with("plan:metadata:verification:")).all(|block| block.metadata.markdown));
            assert!(block.iter().filter(|block| block.id.0.starts_with("plan:section:")).all(|block| !block.metadata.markdown));
            assert!(!target.values().any(|anchor| anchor.json_path == "/design/document/checks"));
            assert!(target.values().any(|anchor| anchor.json_path == "/design/document/verification"));
            forge_buffer::document::BufferDocument::new(forge_buffer::identity::DocumentId("validation".into()), block.clone()).unwrap();
            let declaration = block.iter().find(|block| block.text.row(0) == Some("pub struct State;")).unwrap();
            assert!(!declaration.metadata.markdown);
            assert!(declaration.metadata.target.iter().any(|range| target.contains_key(&range.id)));
        }
    }

    #[test]
    fn declaration_folds_keep_attributes_and_valid_filtered_endpoints() {
        let mut document = crate::plan::document::test_fixture("containers", "Containers");
        let mut design = super::super::DeclarationDesign::default();
        design.document.design = "Expose configuration failures and state.".into();
        design.proposed.insert("config.rs".into(), "#[derive(Debug)]\npub enum ConfigError {\n  /// Invalid arena size.\n  ArenaSize,\n  Radius,\n}\n\npub struct State {\n  pub count: u64,\n  private: u64,\n}\n\nimpl State {\n  pub fn count(&self) -> u64;\n  fn hidden();\n}\n\npub struct Empty {\n  private: u64,\n}\n".into());
        document.design = Some(design);
        for public_only in [false, true] {
            let (block, _) = project(&document, &Default::default(), &[], &HashMap::new(), None, &HashMap::new(), public_only, None, &HashSet::new()).unwrap();
            let index: HashMap<_, _> = block.iter().enumerate().map(|(row, block)| (block.id.clone(), row)).collect();
            let mut declarations = 0;
            for (row, owner) in block.iter().enumerate() {
                for fold in &owner.metadata.fold {
                    let Some(suffix) = &fold.collapsed_suffix else { continue };
                    declarations += 1;
                    assert_eq!(suffix, "...}");
                    assert!(index[&fold.end.block] > row);
                    assert!(fold.heading_start.as_ref().is_some_and(|heading| index[&heading.block] <= row));
                    assert_eq!(fold.closed, owner.text.row(0).unwrap().contains("enum ConfigError"));
                    if owner.text.row(0).unwrap().contains("enum ConfigError") {
                        assert_eq!(block[index[&fold.heading_start.as_ref().unwrap().block]].text.row(0), Some("#[derive(Debug)]"));
                    }
                }
            }
            assert_eq!(declarations, if public_only { 3 } else { 4 });
            if public_only {
                assert!(block.iter().any(|block| block.text.row(0) == Some("pub struct Empty {}")));
            }
        }
    }

    #[test]
    fn behavior_only_description_retains_comment_targets_in_both_views() {
        let mut document = crate::plan::document::test_fixture("summary", "Summary");
        let mut design = super::super::DeclarationDesign::default();
        design.document.objective = "Support observable cancellation and safe texture replacement.".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Requests retain their cancellation state until all pending work finishes. Publication occurs only after upload completion and the previous allocation remains alive until submitted frames finish.".into();
        document.design = Some(design);
        let saved = serde_json::to_vec(&document).unwrap();
        let rendered = render(&document).unwrap();
        assert!(
            rendered
                .markdown
                .starts_with(" Objective \nSupport observable cancellation and safe texture replacement.\n\n Requirements ")
        );
        let task_anchor = rendered.navigation.resolve_line(1).unwrap();
        assert_eq!(task_anchor.json_path, "/design/document/objective");
        assert_eq!(task_anchor.target, PlanReviewTarget::Section { section: super::super::PlanSection::Objective });
        let design_line = rendered.markdown.lines().position(|line| line == " Design ").unwrap() as u32 + 1;
        let anchor = rendered.navigation.resolve_line(design_line).unwrap();
        assert_eq!(anchor.json_path, "/design/document/design");
        let annotation = ReviewAnnotation {
                parent_id: None,
                kind: Default::default(),
                reply: None,
            id: "description".into(),
            anchor: None,
            source: super::super::PlanAnnotationInput {
                start_line: design_line + 1,
                end_line: design_line + 1,
                body: "Confirm the cancellation lifecycle".into(),
            },
        };
        for (public_only, columns) in [(false, 40), (true, 40), (false, 120), (true, 120)] {
            let (block, target) = project(
                &document,
                &WidthProfile { columns, ..WidthProfile::default() },
                &[annotation.clone()],
                &HashMap::new(),
                None,
                &HashMap::new(),
                public_only,
                None,
                &HashSet::new(),
            )
            .unwrap();
            let description = block
                .iter()
                .find(|block| block.id.0 == "plan:metadata:design:0")
                .unwrap();
            assert_eq!(description.text.row_count(), 1);
            assert_eq!(
                description.text.row(0),
                Some(document.design.as_ref().unwrap().document.design.as_str()),
            );
            let header = block
                .iter()
                .find(|block| block.id.0 == "plan:section:design")
                .unwrap();
            let endpoint = &header.metadata.fold[0].end;
            assert_eq!(endpoint.block.0, "plan:annotation:description");
            assert!(
                block
                    .iter()
                    .any(|block| block.text.row(0) == Some(" Changes "))
            );
            assert!(
                block
                    .iter()
                    .filter(|block| block.id.0.starts_with("plan:section:"))
                    .flat_map(|block| &block.metadata.fold)
                    .all(|fold| !fold.closed)
            );
            assert!(
                target
                    .values()
                    .any(|anchor| anchor.json_path == "/design/document/design")
            );
            assert!(
                block
                    .iter()
                    .flat_map(|block| block.text.wire_rows())
                    .any(|line| line.contains("Confirm the cancellation"))
            );
            forge_buffer::document::BufferDocument::new(
                forge_buffer::identity::DocumentId("summary".into()),
                block,
            )
            .unwrap();
        }
        assert_eq!(serde_json::to_vec(&document).unwrap(), saved);
    }

    #[test]
    fn public_filter_preserves_targets_folds_and_hidden_comment_storage() {
        let mut document = crate::plan::document::test_fixture("public", "Visibility");
        let mut design = super::super::DeclarationDesign::default();
        design.proposed.insert(
            "lib.rs".into(),
            "pub struct Api { pub value: u64, secret: u64 }\nstruct Hidden;\n".into(),
        );
        document.design = Some(design);
        let saved = serde_json::to_vec(&document).unwrap();
        let rendered = render(&document).unwrap();
        let private = rendered
            .navigation
            .anchor
            .iter()
            .find(|anchor| anchor.label.contains("secret:"))
            .unwrap();
        let annotation = ReviewAnnotation {
                parent_id: None,
                kind: Default::default(),
                reply: None,
            id: "private".into(),
            anchor: None,
            source: super::super::PlanAnnotationInput {
                start_line: private.line,
                end_line: private.line,
                body: "Private field review".into(),
            },
        };
        let (block, target) = project(
            &document,
            &WidthProfile::default(),
            &[annotation],
            &HashMap::new(),
            None,
            &HashMap::new(),
            true,
            None,
            &HashSet::new(),
        )
        .unwrap();
        let text = block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(text.contains("pub struct Api") && text.contains("pub value"));
        assert!(
            !text.contains("secret")
                && !text.contains("Hidden")
                && !text.contains("Private field review")
        );
        assert!(
            target
                .values()
                .all(|anchor| !anchor.label.contains("secret") && !anchor.label.contains("Hidden"))
        );
        forge_buffer::document::BufferDocument::new(
            forge_buffer::identity::DocumentId("public".into()),
            block,
        )
        .unwrap();
        assert_eq!(serde_json::to_vec(&document).unwrap(), saved);
        document
            .design
            .as_mut()
            .unwrap()
            .proposed
            .insert("lib.rs".into(), "struct Hidden;\n".into());
        let (block, target) = project(
            &document,
            &WidthProfile::default(),
            &[],
            &HashMap::new(),
            None,
            &HashMap::new(),
            true,
            None,
            &HashSet::new(),
        )
        .unwrap();
        assert!(
            block
                .iter()
                .any(|block| block.text.row(0) == Some("No public declaration changes."))
        );
        assert!(
            block
                .iter()
                .any(|block| block.text.row(0) == Some(" Design "))
        );
        assert!(
            block
                .iter()
                .any(|block| block.text.row(0) == Some(" Changes "))
        );
        assert!(
            target
                .values()
                .all(|anchor| matches!(anchor.target, PlanReviewTarget::Section { .. }))
        );
        forge_buffer::document::BufferDocument::new(
            forge_buffer::identity::DocumentId("public-empty".into()),
            block,
        )
        .unwrap();
    }

    #[test]
    fn inspection_formats_both_sides_without_modifying_the_design() {
        let mut document = crate::plan::document::test_fixture("plan", "Review");
        let original = "pub struct Registry { first: u64, second: u64 }\n";
        let proposed = "pub struct Registry {\n        first: u64,\n        second: String,\n}\n";
        let mut design = super::super::DeclarationDesign::default();
        design.baseline.insert(
            "registry.rs".into(),
            super::super::DeclarationFile {
                text: original.into(),
                source_digest: String::new(),
            },
        );
        design
            .proposed
            .insert("registry.rs".into(), proposed.into());
        document.design = Some(design);
        let saved = serde_json::to_vec(&document).unwrap();
        let rendered = render(&document).unwrap();
        assert!(!rendered.markdown.contains("  first: u64,"));
        assert!(rendered.markdown.contains("  second: String,"));
        let first = rendered
            .navigation
            .anchor
            .iter()
            .find(|anchor| anchor.label.ends_with("  second: String,"))
            .unwrap();
        let second = rendered.navigation.anchor.iter().find(|anchor| matches!(&anchor.target, PlanReviewTarget::Declaration { side, .. } if side == "baseline") && anchor.label.ends_with("  second: u64,")).unwrap();
        assert_ne!(first.json_path, second.json_path);
        assert_eq!(serde_json::to_vec(&document).unwrap(), saved);
        document.design.as_mut().unwrap().proposed.insert(
            "registry.rs".into(),
            "pub struct Registry {\n        first: u64,\n        second: u64,\n}\n".into(),
        );
        assert!(
            render(&document)
                .unwrap()
                .markdown
                .contains(" Changes \nNo declaration changes.")
        );
    }

    #[test]
    fn review_reuses_diff_gutters_and_preserves_both_comment_sides() {
        let mut document = crate::plan::document::test_fixture("plan", "Review");
        let mut design = super::super::DeclarationDesign::default();
        design.document.requirements.push("Preserve review targets on both sides of a declaration change.".into());
        design.baseline.insert(
            "src/lib.rs".into(),
            super::super::DeclarationFile {
                text: "pub struct Before;\n".into(),
                source_digest: String::new(),
            },
        );
        design
            .proposed
            .insert("src/lib.rs".into(), "pub struct After;\n".into());
        document.design = Some(design);
        let rendered = render(&document).unwrap();
        assert!(rendered.markdown.contains("Modified src/lib.rs +1 -1"));
        let before = rendered.navigation.anchor.iter().find(|anchor| matches!(&anchor.target,PlanReviewTarget::Declaration { side,.. } if side == "baseline")).unwrap();
        let after = rendered.navigation.anchor.iter().find(|anchor| matches!(&anchor.target,PlanReviewTarget::Declaration { side,.. } if side == "proposed")).unwrap();
        assert_ne!(before.json_path, after.json_path);
        let comment = ReviewAnnotation {
                parent_id: None,
                kind: Default::default(),
                reply: None,
            anchor: None,
            id: "review".into(),
            source: super::super::PlanAnnotationInput {
                start_line: after.line,
                end_line: after.line,
                body: "Rename this type".into(),
            },
        };
        let (block, target) = project(
            &document,
            &WidthProfile::default(),
            &[comment],
            &HashMap::new(),
            Some("review"),
            &HashMap::new(),
            false,
            None,
            &HashSet::new(),
        )
        .unwrap();
        assert!(block.iter().any(|block| !block.metadata.gutter.is_empty()));
        let folded_header: Vec<_> = block
            .iter()
            .filter(|block| !block.metadata.fold.is_empty())
            .collect();
        assert_eq!(folded_header.len(), 9);
        assert_eq!(folded_header[0].text.row(0), Some(" Objective "));
        assert_eq!(folded_header[1].text.row(0), Some(" Requirements "));
        assert_eq!(folded_header[2].text.row(0), Some(" Background "));
        assert_eq!(folded_header[3].text.row(0), Some(" Design "));
        assert_eq!(folded_header[4].text.row(0), Some(" Changes "));
        assert!(folded_header[5].text.row(0).unwrap().starts_with("Modified src/lib.rs"));
        assert!(folded_header[6].text.row(0).unwrap().starts_with("@@"));
        assert_eq!(folded_header[7].text.row(0), Some(" Checks "));
        assert_eq!(folded_header[8].text.row(0), Some(" Verification "));
        for header in folded_header {
            let fold = &header.metadata.fold[0];
            assert!(fold.heading_start.is_none());
            assert_eq!(fold.start, TextPosition { row: 0, column: 0 });
        }

        assert!(
            block
                .iter()
                .any(|block| !block.metadata.editable_region.is_empty())
        );
        assert!(target.values().any(|anchor| anchor.line == after.line));
        let annotation = super::super::resolve_annotations(
            &rendered,
            vec![super::super::PlanAnnotationInput {
                start_line: before.line,
                end_line: after.line,
                body: "Review both sides".into(),
            }],
        )
        .unwrap();
        assert_eq!(annotation[0].subject.len(), 2);
    }

    #[test]
    fn saved_review_traverses_unchanged_files_and_keeps_file_ids_stable() {
        let mut document = crate::plan::document::test_fixture("plan", "Review");
        let mut design = super::super::DeclarationDesign::default();
        for (path, baseline, proposed) in [
            ("a.ts", "import './b';\nexport type A = string;\n", "import './b';\nexport type A = number;\n"),
            ("b.ts", "import './c';\n", "import './c';\n"),
            ("c.ts", "export type C = string;\n", "export type C = number;\n"),
        ] {
            design.baseline.insert(path.into(), super::super::DeclarationFile { text: baseline.into(), source_digest: String::new() });
            design.proposed.insert(path.into(), proposed.into());
        }
        document.design = Some(design);
        let reopened: PlanDocument = serde_json::from_slice(&serde_json::to_vec(&document).unwrap()).unwrap();
        let rendered = render(&reopened).unwrap();
        let (block, _, _, _) = rows(&reopened, &HashMap::new(), false, false, None, &HashSet::new()).unwrap();
        let directory = tempfile::tempdir().unwrap();
        let store = std::sync::Arc::new(crate::trace::TraceStore::open(directory.path()).unwrap());
        store.configure(true).unwrap();
        let trace = ReviewTrace { store: store.clone(), session_id: "review-test".into() };
        rows(&reopened, &HashMap::new(), false, false, Some(&trace), &HashSet::new()).unwrap();
        let recorded = std::fs::read_to_string(store.status().path).unwrap();
        assert!(recorded.contains("\"event\":\"plan.review.file_order\""));
        assert!(recorded.contains("\"count\":2"));
        let file_headers = block.iter().filter(|row| row.id.0.starts_with("plan:design:file:") && !row.id.0.contains(":hunk:")).collect::<Vec<_>>();
        assert_eq!(file_headers.len(), 2);
        assert!(file_headers[0].text.row(0).unwrap().contains("a.ts"));
        assert!(file_headers[1].text.row(0).unwrap().contains("c.ts"));
        assert!(!rendered.markdown.contains("Modified b.ts"));
        let stable_id = file_headers[1].id.clone();
        let design = document.design.as_mut().unwrap();
        design.baseline.insert("0.ts".into(), super::super::DeclarationFile { text: "export type Zero = string;\n".into(), source_digest: String::new() });
        design.proposed.insert("0.ts".into(), "export type Zero = number;\n".into());
        let (block, _, _, _) = rows(&document, &HashMap::new(), false, false, None, &HashSet::new()).unwrap();
        assert!(block.iter().any(|row| row.id == stable_id && row.text.row(0).unwrap().contains("c.ts")));
    }
}
