use std::collections::{HashMap, HashSet};
use std::time::Instant;

use anyhow::{Context, Result, ensure};
use forge_buffer::block::{
    BlockAnchor, BlockMetadata, BufferBlock, FoldRange, TargetRange, TextChunk, TextPosition, TextRange,
};
use forge_buffer::identity::{BlockId, EditSequence, FoldId, RegionRevision, TargetId};
use forge_buffer::text::BufferText;
use forge_buffer::width::WidthProfile;
use forge_diff::display::RowKind;
use forge_diff::file_header::FileChange;
use forge_diff::patch::UnifiedPatch;
use forge_diff::source::{Representation, SourcePair, SourceVersion};
use forge_diff::syntax::{DeclarationFolding, DeclarationVisibility};

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

pub(super) fn project(
    document: &PlanDocument,
    width: &WidthProfile,
    annotation: &[ReviewAnnotation],
    revision: &HashMap<String, (RegionRevision, EditSequence)>,
    focused: Option<&str>,
    syntax: &HashMap<(String, String), forge_diff::syntax::SyntaxHandle>,
    public_only: bool,
    trace: Option<&ReviewTrace>,
    revealed: &HashSet<(String, String)>,
) -> Result<(Vec<BufferBlock>, HashMap<TargetId, PlanNavigationAnchor>)> {
    let (source, navigation, hidden, contextual) = rows(document, syntax, public_only, true, trace, revealed)?;
    let mut target = HashMap::new();
    let mut block = Vec::new();
    let mut source_end = HashMap::new();
    let mut omitted = 0;
    for (index, mut row) in source.into_iter().enumerate() {
        if row.id.0.starts_with("plan:metadata:")
            && !row.id.0.starts_with("plan:metadata:verification/automated:") {
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
            if matches!(anchor.target, PlanReviewTarget::Call { .. } | PlanReviewTarget::Change { .. }) {
                super::review_projection::append_semantic_style(&mut row.metadata, &anchor.target, row.text.row(0).unwrap_or_default());
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
    syntax: &HashMap<(String, String), forge_diff::syntax::SyntaxHandle>,
    public_only: bool,
    inspection: bool,
    trace: Option<&ReviewTrace>,
    revealed: &HashSet<(String, String)>,
) -> Result<(Vec<BufferBlock>, PlanNavigationIndex, HashSet<BlockId>, HashSet<BlockId>)> {
    let design = document
        .design
        .as_ref()
        .context("declaration review has no design")?;
    let mut patch = Vec::new();
    let mut hidden = HashSet::new();
    let mut contextual_path = HashSet::new();
    let mut contextual_block = HashSet::new();
    let mut visibility = HashMap::<(String, String), DeclarationVisibility>::new();
    let mut presentation = HashMap::<(String, String), super::calls::CallPresentation>::new();
    let started = Instant::now();
    let included = revealed.iter().map(|(path, _)| path.clone()).collect();
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
        let diff = forge_diff::raw::compute_hunks(SourcePair {
            old: SourceVersion::new(
                before
                    .as_ref()
                    .map(|file| file.declaration.text.as_str())
                    .unwrap_or_default()
                    .as_bytes()
                    .to_vec(),
                Representation::DisplayOnly,
            )?,
            new: SourceVersion::new(
                after
                    .as_ref()
                    .map(|file| file.declaration.text.as_str())
                    .unwrap_or_default()
                    .as_bytes()
                    .to_vec(),
                Representation::DisplayOnly,
            )?,
        })?;
        let old_name = format!("a/{path}");
        let new_name = format!("b/{destination}");
        if diff.hunks().is_empty() && before.is_some() && after.is_some() && path == destination
            && !revealed.contains(&(path.clone(), "baseline".into()))
            && !revealed.contains(&(destination.clone(), "proposed".into())) {
            continue;
        }
        if diff.hunks().is_empty() && before.is_some() && after.is_some() && path == destination {
            contextual_path.insert(path.clone());
        }
        forge_diff::unified::write_file_header(&old_name, &new_name, &mut patch)?;
        if diff.hunks().is_empty() {
            if before.is_none() {
                patch.extend_from_slice(b"new file mode 100644\n");
            } else if after.is_none() {
                patch.extend_from_slice(b"deleted file mode 100644\n");
            } else if let Some(after) = &after
                && (revealed.contains(&(path.clone(), "baseline".into()))
                    || revealed.contains(&(destination.clone(), "proposed".into()))) {
                let count = after.declaration.source.len();
                patch.extend_from_slice(format!("--- {old_name}\n+++ {new_name}\n@@ -1,{count} +1,{count} @@\n").as_bytes());
                for row in after.declaration.text.lines() {
                    patch.extend_from_slice(format!(" {row}\n").as_bytes());
                }
            }
        } else {
            forge_diff::unified::write_unified(
                &diff,
                if before.is_some() {
                    &old_name
                } else {
                    "/dev/null"
                },
                if after.is_some() {
                    &new_name
                } else {
                    "/dev/null"
                },
                before
                    .as_ref()
                    .map(|file| file.declaration.source.len())
                    .unwrap_or(0)
                    .max(after.as_ref().map(|file| file.declaration.source.len()).unwrap_or(0)),
                &mut patch,
            )?;
        }
        if let Some(before) = before {
            if inspection {
                visibility.insert(
                    (path.clone(), "baseline".into()),
                    super::calls::visibility(&path, &before, public_only && !revealed.contains(&(path.clone(), "baseline".into())))?,
                );
            }
            presentation.insert((path.clone(), "baseline".into()), before);
        }
        if let Some(after) = after {
            if inspection {
                visibility.insert(
                    (destination.clone(), "proposed".into()),
                    super::calls::visibility(&destination, &after, public_only && !revealed.contains(&(destination.clone(), "proposed".into())))?,
                );
            }
            presentation.insert((destination, "proposed".into()), after);
        }
    }
    let patch = String::from_utf8(patch)?;
    let parsed = UnifiedPatch::parse(&patch)?;
    let mut block = Vec::new();
    let mut display_row = HashMap::<(String, String, usize), usize>::new();
    let mut navigation = PlanNavigationIndex {
        plan_id: document.plan_id.clone(),
        plan_version: document.version,
        anchor: Vec::new(),
    };
    for file in &parsed.file {
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
        let added = file
            .hunk
            .iter()
            .flat_map(|hunk| &hunk.row)
            .filter(|row| row.kind == RowKind::Added)
            .count() as u64;
        let removed = file
            .hunk
            .iter()
            .flat_map(|hunk| &hunk.row)
            .filter(|row| row.kind == RowKind::Removed)
            .count() as u64;
        let identity = file.old_path.as_deref().unwrap_or(path);
        let id = format!("plan:design:file:{}", super::digest(identity.as_bytes()));
        block.push(forge_diff::projection::header(
            BlockId(id.clone()),
            status.header(&display, Some((added, removed)), false),
            0,
        )?);
        navigation.anchor.push(PlanNavigationAnchor {
            line: block.len() as u32,
            target: PlanReviewTarget::File { path: path.clone() },
            json_path: format!("/design/proposed/{}", pointer(path)),
            path: Some(path.clone()),
            label: display,
        });
        for (hunk_index, hunk) in file.hunk.iter().enumerate() {
            let hunk_start = block.len();
            let hunk_id = format!("{id}:hunk:{hunk_index}");
            block.push(forge_diff::projection::header(
                BlockId(hunk_id.clone()),
                vec![TextChunk {
                    text: hunk.header.into(),
                    capture: "ForgeHunkHeader".into(),
                }],
                0,
            )?);
            let emphasis = forge_diff::intraline::patch_emphasis(
                forge_diff::intraline::IntralinePolicy::default(),
                &hunk.row,
            );
            for (row_index, row) in hunk.row.iter().enumerate() {
                let mut metadata = BlockMetadata::default();
                forge_diff::projection::append_patch_row(
                    &mut metadata,
                    0,
                    row,
                    hunk,
                    &emphasis[row_index],
                    0,
                )?;
                let (side, source_path, line) = if row.kind == RowKind::Removed
                    || (row.kind == RowKind::Context && file.old_path.as_ref() == file.new_path.as_ref()
                        && revealed.contains(&(path.clone(), "baseline".into()))
                        && !revealed.contains(&(path.clone(), "proposed".into()))) {
                    (
                        "baseline",
                        file.old_path.as_ref().unwrap(),
                        row.old_line.unwrap(),
                    )
                } else {
                    ("proposed", path, row.new_line.unwrap())
                };
                let row_text = visibility
                    .get(&(source_path.clone(), side.into()))
                    .and_then(|visibility| visibility.replacement.get(&line))
                    .map(String::as_str)
                    .unwrap_or(row.text);
                let syntax_row = presentation.get(&(source_path.clone(), side.into())).and_then(|file| {
                    let original = *file.plain_row.get(line)?;
                    (file.declaration_row.get(&original) == Some(&line)).then_some(original)
                });
                if let Some(source_row) = syntax_row
                    && let Some(handle) = syntax.get(&(source_path.clone(), side.into())) {
                    forge_diff::projection::append_syntax_row(
                        &mut metadata,
                        handle,
                        source_row,
                        0,
                        row_text,
                    )?;
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
                if row.kind == RowKind::Context {
                    display_row.insert((file.old_path.as_ref().unwrap().clone(), "baseline".into(), row.old_line.unwrap()), block.len());
                }
                display_row.insert((source_path.clone(), side.into(), line), block.len());
                block.push(BufferBlock {
                    id: BlockId(format!("{hunk_id}:row:{row_index}")),
                    text: BufferText::from_rows([row_text])?,
                    metadata,
                });
                if row.kind == RowKind::Context {
                    let baseline_path = file
                        .old_path
                        .as_ref()
                        .context("context row has no baseline file")?;
                    if let Some(position) = presentation
                        .get(&(baseline_path.clone(), "baseline".into()))
                        .and_then(|file| row.old_line.and_then(|line| file.declaration.source.get(line)))
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
                if let Some((owner, offset)) = presentation.get(&(source_path.clone(), side.into())).and_then(|file| file.change_row.get(&line)) {
                    navigation.anchor.push(PlanNavigationAnchor {
                        line: block.len() as u32,
                        target: PlanReviewTarget::Change { path: source_path.clone(), side: side.into(), owner: owner.clone(), offset: *offset },
                        json_path: format!("/design/{side}_calls/{}/{}/change/{offset}", pointer(source_path), pointer(owner)),
                        path: Some(source_path.clone()), label: format!("{source_path}: {owner}: Change"),
                    });
                }
            }
            fold(&mut block, hunk_start, &hunk_id)?;
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
    let mut verification_start = None;
    for metadata in design.document.sections() {
        let name = metadata.path;
        let section = metadata.section;
        if matches!(section, super::PlanSection::Usage | super::PlanSection::Decisions)
            && metadata.text.trim().is_empty() {
            continue;
        }
        if section == super::PlanSection::Tests {
            let changes_start = overview.len();
            overview.push(forge_diff::projection::header(
                BlockId("plan:section:changes".into()),
                vec![TextChunk { text: "Proposed declaration changes:".into(), capture: "RenderMarkdownH1".into() }], 0,
            )?);
            let offset = overview.len() as u32;
            for anchor in &mut navigation.anchor { anchor.line += offset; }
            section_anchor.push(PlanNavigationAnchor {
                line: offset, target: PlanReviewTarget::Section { section: super::PlanSection::Files },
                json_path: "/design".into(), path: None, label: "Proposed declaration changes".into(),
            });
            overview.append(&mut block);
            fold(&mut overview, changes_start, "plan:section:changes")?;
            overview.push(BufferBlock {
                id: BlockId("plan:section:changes:separator".into()),
                text: BufferText::from_rows([""])?, metadata: BlockMetadata::default(),
            });
            let offset = overview.len() as u32;
            let (mut tests, mut anchors) = test_inventory(&design.document.tests)?;
            for anchor in &mut anchors { anchor.line += offset; }
            overview.append(&mut tests);
            section_anchor.append(&mut anchors);
            overview.push(BufferBlock {
                id: BlockId("plan:section:tests:separator".into()),
                text: BufferText::from_rows([""])?, metadata: BlockMetadata::default(),
            });
            continue;
        }
        if section == super::PlanSection::AutomatedVerification {
            verification_start = Some(overview.len());
            overview.push(forge_diff::projection::header(
                BlockId("plan:section:verification".into()),
                vec![TextChunk { text: "Verification:".into(), capture: "RenderMarkdownH1".into() }], 0,
            )?);
            section_anchor.push(PlanNavigationAnchor {
                line: overview.len() as u32,
                target: PlanReviewTarget::Section { section: super::PlanSection::Verification },
                json_path: "/design/document/verification".into(), path: None, label: "Verification".into(),
            });
        }
        let start = overview.len();
        let fold_id = format!("plan:section:{name}");
        let nested = name.starts_with("verification/");
        let heading_indent = if nested { "  " } else { "" };
        let text_indent = if section == super::PlanSection::ManualVerification { "  " } else if nested { "    " } else { "" };
        let title = metadata.title.rsplit('/').next().unwrap();
        overview.push(forge_diff::projection::header(
            BlockId(fold_id.clone()),
            vec![TextChunk {
                text: format!("{heading_indent}{title}:"),
                capture: if nested { "ForgeStatusHeader" } else { "RenderMarkdownH1" }.into(),
            }], 0,
        )?);
        let text = if metadata.text.trim().is_empty() { "None specified." } else { metadata.text.as_str() };
        for (index, line) in text.lines().enumerate() {
            overview.push(BufferBlock {
                id: BlockId(format!("plan:metadata:{name}:{index}")),
                text: BufferText::from_rows([format!("{text_indent}{line}")])?, metadata: BlockMetadata::default(),
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
    if let Some(start) = verification_start { fold(&mut overview, start, "plan:section:verification")?; }
    navigation.anchor.extend(section_anchor);
    block = overview;
    navigation.anchor.sort_by_key(|anchor| anchor.line);
    ensure!(block.len() <= 65536, "declaration diff exceeds 65536 rows");
    Ok((block, navigation, hidden, contextual_block))
}

fn test_inventory(files: &[super::design_tests::DesignTestFile]) -> Result<(Vec<BufferBlock>, Vec<PlanNavigationAnchor>)> {
    let mut block = vec![forge_diff::projection::header(
        BlockId("plan:section:tests".into()),
        vec![TextChunk { text: format!("Tests · {}", super::design_tests::summary(files)), capture: "RenderMarkdownH1".into() }], 0,
    )?];
    let mut anchors = vec![PlanNavigationAnchor {
        line: 1, target: PlanReviewTarget::Section { section: super::PlanSection::Tests },
        json_path: "/design/document/tests".into(), path: None, label: "Tests".into(),
    }];
    for (file_index, file) in files.iter().enumerate() {
        let start = block.len();
        let id = format!("plan:tests:file:{}", super::digest(file.file.as_bytes()));
        block.push(forge_diff::projection::header(
            BlockId(id.clone()),
            vec![TextChunk { text: format!("  {}", file.file), capture: "ForgeStatusHeader".into() }], 0,
        )?);
        anchors.push(PlanNavigationAnchor {
            line: block.len() as u32, target: PlanReviewTarget::Section { section: super::PlanSection::Tests },
            json_path: format!("/design/document/tests/{file_index}"), path: Some(file.file.clone()), label: file.file.clone(),
        });
        for (case_index, case) in file.cases.iter().enumerate() {
            let case_start = block.len();
            let case_id = format!("{id}:{}", super::digest(case.name.as_bytes()));
            let (marker, capture) = case.change.presentation();
            block.push(forge_diff::projection::header(
                BlockId(case_id.clone()),
                vec![TextChunk { text: format!("    {marker}"), capture: capture.into() },
                    TextChunk { text: case.name.clone(), capture: "Normal".into() }], 0,
            )?);
            for (line_index, line) in case.description.lines().enumerate() {
                block.push(BufferBlock {
                    id: BlockId(format!("{case_id}:description:{line_index}")),
                    text: BufferText::from_rows([format!("      {line}")])?, metadata: BlockMetadata::default(),
                });
            }
            for line in case_start + 1..=block.len() {
                anchors.push(PlanNavigationAnchor {
                    line: line as u32, target: PlanReviewTarget::Section { section: super::PlanSection::Tests },
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
    presentation: &HashMap<(String, String), super::calls::CallPresentation>,
    display_row: &HashMap<(String, String, usize), usize>,
) -> Result<()> {
    let mut candidates = Vec::new();
    for (source_path, side) in [(paths[0], "baseline"), (paths[1], "proposed")] {
        let Some(source_path) = source_path else { continue };
        let Some(source) = presentation.get(&(source_path.clone(), side.into())) else { continue };
        for declaration in DeclarationFolding::analyze(source_path, &source.plain).map_err(|error| anyhow::anyhow!("{error:?}"))? {
            let mapped = |line| source.declaration_row.get(&line).and_then(|row| display_row.get(&(source_path.clone(), side.into(), *row)).copied());
            let (Some(opening), Some(closing)) = (mapped(declaration.start), mapped(declaration.end)) else { continue };
            if opening >= closing || !block[opening].text.wire_rows()[0].trim_end().ends_with('{') { continue; }
            candidates.push((opening, closing, mapped(declaration.heading).unwrap_or(opening), declaration.collapsed_suffix, declaration.closed));
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

fn call_folds(block: &mut [BufferBlock], paths: [Option<&String>; 2], presentation: &HashMap<(String, String), super::calls::CallPresentation>, display_row: &HashMap<(String, String, usize), usize>) {
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
        if empty && blank {
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
    let mut trailing = output.split_off(tests);
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
    forge_diff::projection::fold_header(&mut block[start], FoldId(id.into()), end, false, false);
    Ok(())
}

fn pointer(path: &str) -> String {
    path.replace('~', "~0").replace('/', "~1")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn change_only_edits_have_semantic_targets_and_closed_function_folds() {
        let mut document = crate::plan::document::test_fixture("change", "Behavior change");
        let mut design = super::super::DeclarationDesign::default();
        let declaration = "pub fn run();\n";
        design.baseline.insert("main.rs".into(), super::super::DeclarationFile { text: declaration.into(), source_digest: String::new() });
        design.proposed.insert("main.rs".into(), declaration.into());
        design.proposed_calls.insert("main.rs".into(), vec![super::super::FunctionBody { owner: "run".into(), call: None, change: Some("Stop retrying authentication failures.\nRecord the final attempt.".into()) }]);
        document.design = Some(design);
        let original = document.clone();
        let rendered = render(&document).unwrap();
        assert!(rendered.markdown.contains("Change") && rendered.markdown.contains("Stop retrying"));
        assert!(!rendered.markdown.contains("Calls"));
        for public_only in [false, true] {
            let (blocks, targets) = project(&document, &Default::default(), &[], &HashMap::new(), None, &HashMap::new(), public_only, None, &HashSet::new()).unwrap();
            assert_eq!(targets.values().filter(|anchor| matches!(anchor.target, PlanReviewTarget::Change { .. })).count(), 3);
            assert!(blocks.iter().filter(|block| block.text.row(0).unwrap().contains("pub fn run")).any(|heading| heading.metadata.fold.iter().any(|fold| fold.closed && fold.collapsed_suffix.as_ref().is_some_and(|suffix| suffix.contains("[changed]")))));
            for block in blocks.iter().filter(|block| block.text.row(0).unwrap().contains("Stop retrying")) {
                assert!(block.metadata.visible_decoration.iter().any(|capture| capture.capture == "@comment"));
            }
        }
        assert_eq!(document, original);
    }

    #[tokio::test]
    async fn synthetic_function_bodies_preserve_following_declaration_syntax() {
        use std::sync::Arc;
        use forge_diff::syntax::{SyntaxEngine, SyntaxLanguage, SyntaxLimits, SyntaxRequest};
        use forge_diff::workers::{AnalysisPool, PoolLimits, WorkPriority};

        let mut document = crate::plan::document::test_fixture("syntax-rows", "Syntax rows");
        let mut design = super::super::DeclarationDesign::default();
        let before = "pub struct ArenaPlugin;\nimpl ArenaPlugin { pub fn new(); }\n/// Registers the old arena systems.\nimpl Plugin for ArenaPlugin {}\n";
        let after = before.replace("old", "shared");
        design.baseline.insert("arena.rs".into(), super::super::DeclarationFile { text: before.into(), source_digest: String::new() });
        design.proposed.insert("arena.rs".into(), after.clone());
        let calls = vec![super::super::FunctionBody { change: None, owner: "ArenaPlugin::new".into(), call: Some(vec![
            super::super::CallSite { kind: super::super::CallKind::Call, name: "ArenaConfig::validate".into(), source: None, unresolved: false },
            super::super::CallSite { kind: super::super::CallKind::Property, name: "ArenaConfig::enabled".into(), source: None, unresolved: false },
        ]) }];
        design.baseline_calls.insert("arena.rs".into(), calls.clone());
        let mut proposed = calls;
        proposed[0].change = Some("Reject invalid configuration before registration.".into());
        design.proposed_calls.insert("arena.rs".into(), proposed);
        document.design = Some(design);
        let original = document.clone();
        let engine = SyntaxEngine::new(Arc::new(AnalysisPool::new(PoolLimits { workers: 1, jobs: 2, input_bytes: 8 * 1024 * 1024 })), SyntaxLimits::default());
        let mut syntax = HashMap::new();
        for (side, text) in [("baseline", before), ("proposed", after.as_str())] {
            let plain = forge_diff::syntax::DeclarationOverview::present("arena.rs", text).unwrap().text;
            let handle = engine.analyze(SyntaxRequest {
                source: SourceVersion::new(plain.into_bytes(), Representation::DisplayOnly).unwrap(),
                language: SyntaxLanguage::Rust, priority: WorkPriority::Foreground, deadline: None,
            }).await.unwrap();
            syntax.insert(("arena.rs".into(), side.into()), handle);
        }
        for public_only in [false, true] {
            let revealed = if public_only { HashSet::from([("arena.rs".into(), "baseline".into()), ("arena.rs".into(), "proposed".into())]) } else { HashSet::new() };
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
            assert!(synthetic_rows >= 4);
        }
        assert_eq!(document, original);
    }

    #[test]
    fn call_rows_highlight_qualified_types_methods_and_properties() {
        let mut document = crate::plan::document::test_fixture("call-colors", "Call colors");
        let mut design = super::super::DeclarationDesign::default();
        design.proposed.insert("lib.rs".into(), "pub fn main();\n".into());
        let cases = [
            ("App::add_plugins", super::super::CallKind::Call, vec![("App", "@type"), ("add_plugins", "@function.method.call")]),
            ("std::time::Instant::now", super::super::CallKind::Call, vec![("std", "@module"), ("time", "@module"), ("Instant", "@type"), ("now", "@function.method.call")]),
            ("validate_policy", super::super::CallKind::Call, vec![("validate_policy", "@function.call")]),
            ("client.send", super::super::CallKind::Call, vec![("client", "@variable"), ("send", "@function.method.call")]),
            ("État::更新", super::super::CallKind::Call, vec![("État", "@type"), ("更新", "@function.method.call")]),
            ("Client::count", super::super::CallKind::Property, vec![("Client", "@type"), ("count", "@variable.member")]),
        ];
        design.proposed_calls.insert("lib.rs".into(), vec![super::super::FunctionBody {
            owner: "main".into(),
            change: None,
            call: Some(cases.iter().map(|(name, kind, _)| super::super::CallSite {
                name: (*name).into(), kind: *kind, source: None, unresolved: false,
            }).collect()),
        }]);
        document.design = Some(design);
        let original = document.clone();
        for public_only in [false, true] {
            let (blocks, _) = project(&document, &Default::default(), &[], &HashMap::new(), None, &HashMap::new(), public_only, None, &HashSet::new()).unwrap();
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
    fn test_inventory_preserves_file_folds_targets_and_change_markers() {
        let mut document = crate::plan::document::test_fixture("tests", "Tests");
        let mut design = super::super::DeclarationDesign::default();
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
            assert!(rows.contains(&"Tests · 1 new · 1 modified · 1 removed · 1 reused"));
            for expected in ["    + tests::new_case", "    ~ tests::changed_case", "    − tests::old_case", "    existing_case"] {
                assert!(rows.contains(&expected));
            }
            assert!(!rows.iter().any(|row| row.contains("= existing_case")));
            assert!(target.values().any(|anchor| anchor.json_path == "/design/document/tests/1/cases/0"));
            let tests_start = block.iter().position(|block| block.id.0 == "plan:section:tests").unwrap();
            let verification_start = block.iter().position(|block| block.id.0 == "plan:section:verification").unwrap();
            assert!(tests_start < verification_start);
            let endpoint = &block[tests_start].metadata.fold[0].end.block;
            assert!(block.iter().position(|block| &block.id == endpoint).unwrap() < verification_start);
            assert_eq!(block[tests_start..verification_start].iter().map(|block| block.metadata.fold.len()).sum::<usize>(), 3);
            let changes = block.iter().find(|block| block.id.0 == "plan:section:changes").unwrap();
            let endpoint = &changes.metadata.fold[0].end.block;
            assert!(block.iter().position(|block| &block.id == endpoint).unwrap() < tests_start);
            forge_buffer::document::BufferDocument::new(forge_buffer::identity::DocumentId("test-inventory".into()), block).unwrap();
        }
        document.design.as_mut().unwrap().document.tests.clear();
        assert!(render(&document).unwrap().markdown.contains("Tests · None planned"));
    }

    #[test]
    fn validation_evidence_stays_out_of_review_projection() {
        let mut document = crate::plan::document::test_fixture("validation", "Validation");
        let mut design = super::super::DeclarationDesign::default();
        design.proposed.insert("lib.rs".into(), "pub struct State;\n".into());
        design.document.verification.automated = "cargo test -- --test-threads=1\nprintf '# *literal*'".into();
        design.document.verification.manual = "- Resize and confirm `State` remains visible.".into();
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
            let command = block.iter().find(|block| block.id.0 == "plan:metadata:verification/automated:1").unwrap();
            assert_eq!(command.text.row(0), Some("    printf '# *literal*'"));
            assert!(!command.metadata.markdown);
            let decision = block.iter().find(|block| block.id.0 == "plan:metadata:decisions:0").unwrap();
            assert_eq!(decision.text.row(0), Some("- **Keep state explicit.** Callers can inspect it."));
            assert!(decision.metadata.markdown);
            assert!(decision.metadata.decoration.iter().any(|style| style.capture == "ForgeStatusHeader"));
            assert!(block.iter().filter(|block| block.id.0.starts_with("plan:metadata:verification/manual:")).all(|block| block.metadata.markdown));
            assert!(block.iter().filter(|block| block.id.0.starts_with("plan:section:")).all(|block| !block.metadata.markdown));
            assert!(target.values().any(|anchor| anchor.json_path == "/design/document/verification/automated"));
            assert!(target.values().any(|anchor| anchor.json_path == "/design/document/verification/manual"));
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
                .starts_with("Objective:\nSupport observable cancellation and safe texture replacement.\n\nRequirements:")
        );
        let task_anchor = rendered.navigation.resolve_line(1).unwrap();
        assert_eq!(task_anchor.json_path, "/design/document/objective");
        assert_eq!(task_anchor.target, PlanReviewTarget::Section { section: super::super::PlanSection::Objective });
        let design_line = rendered.markdown.lines().position(|line| line == "Design:").unwrap() as u32 + 1;
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
                    .any(|block| block.text.row(0) == Some("Proposed declaration changes:"))
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
                .any(|block| block.text.row(0) == Some("Design:"))
        );
        assert!(
            block
                .iter()
                .any(|block| block.text.row(0) == Some("Proposed declaration changes:"))
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
        assert!(rendered.markdown.contains("  first: u64,"));
        assert!(rendered.markdown.contains("  second: String,"));
        let first = rendered
            .navigation
            .anchor
            .iter()
            .find(|anchor| anchor.label.ends_with("  first: u64,"))
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
                .contains("Proposed declaration changes:\nNo declaration changes.")
        );
    }

    #[test]
    fn review_reuses_diff_gutters_and_preserves_both_comment_sides() {
        let mut document = crate::plan::document::test_fixture("plan", "Review");
        let mut design = super::super::DeclarationDesign::default();
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
        assert_eq!(folded_header.len(), 10);
        assert_eq!(folded_header[0].text.row(0), Some("Objective:"));
        assert_eq!(folded_header[1].text.row(0), Some("Requirements:"));
        assert_eq!(folded_header[2].text.row(0), Some("Background:"));
        assert_eq!(folded_header[3].text.row(0), Some("Design:"));
        assert_eq!(folded_header[4].text.row(0), Some("Proposed declaration changes:"));
        assert!(folded_header[5].text.row(0).unwrap().starts_with("Modified src/lib.rs"));
        assert!(folded_header[6].text.row(0).unwrap().starts_with("@@"));
        assert_eq!(folded_header[7].text.row(0), Some("Verification:"));
        assert_eq!(folded_header[8].text.row(0), Some("  Automated:"));
        assert_eq!(folded_header[9].text.row(0), Some("  Manual:"));
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
