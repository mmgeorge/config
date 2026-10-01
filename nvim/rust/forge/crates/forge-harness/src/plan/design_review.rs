use std::collections::{BTreeSet, HashMap, HashSet};

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
use forge_diff::syntax::{DeclarationFolding, DeclarationOverview, DeclarationPresentation, DeclarationVisibility};

use super::review_annotation::ReviewAnnotation;
use super::{
    PlanDocument, PlanNavigationAnchor, PlanNavigationIndex, PlanReviewTarget, RenderedPlan,
};

pub(super) fn render(document: &PlanDocument) -> Result<RenderedPlan> {
    let (block, navigation, _) = rows(document, &HashMap::new(), false, false)?;
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
) -> Result<(Vec<BufferBlock>, HashMap<TargetId, PlanNavigationAnchor>)> {
    let (source, navigation, hidden) = rows(document, syntax, public_only, true)?;
    let mut target = HashMap::new();
    let mut block = Vec::new();
    let mut source_end = HashMap::new();
    for (index, mut row) in source.into_iter().enumerate() {
        if row.id.0.starts_with("plan:description:") {
            row = forge_buffer::markdown::MarkdownRenderer::source(
                row.id.clone(),
                &width
                    .wrap_plain(&row.text.wire_rows().join("\n"), 0)?
                    .join("\n"),
                width,
            )?
            .block;
        }
        if let Some(anchor) = navigation.resolve_line(index as u32 + 1) {
            let id = TargetId(format!("plan:declaration:row:{index}"));
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
            target.insert(id, anchor.clone());
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
) -> Result<(Vec<BufferBlock>, PlanNavigationIndex, HashSet<BlockId>)> {
    let design = document
        .design
        .as_ref()
        .context("declaration review has no design")?;
    let mut patch = Vec::new();
    let mut hidden = HashSet::new();
    let mut visibility = HashMap::<(String, String), DeclarationVisibility>::new();
    let mut presentation = HashMap::<(String, String), DeclarationPresentation>::new();
    let destinations = design.moved.values().collect::<BTreeSet<_>>();
    for path in design.changed_paths() {
        if destinations.contains(&path) {
            continue;
        }
        let destination = design.moved.get(&path).unwrap_or(&path);
        let before = design
            .baseline
            .get(&path)
            .map(|file| DeclarationOverview::present(&path, &file.text))
            .transpose()
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
        let after = design
            .proposed
            .get(destination)
            .map(|text| DeclarationOverview::present(destination, text))
            .transpose()
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
        let diff = forge_diff::raw::compute_hunks(SourcePair {
            old: SourceVersion::new(
                before
                    .as_ref()
                    .map(|file| file.text.as_str())
                    .unwrap_or_default()
                    .as_bytes()
                    .to_vec(),
                Representation::DisplayOnly,
            )?,
            new: SourceVersion::new(
                after
                    .as_ref()
                    .map(|file| file.text.as_str())
                    .unwrap_or_default()
                    .as_bytes()
                    .to_vec(),
                Representation::DisplayOnly,
            )?,
        })?;
        let old_name = format!("a/{path}");
        let new_name = format!("b/{destination}");
        if diff.hunks().is_empty() && before.is_some() && after.is_some() && path == *destination {
            continue;
        }
        forge_diff::unified::write_file_header(&old_name, &new_name, &mut patch)?;
        if diff.hunks().is_empty() {
            if before.is_none() {
                patch.extend_from_slice(b"new file mode 100644\n");
            } else if after.is_none() {
                patch.extend_from_slice(b"deleted file mode 100644\n");
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
                    .map(|file| file.source.len())
                    .unwrap_or(0)
                    .max(after.as_ref().map(|file| file.source.len()).unwrap_or(0)),
                &mut patch,
            )?;
        }
        if let Some(before) = before {
            if inspection {
                visibility.insert(
                    (path.clone(), "baseline".into()),
                    DeclarationVisibility::analyze(&path, &before.text, public_only)
                        .map_err(|error| anyhow::anyhow!("{error:?}"))?,
                );
            }
            presentation.insert((path.clone(), "baseline".into()), before);
        }
        if let Some(after) = after {
            if inspection {
                visibility.insert(
                    (destination.clone(), "proposed".into()),
                    DeclarationVisibility::analyze(destination, &after.text, public_only)
                        .map_err(|error| anyhow::anyhow!("{error:?}"))?,
                );
            }
            presentation.insert((destination.clone(), "proposed".into()), after);
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
    for (file_index, file) in parsed.file.iter().enumerate() {
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
        let id = format!("plan:design:file:{file_index}");
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
                let (side, source_path, line) = if row.kind == RowKind::Removed {
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
                if let Some(handle) = syntax.get(&(source_path.clone(), side.into())) {
                    forge_diff::projection::append_syntax_row(
                        &mut metadata,
                        handle,
                        line,
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
                        .and_then(|file| row.old_line.and_then(|line| file.source.get(line)))
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
                    .and_then(|file| file.source.get(line))
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
            }
            fold(&mut block, hunk_start, &hunk_id)?;
        }
        if inspection {
            declaration_folds(&mut block, [file.old_path.as_ref(), file.new_path.as_ref()], &presentation, &display_row)?;
        }
        fold(&mut block, start, &id)?;
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
    let mut description = vec![forge_diff::projection::header(
        BlockId("plan:section:description".into()),
        vec![TextChunk {
            text: "Description:".into(),
            capture: "ForgeStatusHeader".into(),
        }],
        0,
    )?];
    let text = if design.document.description.trim().is_empty() {
        "No description."
    } else {
        &design.document.description
    };
    for (index, line) in text.lines().enumerate() {
        description.push(BufferBlock {
            id: BlockId(format!("plan:description:{index}")),
            text: BufferText::from_rows([line])?,
            metadata: BlockMetadata::default(),
        });
    }
    fold(&mut description, 0, "plan:section:description")?;
    let description_count = description.len() as u32;
    description.push(BufferBlock {
        id: BlockId("plan:section:separator".into()),
        text: BufferText::from_rows([""])?,
        metadata: BlockMetadata::default(),
    });
    let changes_start = description.len();
    description.push(forge_diff::projection::header(
        BlockId("plan:section:changes".into()),
        vec![TextChunk {
            text: "Changes:".into(),
            capture: "ForgeStatusHeader".into(),
        }],
        0,
    )?);
    let count = description.len() as u32;
    for anchor in &mut navigation.anchor {
        anchor.line += count;
    }
    for line in 1..=description_count {
        navigation.anchor.push(PlanNavigationAnchor {
            line,
            target: PlanReviewTarget::Section {
                section: super::PlanSection::Overview,
            },
            json_path: "/design/document/description".into(),
            path: None,
            label: "Change description".into(),
        });
    }
    navigation.anchor.push(PlanNavigationAnchor {
        line: count,
        target: PlanReviewTarget::Section {
            section: super::PlanSection::Files,
        },
        json_path: "/design".into(),
        path: None,
        label: "Declaration changes".into(),
    });
    description.extend(block);
    fold(&mut description, changes_start, "plan:section:changes")?;
    block = description;
    navigation.anchor.sort_by_key(|anchor| anchor.line);
    ensure!(block.len() <= 65536, "declaration diff exceeds 65536 rows");
    Ok((block, navigation, hidden))
}

fn declaration_folds(
    block: &mut [BufferBlock],
    paths: [Option<&String>; 2],
    presentation: &HashMap<(String, String), DeclarationPresentation>,
    display_row: &HashMap<(String, String, usize), usize>,
) -> Result<()> {
    let mut candidates = Vec::new();
    for (source_path, side) in [(paths[0], "baseline"), (paths[1], "proposed")] {
        let Some(source_path) = source_path else { continue };
        let Some(source) = presentation.get(&(source_path.clone(), side.into())) else { continue };
        for declaration in DeclarationFolding::analyze(source_path, &source.text).map_err(|error| anyhow::anyhow!("{error:?}"))? {
            let mapped = |line| display_row.get(&(source_path.clone(), side.into(), line)).copied();
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
            closed, collapse_children: false });
        installed.push((opening, closing));
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
    if output[changes + 1..]
        .iter()
        .all(|block| block.id.0.starts_with("plan:annotation:"))
    {
        output.push(forge_diff::projection::header(
            BlockId("plan:design:public:empty".into()),
            vec![TextChunk {
                text: "No public declaration changes.".into(),
                capture: "Comment".into(),
            }],
            0,
        )?);
    }
    output[changes].metadata.fold.clear();
    fold(&mut output, changes, "plan:section:changes")?;
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
    fn declaration_folds_keep_attributes_and_valid_filtered_endpoints() {
        let mut document = crate::plan::document::test_fixture("containers", "Containers");
        let mut design = super::super::DeclarationDesign::default();
        design.document.description = "Expose configuration failures and state.".into();
        design.proposed.insert("config.rs".into(), "#[derive(Debug)]\npub enum ConfigError {\n  /// Invalid arena size.\n  ArenaSize,\n  Radius,\n}\n\npub struct State {\n  pub count: u64,\n  private: u64,\n}\n\nimpl State {\n  pub fn count(&self) -> u64;\n  fn hidden();\n}\n\npub struct Empty {\n  private: u64,\n}\n".into());
        document.design = Some(design);
        for public_only in [false, true] {
            let (block, _) = project(&document, &Default::default(), &[], &HashMap::new(), None, &HashMap::new(), public_only).unwrap();
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
        design.document.description = "Requests retain their cancellation state until all pending work finishes. Publication occurs only after upload completion and the previous allocation remains alive until submitted frames finish.".into();
        document.design = Some(design);
        let saved = serde_json::to_vec(&document).unwrap();
        let rendered = render(&document).unwrap();
        assert!(
            rendered
                .markdown
                .starts_with("Description:\nRequests retain")
        );
        let anchor = rendered.navigation.resolve_line(1).unwrap();
        assert_eq!(anchor.json_path, "/design/document/description");
        let annotation = ReviewAnnotation {
            id: "description".into(),
            anchor: None,
            source: super::super::PlanAnnotationInput {
                start_line: 2,
                end_line: 2,
                body: "Confirm the cancellation lifecycle".into(),
            },
        };
        for public_only in [false, true] {
            let (block, target) = project(
                &document,
                &WidthProfile::default(),
                &[annotation.clone()],
                &HashMap::new(),
                None,
                &HashMap::new(),
                public_only,
            )
            .unwrap();
            let description = block
                .iter()
                .find(|block| block.id.0 == "plan:description:0")
                .unwrap();
            assert!(description.text.row_count() > 1);
            let header = block
                .iter()
                .find(|block| block.id.0 == "plan:section:description")
                .unwrap();
            let endpoint = &header.metadata.fold[0].end;
            assert_eq!(endpoint.block.0, "plan:annotation:description");
            assert!(
                block
                    .iter()
                    .any(|block| block.text.row(0) == Some("Changes:"))
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
                    .any(|anchor| anchor.json_path == "/design/document/description")
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
                .any(|block| block.text.row(0) == Some("Description:"))
        );
        assert!(
            block
                .iter()
                .any(|block| block.text.row(0) == Some("Changes:"))
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
                .contains("Changes:\nNo declaration changes.")
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
        )
        .unwrap();
        assert!(block.iter().any(|block| !block.metadata.gutter.is_empty()));
        let folded_header: Vec<_> = block
            .iter()
            .filter(|block| !block.metadata.fold.is_empty())
            .collect();
        assert_eq!(folded_header.len(), 4);
        assert_eq!(folded_header[0].text.row(0), Some("Description:"));
        assert_eq!(folded_header[1].text.row(0), Some("Changes:"));
        assert!(
            folded_header[2]
                .text
                .row(0)
                .unwrap()
                .starts_with("Modified src/lib.rs")
        );
        assert!(folded_header[3].text.row(0).unwrap().starts_with("@@"));
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
}
