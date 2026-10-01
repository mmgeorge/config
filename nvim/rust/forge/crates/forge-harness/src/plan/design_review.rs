use std::collections::{BTreeSet, HashMap, HashSet};

use anyhow::{Context, Result, ensure};
use forge_buffer::block::{
    BlockAnchor, BlockMetadata, BufferBlock, TargetRange, TextChunk, TextPosition, TextRange,
};
use forge_buffer::identity::{BlockId, EditSequence, FoldId, RegionRevision, TargetId};
use forge_buffer::text::BufferText;
use forge_buffer::width::WidthProfile;
use forge_diff::display::RowKind;
use forge_diff::file_header::FileChange;
use forge_diff::patch::UnifiedPatch;
use forge_diff::source::{Representation, SourcePair, SourceVersion};
use forge_diff::syntax::{DeclarationOverview, DeclarationPresentation, DeclarationVisibility};

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
    let mut annotation_end = HashMap::new();
    for (index, mut row) in source.into_iter().enumerate() {
        if let Some(anchor) = navigation.resolve_line(index as u32 + 1) {
            let id = TargetId(format!("plan:declaration:row:{index}"));
            row.metadata.target.push(TargetRange {
                id: id.clone(),
                range: TextRange {
                    start: TextPosition { row: 0, column: 0 },
                    end: TextPosition { row: 1, column: 0 },
                },
            });
            target.insert(id, anchor.clone());
        }
        let row_id = row.id.clone();
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
            annotation_end.insert(
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
                && let Some(end) = annotation_end.get(&fold.end.block)
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
                section: super::PlanSection::Overview,
            },
            json_path: "/overview".into(),
            path: None,
            label: "No declaration changes".into(),
        });
    }
    ensure!(block.len() <= 65536, "declaration diff exceeds 65536 rows");
    Ok((block, navigation, hidden))
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
            && block.metadata.fold.is_empty()
            && !block.id.0.starts_with("plan:annotation:")
            && block
                .text
                .wire_rows()
                .into_iter()
                .any(|row| !row.trim().is_empty());
        public_count.push(public_count.last().unwrap() + usize::from(declaration));
    }
    for (start, header) in block.iter().enumerate() {
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
    if output.is_empty() {
        output.push(forge_diff::projection::header(
            BlockId("plan:design:public:empty".into()),
            vec![TextChunk {
                text: "No public declaration changes.".into(),
                capture: "Comment".into(),
            }],
            0,
        )?);
    }
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
        assert_eq!(block.len(), 1);
        assert_eq!(block[0].text.row(0), Some("No public declaration changes."));
        assert!(target.is_empty());
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
                .starts_with("No declaration changes.")
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
        assert_eq!(folded_header.len(), 2);
        assert!(
            folded_header[0]
                .text
                .row(0)
                .unwrap()
                .starts_with("Modified src/lib.rs")
        );
        assert!(folded_header[1].text.row(0).unwrap().starts_with("@@"));
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
