use super::review_annotation::ReviewAnnotation;
use super::review_source::PlanReviewSource;
use super::{PlanNavigationAnchor, PlanReviewTarget};
use anyhow::{Context, Result, ensure};
use forge_buffer::block::{
    BlockAnchor, BlockMetadata, BufferBlock, Decoration, EditableRegion, FoldRange, TargetRange,
    TextPosition, TextRange,
};
use forge_buffer::identity::{BlockId, EditSequence, FoldId, RegionId, RegionRevision, TargetId};
use forge_buffer::markdown::MarkdownRenderer;
use forge_buffer::text::BufferText;
use forge_buffer::width::WidthProfile;
use std::collections::HashMap;

pub(crate) fn project(
    source: &PlanReviewSource,
    width: &WidthProfile,
    annotation: &[ReviewAnnotation],
    revision: &HashMap<String, (RegionRevision, EditSequence)>,
) -> Result<(Vec<BufferBlock>, HashMap<TargetId, PlanNavigationAnchor>)> {
    let rendered = MarkdownRenderer::source(
        BlockId("plan:markdown".into()),
        source
            .rendered
            .markdown
            .strip_suffix('\n')
            .unwrap_or(&source.rendered.markdown),
        width,
    )?;
    let rows = rendered.block.text.row_count();
    ensure!(rows <= 65536, "plan projection exceeds 65536 rows");
    let mut source_output = HashMap::new();
    for mapping in &rendered.source {
        for source_row in mapping.source.clone() {
            source_output.insert(source_row + 1, mapping.output.clone());
        }
    }
    let mut row_anchor = HashMap::new();
    let mut target = HashMap::new();
    for anchor in &source.rendered.navigation.anchor {
        let output = source_output
            .get(&(anchor.line as usize))
            .context("plan source mapping is missing")?;
        for row in output.clone() {
            row_anchor.entry(row).or_insert(anchor);
        }
    }
    let mut insertion: HashMap<usize, Vec<&ReviewAnnotation>> = HashMap::new();
    for annotation in annotation {
        let output = source_output
            .get(&(annotation.source.end_line as usize))
            .context("annotation source mapping is missing")?;
        insertion
            .entry(output.end.saturating_sub(1))
            .or_default()
            .push(annotation);
    }
    let mut block = Vec::new();
    let mut row_block = Vec::new();
    let mut row_decoration: HashMap<usize, Vec<Decoration>> = HashMap::new();
    for decoration in &rendered.block.metadata.decoration {
        for row in decoration.range.start.row..=decoration.range.end.row.min(rows.saturating_sub(1))
        {
            let length = rendered
                .block
                .text
                .row(row)
                .context("decoration source row is missing")?
                .len();
            if let Some(range) = row_range(&decoration.range, row, length) {
                row_decoration.entry(row).or_default().push(Decoration {
                    range,
                    capture: decoration.capture.clone(),
                    priority: decoration.priority,
                });
            }
        }
    }
    for row in 0..rows {
        row_block.push(block.len());
        let text = rendered
            .block
            .text
            .row(row)
            .context("rendered plan row is missing")?;
        let mut metadata = BlockMetadata::default();
        metadata.decoration = row_decoration.remove(&row).unwrap_or_default();
        if let Some(syntax) = &source.syntax {
            metadata.decoration.clear();
            forge_diff::projection::append_source_syntax_row(&mut metadata, syntax, row, 0, text)?;
        }
        if let Some(anchor) = row_anchor.get(&row).filter(|_| !text.is_empty()) {
            append_semantic_style(&mut metadata, &anchor.target, text);
            let id = TargetId(format!("plan:source:{}:{row}", anchor.line));
            metadata.target.push(TargetRange {
                id: id.clone(),
                range: TextRange {
                    start: TextPosition { row: 0, column: 0 },
                    end: TextPosition {
                        row: 0,
                        column: text.len(),
                    },
                },
            });
            target.insert(id, (*anchor).clone());
        }
        block.push(BufferBlock {
            id: BlockId(format!("plan:row:{row}")),
            text: BufferText::from_rows([text])?,
            metadata,
        });
        for annotation in insertion.remove(&row).unwrap_or_default() {
            block.push(annotation_block(
                annotation,
                revision
                    .get(&annotation.id)
                    .copied()
                    .unwrap_or((RegionRevision(0), EditSequence(0))),
            )?);
        }
    }
    let headings: Vec<_> = source
        .rendered
        .markdown
        .lines()
        .enumerate()
        .filter_map(|(row, text)| {
            let depth = text.bytes().take_while(|byte| *byte == b'#').count();
            (depth > 0 && depth <= 6 && text.as_bytes().get(depth) == Some(&b' '))
                .then(|| {
                    source_output
                        .get(&(row + 1))
                        .map(|output| (row, depth, output.start))
                })
                .flatten()
        })
        .collect();
    let mut following = Vec::new();
    for (source_row, depth, start) in headings.iter().rev() {
        while following
            .last()
            .is_some_and(|(next_depth, _)| next_depth > depth)
        {
            following.pop();
        }
        let end = following.last().map(|(_, start)| *start).unwrap_or(rows);
        following.push((*depth, *start));
        if end <= start + 1 {
            continue;
        }
        let start_block = row_block[*start];
        let end_block = if end == rows {
            block.len()
        } else {
            row_block[end]
        };
        let endpoint = &block[end_block - 1];
        let end = BlockAnchor {
            block: endpoint.id.clone(),
            position: TextPosition {
                row: endpoint.text.row_count(),
                column: 0,
            },
        };
        block[start_block].metadata.fold.push(FoldRange {
            id: FoldId(format!("plan:heading:{source_row}")),
            start: TextPosition { row: 0, column: 0 },
            end,
            closed: false,
        });
    }
    for (source_start, source_end) in task_ranges(
        &source.rendered.navigation.anchor,
        source.rendered.markdown.lines().count() + 1,
    ) {
        let start = source_output[&source_start].start;
        let end = source_output
            .get(&source_end)
            .map(|range| range.start)
            .unwrap_or(rows);
        if end <= start + 1 {
            continue;
        }
        let end_block = row_block.get(end).copied().unwrap_or(block.len());
        let endpoint = &block[end_block - 1];
        let endpoint = BlockAnchor {
            block: endpoint.id.clone(),
            position: TextPosition {
                row: endpoint.text.row_count(),
                column: 0,
            },
        };
        let fold_start_row = source
            .rendered
            .navigation
            .anchor
            .iter()
            .find(|anchor| anchor.line as usize == source_start)
            .is_some_and(|anchor| matches!(anchor.target, PlanReviewTarget::Subtask { .. }))
            .then_some(0)
            .unwrap_or(1);
        block[row_block[start]].metadata.fold.push(FoldRange {
            id: FoldId(format!("plan:task:{source_start}")),
            start: TextPosition {
                row: fold_start_row,
                column: 0,
            },
            end: endpoint,
            closed: false,
        });
    }
    for (source_start, source_end) in file_tree_ranges(
        &source.rendered.navigation.anchor,
        source.rendered.markdown.lines().count() + 1,
    ) {
        let start = source_output[&source_start].start;
        let end = source_output
            .get(&source_end)
            .map(|range| range.start)
            .unwrap_or(rows);
        if end <= start + 1 {
            continue;
        }
        let end_block = row_block.get(end).copied().unwrap_or(block.len());
        let endpoint_block = &block[end_block - 1];
        let endpoint = BlockAnchor {
            block: endpoint_block.id.clone(),
            position: TextPosition { row: 0, column: 0 },
        };
        block[row_block[start]].metadata.fold.push(FoldRange {
            id: FoldId(format!("plan:file-tree:{source_start}")),
            start: TextPosition { row: 0, column: 0 },
            end: endpoint,
            closed: true,
        });
    }
    Ok((block, target))
}

fn append_semantic_style(metadata: &mut BlockMetadata, target: &PlanReviewTarget, text: &str) {
    match target {
        PlanReviewTarget::Task { title } => {
            append_term(metadata, text, title, "ForgeWalkthroughItemTitle", 0)
        }
        PlanReviewTarget::File { path }
        | PlanReviewTarget::DependencyManifest { manifest: path } => {
            append_term(metadata, text, "file", "ForgeFileKeyword", 0);
            append_term(metadata, text, path, "ForgeWalkthroughItemTitle", 0);
        }
        PlanReviewTarget::Dependency { name } => {
            append_action(metadata, text);
            append_term(metadata, text, name, "ForgeDependencyName", 0);
            if let Some(start) = text
                .find('(')
                .zip(text.rfind(')'))
                .filter(|(start, end)| start < end)
            {
                append_range(metadata, start.0, start.1 + 1, "ForgePlanMetadata");
            }
        }
        PlanReviewTarget::Entity { name } => {
            append_action(metadata, text);
            append_declaration_keyword(metadata, text);
            append_term(
                metadata,
                text,
                name,
                if text.contains("fn ") {
                    "@function"
                } else {
                    "@type"
                },
                0,
            );
            append_inline_path(metadata, text);
            append_owner_alignment(metadata, text);
        }
        PlanReviewTarget::EntityMember { member, .. } => {
            append_term(
                metadata,
                text,
                member,
                if text.contains('(') {
                    "@function.method"
                } else {
                    "@variable.member"
                },
                0,
            );
            append_inline_path(metadata, text);
            append_owner_alignment(metadata, text);
        }
        PlanReviewTarget::EnumVariant { variant, .. } => {
            append_term(metadata, text, variant, "@variable", 0)
        }
        PlanReviewTarget::EnumVariantField { field, .. } => {
            append_term(metadata, text, field, "@variable.member", 0)
        }
        PlanReviewTarget::FlowStep {
            target_name,
            target_is_type,
            ..
        } => {
            append_term(
                metadata,
                text,
                target_name,
                if *target_is_type {
                    "@type"
                } else {
                    "@function"
                },
                0,
            );
            append_inline_path(metadata, text);
            append_owner_alignment(metadata, text);
        }
        PlanReviewTarget::FlowEdge {
            callable_name,
            callable_kind,
            target_name,
            target_is_type,
            ..
        } => {
            if let Some(callable_name) = callable_name {
                let capture = if callable_kind
                    .is_some_and(|kind| format!("{kind:?}").eq_ignore_ascii_case("method"))
                {
                    "@function.method.call"
                } else {
                    "@function.call"
                };
                append_term(metadata, text, callable_name, capture, 0);
            }
            if *target_is_type {
                append_last_term(metadata, text, target_name, "@type");
            }
            append_inline_path(metadata, text);
        }
        PlanReviewTarget::FlowEdgeResult { type_name } => {
            append_last_term(metadata, text, type_name, "@type")
        }
        PlanReviewTarget::FlowBranch { .. } => {
            append_term(metadata, text, "when", "@keyword.conditional", 0)
        }
        PlanReviewTarget::Test { category } => {
            append_action(metadata, text);
            append_term(metadata, text, &format!("{category:?}Test"), "@type", 0);
        }
        PlanReviewTarget::Subtask { .. } => append_action(metadata, text),
        PlanReviewTarget::FileTreeFile { path } => {
            append_file_tree_prefix(metadata, text);
            append_file_tree_action(metadata, text);
            let name = path.rsplit(['/', '\\']).next().unwrap_or(path);
            append_term(metadata, text, name, "ForgeWalkthroughItemTitle", 0);
        }
        PlanReviewTarget::FileTreeEntity { name, .. } => {
            append_file_tree_prefix(metadata, text);
            append_file_tree_action(metadata, text);
            append_declaration_keyword(metadata, text);
            append_term(metadata, text, name, "@type", 0)
        }
        PlanReviewTarget::FileTreeTest { name, category, .. } => {
            append_file_tree_prefix(metadata, text);
            append_file_tree_action(metadata, text);
            append_term(metadata, text, &format!("{category:?}Test"), "@type", 0);
            append_term(metadata, text, name, "@function", 0);
        }
        _ => {}
    }
}

fn append_file_tree_prefix(metadata: &mut BlockMetadata, text: &str) {
    let content = text.trim_start_matches([' ', '├', '└', '─', '│']);
    append_range(
        metadata,
        0,
        text.len().saturating_sub(content.len()),
        "ForgePlanMetadata",
    );
}

fn append_file_tree_action(metadata: &mut BlockMetadata, text: &str) {
    for (term, capture) in [
        ("(new)", "ForgeWalkthroughActionAdd"),
        ("(remove)", "ForgeWalkthroughActionRemove"),
    ] {
        if text.contains(term) {
            append_term(metadata, text, term, capture, 0);
            return;
        }
    }
}

fn append_action(metadata: &mut BlockMetadata, text: &str) {
    for (term, capture) in [
        ("Add", "ForgeWalkthroughActionAdd"),
        ("Modify", "ForgeWalkthroughActionModify"),
        ("Remove", "ForgeWalkthroughActionRemove"),
        ("Rename", "ForgeWalkthroughActionRename"),
    ] {
        if text.split_whitespace().any(|word| word == term) {
            append_term(metadata, text, term, capture, 0);
            break;
        }
    }
}

fn append_declaration_keyword(metadata: &mut BlockMetadata, text: &str) {
    for term in [
        "abstract class",
        "interface",
        "resource",
        "struct",
        "class",
        "trait",
        "config",
        "cache",
        "adapter",
        "enum",
        "fn",
    ] {
        if text.contains(term) {
            append_term(metadata, text, term, "@keyword", 0);
            break;
        }
    }
    append_term(metadata, text, "extends", "@keyword", 0);
}

fn append_inline_path(metadata: &mut BlockMetadata, text: &str) {
    if let Some(start) = text
        .rfind('[')
        .filter(|start| text[*start..].trim_end().ends_with(']'))
    {
        append_range(metadata, start, text.trim_end().len(), "ForgePlanMetadata");
    }
}

fn append_owner_alignment(metadata: &mut BlockMetadata, text: &str) {
    let Some(path_start) = text.rfind('[') else {
        return;
    };
    let path_end = text.trim_end().len();
    metadata
        .source_overlay
        .push(forge_buffer::block::SourceOverlay {
            range: TextRange {
                start: TextPosition {
                    row: 0,
                    column: path_start,
                },
                end: TextPosition {
                    row: 0,
                    column: path_end,
                },
            },
            text: text[path_start..path_end].into(),
            capture: "ForgeRightAlignedOwner".into(),
            priority: 200,
        });
}

fn append_last_term(metadata: &mut BlockMetadata, text: &str, term: &str, capture: &str) {
    if let Some(column) = text.rfind(term) {
        append_range(metadata, column, column + term.len(), capture);
    }
}

fn append_term(metadata: &mut BlockMetadata, text: &str, term: &str, capture: &str, offset: usize) {
    if term.is_empty() || offset > text.len() {
        return;
    }
    if let Some(relative) = text[offset..].find(term) {
        let column = offset + relative;
        append_range(metadata, column, column + term.len(), capture);
    }
}

fn append_range(metadata: &mut BlockMetadata, start: usize, end: usize, capture: &str) {
    if start >= end {
        return;
    }
    metadata.visible_decoration.push(Decoration {
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
        priority: 200,
    });
}

fn file_tree_ranges(anchor: &[PlanNavigationAnchor], end: usize) -> Vec<(usize, usize)> {
    let mut active = None;
    let mut ranges = Vec::new();
    for anchor in anchor {
        match &anchor.target {
            PlanReviewTarget::FileTreeFile { .. } => {
                if let Some(start) = active.replace(anchor.line as usize) {
                    ranges.push((start, anchor.line as usize));
                }
            }
            PlanReviewTarget::FileTreeEntity { .. } | PlanReviewTarget::FileTreeTest { .. } => {}
            _ => {
                if let Some(start) = active.take() {
                    ranges.push((start, anchor.line as usize));
                }
            }
        }
    }
    if let Some(start) = active {
        ranges.push((start, end));
    }
    ranges
}

fn task_ranges(anchor: &[PlanNavigationAnchor], end: usize) -> Vec<(usize, usize)> {
    let mut active: Vec<(usize, &str, usize, bool)> = Vec::new();
    let mut ranges = Vec::new();
    for anchor in anchor {
        let depth = match &anchor.target {
            PlanReviewTarget::Section { .. } => 0,
            PlanReviewTarget::Task { .. } => 1,
            PlanReviewTarget::File { .. } if anchor.json_path.starts_with("/tasks/") => 2,
            PlanReviewTarget::Subtask { .. } => 3,
            PlanReviewTarget::Test { .. } if anchor.json_path.starts_with("/tasks/") => 3,
            _ => continue,
        };
        if let Some((_, _, start, _)) = active
            .last_mut()
            .filter(|(_, path, _, _)| *path == anchor.json_path)
        {
            *start = anchor.line as usize;
            continue;
        }
        while active
            .last()
            .is_some_and(|(current, _, _, _)| *current >= depth)
        {
            let (_, _, start, fold) = active.pop().expect("active task range");
            if fold {
                ranges.push((start, anchor.line as usize));
            }
        }
        if depth > 0 {
            let fold = matches!(
                anchor.target,
                PlanReviewTarget::Task { .. } | PlanReviewTarget::Subtask { .. }
            );
            active.push((depth, &anchor.json_path, anchor.line as usize, fold));
        }
    }
    for (_, _, start, fold) in active {
        if fold {
            ranges.push((start, end));
        }
    }
    ranges
}

fn annotation_block(
    annotation: &ReviewAnnotation,
    version: (RegionRevision, EditSequence),
) -> Result<BufferBlock> {
    let mut rows = vec!["Comment".to_owned()];
    rows.extend(annotation.source.body.split('\n').map(str::to_owned));
    let end = TextPosition {
        row: rows.len() - 1,
        column: rows.last().context("annotation row is missing")?.len(),
    };
    Ok(BufferBlock {
        id: BlockId(format!("plan:annotation:{}", annotation.id)),
        text: BufferText::from_rows(rows)?,
        metadata: BlockMetadata {
            editable_region: vec![EditableRegion {
                id: RegionId(annotation.id.clone()),
                revision: version.0,
                sequence: version.1,
                range: TextRange {
                    start: TextPosition { row: 1, column: 0 },
                    end,
                },
            }],
            ..Default::default()
        },
    })
}

fn row_range(range: &TextRange, row: usize, length: usize) -> Option<TextRange> {
    if row < range.start.row
        || row > range.end.row
        || (row == range.end.row && range.end.column == 0)
    {
        return None;
    }
    let start = if row == range.start.row {
        range.start.column
    } else {
        0
    };
    let end = if row == range.end.row {
        range.end.column
    } else {
        length
    };
    (start < end).then_some(TextRange {
        start: TextPosition {
            row: 0,
            column: start,
        },
        end: TextPosition {
            row: 0,
            column: end,
        },
    })
}

#[cfg(test)]
mod tests {
    #[test]
    fn file_style_uses_navigation_identity_and_utf8_byte_ranges() {
        let text = "└─ file src/日本語.rs";
        let target = super::PlanReviewTarget::File {
            path: "src/日本語.rs".into(),
        };
        let mut metadata = super::BlockMetadata::default();
        super::append_semantic_style(&mut metadata, &target, text);
        let span = &metadata.visible_decoration;
        assert_eq!(span.len(), 2);
        assert_eq!(
            &text[span[0].range.start.column..span[0].range.end.column],
            "file"
        );
        assert_eq!(span[0].capture, "ForgeFileKeyword");
        assert_eq!(
            &text[span[1].range.start.column..span[1].range.end.column],
            "src/日本語.rs"
        );
        assert_eq!(span[1].capture, "ForgeWalkthroughItemTitle");
        let mut unrelated = super::BlockMetadata::default();
        super::append_semantic_style(
            &mut unrelated,
            &super::PlanReviewTarget::Task {
                title: "file src/日本語.rs".into(),
            },
            text,
        );
        assert_eq!(unrelated.visible_decoration.len(), 1);
        assert_eq!(
            unrelated.visible_decoration[0].capture,
            "ForgeWalkthroughItemTitle"
        );
    }

    #[test]
    fn semantic_style_uses_typed_plan_targets() {
        let mut dependency = super::BlockMetadata::default();
        super::append_semantic_style(
            &mut dependency,
            &super::PlanReviewTarget::Dependency {
                name: "serde".into(),
            },
            "└─ Add serde (1.0, MIT) - serialization",
        );
        let capture_list: Vec<_> = dependency
            .visible_decoration
            .iter()
            .map(|span| span.capture.as_str())
            .collect();
        assert_eq!(
            capture_list,
            [
                "ForgeWalkthroughActionAdd",
                "ForgeDependencyName",
                "ForgePlanMetadata"
            ]
        );

        let mut flow = super::BlockMetadata::default();
        super::append_semantic_style(
            &mut flow,
            &super::PlanReviewTarget::FlowEdgeResult {
                type_name: "PersistedMigration".into(),
            },
            "└─ → PersistedMigration",
        );
        let result = &flow.visible_decoration[0];
        assert_eq!(result.capture, "@type");
        assert_eq!(
            &"└─ → PersistedMigration"[result.range.start.column..result.range.end.column],
            "PersistedMigration"
        );

        let mut file_entity = super::BlockMetadata::default();
        super::append_semantic_style(
            &mut file_entity,
            &super::PlanReviewTarget::FileTreeEntity {
                name: "requested_change".into(),
                path: "src/change.rs".into(),
            },
            "   ├─ (new) fn requested_change",
        );
        assert_eq!(
            file_entity
                .visible_decoration
                .iter()
                .map(|span| span.capture.as_str())
                .collect::<Vec<_>>(),
            [
                "ForgePlanMetadata",
                "ForgeWalkthroughActionAdd",
                "@keyword",
                "@type"
            ]
        );

        let mut file_test = super::BlockMetadata::default();
        super::append_semantic_style(
            &mut file_test,
            &super::PlanReviewTarget::FileTreeTest {
                name: "verify_requested_change".into(),
                action: crate::plan::ChangeAction::Add,
                path: "src/change.rs".into(),
                category: crate::plan::TestCategory::Unit,
            },
            "   └─ (new) UnitTest verify_requested_change",
        );
        assert_eq!(
            file_test
                .visible_decoration
                .iter()
                .map(|span| span.capture.as_str())
                .collect::<Vec<_>>(),
            [
                "ForgePlanMetadata",
                "ForgeWalkthroughActionAdd",
                "@type",
                "@function"
            ]
        );

        let mut owner = super::BlockMetadata::default();
        super::append_semantic_style(
            &mut owner,
            &super::PlanReviewTarget::Entity {
                name: "requested_change".into(),
            },
            "*fn requested_change                [src/change.rs]",
        );
        assert_eq!(owner.source_overlay.len(), 1);
        assert_eq!(owner.source_overlay[0].capture, "ForgeRightAlignedOwner");
        assert_eq!(owner.source_overlay[0].text, "[src/change.rs]");
    }

    #[test]
    fn file_tree_ranges_close_each_file_over_its_children() {
        let anchor = |line, target| super::PlanNavigationAnchor {
            line,
            json_path: "/fixture".into(),
            target,
            path: None,
            label: "fixture".into(),
        };
        let ranges = super::file_tree_ranges(
            &[
                anchor(
                    10,
                    super::PlanReviewTarget::FileTreeFile {
                        path: "src/first.rs".into(),
                    },
                ),
                anchor(
                    11,
                    super::PlanReviewTarget::FileTreeEntity {
                        name: "First".into(),
                        path: "src/first.rs".into(),
                    },
                ),
                anchor(
                    12,
                    super::PlanReviewTarget::FileTreeFile {
                        path: "src/second.rs".into(),
                    },
                ),
                anchor(
                    13,
                    super::PlanReviewTarget::FileTreeTest {
                        name: "second_works".into(),
                        action: crate::plan::ChangeAction::Add,
                        path: "src/second.rs".into(),
                        category: crate::plan::TestCategory::Unit,
                    },
                ),
                anchor(
                    15,
                    super::PlanReviewTarget::Section {
                        section: crate::plan::PlanSection::Dependencies,
                    },
                ),
            ],
            20,
        );
        assert_eq!(ranges, [(10, 12), (12, 15)]);
    }

    #[test]
    fn task_ranges_retain_external_entity_children_and_wrapped_subtasks() {
        let anchor = |line, path: &str, target| super::PlanNavigationAnchor {
            line,
            json_path: path.into(),
            target,
            path: None,
            label: "label".into(),
        };
        let ranges = super::task_ranges(
            &[
                anchor(
                    1,
                    "/tasks/0",
                    super::PlanReviewTarget::Task {
                        title: "first".into(),
                    },
                ),
                anchor(
                    2,
                    "/tasks/0/files/0",
                    super::PlanReviewTarget::File { path: "a".into() },
                ),
                anchor(
                    3,
                    "/tasks/0/files/0/subtasks/0",
                    super::PlanReviewTarget::Subtask { path: "a".into() },
                ),
                anchor(
                    4,
                    "/tasks/0/files/0/subtasks/0",
                    super::PlanReviewTarget::Subtask { path: "a".into() },
                ),
                anchor(
                    5,
                    "/entity_changes/0",
                    super::PlanReviewTarget::Entity {
                        name: "entity".into(),
                    },
                ),
                anchor(
                    7,
                    "/tasks/1",
                    super::PlanReviewTarget::Task {
                        title: "second".into(),
                    },
                ),
            ],
            10,
        );
        assert_eq!(ranges, [(4, 7), (1, 7), (7, 10)]);
    }
    use super::*;
    use crate::plan::{PlanAnnotationInput, PlanFileStore};
    use forge_buffer::document::BufferDocument;
    use forge_buffer::identity::DocumentId;

    #[test]
    fn projected_annotations_keep_literal_text_and_exact_canonical_source_rows() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path(), temporary.path());
        let canonical = crate::plan::document::test_fixture(
            "plan",
            "A **bold** overview with several wrapping words",
        );
        store
            .write_working_document("session", "plan", &canonical)
            .unwrap();
        let (_, _, checksum) = store
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let source = store
            .capture_review_source("session", "plan", 1, &checksum)
            .unwrap();
        let anchor = source
            .rendered
            .navigation
            .anchor
            .iter()
            .find(|anchor| anchor.json_path == "/overview")
            .unwrap();
        let annotation = ReviewAnnotation {
            id: "note".into(),
            source: PlanAnnotationInput {
                start_line: anchor.line,
                end_line: anchor.line,
                body: "literal **comment**\n".into(),
            },
        };
        let mut width = WidthProfile::default();
        width.columns = 20;
        let (block, target) = project(
            &source,
            &width,
            &[annotation],
            &HashMap::from([("note".into(), (RegionRevision(7), EditSequence(9)))]),
        )
        .unwrap();
        let projected_source: Vec<_> = block
            .iter()
            .filter(|block| block.id.0.starts_with("plan:row:"))
            .flat_map(|block| block.text.wire_rows())
            .collect();
        assert_eq!(
            projected_source,
            source
                .rendered
                .markdown
                .strip_suffix('\n')
                .unwrap_or(&source.rendered.markdown)
                .split('\n')
                .collect::<Vec<_>>()
        );
        for fold in block.iter().flat_map(|block| &block.metadata.fold) {
            if fold.id.0.starts_with("plan:task:") {
                let endpoint = block
                    .iter()
                    .find(|block| block.id == fold.end.block)
                    .unwrap();
                assert!(
                    endpoint
                        .text
                        .row(fold.end.position.row - 1)
                        .unwrap()
                        .trim()
                        .is_empty()
                );
            }
        }
        let document = BufferDocument::new(DocumentId("review".into()), block).unwrap();
        let annotation = document
            .block(&BlockId("plan:annotation:note".into()))
            .unwrap();
        assert_eq!(
            annotation.metadata.editable_region[0].sequence,
            EditSequence(9)
        );
        assert_eq!(
            annotation.text.wire_rows(),
            ["Comment", "literal **comment**", ""]
        );
        assert_eq!(
            annotation.metadata.editable_region[0].revision,
            RegionRevision(7)
        );
        assert!(
            target
                .values()
                .any(|target| target.json_path == "/overview")
        );
        assert!(
            document
                .snapshot()
                .block
                .iter()
                .any(|block| !block.metadata.fold.is_empty())
        );
        assert!(
            document
                .snapshot()
                .block
                .iter()
                .any(|block| !block.metadata.decoration.is_empty())
        );
    }
}
