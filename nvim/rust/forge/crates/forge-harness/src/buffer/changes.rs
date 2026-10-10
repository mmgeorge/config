use std::collections::HashMap;

use anyhow::Result;
use forge_buffer::block::{
    BlockAnchor, BlockMetadata, BufferBlock, Decoration, TargetRange, TextChunk,
    TextPosition, TextRange,
};
use forge_buffer::identity::{BlockId, FoldId, TargetId};
use forge_buffer::text::BufferText;
use forge_diff::display::RowKind;
use forge_diff::file_header::FileChange;
use forge_diff::patch::UnifiedPatch;

use super::projection::TranscriptAction;
use super::transcript::TranscriptRenderer;

/// Owns the folded presentation of one saved exchange, checkpoint, or tool patch.
pub(super) struct ChangeTree {
    pub block: Vec<BufferBlock>,
    pub action: HashMap<TargetId, TranscriptAction>,
}

fn file_action(path: &str, line: usize, previous: bool, declaration: Option<(&crate::exchange::DeclarationRevision, bool)>) -> TranscriptAction {
    match declaration {
        Some((declaration, document)) => TranscriptAction::Declaration {
            plan_id: declaration.plan_id.clone(),
            revision: if previous && declaration.revision > 1 && !declaration.baseline_paths.contains(path) { declaration.revision - 1 } else { declaration.revision },
            baseline: previous && (declaration.revision == 1 || declaration.baseline_paths.contains(path)),
            path: path.into(), line, document,
        },
        None => TranscriptAction::File { path: path.into(), line },
    }
}

impl ChangeTree {
    /// Retain file and hunk rows under stable folds while preserving the raw patch action.
    pub fn render(
        renderer: &TranscriptRenderer<'_>,
        id: &str,
        label: &str,
        suffix: &str,
        text: &str,
        file_label: Option<(&str, &str)>,
        declaration: Option<(&crate::exchange::DeclarationRevision, bool)>,
    ) -> Result<Self> {
        let mut tree = Self {
            block: Vec::new(),
            action: HashMap::new(),
        };
        let patch = match UnifiedPatch::parse(text) {
            Ok(patch) => patch,
            Err(error) => {
                let mut block = renderer.literal(
                    BlockId(id.into()),
                    &format!("◇ {label}: preview unavailable ({error})"),
                    2,
                )?;
                TranscriptRenderer::heading_marker(&mut block, "Normal");
                tree.push_action(block, TranscriptAction::Diff { text: text.into() })?;
                return Ok(tree);
            }
        };
        if patch.file.is_empty() {
            return Ok(tree);
        }
        let added = patch
            .file
            .iter()
            .flat_map(|file| &file.hunk)
            .flat_map(|hunk| &hunk.row)
            .filter(|row| row.kind == RowKind::Added)
            .count();
        let removed = patch
            .file
            .iter()
            .flat_map(|file| &file.hunk)
            .flat_map(|hunk| &hunk.row)
            .filter(|row| row.kind == RowKind::Removed)
            .count();
        let count = patch.file.len();
        let unit = if declaration.is_some_and(|(_, document)| document) { "section" } else { "file" };
        let summary = format!(
            "▸ {label} {count} {} +{added} -{removed}{suffix}",
            if count == 1 { unit.into() } else { format!("{unit}s") }
        );
        let mut heading = renderer.literal(BlockId(id.into()), &summary, 2)?;
        TranscriptRenderer::heading_marker(&mut heading, "Normal");
        decorate_counts(&mut heading, added, removed);
        tree.push_action(heading, TranscriptAction::Diff { text: text.into() })?;
        for (file_index, file) in patch.file.iter().enumerate() {
            let file_start = tree.block.len();
            let path = file
                .new_path
                .as_ref()
                .or(file.old_path.as_ref())
                .expect("validated patch path");
            let file_id = format!("{id}:file:{file_index}");
            let added = file
                .hunk
                .iter()
                .flat_map(|hunk| &hunk.row)
                .filter(|row| row.kind == RowKind::Added)
                .count();
            let removed = file
                .hunk
                .iter()
                .flat_map(|hunk| &hunk.row)
                .filter(|row| row.kind == RowKind::Removed)
                .count();
            let status = if file.old_path.is_none() {
                FileChange::Added
            } else if file.new_path.is_none() {
                FileChange::Deleted
            } else if file.old_path != file.new_path {
                FileChange::Renamed
            } else {
                FileChange::Modified
            };
            let display_path = if let Some((_, label)) = file_label
                .filter(|(source, _)| source.replace('\\', "/") == path.replace('\\', "/"))
            {
                label.replace(['\n', '\r'], " ")
            } else if matches!(status, FileChange::Renamed) {
                format!("{} → {path}", file.old_path.as_deref().unwrap())
            } else {
                path.clone()
            };
            let heading = forge_diff::projection::header(
                BlockId(file_id.clone()),
                status.header(&display_path, Some((added as u64, removed as u64)), false),
                0,
            )?;
            if let Some(path) = &file.new_path {
                tree.push_action(
                    heading,
                    file_action(path, 1, false, declaration),
                )?;
            } else if declaration.is_some() {
                tree.push_action(heading, file_action(path, 1, true, declaration))?;
            } else {
                tree.block.push(heading);
            }
            if file.hunk.is_empty() {
                let label = if file.binary {
                    "Binary file"
                } else {
                    "No textual diff"
                };
                tree.block.push(forge_diff::projection::header(
                    BlockId(format!("{file_id}:empty")),
                    vec![TextChunk {
                        text: label.into(),
                        capture: "Comment".into(),
                    }],
                    0,
                )?);
            }
            for (hunk_index, hunk) in file.hunk.iter().enumerate() {
                let hunk_start = tree.block.len();
                let hunk_id = format!("{file_id}:hunk:{hunk_index}");
                let heading = forge_diff::projection::header(
                    BlockId(hunk_id.clone()),
                    vec![TextChunk {
                        text: hunk.header.into(),
                        capture: "ForgeHunkHeader".into(),
                    }],
                    0,
                )?;
                tree.block.push(heading);
                let emphasis = forge_diff::intraline::patch_emphasis(
                    forge_diff::intraline::IntralinePolicy::default(),
                    &hunk.row,
                );
                for (batch_index, rows) in hunk.row.chunks(128).enumerate() {
                    let mut block = BufferBlock {
                        id: BlockId(format!("{hunk_id}:rows:{batch_index}")),
                        text: BufferText::from_rows(
                            rows.iter()
                                .map(|row| row.text.trim_end_matches('\r').to_owned())
                                .collect::<Vec<_>>(),
                        )?,
                        metadata: BlockMetadata::default(),
                    };
                    for (row_index, row) in rows.iter().enumerate() {
                        forge_diff::projection::append_patch_row(
                            &mut block.metadata,
                            row_index,
                            row,
                            hunk,
                            &emphasis[batch_index * 128 + row_index],
                            0,
                        )?;
                        let location = if let (Some(path), Some(line)) = (&file.new_path, row.new_line) {
                            Some((path, line, false))
                        } else if declaration.is_some() {
                            file.old_path.as_ref().zip(row.old_line).map(|(path, line)| (path, line, true))
                        } else { None };
                        if let Some((path, line, baseline)) = location {
                            let target = TargetId(format!("{}:row:{row_index}", block.id.0));
                            block.metadata.target.push(TargetRange {
                                id: target.clone(),
                                range: TextRange {
                                    start: TextPosition {
                                        row: row_index,
                                        column: 0,
                                    },
                                    end: TextPosition {
                                        row: row_index + 1,
                                        column: 0,
                                    },
                                },
                            });
                            tree.action.insert(
                                target,
                                file_action(path, line + 1, baseline, declaration),
                            );
                        }
                    }
                    tree.block.push(block);
                }
                tree.fold(hunk_start, &hunk_id, false)?;
            }
            tree.fold(file_start, &file_id, true)?;
        }
        tree.fold(0, id, false)?;
        Ok(tree)
    }

    fn push_action(&mut self, mut block: BufferBlock, action: TranscriptAction) -> Result<()> {
        let target = TargetId(block.id.0.clone());
        block.metadata.target.push(TargetRange {
            id: target.clone(),
            range: whole_block(&block),
        });
        self.action.insert(target, action);
        self.block.push(block);
        Ok(())
    }

    fn fold(&mut self, start: usize, id: &str, expand_children: bool) -> Result<()> {
        let last = self.block.last().expect("change heading exists");
        let end = BlockAnchor {
            block: last.id.clone(),
            position: TextPosition {
                row: last.text.row_count(),
                column: 0,
            },
        };
        forge_diff::projection::fold_header(
            &mut self.block[start],
            FoldId(id.into()),
            end,
            true,
            start == 0,
        );
        self.block[start].metadata.fold[0].expand_children = expand_children;
        self.block[start].metadata.node = Some(forge_buffer::node::NodeState::new(
            FoldId(id.into()), if start == 0 { forge_buffer::node::NodeKind::Changes }
            else if expand_children { forge_buffer::node::NodeKind::File }
            else { forge_buffer::node::NodeKind::Hunk }, forge_buffer::node::NodeDisplay::Heading));
        if start == 0 || expand_children {
            TranscriptRenderer::fold_marker(&mut self.block[start]);
            if expand_children { self.block[start].metadata.layout.as_mut().unwrap().indent = 4; }
        }
        Ok(())
    }
}

fn whole_block(block: &BufferBlock) -> TextRange {
    TextRange {
        start: TextPosition { row: 0, column: 0 },
        end: TextPosition {
            row: block.text.row_count(),
            column: 0,
        },
    }
}

fn decorate_counts(block: &mut BufferBlock, added: usize, removed: usize) {
    for (text, capture) in [
        (format!("+{added}"), "ForgeAddRange"),
        (format!("-{removed}"), "ForgeDeleteRange"),
    ] {
        for row in 0..block.text.row_count() {
            if let Some(column) = block.text.row(row).unwrap().rfind(&text) {
                block.metadata.decoration.push(Decoration {
                    range: TextRange {
                        start: TextPosition { row, column },
                        end: TextPosition {
                            row,
                            column: column + text.len(),
                        },
                    },
                    capture: capture.into(),
                    priority: 100,
                });
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_buffer::identity::{DocumentId, DocumentRevision};
    use forge_buffer::patch::BufferSnapshot;
    use forge_buffer::width::WidthProfile;

    #[test]
    fn long_file_header_keeps_path_with_status_and_preserves_navigation() {
        let path = format!(
            "D:/.local/share/nvim-data/forge/harness/plans/{}/working 雪.md",
            "long-id/".repeat(12)
        );
        let diff = format!("--- {path}\n+++ {path}\n@@ -1 +1 @@\n-before\n+after\n");
        for columns in [40, 80, 120] {
            let width = WidthProfile {
                columns,
                ..WidthProfile::default()
            };
            let renderer = TranscriptRenderer::new(&width).unwrap();
            let tree =
                ChangeTree::render(&renderer, "changes", "Changed", "", &diff, None, None).unwrap();
            let header = &tree.block[1];
            assert_eq!(header.text.row_count(), 1);
            assert_eq!(
                header.text.row(0),
                Some(format!("Modified {path} +1 -1").as_str())
            );
            for capture in [
                "ForgeStatusFileModified",
                "ForgeStatusPath",
                "ForgeAddRange",
                "ForgeDeleteRange",
            ] {
                assert!(
                    header
                        .metadata
                        .decoration
                        .iter()
                        .any(|span| span.capture == capture)
                );
            }
            assert!(tree.action.values().any(|action| matches!(action,
                TranscriptAction::File { path: target, line: 1 } if target == &path)));
        }
    }

    #[test]
    fn change_tree_preserves_counts_folds_gutters_and_source_actions() {
        let width = WidthProfile::default();
        let renderer = TranscriptRenderer::new(&width).unwrap();
        let diff = "diff --git a/test.rs b/test.rs\n--- a/test.rs\n+++ b/test.rs\n@@ -10,2 +20,2 @@ fn test\n-old\n+new\n context\ndiff --git a/gone b/gone\ndeleted file mode 100644\n--- a/gone\n+++ /dev/null\n@@ -1 +0,0 @@\n-gone\n";
        let tree = ChangeTree::render(
            &renderer,
            "exchange:changes",
            "Changed",
            " · checkpoint matched",
            diff,
            None,
            None,
        )
        .unwrap();
        assert_eq!(
            tree.block[0].text.row(0),
            Some("▸ Changed 2 files +1 -2 · checkpoint matched")
        );
        let snapshot = BufferSnapshot {
            document: DocumentId("changes".into()),
            revision: DocumentRevision(0),
            block: tree.block.clone(),
        };
        snapshot.validate().unwrap();
        let folds = tree
            .block
            .iter()
            .flat_map(|block| &block.metadata.fold)
            .collect::<Vec<_>>();
        assert_eq!(folds.len(), 5);
        assert!(folds.iter().all(|fold| fold.closed));
        assert!(tree.block[0].metadata.fold[0].collapse_children);
        assert!(folds.iter().skip(1).all(|fold| !fold.collapse_children));
        let file = &tree.block[1];
        let hunk = &tree.block[2];
        assert!(file.metadata.fold[0].expand_children);
        assert_eq!(tree.block[0].metadata.layout.as_ref().unwrap().marker.as_ref().unwrap().text, "▸");
        assert_eq!(file.metadata.layout.as_ref().unwrap().marker.as_ref().unwrap().text, "▸");
        assert!(hunk.metadata.layout.as_ref().is_none_or(|layout| layout.marker.is_none()));
        assert!(!hunk.metadata.fold[0].expand_children && !tree.block[0].metadata.fold[0].expand_children);
        assert_eq!(file.text.row(0), Some("Modified test.rs +1 -1"));
        assert_eq!(hunk.text.row(0), Some("@@ -10,2 +20,2 @@ fn test"));
        assert_eq!(file.metadata.gutter, hunk.metadata.gutter);
        assert_eq!(file.metadata.gutter, tree.block[0].metadata.gutter);
        let body = tree
            .block
            .iter()
            .find(|block| block.text.row(0) == Some("old"))
            .unwrap();
        assert_eq!(body.text.wire_rows(), vec!["old", "new", "context"]);
        let gutter = body.metadata.gutter[1]
            .chunk
            .iter()
            .map(|chunk| chunk.text.as_str())
            .collect::<String>();
        assert!(gutter.contains("20"));
        assert!(gutter.contains('+'));
        assert!(
            body.metadata
                .decoration
                .iter()
                .any(|decoration| decoration.capture == "ForgeAddBg")
        );
        assert!(
            body.metadata
                .decoration
                .iter()
                .any(|decoration| decoration.capture == "ForgeDeleteBg")
        );
        assert!(tree.action.values().any(|action| matches!(action, TranscriptAction::File { path, line: 20 } if path == "test.rs")));
        assert!(
            !tree.action.values().any(
                |action| matches!(action, TranscriptAction::File { path, .. } if path == "gone")
            )
        );
        assert_eq!(
            tree.action
                .values()
                .filter(|action| matches!(action, TranscriptAction::Diff { text } if text == diff))
                .count(),
            1
        );
    }

    #[test]
    fn malformed_patch_keeps_an_explicit_diagnostic_and_complete_raw_action() {
        let width = WidthProfile::default();
        let renderer = TranscriptRenderer::new(&width).unwrap();
        let source = "--- a/file\n+++ b/file\n@@ -1 +1 @@\n-old\n";
        let tree =
            ChangeTree::render(&renderer, "bad:changes", "Changed", "", source, None, None).unwrap();
        assert!(
            tree.block[0]
                .text
                .wire_rows()
                .join(" ")
                .contains("truncated saved patch hunk")
        );
        assert!(
            tree.action
                .values()
                .any(|action| matches!(action, TranscriptAction::Diff { text } if text == source))
        );
    }

    #[test]
    fn large_hunks_retain_all_rows_across_metadata_batches() {
        let width = WidthProfile::default();
        let renderer = TranscriptRenderer::new(&width).unwrap();
        let source = format!(
            "--- /dev/null\n+++ b/new.rs\n@@ -0,0 +1,300 @@\n{}",
            "+line\n".repeat(300)
        );
        let tree =
            ChangeTree::render(&renderer, "large:changes", "Changed", "", &source, None, None).unwrap();
        let rows = tree
            .block
            .iter()
            .filter(|block| block.id.0.contains(":rows:"))
            .collect::<Vec<_>>();
        assert_eq!(
            rows.iter()
                .map(|block| block.text.row_count())
                .sum::<usize>(),
            300
        );
        assert!(rows.iter().all(|block| block.metadata.gutter.len() <= 128));
        assert!(
            tree.action
                .values()
                .any(|action| matches!(action, TranscriptAction::File { line: 300, .. }))
        );
    }

    #[test]
    fn replacement_emphasis_crosses_delivery_batches_without_changing_source_bytes() {
        let width = WidthProfile::default();
        let renderer = TranscriptRenderer::new(&width).unwrap();
        let source = format!(
            "--- a/test.rs\n+++ b/test.rs\n@@ -1,128 +1,128 @@\n{}-const café = 1;\n+const café = 2;\n",
            " context\n".repeat(127)
        );
        let tree = ChangeTree::render(
            &renderer,
            "replacement:changes",
            "Changed",
            "",
            &source,
            None,
            None,
        )
        .unwrap();
        let mut found = Vec::new();
        for block in &tree.block {
            for decoration in &block.metadata.visible_decoration {
                let row = block.text.row(decoration.range.start.row).unwrap();
                let highlighted = &row[decoration.range.start.column..decoration.range.end.column];
                found.push((decoration.capture.as_str(), highlighted));
            }
        }
        assert_eq!(
            found,
            vec![("ForgeInlineDeleteBg", "1"), ("ForgeInlineAddBg", "2")]
        );
        assert!(
            tree.block
                .iter()
                .any(|block| block.text.wire_rows().contains(&"const café = 1;"))
        );
        assert!(
            tree.block
                .iter()
                .any(|block| block.text.wire_rows().contains(&"const café = 2;"))
        );
    }
}
#[test]
fn large_diff_retains_source_and_navigation_without_byte_admission() -> Result<()> {
    let width = forge_buffer::width::WidthProfile::default();
    let renderer = TranscriptRenderer::new(&width)?;
    let line = "x".repeat(8 * 1024 * 1024 + 1);
    let source = format!("--- /dev/null\n+++ b/large.txt\n@@ -0,0 +1 @@\n+{line}\n");
    let tree = ChangeTree::render(&renderer, "large", "Changes", "", &source, None, None)?;
    assert!(tree.block.iter().any(|block| block.text.row(0) == Some(line.as_str())));
    assert!(tree.action.values().any(|action| matches!(action, TranscriptAction::Diff { text } if text == &source)));
    Ok(())
}

#[test]
fn declaration_changes_navigate_immutable_sides_including_removed_rows() {
    let width = forge_buffer::width::WidthProfile::default();
    let renderer = TranscriptRenderer::new(&width).unwrap();
    let diff = "diff --git a/lib.rs b/lib.rs\n--- a/lib.rs\n+++ b/lib.rs\n@@ -1 +1 @@\n-pub struct Before;\n+pub struct After;\ndiff --git a/gone.rs b/gone.rs\ndeleted file mode 100644\n--- a/gone.rs\n+++ /dev/null\n@@ -1 +0,0 @@\n-pub struct Gone;\n";
    for revision in [1, 3] {
        let declaration = crate::exchange::DeclarationRevision { plan_id: "plan".into(), revision, document_diff: String::new(), baseline_paths: Default::default() };
        let tree = ChangeTree::render(&renderer, "declaration", "Proposed changes", "", diff, None, Some((&declaration, false))).unwrap();
        assert_eq!(tree.block[0].text.row(0), Some("▸ Proposed changes 2 files +1 -2"));
        assert!(!tree.action.values().any(|action| matches!(action, TranscriptAction::File { .. })));
        assert!(tree.action.values().any(|action| matches!(action,
            TranscriptAction::Declaration { path, revision: target, baseline, line: 1, .. }
                if path == "gone.rs" && *target == if revision == 1 { 1 } else { 2 } && *baseline == (revision == 1))));
        assert!(tree.action.values().any(|action| matches!(action,
            TranscriptAction::Declaration { path, revision: target, baseline: false, .. }
                if path == "lib.rs" && *target == revision)));
    }
    let declaration = crate::exchange::DeclarationRevision {
        plan_id: "plan".into(), revision: 3, document_diff: String::new(),
        baseline_paths: ["gone.rs".into()].into(),
    };
    let tree = ChangeTree::render(&renderer, "lazy", "Proposed changes", "", diff, None, Some((&declaration, false))).unwrap();
    assert!(tree.action.values().any(|action| matches!(action,
        TranscriptAction::Declaration { path, revision: 3, baseline: true, line: 1, .. } if path == "gone.rs")));
}
