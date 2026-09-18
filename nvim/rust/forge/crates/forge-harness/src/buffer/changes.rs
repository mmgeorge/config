use std::collections::HashMap;

use anyhow::{Result, ensure};
use forge_buffer::block::{
    BlockAnchor, BlockMetadata, BufferBlock, Decoration, FoldRange, TargetRange, TextChunk,
    TextPosition, TextRange,
};
use forge_buffer::identity::{BlockId, FoldId, TargetId};
use forge_buffer::text::BufferText;
use forge_diff::display::RowKind;
use forge_diff::patch::UnifiedPatch;

use super::projection::TranscriptAction;
use super::transcript::TranscriptRenderer;

/// Owns the folded presentation of one saved exchange, checkpoint, or tool patch.
pub(super) struct ChangeTree {
    pub block: Vec<BufferBlock>,
    pub action: HashMap<TargetId, TranscriptAction>,
    pub bytes: usize,
}

impl ChangeTree {
    /// Retain file and hunk rows under stable folds while preserving the raw patch action.
    pub fn render(
        renderer: &TranscriptRenderer<'_>,
        id: &str,
        label: &str,
        suffix: &str,
        text: &str,
    ) -> Result<Self> {
        let mut tree = Self {
            block: Vec::new(),
            action: HashMap::new(),
            bytes: 0,
        };
        let patch = match UnifiedPatch::parse(text) {
            Ok(patch) => patch,
            Err(error) => {
                let block = renderer.literal(
                    BlockId(id.into()),
                    &format!("▸ {label}: preview unavailable ({error})"),
                    2,
                )?;
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
        let summary = format!(
            "▸ {label} {count} {} +{added} -{removed}{suffix}",
            if count == 1 { "file" } else { "files" }
        );
        let mut heading = renderer.literal(BlockId(id.into()), &summary, 2)?;
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
                "A"
            } else if file.new_path.is_none() {
                "D"
            } else if file.old_path != file.new_path {
                "R"
            } else {
                "M"
            };
            let display_path = if status == "R" {
                format!("{} → {path}", file.old_path.as_deref().unwrap())
            } else {
                path.clone()
            };
            let mut heading = renderer.literal(
                BlockId(file_id.clone()),
                &format!("  ▸ {status} {display_path} +{added} -{removed}"),
                6,
            )?;
            decorate_counts(&mut heading, added, removed);
            if let Some(path) = &file.new_path {
                tree.push_action(
                    heading,
                    TranscriptAction::File {
                        path: path.clone(),
                        line: 1,
                    },
                )?;
            } else {
                tree.push(heading)?;
            }
            if file.hunk.is_empty() {
                let label = if file.binary {
                    "Binary file"
                } else {
                    "No textual diff"
                };
                tree.push(renderer.literal(
                    BlockId(format!("{file_id}:empty")),
                    &format!("    {label}"),
                    4,
                )?)?;
            }
            for (hunk_index, hunk) in file.hunk.iter().enumerate() {
                let hunk_start = tree.block.len();
                let hunk_id = format!("{file_id}:hunk:{hunk_index}");
                let mut heading = renderer.literal(
                    BlockId(hunk_id.clone()),
                    &format!("    ▸ {}", hunk.header),
                    6,
                )?;
                heading.metadata.decoration.push(Decoration {
                    range: whole_block(&heading),
                    capture: "ForgeHunkHeader".into(),
                    priority: 100,
                });
                tree.push(heading)?;
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
                        )?;
                        block.metadata.gutter.last_mut().unwrap().chunk.insert(
                            0,
                            TextChunk {
                                text: "      ".into(),
                                capture: "Normal".into(),
                            },
                        );
                        if let (Some(path), Some(line)) = (&file.new_path, row.new_line) {
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
                            tree.add_action(
                                target,
                                TranscriptAction::File {
                                    path: path.clone(),
                                    line: line + 1,
                                },
                            )?;
                        }
                    }
                    tree.push(block)?;
                }
                tree.fold(hunk_start, &hunk_id)?;
            }
            tree.fold(file_start, &file_id)?;
        }
        tree.fold(0, id)?;
        Ok(tree)
    }

    fn push_action(&mut self, mut block: BufferBlock, action: TranscriptAction) -> Result<()> {
        let target = TargetId(block.id.0.clone());
        block.metadata.target.push(TargetRange {
            id: target.clone(),
            range: whole_block(&block),
        });
        self.add_action(target, action)?;
        self.push(block)
    }

    fn add_action(&mut self, target: TargetId, action: TranscriptAction) -> Result<()> {
        let bytes = match &action {
            TranscriptAction::Diff { text } => text.len(),
            TranscriptAction::File { path, .. } => path.len(),
            _ => 512,
        };
        self.reserve(bytes + target.0.len() + 128)?;
        self.action.insert(target, action);
        Ok(())
    }

    fn push(&mut self, block: BufferBlock) -> Result<()> {
        self.reserve(block.retained_bytes())?;
        self.block.push(block);
        Ok(())
    }

    fn reserve(&mut self, bytes: usize) -> Result<()> {
        ensure!(
            self.bytes.saturating_add(bytes) <= 24 * 1024 * 1024,
            "saved change tree exceeds 24 MiB"
        );
        self.bytes += bytes;
        Ok(())
    }

    fn fold(&mut self, start: usize, id: &str) -> Result<()> {
        self.reserve(id.len() + 512)?;
        let last = self.block.last().expect("change heading exists");
        let end = BlockAnchor {
            block: last.id.clone(),
            position: TextPosition {
                row: last.text.row_count(),
                column: 0,
            },
        };
        self.block[start].metadata.fold.push(FoldRange {
            id: FoldId(id.into()),
            start: TextPosition { row: 0, column: 0 },
            end,
            closed: true,
        });
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
        let tree = ChangeTree::render(&renderer, "bad:changes", "Changed", "", source).unwrap();
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
        let tree = ChangeTree::render(&renderer, "large:changes", "Changed", "", &source).unwrap();
        let rows = tree
            .block
            .iter()
            .filter(|block| !block.metadata.gutter.is_empty())
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
        let tree =
            ChangeTree::render(&renderer, "replacement:changes", "Changed", "", &source).unwrap();
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
fn metadata_capacity_cannot_bypass_saved_change_tree_admission() {
    let mut tree = ChangeTree {
        block: Vec::new(),
        action: HashMap::new(),
        bytes: 0,
    };
    let mut block = BufferBlock {
        id: BlockId("metadata-heavy".into()),
        text: BufferText::from_rows(["x"]).unwrap(),
        metadata: BlockMetadata::default(),
    };
    block.metadata.visible_decoration =
        Vec::with_capacity(24 * 1024 * 1024 / std::mem::size_of::<Decoration>() + 1);
    block.validate().unwrap();
    assert!(tree.push(block).unwrap_err().to_string().contains("24 MiB"));
    assert!(tree.block.is_empty());
    assert_eq!(tree.bytes, 0);
}
