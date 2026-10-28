use std::ops::Range;

use forge_buffer::ContractError;
use forge_buffer::block::{
    BlockAnchor, BufferBlock, ContentLayout, Decoration, TextChunk, TextPosition, TextRange,
};
use forge_buffer::identity::{BlockId, FoldId};
use forge_buffer::text::BufferText;

use crate::display::{DisplayHunk, DisplayRow, RowKind};
use crate::file_header::FileChange;
use crate::patch::PatchHunk;

use crate::syntax::SyntaxHandle;

/// Hunk heading content, independent of the consumer's navigation actions.
pub enum HunkHeading<'source> {
    /// Added and removed row counts used by interactive status views.
    Counts { added: usize, removed: usize },
    /// Canonical old and new source ranges.
    Ranges,
    /// Exact saved heading, including an optional declaration suffix.
    Saved(&'source str),
}

#[derive(Clone, Default)]
/// Source-preserving layout applied to every emitted block.
pub struct PresentationOptions {
    /// Existing buffer layout, including the consumer's nesting and marker column.
    pub layout: Option<ContentLayout>,
    /// Additional inline margin before source gutters, bounded to 4096 cells.
    pub indent: usize,
}

#[derive(Clone, Copy, Default)]
/// Expansion policy attached to a file or hunk heading.
pub struct FoldPolicy {
    /// Initial collapsed state before the owning view records an explicit choice.
    pub closed: bool,
    /// Whether closing this fold also closes its descendants.
    pub collapse_children: bool,
    /// Whether opening this fold also opens its descendants.
    pub expand_children: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
/// Change styling shared by source rows and semantic diff summaries.
pub enum RowChange {
    /// An unchanged row without a change marker or background.
    Context,
    /// A new row with an addition marker and background.
    Added,
    /// A modified row with the shared orange marker and background.
    Modified,
    /// A removed row with a deletion marker and background.
    Removed,
}

impl RowChange {
    /// Returns the marker, row background, gutter foreground, and emphasis captures.
    pub(crate) fn style(self) -> (&'static str, &'static str, &'static str, &'static str) {
        match self {
            Self::Context => (
                " ",
                "ForgeContextBg",
                "ForgeContextLineNr",
                "ForgeContextBg",
            ),
            Self::Added => ("+", "ForgeAddBg", "ForgeAddLineNr", "ForgeInlineAddBg"),
            Self::Modified => ("~", "ForgeModifyBg", "ForgeModifyLineNr", "ForgeModifyBg"),
            Self::Removed => (
                "-",
                "ForgeDeleteBg",
                "ForgeDeleteLineNr",
                "ForgeInlineDeleteBg",
            ),
        }
    }
}

#[derive(Clone)]
/// Shared presentation retaining only the current streamed group and layout options.
pub struct DiffPresenter {
    options: PresentationOptions,
    current_group: Option<usize>,
}

impl DiffPresenter {
    /// Retains layout options without allocating document identities or action targets.
    pub fn new(options: PresentationOptions) -> Self {
        Self {
            options,
            current_group: None,
        }
    }

    /// Emits a streamed group heading once across consecutive bounded deliveries.
    /// Failed formatting leaves the previous group unchanged.
    pub fn begin_hunk(
        &mut self,
        index: usize,
        id: BlockId,
        group: &DisplayHunk,
        heading: HunkHeading<'_>,
    ) -> Result<Option<BufferBlock>, ContractError> {
        if self.current_group == Some(index) {
            return Ok(None);
        }
        let block = self.hunk(id, group, heading)?;
        self.current_group = Some(index);
        Ok(Some(block))
    }

    /// Formats a file heading using the shared change labels and statistics.
    pub fn file(
        &self,
        id: BlockId,
        change: FileChange,
        path: &str,
        counts: Option<(u64, u64)>,
    ) -> Result<BufferBlock, ContractError> {
        self.heading(id, change.header(path, counts, false))
    }

    /// Projects a semantic diff row with optional one-based old/new declaration lines.
    pub fn summary(
        &self,
        id: BlockId,
        text: &str,
        change: RowChange,
        lines: [Option<u32>; 2],
        line_width: usize,
    ) -> Result<BufferBlock, ContractError> {
        let mut block = BufferBlock {
            id,
            text: BufferText::from_rows([text])?,
            metadata: Default::default(),
        };
        block.metadata.layout = self.options.layout.clone();
        let (_, background, number, _) = change.style();
        let width = line_width.max(3).max(
            lines
                .iter()
                .flatten()
                .map(|line| line.to_string().len())
                .max()
                .unwrap_or(0),
        );
        let mut columns = Vec::new();
        for line in lines {
            columns.push(TextChunk {
                text: format!(
                    "{:>width$}",
                    line.map(|line| line.to_string()).unwrap_or_default()
                ),
                capture: number.into(),
            });
            columns.push(TextChunk {
                text: "  ".into(),
                capture: background.into(),
            });
        }
        crate::projection::append_change_row(
            &mut block.metadata,
            0,
            change,
            columns,
            self.options.indent,
        )?;
        Ok(block)
    }

    /// Formats a hunk heading while retaining saved heading text when requested.
    pub fn hunk(
        &self,
        id: BlockId,
        group: &DisplayHunk,
        heading: HunkHeading<'_>,
    ) -> Result<BufferBlock, ContractError> {
        let text = match &heading {
            HunkHeading::Counts { added, removed } => format!("@@ +{added} -{removed}"),
            HunkHeading::Ranges => {
                let range = |lines: &Range<usize>| {
                    format!(
                        "{},{}",
                        lines.start + usize::from(!lines.is_empty()),
                        lines.len()
                    )
                };
                format!(
                    "@@ -{} +{} @@",
                    range(&group.old_lines),
                    range(&group.new_lines)
                )
            }
            HunkHeading::Saved(text) => (*text).to_owned(),
        };
        let mut block = self.heading(
            id,
            vec![TextChunk {
                text,
                capture: "ForgeHunkHeader".into(),
            }],
        )?;
        if let HunkHeading::Counts { added, removed } = heading {
            for (column, text, capture) in [
                (3, format!("+{added}"), "ForgeAddRange"),
                (
                    4 + format!("+{added}").len(),
                    format!("-{removed}"),
                    "ForgeDeleteRange",
                ),
            ] {
                block.metadata.decoration.push(Decoration {
                    range: TextRange {
                        start: TextPosition { row: 0, column },
                        end: TextPosition {
                            row: 0,
                            column: column + text.len(),
                        },
                    },
                    capture: capture.into(),
                    priority: 110,
                });
            }
        }
        Ok(block)
    }

    /// Builds at most 256 source rows with unchanged coordinates and shared syntax and gutters.
    pub fn rows(
        &self,
        id: BlockId,
        rows: &[DisplayRow],
        group: &DisplayHunk,
        old: Option<&SyntaxHandle>,
        new: Option<&SyntaxHandle>,
    ) -> Result<BufferBlock, ContractError> {
        self.batch(id, rows, |_| Some(group), old, new)
    }

    /// Presents one source row with a semantic change category and its original coordinates.
    pub fn row(
        &self,
        id: BlockId,
        row: &DisplayRow,
        group: &DisplayHunk,
        change: RowChange,
    ) -> Result<BufferBlock, ContractError> {
        let mut block = BufferBlock {
            id,
            text: BufferText::from_rows([&row.text])?,
            metadata: Default::default(),
        };
        block.metadata.layout = self.options.layout.clone();
        crate::projection::append_display_row(
            &mut block.metadata,
            0,
            row,
            group,
            None,
            None,
            Some(change),
            self.options.indent,
        )?;
        Ok(block)
    }

    /// Projects a bounded delivery that may cross group boundaries without losing gutter widths.
    pub fn batch<'group>(
        &self,
        id: BlockId,
        rows: &[DisplayRow],
        group: impl Fn(usize) -> Option<&'group DisplayHunk>,
        old: Option<&SyntaxHandle>,
        new: Option<&SyntaxHandle>,
    ) -> Result<BufferBlock, ContractError> {
        let mut block = BufferBlock {
            id,
            text: BufferText::from_rows(rows.iter().map(|row| row.text.as_str()))?,
            metadata: Default::default(),
        };
        block.metadata.layout = self.options.layout.clone();
        for (index, row) in rows.iter().enumerate() {
            let group =
                group(row.group_index).ok_or(ContractError("diff display group is missing"))?;
            crate::projection::append_display_row(
                &mut block.metadata,
                index,
                row,
                group,
                old,
                new,
                None,
                self.options.indent,
            )?;
        }
        Ok(block)
    }

    /// Attaches an exclusive fold endpoint without changing caller-owned identities.
    pub fn fold(header: &mut BufferBlock, id: FoldId, end: BlockAnchor, policy: FoldPolicy) {
        crate::projection::fold_header(header, id, end, policy.closed, policy.collapse_children);
        header.metadata.fold[0].expand_children = policy.expand_children;
    }

    fn heading(&self, id: BlockId, chunks: Vec<TextChunk>) -> Result<BufferBlock, ContractError> {
        let mut block = crate::projection::header(id, chunks, self.options.indent)?;
        block.metadata.layout = self.options.layout.clone();
        Ok(block)
    }
}

/// Normalizes a saved patch once, preserving all supplied context and original coordinates.
pub fn patch_rows(hunk: &PatchHunk<'_>) -> (DisplayHunk, Vec<DisplayRow>) {
    let emphasis = crate::intraline::patch_emphasis(Default::default(), &hunk.row);
    let rows = hunk
        .row
        .iter()
        .zip(emphasis)
        .map(|(row, emphasis)| DisplayRow {
            group_index: 0,
            text: row.text.trim_end_matches('\r').into(),
            source: crate::display::DisplaySource::Diff(row.coordinates()),
            raw_id: None,
            emphasis,
            emphasis_fallback: None,
        })
        .collect();
    (
        DisplayHunk {
            raw_ids: Vec::new(),
            old_lines: hunk.old_lines.clone(),
            new_lines: hunk.new_lines.clone(),
            raw_range: 0..0,
        },
        rows,
    )
}

/// Counts changed rows without counting unchanged context or structural omissions.
pub fn counts<'row>(rows: impl IntoIterator<Item = &'row DisplayRow>) -> (u64, u64) {
    rows.into_iter()
        .fold((0, 0), |(added, removed), row| match row.kind() {
            RowKind::Added => (added + 1, removed),
            RowKind::Removed => (added, removed + 1),
            RowKind::Context => (added, removed),
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn streamed_hunk_heading_is_emitted_once_and_failed_identity_does_not_advance() {
        let mut presenter = DiffPresenter::new(Default::default());
        let group = DisplayHunk::context(1..3, 2..4);
        assert!(
            presenter
                .begin_hunk(0, BlockId("".into()), &group, HunkHeading::Ranges)
                .is_err()
        );
        assert!(
            presenter
                .begin_hunk(0, BlockId("first".into()), &group, HunkHeading::Ranges)
                .unwrap()
                .is_some()
        );
        assert!(
            presenter
                .begin_hunk(0, BlockId("first".into()), &group, HunkHeading::Ranges)
                .unwrap()
                .is_none()
        );
        let second = presenter
            .begin_hunk(
                1,
                BlockId("second".into()),
                &group,
                HunkHeading::Counts {
                    added: 1,
                    removed: 2,
                },
            )
            .unwrap()
            .unwrap();
        assert_eq!(second.text.row(0), Some("@@ +1 -2"));
        assert_eq!(second.metadata.decoration.len(), 3);
    }

    #[test]
    fn saved_patch_preserves_heading_context_layout_and_coordinates_across_batches() {
        let patch = crate::patch::UnifiedPatch::parse("--- a/file.rs\n+++ b/file.rs\n@@ -10,2 +20,2 @@ fn run\n-old value\n+new value\n context\n").unwrap();
        let saved = &patch.file[0].hunk[0];
        let (group, rows) = patch_rows(saved);
        for indent in [2, 6] {
            let presenter = DiffPresenter::new(PresentationOptions {
                layout: Some(ContentLayout {
                    indent,
                    source_indent: 0,
                    marker: None,
                }),
                ..Default::default()
            });
            let heading = presenter
                .hunk(
                    BlockId("hunk".into()),
                    &group,
                    HunkHeading::Saved(&saved.header),
                )
                .unwrap();
            assert_eq!(heading.text.row(0), Some("@@ -10,2 +20,2 @@ fn run"));
            assert_eq!(heading.metadata.layout.as_ref().unwrap().indent, indent);
            let whole = presenter
                .rows(BlockId("body".into()), &rows, &group, None, None)
                .unwrap();
            assert_eq!(
                whole.text.wire_rows(),
                ["old value", "new value", "context"]
            );
            for (index, row) in rows.iter().enumerate() {
                let single = presenter
                    .rows(
                        BlockId(format!("row:{index}")),
                        std::slice::from_ref(row),
                        &group,
                        None,
                        None,
                    )
                    .unwrap();
                assert_eq!(single.text.row(0), whole.text.row(index));
                assert_eq!(
                    single.metadata.gutter[0].chunk,
                    whole.metadata.gutter[index].chunk
                );
                assert_eq!(single.metadata.layout, whole.metadata.layout);
            }
            assert_eq!(rows[0].old_coordinate().unwrap().line, 9);
            assert_eq!(rows[1].new_coordinate().unwrap().line, 19);
            assert_eq!(rows[2].new_coordinate().unwrap().line, 20);
            assert_eq!(counts(&rows), (1, 1));
        }
    }
}
