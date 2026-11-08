use std::ops::Range;

use forge_buffer::ContractError;
use forge_buffer::block::{
    BlockAnchor, BufferBlock, ContentLayout, Decoration, FoldRange, TextChunk, TextPosition,
    TextRange,
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

/// Declaration fold positions in a prepared block sequence. The body content belongs to its view.
pub struct DeclarationFoldLayout {
    pub id: FoldId,
    pub heading: usize,
    pub opening: usize,
    pub closing: usize,
    pub closed: bool,
    pub summary: String,
}

/// Format a collapsed implementation marker from distinct raw edit regions.
/// A signature-only change keeps the body marker without a zero-change label.
pub fn declaration_body_summary(changes: usize, closing: &str) -> String {
    if changes == 0 {
        format!(" … {closing}")
    } else {
        format!(
            " … {changes} change{} {closing}",
            if changes == 1 { "" } else { "s" }
        )
    }
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
    /// Attach a declaration fold without changing source text or consumer action targets.
    ///
    /// Invalid positions return an error before mutation. A one-block declaration or an
    /// already installed identity returns false. Endpoints include the complete closing block.
    pub fn declaration_fold(
        block: &mut [BufferBlock],
        layout: DeclarationFoldLayout,
    ) -> Result<bool, ContractError> {
        if layout.heading > layout.opening
            || layout.opening > layout.closing
            || layout.closing >= block.len()
        {
            return Err(ContractError("declaration fold positions are inconsistent"));
        }
        if layout.opening == layout.closing
            || block[layout.opening]
                .metadata
                .fold
                .iter()
                .any(|fold| fold.id == layout.id)
        {
            return Ok(false);
        }
        let end = BlockAnchor {
            block: block[layout.closing].id.clone(),
            position: TextPosition {
                row: block[layout.closing].text.row_count(),
                column: 0,
            },
        };
        let heading_start = Some(BlockAnchor {
            block: block[layout.heading].id.clone(),
            position: TextPosition { row: 0, column: 0 },
        });
        block[layout.opening].metadata.fold.push(FoldRange {
            id: layout.id,
            start: TextPosition { row: 0, column: 0 },
            end,
            heading_start,
            collapsed_suffix: Some(layout.summary),
            closed: layout.closed,
            collapse_children: false,
            expand_children: false,
            heading_marker: false,
        });
        Ok(true)
    }

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
        let columns = summary_columns(
            change,
            lines.map(|line| line.map(|line| line as usize)),
            line_width,
        );
        crate::projection::append_change_row(
            &mut block.metadata,
            0,
            change,
            columns,
            self.options.indent,
        )?;
        Ok(block)
    }

    /// Render exact signature fragments with a declaration marker and native syntax colors.
    ///
    /// Body-only revisions add no token emphasis. Missing syntax retains usable text.
    /// Foreign syntax returns a contract error before producing a block.
    pub fn declaration_signature(
        &self,
        id: BlockId,
        signature: &crate::source_view::DeclarationSignature,
        old: Option<&SyntaxHandle>,
        new: Option<&SyntaxHandle>,
    ) -> Result<BufferBlock, ContractError> {
        let syntax = match signature.origin.side {
            crate::source::SourceSide::Old => old,
            crate::source::SourceSide::New => new,
        };
        if syntax.is_some_and(|syntax| syntax.source_identity() != signature.source_identity) {
            return Err(ContractError(
                "declaration signature syntax belongs to another source",
            ));
        }
        let mut block = BufferBlock {
            id,
            text: BufferText::from_rows(signature.text.lines())?,
            metadata: Default::default(),
        };
        block.metadata.layout = self.options.layout.clone();
        let name_line = match signature.origin.side {
            crate::source::SourceSide::Old => signature.old_line,
            crate::source::SourceSide::New => signature.new_line,
        };
        for (row, text) in signature.text.lines().enumerate() {
            let source_line = signature.origin.line + row;
            let marker = name_line == Some(source_line + 1);
            let change = if marker {
                signature.change
            } else {
                RowChange::Context
            };
            let lines = if marker {
                [signature.old_line, signature.new_line]
            } else {
                [None, None]
            };
            crate::projection::append_change_row(
                &mut block.metadata,
                row,
                change,
                summary_columns(change, lines, 3),
                self.options.indent,
            )?;
            if let Some(syntax) = syntax {
                crate::projection::append_syntax_fragment(
                    &mut block.metadata,
                    syntax,
                    crate::syntax::SyntaxPoint {
                        row: source_line,
                        column: if row == 0 {
                            signature.origin.byte_column
                        } else {
                            0
                        },
                    },
                    row,
                    text,
                )?;
            }
        }
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
            crate::projection::RowSyntax {
                old: None,
                new: None,
            },
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
                crate::projection::RowSyntax { old, new },
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

fn summary_columns(
    change: RowChange,
    lines: [Option<usize>; 2],
    line_width: usize,
) -> Vec<TextChunk> {
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
    columns
}
