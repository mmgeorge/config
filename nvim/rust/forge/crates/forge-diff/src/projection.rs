use forge_buffer::ContractError;
use forge_buffer::block::{
    BlockAnchor, BlockMetadata, BufferBlock, Decoration, FoldRange, Gutter, TextChunk,
    TextPosition, TextRange,
};
use forge_buffer::identity::{BlockId, FoldId};
use forge_buffer::text::BufferText;

use crate::display::{DisplayHunk, DisplayRow, RowKind};
use crate::syntax::{SyntaxCapture, SyntaxFamily, SyntaxHandle, SyntaxLanguage};

/// Build a file or hunk heading with an independent display margin and unchanged text coordinates.
pub fn header(
    id: BlockId,
    chunk: Vec<TextChunk>,
    indent: usize,
) -> Result<BufferBlock, ContractError> {
    id.validate()?;
    if indent > 4096 {
        return Err(ContractError("diff indentation exceeds 4096 cells"));
    }
    let mut text = String::new();
    let mut metadata = BlockMetadata::default();
    for chunk in chunk {
        let column = text.len();
        text.push_str(&chunk.text);
        metadata.decoration.push(Decoration {
            range: TextRange {
                start: TextPosition { row: 0, column },
                end: TextPosition {
                    row: 0,
                    column: text.len(),
                },
            },
            capture: chunk.capture,
            priority: 100,
        });
    }
    if indent > 0 {
        metadata.gutter.push(Gutter {
            placement: Default::default(),
            position: TextPosition { row: 0, column: 0 },
            chunk: vec![TextChunk {
                text: " ".repeat(indent),
                capture: "Normal".into(),
            }],
            priority: 100,
        });
    }
    Ok(BufferBlock {
        id,
        text: BufferText::from_rows([text])?,
        metadata,
    })
}

/// Attach a file or hunk fold whose collapsed row is its header.
///
/// The endpoint is exclusive. Callers retain default expansion and child-collapse policy.
pub fn fold_header(
    header: &mut BufferBlock,
    id: FoldId,
    end: BlockAnchor,
    closed: bool,
    collapse_children: bool,
) {
    header.metadata.fold = vec![FoldRange {
        collapsed_suffix: None,
        id,
        heading_start: None,
        start: TextPosition { row: 0, column: 0 },
        end,
        closed,
        collapse_children,
        expand_children: false,
    }];
}

pub fn append_source_syntax_row(
    metadata: &mut BlockMetadata,
    syntax: &SyntaxHandle,
    source_row: usize,
    row: usize,
    text: &str,
) -> Result<(), ContractError> {
    append_syntax_row(metadata, syntax, source_row, row, text)?;
    let capture_list: Vec<_> = syntax
        .captures_intersecting_rows(source_row, source_row.saturating_add(1))
        .collect();
    let fenced_code = capture_list.iter().any(|capture| {
        capture.family == SyntaxFamily::Highlight
            && capture.language == SyntaxLanguage::Markdown
            && capture.name.as_ref() == "markup.raw.block"
    });
    let opening_fence = fence_info(text).filter(|_| {
        capture_list.iter().any(|capture| {
            capture.family == SyntaxFamily::Highlight
                && capture.language == SyntaxLanguage::Markdown
                && capture.name.as_ref() == "markup.raw.block"
                && capture.range.start.row == source_row
                && capture.range.end.row > source_row
        })
    });
    if fenced_code {
        metadata.source_highlight.push(Decoration {
            range: TextRange {
                start: TextPosition { row, column: 0 },
                end: TextPosition {
                    row: row + 1,
                    column: 0,
                },
            },
            capture: "RenderMarkdownCode".to_owned(),
            priority: 4096,
        });
    }
    if let Some(info) = opening_fence {
        metadata.conceal.push(forge_buffer::block::Conceal {
            range: TextRange {
                start: TextPosition { row, column: 0 },
                end: TextPosition {
                    row,
                    column: text.len(),
                },
            },
            replacement: String::new(),
            line: false,
            priority: 4097,
        });
        metadata
            .source_overlay
            .push(forge_buffer::block::SourceOverlay {
                range: TextRange {
                    start: TextPosition { row, column: 0 },
                    end: TextPosition {
                        row,
                        column: text.len(),
                    },
                },
                text: if info.is_empty() { "" } else { info }.to_owned(),
                capture: info
                    .split_whitespace()
                    .next()
                    .filter(|language| !language.is_empty())
                    .map(|language| format!("RenderMarkdownCodeInfo:{language}"))
                    .unwrap_or_else(|| "RenderMarkdownCodeInfo".to_owned()),
                priority: 4097,
            });
    }
    for capture in capture_list {
        if capture.family != SyntaxFamily::Highlight {
            continue;
        }
        let Some(range) = syntax_row_range(capture, source_row, row, text, 0) else {
            continue;
        };
        if capture.language == SyntaxLanguage::MarkdownInline
            && capture.name.as_ref() == "markup.raw"
        {
            metadata.source_highlight.push(Decoration {
                range: range.clone(),
                capture: "RenderMarkdownCodeInline".to_owned(),
                priority: 4096,
            });
        }
        if capture.family == SyntaxFamily::Highlight
            && capture.language == crate::syntax::SyntaxLanguage::Markdown
            && capture.name.as_ref() == "markup.list"
            && capture.range.start.row == source_row
        {
            let column = capture.range.start.column;
            if matches!(text.as_bytes().get(column), Some(b'-' | b'+' | b'*')) {
                metadata
                    .source_overlay
                    .push(forge_buffer::block::SourceOverlay {
                        range: TextRange {
                            start: TextPosition { row, column },
                            end: TextPosition {
                                row,
                                column: column + 1,
                            },
                        },
                        text: "●".to_owned(),
                        capture: "RenderMarkdownBullet".to_owned(),
                        priority: 120,
                    });
            }
        }
        if capture.family == SyntaxFamily::Highlight
            && capture.language == crate::syntax::SyntaxLanguage::Markdown
            && capture.name.starts_with("markup.heading")
            && capture.range.start.row == source_row
        {
            let marker = text.bytes().take_while(|byte| *byte == b'#').count();
            if (1..=6).contains(&marker) && text.as_bytes().get(marker) == Some(&b' ') {
                let background = format!("RenderMarkdownH{marker}Bg");
                metadata.source_highlight.push(Decoration {
                    range: range.clone(),
                    capture: background.clone(),
                    priority: 4096,
                });
                let end = TextPosition {
                    row,
                    column: text.len(),
                };
                metadata
                    .source_overlay
                    .push(forge_buffer::block::SourceOverlay {
                        range: TextRange { start: end, end },
                        text: " ".to_owned(),
                        capture: background,
                        priority: 4096,
                    });
                metadata.conceal.push(forge_buffer::block::Conceal {
                    range: TextRange {
                        start: TextPosition { row, column: 0 },
                        end: TextPosition {
                            row,
                            column: marker,
                        },
                    },
                    replacement: String::new(),
                    line: false,
                    priority: capture.priority.max(100),
                });
            }
        }
        if opening_fence.is_some() && capture.conceal_line {
            continue;
        }
        if capture.family != SyntaxFamily::Highlight
            || (capture.conceal.is_none() && !capture.conceal_line)
        {
            continue;
        }
        metadata.conceal.push(forge_buffer::block::Conceal {
            range,
            replacement: capture.conceal.as_deref().unwrap_or("").to_owned(),
            line: capture.conceal_line,
            priority: capture.priority.max(100),
        });
    }
    Ok(())
}

fn fence_info(text: &str) -> Option<&str> {
    let content = text
        .strip_prefix("   ")
        .or_else(|| text.strip_prefix("  "))
        .or_else(|| text.strip_prefix(' '))
        .unwrap_or(text);
    let marker = *content.as_bytes().first()?;
    if !matches!(marker, b'`' | b'~') {
        return None;
    }
    let length = content.bytes().take_while(|byte| *byte == marker).count();
    (length >= 3).then(|| content[length..].trim())
}

fn syntax_row_range(
    capture: &SyntaxCapture,
    source_row: usize,
    row: usize,
    text: &str,
    source_column: usize,
) -> Option<TextRange> {
    let start = if capture.range.start.row == source_row {
        capture
            .range
            .start
            .column
            .saturating_sub(source_column)
            .min(text.len())
    } else {
        0
    };
    let end = if capture.range.end.row == source_row {
        capture
            .range
            .end
            .column
            .saturating_sub(source_column)
            .min(text.len())
    } else {
        text.len()
    };
    (start < end && text.is_char_boundary(start) && text.is_char_boundary(end)).then_some(
        TextRange {
            start: TextPosition { row, column: start },
            end: TextPosition { row, column: end },
        },
    )
}

/// Applies shared change gutters and full-row backgrounds to source or semantic rows.
pub(crate) fn append_change_row(
    metadata: &mut BlockMetadata,
    row: usize,
    change: crate::presentation::RowChange,
    mut columns: Vec<TextChunk>,
    indent: usize,
) -> Result<(), ContractError> {
    if indent > 4096 {
        return Err(ContractError("diff indentation exceeds 4096 cells"));
    }
    let (sign, background, number, _) = change.style();
    if indent > 0 {
        columns.insert(
            0,
            TextChunk {
                text: " ".repeat(indent),
                capture: "Normal".into(),
            },
        );
    }
    columns.extend([
        TextChunk {
            text: sign.into(),
            capture: number.into(),
        },
        TextChunk {
            text: " ".into(),
            capture: background.into(),
        },
    ]);
    metadata.gutter.push(Gutter {
        placement: Default::default(),
        position: TextPosition { row, column: 0 },
        chunk: columns,
        priority: 100,
    });
    if change != crate::presentation::RowChange::Context {
        metadata.decoration.push(Decoration {
            range: TextRange {
                start: TextPosition { row, column: 0 },
                end: TextPosition {
                    row: row + 1,
                    column: 0,
                },
            },
            capture: background.into(),
            priority: 90,
        });
    }
    Ok(())
}

/// Projects source coordinates, change styling, syntax, and intraline emphasis.
/// Syntax trees for both sides of a projected diff row.
pub struct RowSyntax<'syntax> {
    pub old: Option<&'syntax SyntaxHandle>,
    pub new: Option<&'syntax SyntaxHandle>,
}

pub fn append_display_row(
    metadata: &mut BlockMetadata,
    row: usize,
    display: &DisplayRow,
    group: &DisplayHunk,
    syntax: RowSyntax<'_>,
    change: Option<crate::presentation::RowChange>,
    indent: usize,
) -> Result<(), ContractError> {
    let change = change.unwrap_or_else(|| match display.kind() {
        RowKind::Added => crate::presentation::RowChange::Added,
        RowKind::Removed => crate::presentation::RowChange::Removed,
        RowKind::Context => crate::presentation::RowChange::Context,
    });
    let (_, background, number, emphasis) = change.style();
    let old = display
        .old_coordinate()
        .map_or(String::new(), |coordinate| {
            (coordinate.line + 1).to_string()
        });
    let new = display
        .new_coordinate()
        .map_or(String::new(), |coordinate| {
            (coordinate.line + 1).to_string()
        });
    let old_width = group.old_lines.end.to_string().len().max(3);
    let new_width = group.new_lines.end.to_string().len().max(3);
    append_change_row(
        metadata,
        row,
        change,
        vec![
            TextChunk {
                text: format!("{old:>old_width$}"),
                capture: if display.old_coordinate().is_some() {
                    number
                } else {
                    background
                }
                .into(),
            },
            TextChunk {
                text: "  ".into(),
                capture: background.into(),
            },
            TextChunk {
                text: format!("{new:>new_width$}"),
                capture: if display.new_coordinate().is_some() {
                    number
                } else {
                    background
                }
                .into(),
            },
            TextChunk {
                text: "  ".into(),
                capture: background.into(),
            },
        ],
        indent,
    )?;
    if display.is_gap() {
        metadata.visible_decoration.push(Decoration {
            range: TextRange {
                start: TextPosition { row, column: 0 },
                end: TextPosition {
                    row,
                    column: display.text.len(),
                },
            },
            capture: "ForgeHunkBoundary".into(),
            priority: 100,
        });
    }
    let (syntax, coordinate) = if display.kind() == RowKind::Removed {
        (syntax.old, display.old_coordinate())
    } else {
        (syntax.new, display.new_coordinate())
    };
    if let (Some(syntax), Some(coordinate)) = (syntax, coordinate) {
        append_syntax_row(metadata, syntax, coordinate.line, row, &display.text)?;
    }
    for range in &display.emphasis {
        if range.start >= range.end
            || range.end > display.text.len()
            || !display.text.is_char_boundary(range.start)
            || !display.text.is_char_boundary(range.end)
        {
            return Err(ContractError(
                "diff emphasis is outside source byte boundaries",
            ));
        }
        metadata.visible_decoration.push(Decoration {
            range: TextRange {
                start: TextPosition {
                    row,
                    column: range.start,
                },
                end: TextPosition {
                    row,
                    column: range.end,
                },
            },
            capture: emphasis.into(),
            priority: 200,
        });
    }
    Ok(())
}

pub fn append_syntax_row(
    metadata: &mut BlockMetadata,
    syntax: &SyntaxHandle,
    source_row: usize,
    row: usize,
    text: &str,
) -> Result<(), ContractError> {
    append_syntax_fragment(
        metadata,
        syntax,
        crate::syntax::SyntaxPoint {
            row: source_row,
            column: 0,
        },
        row,
        text,
    )
}

/// Project a clipped signature fragment while retaining original syntax byte coordinates.
pub fn append_syntax_fragment(
    metadata: &mut BlockMetadata,
    syntax: &SyntaxHandle,
    source: crate::syntax::SyntaxPoint,
    row: usize,
    text: &str,
) -> Result<(), ContractError> {
    let mut captures: Vec<_> = syntax
        .captures_intersecting_rows(source.row, source.row.saturating_add(1))
        .filter(|capture| capture.family == SyntaxFamily::Highlight)
        .collect();
    captures.sort_by_key(|capture| (capture.tree, capture.pattern));
    for capture in captures {
        let Some(range) = syntax_row_range(capture, source.row, row, text, source.column) else {
            continue;
        };
        metadata.visible_decoration.push(Decoration {
            range,
            capture: format!("@{}.{}", capture.name, capture.language.name()),
            priority: capture.priority.max(100),
        });
    }
    Ok(())
}
