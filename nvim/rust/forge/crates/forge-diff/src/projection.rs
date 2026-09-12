use forge_buffer::ContractError;
use forge_buffer::block::{BlockMetadata, Decoration, Gutter, TextChunk, TextPosition, TextRange};

use crate::display::{DisplayHunk, DisplayRow, RowKind};
use crate::syntax::{SyntaxCapture, SyntaxFamily, SyntaxHandle, SyntaxLanguage};

const MAX_DECORATIONS: usize = 8192;

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
        if metadata.source_highlight.len() >= MAX_DECORATIONS {
            return Err(ContractError("source row exceeds 8192 highlights"));
        }
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
        if metadata.conceal.len() >= MAX_DECORATIONS
            || metadata.source_overlay.len() >= MAX_DECORATIONS
        {
            return Err(ContractError(
                "source row exceeds 8192 code fence decorations",
            ));
        }
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
        let Some(range) = syntax_row_range(capture, source_row, row, text) else {
            continue;
        };
        if capture.language == SyntaxLanguage::MarkdownInline
            && capture.name.as_ref() == "markup.raw"
        {
            if metadata.source_highlight.len() >= MAX_DECORATIONS {
                return Err(ContractError("source row exceeds 8192 highlights"));
            }
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
                if metadata.source_overlay.len() >= MAX_DECORATIONS {
                    return Err(ContractError("source row exceeds 8192 overlays"));
                }
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
                if metadata.conceal.len() >= MAX_DECORATIONS
                    || metadata.source_highlight.len() >= MAX_DECORATIONS
                    || metadata.source_overlay.len() >= MAX_DECORATIONS
                {
                    return Err(ContractError("source row exceeds 8192 heading decorations"));
                }
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
        if metadata.conceal.len() >= MAX_DECORATIONS {
            return Err(ContractError("source row exceeds 8192 conceal ranges"));
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
) -> Option<TextRange> {
    let start = if capture.range.start.row == source_row {
        capture.range.start.column.min(text.len())
    } else {
        0
    };
    let end = if capture.range.end.row == source_row {
        capture.range.end.column.min(text.len())
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

pub fn append_display_row(
    metadata: &mut BlockMetadata,
    row: usize,
    display: &DisplayRow,
    group: &DisplayHunk,
    old_syntax: Option<&SyntaxHandle>,
    new_syntax: Option<&SyntaxHandle>,
) -> Result<(), ContractError> {
    if metadata.gutter.len() >= 256 {
        return Err(ContractError("diff row batch exceeds 256 gutters"));
    }
    let sign = match display.kind {
        RowKind::Added => "+",
        RowKind::Removed => "-",
        RowKind::Context => " ",
    };
    let old = display.old.map_or(String::new(), |coordinate| {
        (coordinate.line + 1).to_string()
    });
    let new = display.new.map_or(String::new(), |coordinate| {
        (coordinate.line + 1).to_string()
    });
    let old_width = group.old_lines.end.to_string().len().max(3);
    let new_width = group.new_lines.end.to_string().len().max(3);
    let (background, number, emphasis) = match display.kind {
        RowKind::Added => ("ForgeAddBg", "ForgeAddLineNr", "ForgeInlineAddBg"),
        RowKind::Removed => ("ForgeDeleteBg", "ForgeDeleteLineNr", "ForgeInlineDeleteBg"),
        RowKind::Context => ("ForgeContextBg", "ForgeContextLineNr", "ForgeContextBg"),
    };
    metadata.gutter.push(Gutter {
        position: TextPosition { row, column: 0 },
        chunk: vec![
            TextChunk {
                text: format!("{old:>old_width$}"),
                capture: if display.old.is_some() {
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
                capture: if display.new.is_some() {
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
                text: sign.into(),
                capture: number.into(),
            },
            TextChunk {
                text: " ".into(),
                capture: background.into(),
            },
        ],
        priority: 100,
    });
    if display.kind != RowKind::Context {
        admit_decoration(metadata)?;
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
    let (syntax, coordinate) = if display.kind == RowKind::Removed {
        (old_syntax, display.old)
    } else {
        (new_syntax, display.new)
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
        admit_decoration(metadata)?;
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
    let mut captures: Vec<_> = syntax
        .captures_intersecting_rows(source_row, source_row.saturating_add(1))
        .filter(|capture| capture.family == SyntaxFamily::Highlight)
        .take(MAX_DECORATIONS + 1)
        .collect();
    if captures.len() > MAX_DECORATIONS {
        return Err(ContractError("diff row batch exceeds 8192 decorations"));
    }
    captures.sort_by_key(|capture| (capture.tree, capture.pattern));
    for capture in captures {
        let Some(range) = syntax_row_range(capture, source_row, row, text) else {
            continue;
        };
        admit_decoration(metadata)?;
        metadata.visible_decoration.push(Decoration {
            range,
            capture: format!("@{}.{}", capture.name, capture.language.name()),
            priority: capture.priority.max(100),
        });
    }
    Ok(())
}

fn admit_decoration(metadata: &BlockMetadata) -> Result<(), ContractError> {
    if metadata.decoration.len() + metadata.visible_decoration.len() >= MAX_DECORATIONS {
        return Err(ContractError("diff row batch exceeds 8192 decorations"));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::{SourceCoordinate, SourceSide};

    #[tokio::test]
    async fn source_markdown_projects_concealment_and_injected_syntax_without_changing_rows() {
        let diff = crate::engine::DiffEngine::new(crate::cache::CacheLimits::default(), 1);
        let engine = crate::syntax::SyntaxEngine::new(
            diff.analysis_pool(),
            crate::syntax::SyntaxLimits::default(),
        );
        let text = "# Heading\n\n**日本語** and `value`\n\n```rust\nfn example() {}\n```\n\n- Bullet\n+ Second\n1. Ordered\n\n```text\n- Literal\n```\n";
        let syntax = engine
            .analyze(crate::syntax::SyntaxRequest {
                source: crate::source::SourceVersion::new(
                    text.as_bytes().to_vec(),
                    crate::source::Representation::Raw,
                )
                .unwrap(),
                language: crate::syntax::SyntaxLanguage::Markdown,
                priority: crate::workers::WorkPriority::Visible,
                deadline: None,
            })
            .await
            .unwrap();
        let mut conceal_count = 0;
        let mut injected = false;
        let mut overlay_count = 0;
        let mut inline_code = false;
        for (source_row, line) in text.split('\n').enumerate() {
            let mut metadata = BlockMetadata::default();
            append_source_syntax_row(&mut metadata, &syntax, source_row, 0, line).unwrap();
            if source_row == 0 {
                assert!(
                    metadata
                        .source_highlight
                        .iter()
                        .any(|span| span.capture == "RenderMarkdownH1Bg")
                );
                assert!(
                    metadata
                        .source_overlay
                        .iter()
                        .any(|span| span.range.start.column == line.len() && span.text == " ")
                );
                assert!(
                    metadata
                        .conceal
                        .iter()
                        .any(|conceal| conceal.range.end.column == 1)
                );
            }
            if source_row == 4 {
                assert!(metadata.source_highlight.iter().any(|span| {
                    span.capture == "RenderMarkdownCode"
                        && span.range.end.row == 1
                        && span.range.end.column == 0
                }));
                assert!(metadata.source_overlay.iter().any(|span| {
                    span.capture == "RenderMarkdownCodeInfo:rust" && span.text == "rust"
                }));
                assert!(!metadata.conceal.iter().any(|conceal| conceal.line));
            }
            if source_row == 5 {
                assert!(
                    metadata
                        .source_highlight
                        .iter()
                        .any(|span| span.capture == "RenderMarkdownCode")
                );
            }
            if source_row == 6 {
                assert!(metadata.conceal.iter().any(|conceal| conceal.line));
            }
            conceal_count += metadata.conceal.len();
            overlay_count += metadata
                .source_overlay
                .iter()
                .filter(|span| span.text == "●")
                .count();
            inline_code |= metadata
                .source_highlight
                .iter()
                .any(|span| span.capture == "RenderMarkdownCodeInline");
            injected |= metadata
                .visible_decoration
                .iter()
                .any(|capture| capture.capture.ends_with(".rust"));
            let block = forge_buffer::block::BufferBlock {
                id: forge_buffer::identity::BlockId(format!("row:{source_row}")),
                text: forge_buffer::text::BufferText::from_rows([line]).unwrap(),
                metadata,
            };
            block.validate().unwrap();
            assert_eq!(block.text.row(0), Some(line));
        }
        assert!(
            conceal_count >= 2,
            "Markdown delimiters have no conceal metadata"
        );
        assert!(injected, "fenced Rust syntax was not projected");
        assert!(inline_code, "inline code has no source background");
        assert_eq!(
            overlay_count, 2,
            "source bullets must exclude ordered lists and fenced text"
        );
    }

    #[test]
    fn projection_keeps_gutters_out_of_source_bytes_and_checks_emphasis() {
        let row = DisplayRow {
            group_index: 0,
            text: "λnew".into(),
            kind: RowKind::Added,
            old: None,
            new: Some(SourceCoordinate {
                side: SourceSide::New,
                line: 6,
                byte_column: 0,
            }),
            raw_id: None,
            emphasis: vec![0..2],
            emphasis_fallback: None,
        };
        let mut metadata = BlockMetadata::default();
        let group = DisplayHunk {
            raw_ids: Vec::new(),
            old_lines: 0..0,
            new_lines: 0..7,
            raw_range: 0..1,
        };
        append_display_row(&mut metadata, 0, &row, &group, None, None).unwrap();
        assert_eq!(row.text, "λnew");
        assert_eq!(
            metadata.gutter[0]
                .chunk
                .iter()
                .map(|chunk| chunk.text.as_str())
                .collect::<String>(),
            "       7  + "
        );
        assert_eq!(metadata.gutter[0].chunk[2].capture, "ForgeAddLineNr");
        assert_eq!(metadata.decoration[0].capture, "ForgeAddBg");
        assert_eq!(metadata.visible_decoration[0].capture, "ForgeInlineAddBg");
        assert_eq!(metadata.visible_decoration[0].range.end.column, 2);
        let mut invalid = row.clone();
        invalid.emphasis = vec![1..2];
        assert!(
            append_display_row(
                &mut BlockMetadata::default(),
                0,
                &invalid,
                &group,
                None,
                None
            )
            .is_err()
        );
    }

    #[test]
    fn projection_rejects_over_budget_before_adding_a_decoration() {
        let range = TextRange {
            start: TextPosition { row: 0, column: 0 },
            end: TextPosition { row: 1, column: 0 },
        };
        let mut metadata = BlockMetadata {
            decoration: vec![
                Decoration {
                    range,
                    capture: "DiffAdd".into(),
                    priority: 90
                };
                MAX_DECORATIONS
            ],
            ..BlockMetadata::default()
        };
        let row = DisplayRow {
            group_index: 0,
            text: "new".into(),
            kind: RowKind::Added,
            old: None,
            new: None,
            raw_id: None,
            emphasis: Vec::new(),
            emphasis_fallback: None,
        };
        let group = DisplayHunk {
            raw_ids: Vec::new(),
            old_lines: 0..0,
            new_lines: 0..1,
            raw_range: 0..1,
        };
        assert!(append_display_row(&mut metadata, 0, &row, &group, None, None).is_err());
        assert_eq!(metadata.decoration.len(), MAX_DECORATIONS);
    }

    #[test]
    fn projection_preserves_side_colors_and_group_gutter_alignment() {
        let group = DisplayHunk {
            raw_ids: Vec::new(),
            old_lines: 998..1001,
            new_lines: 0..7,
            raw_range: 0..1,
        };
        for (kind, background, number) in [
            (RowKind::Removed, "ForgeDeleteBg", "ForgeDeleteLineNr"),
            (RowKind::Added, "ForgeAddBg", "ForgeAddLineNr"),
            (RowKind::Context, "ForgeContextBg", "ForgeContextLineNr"),
        ] {
            let row = DisplayRow {
                group_index: 0,
                text: "text".into(),
                kind,
                old: (kind != RowKind::Added).then_some(SourceCoordinate {
                    side: SourceSide::Old,
                    line: 998,
                    byte_column: 0,
                }),
                new: (kind != RowKind::Removed).then_some(SourceCoordinate {
                    side: SourceSide::New,
                    line: 6,
                    byte_column: 0,
                }),
                raw_id: None,
                emphasis: Vec::new(),
                emphasis_fallback: None,
            };
            let mut metadata = BlockMetadata::default();
            append_display_row(&mut metadata, 0, &row, &group, None, None).unwrap();
            assert_eq!(metadata.gutter[0].chunk[0].text.len(), 4);
            assert_eq!(metadata.gutter[0].chunk[2].text.len(), 3);
            assert_eq!(metadata.gutter[0].chunk[4].capture, number);
            if kind == RowKind::Context {
                assert!(metadata.decoration.is_empty());
            } else {
                assert_eq!(metadata.decoration[0].capture, background);
            }
        }
    }
}
