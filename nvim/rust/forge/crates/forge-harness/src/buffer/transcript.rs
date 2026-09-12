use anyhow::{Result, ensure};
use forge_buffer::block::{
    BlockMetadata, BufferBlock, Decoration, Gutter, TargetRange, TextChunk, TextPosition, TextRange,
};
use forge_buffer::identity::{BlockId, TargetId};
use forge_buffer::markdown::{MarkdownRenderer, RenderedMarkdown};
use forge_buffer::text::BufferText;
use forge_buffer::width::WidthProfile;

use super::tool::ToolOutputPreview;

pub struct TranscriptRenderer<'profile> {
    profile: &'profile WidthProfile,
}

impl<'profile> TranscriptRenderer<'profile> {
    pub fn new(profile: &'profile WidthProfile) -> Result<Self> {
        profile.validate()?;
        Ok(Self { profile })
    }

    pub fn prompt(&self, id: BlockId, source: &str) -> Result<BufferBlock> {
        ensure!(
            source.len() <= 1024 * 1024,
            "prompt exceeds transcript capacity"
        );
        let mut block = self.literal(id, &format!("▸ {source}"), 2)?;
        block.metadata.decoration.push(Decoration {
            range: TextRange {
                start: TextPosition { row: 0, column: 0 },
                end: TextPosition {
                    row: block.text.row_count(),
                    column: 0,
                },
            },
            capture: "ForgeHarnessPrompt".into(),
            priority: 100,
        });
        Ok(block)
    }

    pub fn response(&self, id: BlockId, source: &str) -> Result<RenderedMarkdown> {
        let mut profile = self.profile.clone();
        let margin = 2.min(profile.columns - 1);
        profile.columns -= margin;
        let mut rendered = MarkdownRenderer::render(id, source, &profile)?;
        for row in 0..rendered.block.text.row_count() {
            rendered.block.metadata.gutter.push(Gutter {
                position: TextPosition { row, column: 0 },
                chunk: vec![TextChunk {
                    text: if row == 0 && margin == 2 {
                        "▸ ".into()
                    } else {
                        " ".repeat(margin)
                    },
                    capture: "ForgeHarnessResponse".into(),
                }],
                priority: 100,
            });
        }
        Ok(rendered)
    }

    pub fn commentary(&self, id: BlockId, source: &str) -> Result<BufferBlock> {
        let mut block = self.literal(id, &format!("↳ {source}"), 2)?;
        block.metadata.decoration.push(Decoration {
            range: TextRange {
                start: TextPosition { row: 0, column: 0 },
                end: TextPosition {
                    row: block.text.row_count(),
                    column: 0,
                },
            },
            capture: "ForgeHarnessCommentary".into(),
            priority: 100,
        });
        Ok(block)
    }

    pub fn literal(&self, id: BlockId, source: &str, indent: usize) -> Result<BufferBlock> {
        id.validate()?;
        Ok(BufferBlock {
            id,
            text: BufferText::from_rows(
                self.profile
                    .wrap_plain(source, indent.min(self.profile.columns - 1))?,
            )?,
            metadata: BlockMetadata::default(),
        })
    }

    pub fn tool_preview(
        &self,
        id: BlockId,
        target: TargetId,
        kind: &str,
        status: &str,
        failed: bool,
        title: &str,
        output: &ToolOutputPreview<'_>,
    ) -> Result<BufferBlock> {
        id.validate()?;
        target.validate()?;
        ensure!(
            title.len() <= 4096,
            "tool title exceeds transcript capacity"
        );
        let heading = tool_heading(kind, status, title);
        let indent = 4.min(self.profile.columns - 1);
        let mut row = self.profile.wrap_plain(&heading, indent)?;
        let title_rows = row.len();
        let output_indent = 6.min(self.profile.columns - 1);
        if let Some(first) = output.first {
            row.extend(
                self.profile
                    .wrap_plain(&format!("    └ {first}"), output_indent)?,
            );
        } else {
            row.extend(self.profile.wrap_plain("    └ no output", output_indent)?);
        }
        if output.hidden_rows > 0 {
            row.extend(self.profile.wrap_plain(
                &format!("      … {} hidden rows", output.hidden_rows),
                output_indent,
            )?);
        }
        if let Some(last) = output.last {
            row.extend(
                self.profile
                    .wrap_plain(&format!("      {last}"), output_indent)?,
            );
        }
        let text = BufferText::from_rows(row)?;
        let range = TextRange {
            start: TextPosition { row: 0, column: 0 },
            end: TextPosition {
                row: title_rows,
                column: 0,
            },
        };
        range.validate(&text)?;
        let mut block = BufferBlock {
            id,
            text,
            metadata: BlockMetadata {
                target: vec![TargetRange { id: target, range }],
                ..BlockMetadata::default()
            },
        };
        decorate_tool_heading(&mut block, title_rows, kind, failed);
        if block.text.row_count() > title_rows {
            block.metadata.decoration.push(Decoration {
                range: TextRange {
                    start: TextPosition {
                        row: title_rows,
                        column: 0,
                    },
                    end: TextPosition {
                        row: block.text.row_count(),
                        column: 0,
                    },
                },
                capture: "ForgeHarnessOutput".into(),
                priority: 100,
            });
        }
        Ok(block)
    }

    pub fn active_tool_preview(
        &self,
        id: BlockId,
        kind: &str,
        status: &str,
        failed: bool,
        title: &str,
        output: &str,
    ) -> Result<BufferBlock> {
        id.validate()?;
        ensure!(
            title.len() <= 4096,
            "tool title exceeds transcript capacity"
        );
        let heading = tool_heading(kind, status, title);
        let indent = 4.min(self.profile.columns - 1);
        let mut row = self.profile.wrap_plain(&heading, indent)?;
        let title_rows = row.len();
        let normalized = strip_ansi_escapes::strip_str(output)
            .replace("\r\n", "\n")
            .replace('\r', "");
        let output_indent = 6.min(self.profile.columns - 1);
        let mut preview_rows = 0;
        for output_row in normalized.lines() {
            let prefix = if preview_rows == 0 {
                "    └ "
            } else {
                "      "
            };
            for wrapped in self
                .profile
                .wrap_plain(&format!("{prefix}{output_row}"), output_indent)?
            {
                row.push(wrapped);
                preview_rows += 1;
                if preview_rows == 4 {
                    break;
                }
            }
            if preview_rows == 4 {
                break;
            }
        }
        let text = BufferText::from_rows(row)?;
        let mut block = BufferBlock {
            id,
            text,
            metadata: BlockMetadata::default(),
        };
        decorate_tool_heading(&mut block, title_rows, kind, failed);
        if preview_rows > 0 {
            block.metadata.decoration.push(Decoration {
                range: TextRange {
                    start: TextPosition {
                        row: title_rows,
                        column: 0,
                    },
                    end: TextPosition {
                        row: block.text.row_count(),
                        column: 0,
                    },
                },
                capture: "ForgeHarnessOutput".into(),
                priority: 100,
            });
        }
        Ok(block)
    }
}

fn tool_heading(kind: &str, status: &str, title: &str) -> String {
    let verb = match kind {
        "command" => "Ran",
        "file_change" => "Edited",
        _ if matches!(
            status.to_ascii_lowercase().as_str(),
            "inprogress" | "in_progress"
        ) =>
        {
            "Calling"
        }
        _ => "Called",
    };
    format!("  • {verb} {title}")
}

fn decorate_tool_heading(block: &mut BufferBlock, title_rows: usize, kind: &str, failed: bool) {
    if let Some(column) = block.text.row(0).and_then(|text| text.find('•')) {
        block.metadata.decoration.push(Decoration {
            range: TextRange {
                start: TextPosition { row: 0, column },
                end: TextPosition {
                    row: 0,
                    column: column + '•'.len_utf8(),
                },
            },
            capture: if failed {
                "ForgeHarnessToolFailure".into()
            } else {
                "ForgeHarnessToolSuccess".into()
            },
            priority: 100,
        });
    }
    match kind {
        "command" => decorate_command(block, title_rows),
        "tool_call" => decorate_tool_call(block, title_rows),
        _ => {}
    }
}

fn decorate_command(block: &mut BufferBlock, title_rows: usize) {
    let mut expects_command = true;
    for row in 0..title_rows {
        let Some(text) = block.text.row(row).map(str::to_owned) else {
            continue;
        };
        let content_start = if row == 0 {
            text.find(" Ran ").map_or(text.len(), |column| column + 5)
        } else {
            text.len() - text.trim_start().len()
        };
        for (start, token) in whitespace_tokens(&text[content_start..]) {
            let capture = if expects_command {
                "ForgeHarnessCommand"
            } else if token.starts_with('-') {
                "ForgeHarnessOption"
            } else {
                "ForgeHarnessArgument"
            };
            let start = content_start + start;
            block.metadata.decoration.push(Decoration {
                range: TextRange {
                    start: TextPosition { row, column: start },
                    end: TextPosition {
                        row,
                        column: start + token.len(),
                    },
                },
                capture: capture.into(),
                priority: 110,
            });
            expects_command = matches!(token, "|" | ";" | "&&");
        }
    }
}

fn decorate_tool_call(block: &mut BufferBlock, title_rows: usize) {
    let mut in_arguments = false;
    for row in 0..title_rows {
        let Some(text) = block.text.row(row).map(str::to_owned) else {
            continue;
        };
        let content_start = if row == 0 {
            text.find(" Calling ")
                .map(|column| column + 9)
                .or_else(|| text.find(" Called ").map(|column| column + 8))
                .unwrap_or(text.len())
        } else {
            text.len() - text.trim_start().len()
        };
        let content = &text[content_start..];
        let open = content.find('(');
        let close = content.rfind(')');
        if !in_arguments {
            let name_end = open.unwrap_or(content.len());
            if name_end > 0 {
                append_decoration(
                    block,
                    row,
                    content_start,
                    content_start + name_end,
                    "ForgeHarnessMcpName",
                );
            }
            if let Some(open) = open {
                in_arguments = true;
                let argument_start = open + 1;
                let argument_end = close.unwrap_or(content.len());
                if argument_end > argument_start {
                    append_decoration(
                        block,
                        row,
                        content_start + argument_start,
                        content_start + argument_end,
                        "ForgeHarnessMcpArguments",
                    );
                }
            }
        } else {
            let argument_end = close.unwrap_or(content.len());
            if argument_end > 0 {
                append_decoration(
                    block,
                    row,
                    content_start,
                    content_start + argument_end,
                    "ForgeHarnessMcpArguments",
                );
            }
        }
        if close.is_some() {
            in_arguments = false;
        }
    }
}

fn whitespace_tokens(text: &str) -> Vec<(usize, &str)> {
    let mut token = Vec::new();
    let mut start = None;
    for (column, character) in text.char_indices() {
        if character.is_whitespace() {
            if let Some(start) = start.take() {
                token.push((start, &text[start..column]));
            }
        } else if start.is_none() {
            start = Some(column);
        }
    }
    if let Some(start) = start {
        token.push((start, &text[start..]));
    }
    token
}

fn append_decoration(block: &mut BufferBlock, row: usize, start: usize, end: usize, capture: &str) {
    block.metadata.decoration.push(Decoration {
        range: TextRange {
            start: TextPosition { row, column: start },
            end: TextPosition { row, column: end },
        },
        capture: capture.into(),
        priority: 110,
    });
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn prompt_preserves_literal_markdown_and_real_marker() -> Result<()> {
        let profile = WidthProfile::default();
        let renderer = TranscriptRenderer::new(&profile)?;
        let block = renderer.prompt(BlockId("prompt:1".into()), "**literal** [name](url)")?;
        assert_eq!(block.text.row(0), Some("▸ **literal** [name](url)"));
        assert!(block.metadata.editable_region.is_empty());
        assert!(block.metadata.target.is_empty());
        assert_eq!(block.metadata.decoration[0].capture, "ForgeHarnessPrompt");
        assert_eq!(block.metadata.decoration[0].range.end.row, 1);
        Ok(())
    }

    #[test]
    fn completed_tool_preview_retains_folded_output_and_command_captures() -> Result<()> {
        let profile = WidthProfile::default();
        let renderer = TranscriptRenderer::new(&profile)?;
        let block = renderer.tool_preview(
            BlockId("tool:1".into()),
            TargetId("expand:1".into()),
            "command",
            "completed",
            false,
            "cargo test --lib parser",
            &ToolOutputPreview {
                first: Some("first"),
                last: Some("last"),
                hidden_rows: 98,
                total_rows: 100,
            },
        )?;
        assert_eq!(
            block.text.wire_rows(),
            vec![
                "  • Ran cargo test --lib parser",
                "    └ first",
                "      … 98 hidden rows",
                "      last"
            ]
        );
        assert_eq!(block.metadata.target[0].range.end.row, 1);
        assert!(block.metadata.decoration.iter().any(|decoration| {
            decoration.capture == "ForgeHarnessCommand"
                && decoration.range.start.column == "  • Ran ".len()
        }));
        assert!(
            block
                .metadata
                .decoration
                .iter()
                .any(|decoration| decoration.capture == "ForgeHarnessOption")
        );
        Ok(())
    }

    #[test]
    fn completed_tool_without_output_keeps_muted_fold_body() -> Result<()> {
        let profile = WidthProfile::default();
        let renderer = TranscriptRenderer::new(&profile)?;
        let block = renderer.tool_preview(
            BlockId("tool:empty".into()),
            TargetId("expand:empty".into()),
            "command",
            "completed",
            false,
            "cargo check",
            &ToolOutputPreview {
                first: None,
                last: None,
                hidden_rows: 0,
                total_rows: 0,
            },
        )?;

        assert_eq!(
            block.text.wire_rows(),
            vec!["  • Ran cargo check", "    └ no output"]
        );
        assert!(block.metadata.decoration.iter().any(|decoration| {
            decoration.capture == "ForgeHarnessOutput" && decoration.range.start.row == 1
        }));
        Ok(())
    }

    #[test]
    fn active_tool_preview_keeps_four_output_rows_and_call_syntax() -> Result<()> {
        let profile = WidthProfile::default();
        let renderer = TranscriptRenderer::new(&profile)?;
        let block = renderer.active_tool_preview(
            BlockId("active:tool".into()),
            "tool_call",
            "in_progress",
            true,
            "docs_lookup(crate, Item)",
            "one\r\ntwo\nthree\nfour\nfive\n",
        )?;

        assert_eq!(
            block.text.wire_rows(),
            vec![
                "  • Calling docs_lookup(crate, Item)",
                "    └ one",
                "      two",
                "      three",
                "      four"
            ]
        );
        assert!(
            block
                .metadata
                .decoration
                .iter()
                .any(|decoration| decoration.capture == "ForgeHarnessToolFailure")
        );
        assert!(
            block
                .metadata
                .decoration
                .iter()
                .any(|decoration| decoration.capture == "ForgeHarnessMcpName")
        );
        assert!(
            block
                .metadata
                .decoration
                .iter()
                .any(|decoration| decoration.capture == "ForgeHarnessMcpArguments")
        );
        Ok(())
    }

    #[test]
    fn narrow_window_keeps_prompt_marker_and_source() -> Result<()> {
        let profile = WidthProfile {
            columns: 1,
            ..WidthProfile::default()
        };
        let block = TranscriptRenderer::new(&profile)?.prompt(BlockId("prompt:1".into()), "abc")?;
        assert_eq!(block.text.wire_rows().concat(), "▸ abc");
        assert_eq!(
            block.metadata.decoration[0].range.end.row,
            block.text.row_count()
        );
        Ok(())
    }

    #[test]
    fn wrapped_prompt_uses_two_space_continuations() -> Result<()> {
        let profile = WidthProfile {
            columns: 12,
            ..WidthProfile::default()
        };
        let block = TranscriptRenderer::new(&profile)?
            .prompt(BlockId("prompt:wrapped".into()), "alpha beta gamma")?;

        assert_eq!(block.text.wire_rows(), vec!["▸ alpha ", "  beta gamma"]);
        assert_eq!(block.metadata.decoration[0].range.end.row, 2);
        block.validate()?;
        Ok(())
    }

    #[test]
    fn response_margin_preserves_link_byte_targets_and_wrap_capacity() -> Result<()> {
        let profile = WidthProfile {
            columns: 14,
            ..WidthProfile::default()
        };
        let rendered = TranscriptRenderer::new(&profile)?.response(
            BlockId("response".into()),
            "A [界 link](https://example.test).\n\nMore text wraps.",
        )?;
        assert_eq!(rendered.block.metadata.gutter[0].chunk[0].text, "▸ ");
        assert!(
            rendered
                .block
                .metadata
                .gutter
                .iter()
                .skip(1)
                .all(|gutter| gutter.chunk[0].text == "  ")
        );
        assert_eq!(rendered.block.metadata.target[0].range.start.column, 2);
        for row in rendered.block.text.wire_rows() {
            assert!(profile.cells(&row, 2)? <= 12);
        }
        rendered.block.validate()?;
        Ok(())
    }
}
