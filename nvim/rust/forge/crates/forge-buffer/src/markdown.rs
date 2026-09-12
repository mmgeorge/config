use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::ops::Range;

use pulldown_cmark::{Event, Options, Parser, Tag, TagEnd};
use unicode_segmentation::UnicodeSegmentation;

use crate::ContractError;
use crate::block::{BlockMetadata, BufferBlock, Decoration, TargetRange, TextPosition, TextRange};
use crate::identity::{BlockId, TargetId};
use crate::text::BufferText;
use crate::width::WidthProfile;

pub struct MarkdownLink {
    pub target: TargetId,
    pub destination: String,
}

pub struct RenderedMarkdown {
    pub block: BufferBlock,
    pub link: Vec<MarkdownLink>,
    pub source: Vec<MarkdownSourceRange>,
}

/// Zero-based source rows and the rendered rows that retain their content.
///
/// Wrapped source rows span several output rows. Joined source rows share output rows.
/// Removed delimiters use the next represented row, or the previous row at the end.
/// Interior blank source rows retain their own output rows. Each input row occurs
/// exactly once in the source map.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MarkdownSourceRange {
    pub source: Range<usize>,
    pub output: Range<usize>,
}

pub struct MarkdownRenderer;

struct RenderState<'profile> {
    profile: &'profile WidthProfile,
    row: Vec<String>,
    metadata: BlockMetadata,
    capture: Vec<&'static str>,
    link: Vec<MarkdownLink>,
    active_link: Option<TargetId>,
    list: Vec<Option<u64>>,
    code: bool,
    bytes: usize,
    source_row: Range<usize>,
    source_output: Vec<Option<Range<usize>>>,
    pending_source: Option<MarkdownSourceRange>,
}

impl MarkdownRenderer {
    /// Preserves physical source rows and delimiters while decorating Markdown spans.
    ///
    /// Views retain responsibility for soft wrapping. Source and output row identities
    /// remain equal, including blank and trailing rows, so annotations keep exact anchors.
    pub fn source(
        id: BlockId,
        source: &str,
        profile: &WidthProfile,
    ) -> Result<RenderedMarkdown, ContractError> {
        id.validate()?;
        profile.validate()?;
        if source.len() > 8 * 1024 * 1024 || source.contains('\0') {
            return Err(ContractError(
                "Markdown source exceeds document admission limits",
            ));
        }
        let row: Vec<_> = source.split('\n').collect();
        if row.len() > 65_536 {
            return Err(ContractError("Markdown source row budget exceeded"));
        }
        let mut offset = 0;
        let start: Vec<_> = row
            .iter()
            .map(|line| {
                let current = offset;
                offset += line.len() + 1;
                current
            })
            .collect();
        let mut metadata = BlockMetadata::default();
        let mut link = Vec::new();
        for (event, range) in Parser::new_ext(source, Options::all()).into_offset_iter() {
            let capture = match event {
                Event::Start(Tag::Heading { .. }) => "@markup.heading",
                Event::Start(Tag::Emphasis) => "@markup.italic",
                Event::Start(Tag::Strong) => "@markup.strong",
                Event::Start(Tag::Strikethrough) => "@markup.strikethrough",
                Event::Start(Tag::CodeBlock(_)) => "@markup.raw.block",
                Event::Code(_) | Event::InlineMath(_) | Event::DisplayMath(_) => "@markup.raw",
                Event::Start(Tag::Link { dest_url, .. }) => {
                    let target = TargetId(format!("markdown-source-link:{}:{}", id.0, range.start));
                    let first = start
                        .partition_point(|offset| *offset <= range.start)
                        .saturating_sub(1);
                    let last = start
                        .partition_point(|offset| *offset <= range.end)
                        .saturating_sub(1);
                    metadata.target.push(TargetRange {
                        id: target.clone(),
                        range: TextRange {
                            start: TextPosition {
                                row: first,
                                column: range.start - start[first],
                            },
                            end: TextPosition {
                                row: last,
                                column: range.end - start[last],
                            },
                        },
                    });
                    link.push(MarkdownLink {
                        target,
                        destination: dest_url.into_string(),
                    });
                    "@markup.link.label"
                }
                _ => continue,
            };
            let first = start
                .partition_point(|offset| *offset <= range.start)
                .saturating_sub(1);
            let last = start
                .partition_point(|offset| *offset <= range.end.saturating_sub(1))
                .saturating_sub(1);
            for source_row in first..=last {
                let first_column = range
                    .start
                    .saturating_sub(start[source_row])
                    .min(row[source_row].len());
                let last_column = range
                    .end
                    .saturating_sub(start[source_row])
                    .min(row[source_row].len());
                if first_column < last_column {
                    metadata.decoration.push(Decoration {
                        range: TextRange {
                            start: TextPosition {
                                row: source_row,
                                column: first_column,
                            },
                            end: TextPosition {
                                row: source_row,
                                column: last_column,
                            },
                        },
                        capture: capture.into(),
                        priority: 100,
                    });
                }
            }
            if metadata.decoration.len() > 262_144 {
                return Err(ContractError("Markdown decoration budget exceeded"));
            }
        }
        let source = (0..row.len())
            .map(|row| MarkdownSourceRange {
                source: row..row + 1,
                output: row..row + 1,
            })
            .collect();
        let block = BufferBlock {
            id,
            text: BufferText::from_rows(row)?,
            metadata,
        };
        block.validate()?;
        Ok(RenderedMarkdown {
            block,
            link,
            source,
        })
    }

    /// Produces read-only presentation. Editable bodies retain their original source rows.
    pub fn render(
        id: BlockId,
        source: &str,
        profile: &WidthProfile,
    ) -> Result<RenderedMarkdown, ContractError> {
        id.validate()?;
        profile.validate()?;
        if source.len() > 8 * 1024 * 1024 || source.contains('\0') {
            return Err(ContractError(
                "Markdown source exceeds document admission limits",
            ));
        }
        let mut source_start = vec![0usize];
        source_start.extend(
            source
                .bytes()
                .enumerate()
                .filter_map(|(offset, byte)| (byte == b'\n').then_some(offset + 1)),
        );
        if source_start.len() > 65_536 {
            return Err(ContractError("Markdown source row budget exceeded"));
        }
        let mut state = RenderState {
            profile,
            row: vec![String::new()],
            metadata: BlockMetadata::default(),
            capture: Vec::new(),
            link: Vec::new(),
            active_link: None,
            list: Vec::new(),
            code: false,
            bytes: 0,
            source_row: 0..1,
            source_output: vec![None; source_start.len()],
            pending_source: None,
        };
        let options = Options::ENABLE_TABLES
            | Options::ENABLE_STRIKETHROUGH
            | Options::ENABLE_TASKLISTS
            | Options::ENABLE_FOOTNOTES
            | Options::ENABLE_MATH;
        let source_line: Vec<&str> = source.split('\n').collect();
        let mut scanned_source = 0;
        for (event, source_range) in Parser::new_ext(source, options).into_offset_iter() {
            let first_source = source_start
                .partition_point(|start| *start <= source_range.start)
                .saturating_sub(1);
            let last_source = source_start
                .partition_point(|start| *start <= source_range.end.saturating_sub(1))
                .saturating_sub(1);
            if first_source > scanned_source {
                state.flush_source();
                for row in scanned_source..first_source {
                    if source_line[row].trim().is_empty()
                        && state.source_output[row].is_none()
                        && (state.row.len() > 1 || !state.row[0].is_empty())
                    {
                        state.break_row(false)?;
                        state.source_row = row..row + 1;
                        state.mark_source();
                        state.break_row(true)?;
                    }
                }
                scanned_source = first_source;
            }
            state.source_row = first_source..last_source.max(first_source) + 1;
            match event {
                Event::Start(tag) => {
                    state.source_row = first_source..first_source + 1;
                    match tag {
                        Tag::Paragraph => state.break_row(false)?,
                        Tag::Heading { .. } => {
                            state.break_row(false)?;
                            state.capture.push("@markup.heading");
                        }
                        Tag::Emphasis => state.capture.push("@markup.italic"),
                        Tag::Strong => state.capture.push("@markup.strong"),
                        Tag::Strikethrough => state.capture.push("@markup.strikethrough"),
                        Tag::CodeBlock(_) => {
                            state.break_row(false)?;
                            state.code = true;
                            state.capture.push("@markup.raw.block");
                        }
                        Tag::List(start) => {
                            state.break_row(false)?;
                            state.list.push(start);
                        }
                        Tag::Item => {
                            state.break_row(false)?;
                            let prefix = match state.list.last_mut() {
                                Some(Some(number)) => {
                                    let prefix = format!("{number}. ");
                                    *number = number.saturating_add(1);
                                    prefix
                                }
                                _ => "• ".to_owned(),
                            };
                            state.append(
                                &format!(
                                    "{}{prefix}",
                                    "  ".repeat(state.list.len().saturating_sub(1))
                                ),
                                false,
                            )?;
                        }
                        Tag::BlockQuote(_) => {
                            state.break_row(false)?;
                            state.append("│ ", false)?;
                        }
                        Tag::Link { dest_url, .. } | Tag::Image { dest_url, .. } => {
                            let mut hash = DefaultHasher::new();
                            id.hash(&mut hash);
                            source_range.start.hash(&mut hash);
                            let target = TargetId(format!("markdown-link:{:016x}", hash.finish()));
                            state.link.push(MarkdownLink {
                                target: target.clone(),
                                destination: dest_url.into_string(),
                            });
                            state.active_link = Some(target);
                            state.capture.push("@markup.link.label");
                        }
                        Tag::Table(_) | Tag::TableHead | Tag::TableRow => state.break_row(false)?,
                        Tag::TableCell => {
                            if !state.row.last().unwrap().is_empty() {
                                state.append(" │ ", false)?;
                            }
                        }
                        _ => {}
                    }
                }
                Event::End(tag) => match tag {
                    TagEnd::Heading(_) | TagEnd::CodeBlock => {
                        state.capture.pop();
                        state.code = false;
                        state.break_row(false)?;
                    }
                    TagEnd::Emphasis | TagEnd::Strong | TagEnd::Strikethrough => {
                        state.capture.pop();
                    }
                    TagEnd::Link | TagEnd::Image => {
                        state.capture.pop();
                        state.active_link = None;
                    }
                    TagEnd::Paragraph
                    | TagEnd::Item
                    | TagEnd::BlockQuote(_)
                    | TagEnd::TableRow
                    | TagEnd::TableHead => state.break_row(false)?,
                    TagEnd::List(_) => {
                        state.list.pop();
                        state.break_row(false)?;
                    }
                    _ => {}
                },
                Event::Text(text) | Event::Html(text) | Event::InlineHtml(text) => {
                    state.append(&text, !state.code)?
                }
                Event::Code(text) | Event::InlineMath(text) | Event::DisplayMath(text) => {
                    state.capture.push("@markup.raw");
                    state.append(&text, !state.code)?;
                    state.capture.pop();
                }
                Event::SoftBreak => state.append(" ", true)?,
                Event::HardBreak => state.break_row(true)?,
                Event::Rule => {
                    state.break_row(false)?;
                    state.append(&"─".repeat(profile.columns), false)?;
                    state.break_row(false)?;
                }
                Event::TaskListMarker(checked) => {
                    state.append(if checked { "[✓] " } else { "[ ] " }, false)?
                }
                Event::FootnoteReference(label) => state.append(&format!("[{label}]"), true)?,
            }
        }
        if state.row.len() > 1 && state.row.last().is_some_and(String::is_empty) {
            state.row.pop();
        }
        state.flush_source();
        let source_map = state.finish_source_map();
        let block = BufferBlock {
            id,
            text: BufferText::from_rows(state.row)?,
            metadata: state.metadata,
        };
        block.validate()?;
        Ok(RenderedMarkdown {
            block,
            link: state.link,
            source: source_map,
        })
    }
}

impl RenderState<'_> {
    fn break_row(&mut self, force: bool) -> Result<(), ContractError> {
        if force || !self.row.last().unwrap().is_empty() {
            if self.row.len() >= 65_536 {
                return Err(ContractError("Markdown row budget exceeded"));
            }
            self.row.push(String::new());
        }
        Ok(())
    }

    fn append(&mut self, text: &str, wrap: bool) -> Result<(), ContractError> {
        let source_row = self.source_row.clone();
        let physical_source = text.contains('\n') && source_row.len() > 1;
        for (index, physical) in text.split('\n').enumerate() {
            if physical_source {
                let source = (source_row.start + index).min(source_row.end - 1);
                self.source_row = source..source + 1;
            }
            if index > 0 {
                self.break_row(true)?;
            }
            if physical.is_empty() && index < source_row.len() {
                self.mark_source();
            }
            if wrap {
                for part in physical.split_inclusive(char::is_whitespace) {
                    let current = self.row.last().unwrap();
                    if !current.is_empty()
                        && self.profile.cells(current, 0)?
                            + self.profile.cells(part, self.profile.cells(current, 0)?)?
                            > self.profile.columns
                    {
                        self.break_row(false)?;
                    }
                    if self.profile.cells(part, 0)? > self.profile.columns {
                        for cluster in part.graphemes(true) {
                            let current = self.row.last().unwrap();
                            if !current.is_empty()
                                && self.profile.cells(&format!("{current}{cluster}"), 0)?
                                    > self.profile.columns
                            {
                                self.break_row(false)?;
                            }
                            self.append_span(cluster)?;
                        }
                    } else {
                        self.append_span(part)?;
                    }
                }
            } else {
                self.append_span(physical)?;
            }
        }
        self.source_row = source_row;
        Ok(())
    }

    fn mark_source(&mut self) {
        let output_row = self.row.len() - 1;
        if let Some(pending) = self
            .pending_source
            .as_mut()
            .filter(|pending| pending.source == self.source_row)
        {
            pending.output.end = output_row + 1;
            return;
        }
        self.flush_source();
        self.pending_source = Some(MarkdownSourceRange {
            source: self.source_row.clone(),
            output: output_row..output_row + 1,
        });
    }

    fn flush_source(&mut self) {
        let Some(pending) = self.pending_source.take() else {
            return;
        };
        for source in self.source_output[pending.source].iter_mut() {
            match source {
                Some(range) => {
                    range.start = range.start.min(pending.output.start);
                    range.end = range.end.max(pending.output.end);
                }
                None => *source = Some(pending.output.clone()),
            }
        }
    }

    fn finish_source_map(&self) -> Vec<MarkdownSourceRange> {
        let output_count = self.row.len();
        let last = self
            .source_output
            .iter()
            .rev()
            .flatten()
            .next()
            .cloned()
            .unwrap_or(0..1);
        let mut nearest = last;
        let mut mapping = Vec::with_capacity(self.source_output.len());
        for (row, output) in self.source_output.iter().enumerate().rev() {
            if let Some(output) = output {
                nearest = output.clone();
            }
            let start = nearest.start.min(output_count - 1);
            mapping.push(MarkdownSourceRange {
                source: row..row + 1,
                output: start..nearest.end.min(output_count).max(start + 1),
            });
        }
        mapping.reverse();
        mapping
    }

    fn append_span(&mut self, text: &str) -> Result<(), ContractError> {
        if text.is_empty() {
            return Ok(());
        }
        self.mark_source();
        self.bytes = self
            .bytes
            .checked_add(text.len())
            .ok_or(ContractError("Markdown size overflow"))?;
        if self.bytes > 16 * 1024 * 1024
            || self.metadata.decoration.len() + self.metadata.target.len() >= 65_536
        {
            return Err(ContractError("Markdown presentation budget exceeded"));
        }
        let row = self.row.len() - 1;
        let output = self.row.last_mut().unwrap();
        let start = output.len();
        output.push_str(text);
        let range = TextRange {
            start: TextPosition { row, column: start },
            end: TextPosition {
                row,
                column: output.len(),
            },
        };
        if let Some(capture) = self.capture.last() {
            if let Some(previous) = self.metadata.decoration.last_mut().filter(|previous| {
                previous.capture == *capture && previous.range.end == range.start
            }) {
                previous.range.end = range.end;
            } else {
                self.metadata.decoration.push(Decoration {
                    range: range.clone(),
                    capture: (*capture).to_owned(),
                    priority: 110,
                });
            }
        }
        if let Some(target) = &self.active_link {
            if let Some(previous) = self
                .metadata
                .target
                .last_mut()
                .filter(|previous| previous.id == *target && previous.range.end == range.start)
            {
                previous.range.end = range.end;
            } else {
                self.metadata.target.push(TargetRange {
                    id: target.clone(),
                    range,
                });
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn source_projection_preserves_task_rows_fences_and_unicode_at_every_width() {
        use super::*;
        let source = "# Plan\n\n1. **Implement**\n   ├─ file.rs\n   └─ preserve rows\n\n```rust\n\tlet 界 = 1;\n```\n\n[reference](https://example.test)\n";
        for columns in [20, 80, 160] {
            let rendered = MarkdownRenderer::source(
                BlockId("source".into()),
                source,
                &WidthProfile {
                    columns,
                    ..WidthProfile::default()
                },
            )
            .unwrap();
            assert_eq!(
                rendered.block.text.wire_rows(),
                source.split('\n').collect::<Vec<_>>()
            );
            for (row, mapping) in rendered.source.iter().enumerate() {
                assert_eq!(mapping.source, row..row + 1);
                assert_eq!(mapping.output, row..row + 1);
            }
            assert!(
                rendered
                    .block
                    .metadata
                    .decoration
                    .iter()
                    .any(|span| span.capture == "@markup.strong")
            );
            assert_eq!(rendered.link[0].destination, "https://example.test");
            rendered.block.validate().unwrap();
        }
    }

    use super::*;

    #[test]
    fn markdown_renders_native_rows_and_link_byte_targets() {
        let rendered = MarkdownRenderer::render(
            BlockId("body".into()),
            "# Title\n\n**bold** [界 link](https://example.test)\n\n- [x] done\n",
            &WidthProfile::default(),
        )
        .unwrap();
        assert_eq!(
            rendered.block.text.wire_rows(),
            vec!["Title", "", "bold 界 link", "", "• [✓] done"]
        );
        assert_eq!(rendered.link.len(), 1);
        assert_eq!(rendered.link[0].destination, "https://example.test");
        let target = &rendered.block.metadata.target[0];
        assert_eq!(target.range.start.column, 5);
        assert!(
            rendered
                .block
                .metadata
                .decoration
                .iter()
                .any(|decoration| decoration.capture == "@markup.heading")
        );
    }

    #[test]
    fn code_rows_preserve_source_spacing_and_unicode() {
        let rendered = MarkdownRenderer::render(
            BlockId("body".into()),
            "```rust\n\t界  value\n```",
            &WidthProfile {
                columns: 4,
                ..WidthProfile::default()
            },
        )
        .unwrap();
        assert_eq!(rendered.block.text.wire_rows(), vec!["\t界  value"]);
    }

    #[test]
    fn long_tokens_wrap_without_splitting_flags_or_combining_sequences() {
        let rendered = MarkdownRenderer::render(
            BlockId("body".into()),
            "**🇺🇸🇯🇵éé**",
            &WidthProfile {
                columns: 2,
                ..WidthProfile::default()
            },
        )
        .unwrap();
        assert_eq!(rendered.block.text.wire_rows(), vec!["🇺🇸", "🇯🇵", "éé"]);
        assert_eq!(rendered.block.metadata.decoration.len(), 3);
    }

    #[test]
    fn source_rows_follow_wrapping_soft_joins_and_removed_delimiters() {
        let rendered = MarkdownRenderer::render(
            BlockId("source-map".into()),
            "# Heading\n\nfirst line\nsecond line\n\n- long item wraps words\n",
            &WidthProfile {
                columns: 12,
                ..WidthProfile::default()
            },
        )
        .unwrap();
        let rows = rendered.block.text.wire_rows();
        assert_eq!(rendered.source.len(), 7);
        assert_eq!(rendered.source[0].output, 0..1);
        assert_eq!(rendered.source[1].output, 1..2);
        assert_eq!(rows[1], "");
        let item = &rendered.source[5].output;
        assert!(item.len() >= 2, "list source spans each wrapped output row");
        assert!(rows[item.start].starts_with('•'));
        assert_eq!(rendered.source[6].output, *item);
        let joined = MarkdownRenderer::render(
            BlockId("joined".into()),
            "first\nsecond",
            &WidthProfile::default(),
        )
        .unwrap();
        assert_eq!(joined.block.text.wire_rows(), vec!["first second"]);
        assert_eq!(joined.source[0].output, joined.source[1].output);
    }

    #[test]
    fn source_rows_keep_code_table_and_blank_input_navigation_in_bounds() {
        let source = "```rust\n  first\n\n  last\n```\n\n| A | B |\n| - | - |\n| one | two |";
        let rendered = MarkdownRenderer::render(
            BlockId("structured".into()),
            source,
            &WidthProfile::default(),
        )
        .unwrap();
        let rows = rendered.block.text.wire_rows();
        assert_eq!(&rows[..3], &["  first", "", "  last"]);
        assert_eq!(rendered.source[1].output, 0..1);
        assert_eq!(rendered.source[2].output, 1..2);
        assert_eq!(rendered.source[3].output, 2..3);
        assert_eq!(rendered.source[0].output, 0..1);
        for (row, mapping) in rendered.source.iter().enumerate() {
            assert_eq!(mapping.source, row..row + 1);
            assert!(mapping.output.start < mapping.output.end && mapping.output.end <= rows.len());
        }
        let table_row = rendered.source[8].output.clone();
        assert!(rows[table_row.start].contains("one"));
        assert_eq!(rendered.source[7].output, table_row);
        let empty =
            MarkdownRenderer::render(BlockId("blank".into()), "\n\n", &WidthProfile::default())
                .unwrap();
        assert!(empty.source.iter().all(|mapping| mapping.output == (0..1)));
    }

    #[test]
    fn source_paragraph_gaps_preserve_links_and_do_not_duplicate_code_blanks() {
        let rendered = MarkdownRenderer::render(
            BlockId("gaps".into()),
            "First paragraph.\n\n- one\n- two\n\n\n[界 link](https://example.test)\n\n```rust\nfirst\n\nlast\n```\n\nLast paragraph.",
            &WidthProfile::default(),
        )
        .unwrap();
        assert_eq!(
            rendered.block.text.wire_rows(),
            vec![
                "First paragraph.",
                "",
                "• one",
                "• two",
                "",
                "",
                "界 link",
                "",
                "first",
                "",
                "last",
                "",
                "Last paragraph.",
            ]
        );
        let target = &rendered.block.metadata.target[0];
        assert_eq!(target.range.start, TextPosition { row: 6, column: 0 });
        assert_eq!(rendered.source[10].output, 9..10);
        rendered.block.validate().unwrap();
    }
}
