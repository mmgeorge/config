use anyhow::Result;
use forge_buffer::block::{
    BlockAnchor, BlockMetadata, BufferBlock, Conceal, ContentLayout, Decoration, FoldRange,
    TargetRange, TextChunk, TextPosition, TextRange,
};
use forge_buffer::identity::{BlockId, FoldId, TargetId};
use forge_buffer::markdown::{MarkdownRenderer, RenderedMarkdown};
use forge_buffer::node::{NodeDisplay, NodeKind, NodeState};
use forge_buffer::text::BufferText;
use forge_buffer::width::WidthProfile;

use super::duration::tool_duration;
use super::markdown_math;
use super::tool::{ToolOutputPreview, ToolOutputView};

pub struct TranscriptRenderer<'profile> {
    profile: &'profile WidthProfile,
}

/// Captures a tool heading's presentation independently of its output body.
#[derive(Clone, Copy)]
pub struct ToolHeading<'call> {
    pub kind: &'call str,
    pub shell: Option<&'call str>,
    pub elapsed_ms: Option<u64>,
    pub failed: bool,
    pub title: &'call str,
    pub expanded: bool,
}

impl<'profile> TranscriptRenderer<'profile> {
    /// Builds the independent command heading without retaining an output body.
    pub(super) fn tool_header(
        &self,
        id: BlockId,
        target: TargetId,
        heading: ToolHeading<'_>,
    ) -> Result<BufferBlock> {
        let empty = ToolOutputPreview {
            row: Vec::new(),
            hidden_rows: 0,
            total_rows: 0,
        };
        let mut block = self.tool_preview(id, target, heading, &empty)?;
        let rows = block
            .metadata
            .decoration
            .iter()
            .find(|span| span.capture == "ForgeHarnessOutput")
            .map_or(block.text.row_count(), |span| span.range.start.row);
        block.text = BufferText::from_rows(block.text.slice(0..rows)?)?;
        block
            .metadata
            .decoration
            .retain(|span| span.capture != "ForgeHarnessOutput");
        block.metadata.target[0].range.end.row = rows;
        Ok(block)
    }

    /// Projects one stable output chunk and an independent collapsed hidden-count block.
    pub(super) fn tool_body(
        &self,
        call_id: &super::identity::DisplayId,
        output: &ToolOutputView,
        expanded: bool,
        chunk: usize,
    ) -> Result<BufferBlock> {
        let id = if chunk == 0 {
            format!("{call_id}:preview")
        } else {
            format!("{call_id}:output:{chunk}")
        };
        let rows = if expanded {
            let source = output.inline_chunk(chunk)?;
            let mut rows = if chunk == 0 {
                output.header().map_or(Ok(Vec::new()), |(header, _)| {
                    tool_source_rows(self.profile, &header.kind, &header.title)
                })?
            } else {
                Vec::new()
            };
            if source.is_empty() && chunk == 0 {
                rows.extend(tool_body_rows(self.profile, "no output", rows.is_empty())?);
            } else {
                for (index, text) in source.into_iter().enumerate() {
                    let branch = rows.is_empty() && chunk == 0 && index == 0;
                    rows.extend(tool_body_rows(self.profile, text, branch)?);
                }
            }
            rows
        } else {
            let mut rows = preview_rows(self.profile, &output.preview(false)?)?.0;
            if output.live_preview && output.preview(false)?.total_rows == 0 {
                rows.clear();
            }
            rows
        };
        self.output_block(BlockId(id), rows)
    }

    /// Changes hidden output counts without replacing the heading or its preview.
    pub(super) fn tool_hidden(
        &self,
        call_id: &super::identity::DisplayId,
        output: &ToolOutputView,
        expanded: bool,
    ) -> Result<BufferBlock> {
        let hidden = if expanded || output.live_preview {
            0
        } else {
            preview_rows(self.profile, &output.preview(false)?)?.1
        };
        let rows = if hidden == 0 {
            Vec::new()
        } else {
            tool_body_rows(
                self.profile,
                &format!(
                    "…({hidden} earlier {})",
                    if hidden == 1 { "line" } else { "lines" }
                ),
                false,
            )?
        };
        self.output_block(BlockId(format!("{call_id}:hidden")), rows)
    }

    fn output_block(&self, id: BlockId, rows: Vec<String>) -> Result<BufferBlock> {
        let text = BufferText::from_rows(rows)?;
        let call =
            id.0.strip_suffix(":preview")
                .or_else(|| id.0.split_once(":output:").map(|(call, _)| call))
                .or_else(|| id.0.strip_suffix(":hidden"));
        let content_node = call.map(|call| forge_buffer::identity::FoldId(format!("{call}:tool")));
        let mut block = BufferBlock {
            id,
            text,
            metadata: BlockMetadata {
                content_node,
                ..Default::default()
            },
        };
        if block.text.row_count() > 0 {
            let range = TextRange {
                start: TextPosition { row: 0, column: 0 },
                end: TextPosition {
                    row: block.text.row_count(),
                    column: 0,
                },
            };
            block.metadata.target.push(TargetRange {
                id: TargetId(format!("{}:tool", block.id.0)),
                range: range.clone(),
            });
            block.metadata.decoration.push(Decoration {
                range,
                capture: "ForgeHarnessOutput".into(),
                priority: 100,
            });
        }
        Ok(block)
    }
    /// Refresh an existing tool heading while preserving its presentation identity.
    pub(super) fn refresh_tool_heading(
        &self,
        previous: &BufferBlock,
        heading: ToolHeading<'_>,
    ) -> Result<BufferBlock> {
        let mut block = self.tool_header(
            previous.id.clone(),
            previous.metadata.target[0].id.clone(),
            heading,
        )?;
        block.metadata.layout = previous.metadata.layout.clone();
        block.metadata.fold = previous.metadata.fold.clone();
        block.metadata.node = previous.metadata.node.clone();
        block.metadata.content_node = previous.metadata.content_node.clone();
        for fold in &mut block.metadata.fold {
            if fold.end.block == block.id && fold.end.position.row == previous.text.row_count() {
                fold.end.position.row = block.text.row_count();
            }
        }
        Ok(block)
    }

    pub fn new(profile: &'profile WidthProfile) -> Result<Self> {
        profile.validate()?;
        Ok(Self { profile })
    }

    pub fn prompt(&self, id: BlockId, source: &str) -> Result<BufferBlock> {
        // The concealed marker moves into the sign column, which indents every display row.
        let mut block = self.literal(id, &format!("● {source}"), 0)?;
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
        Self::heading_marker(&mut block, "ForgeHarnessPrompt");
        block.metadata.node = Some(forge_buffer::node::NodeState::new(
            forge_buffer::identity::FoldId(block.id.0.clone()),
            forge_buffer::node::NodeKind::Message,
            forge_buffer::node::NodeDisplay::Preview,
        ));
        Ok(block)
    }

    pub fn response(&self, id: BlockId, source: &str) -> Result<RenderedMarkdown> {
        let source = markdown_math::normalize(source);
        let mut rendered = MarkdownRenderer::file_link_labels(
            id,
            source.trim_end_matches(['\r', '\n']),
            self.profile,
        )?;
        rendered.block.metadata.markdown = true;
        rendered.block.metadata.layout = Some(ContentLayout {
            indent: 2,
            marker: Some(TextChunk {
                text: "●".into(),
                capture: "ForgeHarnessResponse".into(),
            }),
            source_indent: 0,
        });
        Ok(rendered)
    }

    pub fn commentary(&self, id: BlockId, source: &str) -> Result<RenderedMarkdown> {
        let source = markdown_math::normalize(source);
        let mut rendered = MarkdownRenderer::file_link_labels(
            id,
            source.trim_end_matches(['\r', '\n']),
            self.profile,
        )?;
        let block = &mut rendered.block;
        block.metadata.markdown = true;
        block.metadata.decoration.clear();
        block.metadata.layout = Some(ContentLayout {
            indent: 2,
            source_indent: 0,
            marker: Some(TextChunk {
                text: "○".into(),
                capture: "ForgeHarnessCommentary".into(),
            }),
        });
        block.metadata.decoration.push(Decoration {
            range: TextRange {
                start: TextPosition { row: 0, column: 0 },
                end: TextPosition {
                    row: block.text.row_count(),
                    column: 0,
                },
            },
            capture: "ForgeHarnessCommentary".into(),
            priority: 50,
        });
        Ok(rendered)
    }

    pub fn literal(&self, id: BlockId, source: &str, indent: usize) -> Result<BufferBlock> {
        id.validate()?;
        let block = BufferBlock {
            id,
            text: BufferText::from_rows(
                self.profile
                    .wrap_plain(source, indent.min(self.profile.columns - 1))?,
            )?,
            metadata: BlockMetadata::default(),
        };
        Ok(block)
    }

    pub fn heading_marker(block: &mut BufferBlock, capture: &str) {
        if let Some(marker) = block.text.row(0).and_then(|row| {
            ["▸", "◇", "●"]
                .into_iter()
                .find(|marker| row.starts_with(&format!("{marker} ")))
        }) {
            block.metadata.layout = Some(ContentLayout {
                indent: 2,
                source_indent: 0,
                marker: Some(TextChunk {
                    text: marker.into(),
                    capture: capture.into(),
                }),
            });
            block.metadata.conceal.push(Conceal {
                range: TextRange {
                    start: TextPosition { row: 0, column: 0 },
                    end: TextPosition {
                        row: 0,
                        column: "▸ ".len(),
                    },
                },
                replacement: String::new(),
                line: false,
                priority: 100,
            });
        }
    }

    /// Assigns a fold affordance while preserving tool dots and diff hunk labels.
    pub(super) fn fold_marker(block: &mut BufferBlock) {
        let layout = block.metadata.layout.get_or_insert(ContentLayout {
            indent: 2,
            source_indent: 0,
            marker: None,
        });
        Self::fold_layout_marker(&block.text, layout);
    }

    pub(super) fn fold_layout_marker(
        text: &forge_buffer::text::BufferText,
        layout: &mut ContentLayout,
    ) {
        let heading = text.row(0).unwrap_or("").trim_start();
        if heading.starts_with('•') || heading.starts_with("@@") {
            return;
        }
        let capture = layout
            .marker
            .as_ref()
            .map_or("Normal", |marker| marker.capture.as_str())
            .to_owned();
        layout.marker = Some(TextChunk {
            text: "▸".into(),
            capture,
        });
    }

    /// Gives wrapped events a closed fold while retaining their complete source and actions.
    pub(super) fn prepare_event(block: &mut BufferBlock) {
        if block.text.row_count() <= 1
            || block.metadata.node.is_some()
            || !block.metadata.fold.is_empty()
            || block
                .metadata
                .layout
                .as_ref()
                .and_then(|layout| layout.marker.as_ref())
                .is_none_or(|marker| marker.text != "◇")
        {
            return;
        }
        let identity = FoldId(block.id.0.clone());
        block.metadata.fold.push(FoldRange {
            id: identity.clone(),
            start: TextPosition { row: 1, column: 0 },
            end: BlockAnchor {
                block: block.id.clone(),
                position: TextPosition {
                    row: block.text.row_count(),
                    column: 0,
                },
            },
            closed: true,
            heading_start: Some(BlockAnchor {
                block: block.id.clone(),
                position: TextPosition { row: 0, column: 0 },
            }),
            collapsed_suffix: None,
            collapse_children: false,
            expand_children: false,
            heading_marker: false,
        });
        block.metadata.node = Some(NodeState::new(
            identity,
            NodeKind::Group,
            NodeDisplay::Heading,
        ));
        Self::fold_marker(block);
    }

    /// Resolves bodyless headings to events without shifting their byte coordinates.
    pub(super) fn resolve_marker(block: &mut BufferBlock) -> Result<()> {
        let Some(marker) = block
            .metadata
            .layout
            .as_mut()
            .and_then(|layout| layout.marker.as_mut())
        else {
            return Ok(());
        };
        if marker.text == "▸" && block.metadata.fold.is_empty() {
            marker.text = "◇".into();
        }
        let Some(first) = block.text.row(0) else {
            return Ok(());
        };
        if ["▸ ", "◇ ", "● "]
            .iter()
            .any(|prefix| first.starts_with(prefix))
            && !first.starts_with(&marker.text)
        {
            let mut rows = block
                .text
                .wire_rows()
                .iter()
                .map(|row| (*row).to_owned())
                .collect::<Vec<_>>();
            rows[0].replace_range(.."▸".len(), &marker.text);
            block.text = BufferText::from_rows(rows)?;
        }
        Ok(())
    }

    /// Render a call's observed runtime and output without changing its saved title or output.
    pub fn tool_preview(
        &self,
        id: BlockId,
        target: TargetId,
        heading: ToolHeading<'_>,
        output: &ToolOutputPreview<'_>,
    ) -> Result<BufferBlock> {
        let ToolHeading {
            shell,
            kind,
            elapsed_ms,
            failed,
            title,
            expanded,
        } = heading;
        id.validate()?;
        target.validate()?;
        let duration = tool_duration(elapsed_ms);
        let mut row = vec![tool_heading(self.profile, kind, title, &duration, shell)?];
        let title_rows = row.len();
        if expanded {
            row.extend(tool_source_rows(self.profile, kind, title)?);
        }
        row.extend(tool_output_rows(self.profile, output, expanded)?);
        let text = BufferText::from_rows(row)?;
        let range = TextRange {
            start: TextPosition { row: 0, column: 0 },
            end: TextPosition {
                row: text.row_count(),
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
        Self::heading_marker(&mut block, "Normal");
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
}

fn preview_rows(
    profile: &WidthProfile,
    output: &ToolOutputPreview<'_>,
) -> Result<(Vec<String>, usize)> {
    let mut content_profile = profile.clone();
    let margin = 3.min(content_profile.columns - 1);
    content_profile.columns -= margin;
    let mut row = Vec::new();
    let mut hidden_rows = output.hidden_rows;
    for (index, text) in output.row.iter().enumerate().rev() {
        let byte_limit = (profile.columns * 64).min(65536);
        let truncated = text.len() > byte_limit;
        let text = if truncated {
            let mut start = text.len() - byte_limit;
            while !text.is_char_boundary(start) {
                start += 1;
            }
            &text[start..]
        } else {
            text
        };
        let wrapped = content_profile.wrap_plain(text, 0)?;
        let remaining = 4_usize.saturating_sub(row.len());
        let clipped = wrapped.len() > remaining || truncated;
        row.extend(wrapped.into_iter().rev().take(remaining));
        if clipped || row.len() == 4 {
            hidden_rows += index + usize::from(clipped);
            break;
        }
    }
    row.reverse();
    if row.is_empty() {
        row.push("no output".into());
    }
    let mut formatted = Vec::new();
    for (index, text) in row.into_iter().enumerate() {
        formatted.extend(tool_body_rows(profile, &text, index == 0)?);
    }
    Ok((formatted, hidden_rows))
}

fn tool_output_rows(
    profile: &WidthProfile,
    output: &ToolOutputPreview<'_>,
    expanded: bool,
) -> Result<Vec<String>> {
    if expanded {
        let mut row = Vec::new();
        for (index, text) in output.row.iter().enumerate() {
            row.extend(tool_body_rows(profile, text, index == 0)?);
        }
        if row.is_empty() {
            row.extend(tool_body_rows(profile, "no output", true)?);
        }
        return Ok(row);
    }
    let (mut row, hidden) = preview_rows(profile, output)?;
    if hidden > 0 {
        row.extend(tool_body_rows(
            profile,
            &format!(
                "…({hidden} earlier {})",
                if hidden == 1 { "line" } else { "lines" }
            ),
            false,
        )?);
    }
    Ok(row)
}

/// Wraps body content before adding its marker so long tokens cannot strand the marker.
fn tool_body_rows(profile: &WidthProfile, text: &str, branch: bool) -> Result<Vec<String>> {
    let mut content_profile = profile.clone();
    let margin = 3.min(content_profile.columns - 1);
    content_profile.columns -= margin;
    Ok(content_profile
        .wrap_plain(text, 0)?
        .into_iter()
        .enumerate()
        .map(|(index, text)| {
            let prefix = if branch && index == 0 && margin == 3 {
                "└  ".into()
            } else {
                " ".repeat(margin)
            };
            format!("{prefix}{text}")
        })
        .collect())
}

/// Formats a bounded display title without changing the retained provider call.
fn tool_heading(
    profile: &WidthProfile,
    kind: &str,
    title: &str,
    duration: &str,
    shell: Option<&str>,
) -> Result<String> {
    let display = matches!(kind, "command" | "check").then(|| {
        crate::permissions::command::display_command(
            title,
            shell
                .map(crate::permissions::shell::CommandShell::from_executable)
                .unwrap_or_default(),
        )
    });
    let summary = if kind == "command" {
        display.as_ref().and_then(|display| display.summary())
    } else {
        None
    };
    let title = summary.as_deref().unwrap_or_else(|| {
        display
            .as_ref()
            .map_or(title, |display| display.source.as_str())
    });
    let normalized = title.split_whitespace().collect::<Vec<_>>().join(" ");
    let full = if kind == "check" {
        format!("▸ {normalized} · {}", duration.trim())
    } else {
        format!("• {duration} {normalized}")
    };
    if profile.cells(&full, 0)? <= profile.columns {
        return Ok(full);
    }
    let mut prefix = String::new();
    let mut closing = Vec::new();
    let mut quoted = false;
    let mut escaped = false;
    let mut best = "…".to_owned();
    for character in full.chars() {
        prefix.push(character);
        if kind == "tool_call" {
            if quoted {
                if escaped {
                    escaped = false;
                } else if character == '\\' {
                    escaped = true;
                } else if character == '"' {
                    quoted = false;
                }
            } else {
                match character {
                    '"' => quoted = true,
                    '(' => closing.push(')'),
                    '{' => closing.push('}'),
                    '[' => closing.push(']'),
                    ')' | '}' | ']' => {
                        closing.pop();
                    }
                    _ => {}
                }
            }
        }
        if escaped {
            continue;
        }
        let suffix: String = closing.iter().rev().collect();
        let candidate = format!("{prefix}…{}{suffix}", if quoted { "\"" } else { "" });
        if profile.cells(&candidate, 0)? > profile.columns {
            break;
        }
        best = candidate;
    }
    Ok(best)
}

fn tool_source_rows(profile: &WidthProfile, kind: &str, title: &str) -> Result<Vec<String>> {
    if kind == "tool_call" && title.contains('(') {
        return tool_body_rows(profile, &format!("Call: {title}"), true);
    }
    if kind != "command" {
        return Ok(Vec::new());
    }
    let display = crate::permissions::command::display_command(
        title,
        crate::permissions::shell::CommandShell::default(),
    );
    if display.summary().is_none() {
        return Ok(Vec::new());
    }
    tool_body_rows(profile, &format!("Command: {title}"), true)
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

/// Skips the padded duration column so timing text never receives command or MCP styling.
fn tool_content_start(text: &str) -> usize {
    let after_bullet = text.split_once("• ").map(|(_, tail)| tail).unwrap_or("");
    let title = after_bullet
        .trim_start()
        .split_once(char::is_whitespace)
        .map(|(_, title)| title.trim_start())
        .unwrap_or("");
    text.len() - title.len()
}

fn decorate_command(block: &mut BufferBlock, title_rows: usize) {
    let mut expects_command = true;
    for row in 0..title_rows {
        let Some(text) = block.text.row(row).map(str::to_owned) else {
            continue;
        };
        let content_start = if row == 0 {
            tool_content_start(&text)
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
            tool_content_start(&text)
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
