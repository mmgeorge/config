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
                output.header().map_or(Ok(Vec::new()), |header| {
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
            if output.live_preview {
                if output.preview(false)?.total_rows == 0 {
                    rows.clear();
                }
                rows.resize(4, String::new());
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
    pub(super) fn refresh_tool_heading(
        &self,
        previous: &BufferBlock,
        kind: &str,
        elapsed_ms: Option<u64>,
        failed: bool,
        title: &str,
        expanded: bool,
    ) -> Result<BufferBlock> {
        let mut block = self.tool_header(
            previous.id.clone(),
            previous.metadata.target[0].id.clone(),
            crate::buffer::transcript::ToolHeading {
                kind,
                elapsed_ms,
                failed,
                title,
                expanded,
            },
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
            || !block
                .metadata
                .layout
                .as_ref()
                .and_then(|layout| layout.marker.as_ref())
                .is_some_and(|marker| marker.text == "◇")
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
            kind,
            elapsed_ms,
            failed,
            title,
            expanded,
        } = heading;
        id.validate()?;
        target.validate()?;
        let duration = tool_duration(elapsed_ms);
        let mut row = vec![tool_heading(self.profile, kind, title, &duration)?];
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
        let heading_kind = if kind == "tool_call" && shell_tool_display(title).is_some() {
            "command"
        } else {
            kind
        };
        decorate_tool_heading(&mut block, title_rows, heading_kind, failed);
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

fn shell_tool_display(title: &str) -> Option<crate::permissions::command::CommandDisplay> {
    use crate::permissions::shell::CommandShell;

    let (name, arguments) = title.split_once('(')?;
    let name = name.rsplit('.').next()?;
    let shell = match name {
        "bash" => CommandShell::Bash,
        "powershell" => CommandShell::PowerShell,
        "shell" | "exec_command" | "run_shell_command" => CommandShell::default(),
        _ => return None,
    };
    let arguments: serde_json::Value = serde_json::from_str(arguments.strip_suffix(')')?).ok()?;
    let command = arguments
        .get("command")
        .or_else(|| arguments.get("cmd"))?
        .as_str()
        .filter(|command| !command.trim().is_empty())?;
    let shell = arguments
        .get("shell")
        .and_then(serde_json::Value::as_str)
        .map_or(shell, CommandShell::from_executable);
    Some(crate::permissions::command::display_command(command, shell))
}

/// Formats a bounded display title without changing the retained provider call.
fn tool_heading(profile: &WidthProfile, kind: &str, title: &str, duration: &str) -> Result<String> {
    let display = if kind == "tool_call" {
        shell_tool_display(title)
    } else {
        matches!(kind, "command" | "check").then(|| {
            crate::permissions::command::display_command(
                title,
                crate::permissions::shell::CommandShell::default(),
            )
        })
    };
    let kind = if kind == "tool_call" && display.is_some() {
        "command"
    } else {
        kind
    };
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn check_heading_keeps_one_marker_across_expansion_and_refresh() -> Result<()> {
        let profile = WidthProfile::default();
        let renderer = TranscriptRenderer::new(&profile)?;
        for expanded in [false, true] {
            let mut block = renderer.tool_header(
                BlockId("check:tool".into()),
                TargetId("check:tool".into()),
                crate::buffer::transcript::ToolHeading {
                    kind: "check",
                    elapsed_ms: Some(102_000),
                    failed: false,
                    title: "cargo test --locked --lib controls::tests · Running",
                    expanded,
                },
            )?;
            TranscriptRenderer::fold_marker(&mut block);
            for elapsed in [102_000, 103_000] {
                block = renderer.refresh_tool_heading(
                    &block,
                    "check",
                    Some(elapsed),
                    false,
                    "cargo test --locked --lib controls::tests · Running",
                    expanded,
                )?;
                let heading = block.text.row(0).unwrap();
                let mut visible = heading.to_owned();
                for conceal in block.metadata.conceal.iter().rev() {
                    assert_eq!(conceal.range.start.row, 0);
                    assert_eq!(conceal.range.end.row, 0);
                    visible.replace_range(
                        conceal.range.start.column..conceal.range.end.column,
                        &conceal.replacement,
                    );
                }
                let marker = &block
                    .metadata
                    .layout
                    .as_ref()
                    .unwrap()
                    .marker
                    .as_ref()
                    .unwrap()
                    .text;
                let rendered = format!("{marker} {visible}");
                assert!(
                    rendered.starts_with("▸ cargo test --locked --lib controls::tests · Running"),
                    "{rendered}"
                );
                assert_eq!(rendered.matches('▸').count(), 1, "{rendered}");
                assert!(heading.starts_with("▸ "));
            }
        }
        Ok(())
    }

    #[test]
    fn prompt_preserves_literal_markdown_and_real_marker() -> Result<()> {
        let profile = WidthProfile::default();
        let renderer = TranscriptRenderer::new(&profile)?;
        let block = renderer.prompt(BlockId("prompt:1".into()), "**literal** [name](url)")?;
        assert_eq!(block.text.row(0), Some("● **literal** [name](url)"));
        assert!(block.metadata.editable_region.is_empty());
        assert!(block.metadata.target.is_empty());
        assert_eq!(block.metadata.decoration[0].capture, "ForgeHarnessPrompt");
        assert_eq!(block.metadata.decoration[0].range.end.row, 1);
        Ok(())
    }

    #[test]
    fn attachment_prompt_wraps_only_the_visible_label() -> Result<()> {
        let profile = WidthProfile {
            columns: 40,
            ..WidthProfile::default()
        };
        let source = "Inspect [Image #1]";
        let block =
            TranscriptRenderer::new(&profile)?.prompt(BlockId("attachment".into()), source)?;
        assert_eq!(block.text.row_count(), 1);
        assert_eq!(block.text.row(0), Some("● Inspect [Image #1]"));
        Ok(())
    }

    #[test]
    fn expanded_mcp_keeps_one_heading_above_response() -> Result<()> {
        let profile = WidthProfile {
            columns: 70,
            ..WidthProfile::default()
        };
        let renderer = TranscriptRenderer::new(&profile)?;
        let arguments = r#"{"entity_name":"CosmosDbClient","file_path":"cosmos-db-client.ts","hops":1,"token_budget":3500}"#;
        let block = renderer.tool_preview(
            BlockId("tool".into()),
            TargetId("tool".into()),
            crate::buffer::transcript::ToolHeading {
                kind: "tool_call",
                elapsed_ms: Some(2000),
                failed: false,
                title: &format!("sem.sem_context({arguments})"),
                expanded: true,
            },
            &ToolOutputPreview {
                row: vec!["response"],
                hidden_rows: 0,
                total_rows: 1,
            },
        )?;
        let rows = block.text.wire_rows();
        assert!(rows[0].starts_with("•    2s sem.sem_context({"));
        assert!(rows[0].contains('…'));
        assert!(rows.len() > 2);
        assert_eq!(*rows.last().unwrap(), "└  response");
        let call = rows[1..rows.len() - 1]
            .iter()
            .map(|row| row.trim().trim_start_matches('└').trim())
            .collect::<String>();
        assert_eq!(
            call.strip_prefix("Call:").unwrap().trim(),
            format!("sem.sem_context({arguments})")
        );
        assert!(
            rows.iter()
                .all(|row| profile.cells(row, 0).unwrap() <= profile.columns)
        );
        Ok(())
    }

    #[test]
    fn long_json_response_starts_beside_branch_in_preview_and_expansion() -> Result<()> {
        let response = r#"{"ok":false,"tool":"plan_read","phase":"semantic_execution","code":"semantic_execution_failed","message":"plan controls require Harness Plan mode"}"#;
        for columns in [40, 70, 120] {
            let profile = WidthProfile {
                columns,
                ..WidthProfile::default()
            };
            let renderer = TranscriptRenderer::new(&profile)?;
            for expanded in [false, true] {
                let block = renderer.tool_preview(
                    BlockId("tool".into()),
                    TargetId("tool".into()),
                    crate::buffer::transcript::ToolHeading {
                        kind: "tool_call",
                        elapsed_ms: Some(2000),
                        failed: true,
                        title: "plan_read",
                        expanded,
                    },
                    &ToolOutputPreview {
                        row: vec![response],
                        hidden_rows: 0,
                        total_rows: 1,
                    },
                )?;
                let rows = block.text.wire_rows();
                assert!(rows[1].starts_with("└  "));
                let truncated = rows
                    .last()
                    .is_some_and(|row| row.contains("…(1 earlier line)"));
                let content_end = rows.len() - usize::from(truncated);
                let restored = rows[1..content_end]
                    .iter()
                    .enumerate()
                    .map(|(index, row)| {
                        if index == 0 {
                            row.strip_prefix("└  ").unwrap()
                        } else {
                            row.strip_prefix("   ").unwrap()
                        }
                    })
                    .collect::<String>();
                if truncated {
                    assert!(!expanded);
                    assert_eq!(content_end - 1, 4);
                    assert!(response.ends_with(&restored));
                } else {
                    assert_eq!(restored, response);
                }
                assert!(
                    rows.iter()
                        .all(|row| profile.cells(row, 0).unwrap() <= columns)
                );
            }
        }
        Ok(())
    }

    #[test]
    fn tool_runtime_precision_preserves_command_decoration_in_both_views() -> Result<()> {
        let profile = WidthProfile::default();
        let renderer = TranscriptRenderer::new(&profile)?;
        for (elapsed_ms, label) in [
            (0, "0s"),
            (2, "0s"),
            (49, "0s"),
            (50, "0.1s"),
            (149, "0.1s"),
            (150, "0.2s"),
            (439, "0.4s"),
            (950, "1s"),
            (999, "1s"),
            (1000, "1s"),
            (1050, "1.1s"),
            (2500, "2.5s"),
            (9_950, "10s"),
            (99_949, "99.9s"),
            (99_950, "1.7m"),
            (100_050, "1.7m"),
            (1_000_050, "16.7m"),
            (u64::MAX, "2e11d"),
        ] {
            for expanded in [false, true] {
                let block = renderer.tool_preview(
                    BlockId("tool:duration".into()),
                    TargetId("expand:duration".into()),
                    crate::buffer::transcript::ToolHeading {
                        kind: "command",
                        elapsed_ms: Some(elapsed_ms),
                        failed: false,
                        title: "cargo test",
                        expanded,
                    },
                    &ToolOutputPreview {
                        row: vec![],
                        hidden_rows: 0,
                        total_rows: 0,
                    },
                )?;
                assert_eq!(
                    block.text.row(0),
                    Some(format!("• {label:>5} cargo test").as_str())
                );
                assert!(block.metadata.decoration.iter().any(|decoration| {
                    decoration.capture == "ForgeHarnessCommand"
                        && decoration.range.start.column == format!("• {label:>5} ").len()
                }));
            }
        }
        Ok(())
    }

    #[test]
    fn expanded_tool_keeps_its_group_duration_width() -> Result<()> {
        let profile = WidthProfile::default();
        let renderer = TranscriptRenderer::new(&profile)?;
        let heading = renderer.tool_header(
            BlockId("tool".into()),
            TargetId("tool".into()),
            crate::buffer::transcript::ToolHeading {
                kind: "tool_call",
                elapsed_ms: Some(300),
                failed: false,
                title: "inspect({})",
                expanded: false,
            },
        )?;
        assert_eq!(heading.text.row(0), Some("•  0.3s inspect({})"));
        let expanded = TranscriptRenderer::new(&profile)?.refresh_tool_heading(
            &heading,
            "tool_call",
            Some(300),
            false,
            "inspect({})",
            true,
        )?;
        assert_eq!(expanded.text.row(0), heading.text.row(0));
        assert_eq!(expanded.text.row_count(), 1);
        Ok(())
    }

    #[test]
    fn live_tool_preview_reserves_four_rows_without_an_overflow_row() -> Result<()> {
        for columns in [12, 80] {
            let width = WidthProfile {
                columns,
                ..Default::default()
            };
            let renderer = TranscriptRenderer::new(&width)?;
            let mut output = ToolOutputView::new(crate::buffer::identity::DisplayId::test(), "");
            output.live_preview = true;
            for delta in [
                "",
                "one",
                "\ntwo\nthree\nfour\nfive",
                "\n界".repeat(200).as_str(),
            ] {
                output.append(delta)?;
                let body = renderer.tool_body(
                    &crate::buffer::identity::DisplayId::test(),
                    &output,
                    false,
                    0,
                )?;
                assert_eq!(body.text.row_count(), 4);
                assert!(
                    body.text
                        .wire_rows()
                        .iter()
                        .all(|row| width.cells(row, 0).unwrap() <= columns)
                );
                assert_eq!(
                    renderer
                        .tool_hidden(&crate::buffer::identity::DisplayId::test(), &output, false)?
                        .text
                        .row_count(),
                    0
                );
            }
            assert!(output.preview(true)?.row.contains(&"one"));
        }
        Ok(())
    }

    #[test]
    fn tool_headings_stay_single_line_when_expanded_and_resized() -> Result<()> {
        for columns in [12, 40, 90] {
            let profile = WidthProfile {
                columns,
                ..WidthProfile::default()
            };
            let renderer = TranscriptRenderer::new(&profile)?;
            for (kind, title) in [
                (
                    "command",
                    "Get-Content src/game.rs,src/config.rs;\nGet-ChildItem -Force -Name",
                ),
                (
                    "tool_call",
                    r#"sem.context({"path":"src/game.rs","query":"a long query with 界 characters"})"#,
                ),
                (
                    "tool_call",
                    r#"bash({"command":"rg -n 'a long pattern with 界 characters' src/game.rs"})"#,
                ),
            ] {
                let collapsed = renderer.tool_header(
                    BlockId("tool".into()),
                    TargetId("tool".into()),
                    crate::buffer::transcript::ToolHeading {
                        kind,
                        elapsed_ms: Some(400),
                        failed: false,
                        title,
                        expanded: false,
                    },
                )?;
                let expanded = renderer.refresh_tool_heading(
                    &collapsed,
                    kind,
                    Some(400),
                    false,
                    title,
                    true,
                )?;
                assert_eq!(collapsed.text.row_count(), 1);
                assert_eq!(expanded.text, collapsed.text);
                assert!(profile.cells(expanded.text.row(0).unwrap(), 0)? <= columns);
                expanded.validate()?;
            }
        }
        Ok(())
    }

    #[test]
    fn shell_summaries_keep_original_commands_in_expanded_output() -> Result<()> {
        let profile = WidthProfile {
            columns: 100,
            ..WidthProfile::default()
        };
        let renderer = TranscriptRenderer::new(&profile)?;
        for (source, expected) in [
            (
                "pwsh -Command 'Get-Content src/main.rs -TotalCount 40'",
                "Read · src/main.rs · first 40 lines",
            ),
            (
                "bash -lc 'grep -n TODO src/main.rs'",
                "Search · \"TODO\" · src/main.rs · line numbers",
            ),
            ("nu -c 'open --raw src/main.rs'", "Read · src/main.rs · raw"),
        ] {
            let heading = crate::turn::ToolCall {
                id: "shell".into(),
                kind: "command".into(),
                title: source.into(),
                output: Default::default(),
                started_at_ms: Some(0),
                completed_at_ms: Some(100),
                checklist_item_id: None,
                status: "completed".into(),
                failed: false,
                change: Default::default(),
            };
            let mut output = ToolOutputView::new(
                crate::buffer::identity::DisplayId::test(),
                "matching content\n",
            );
            output.heading(&heading);
            let header = renderer.tool_header(
                BlockId("shell".into()),
                TargetId("shell".into()),
                crate::buffer::transcript::ToolHeading {
                    kind: "command",
                    elapsed_ms: Some(100),
                    failed: false,
                    title: source,
                    expanded: false,
                },
            )?;
            assert!(header.text.row(0).unwrap().ends_with(expected));
            let body = renderer.tool_body(
                &crate::buffer::identity::DisplayId::test(),
                &output,
                true,
                0,
            )?;
            let rendered = body.text.wire_rows().join("\n");
            assert!(rendered.contains(&format!("Command: {source}")));
            assert!(rendered.contains("matching content"));
            assert_eq!(output.header().unwrap().title, source);
            header.validate()?;
            body.validate()?;
        }
        assert_eq!(
            tool_heading(&profile, "command", "rg -n TODO src", "1s")?,
            "• 1s rg -n TODO src"
        );
        Ok(())
    }

    #[test]
    fn shell_tool_headings_show_commands_during_execution_and_after_completion() -> Result<()> {
        let profile = WidthProfile {
            columns: 160,
            ..WidthProfile::default()
        };
        let renderer = TranscriptRenderer::new(&profile)?;
        for (source, expected) in [
            (
                r#"bash({"command":"rg -n 'TODO|FIXME' src","description":"Search source","initial_wait":30})"#,
                "rg -n 'TODO|FIXME' src",
            ),
            (
                r#"functions.bash({"command":"bash -lc 'rg -n TODO src'"})"#,
                "rg -n TODO src",
            ),
            (
                r#"powershell({"command":"Get-Content src/main.rs -TotalCount 40"})"#,
                "Read · src/main.rs · first 40 lines",
            ),
            (
                r#"shell({"command":"rg -n TODO src && git status --short"})"#,
                "rg -n TODO src && git status --short",
            ),
            (
                r#"functions.exec_command({"cmd":"rg -n TODO src"})"#,
                "rg -n TODO src",
            ),
            (
                r#"run_shell_command({"command":"nu -c 'open --raw src/main.rs'"})"#,
                "Read · src/main.rs · raw",
            ),
        ] {
            let running = renderer.tool_header(
                BlockId("shell".into()),
                TargetId("shell".into()),
                crate::buffer::transcript::ToolHeading {
                    kind: "tool_call",
                    elapsed_ms: Some(100),
                    failed: false,
                    title: source,
                    expanded: false,
                },
            )?;
            assert_eq!(
                running.text.row(0),
                Some(format!("• {} {expected}", tool_duration(Some(100))).as_str())
            );
            assert!(
                running
                    .metadata
                    .decoration
                    .iter()
                    .any(|decoration| { decoration.capture == "ForgeHarnessCommand" })
            );
            assert!(
                running
                    .metadata
                    .decoration
                    .iter()
                    .all(|decoration| { decoration.capture != "ForgeHarnessMcpName" })
            );
            let completed = renderer.refresh_tool_heading(
                &running,
                "tool_call",
                Some(100),
                false,
                source,
                false,
            )?;
            assert_eq!(completed.text, running.text);
            let expanded_source = tool_source_rows(&profile, "tool_call", source)?.join("\n");
            assert!(expanded_source.contains(source));
            running.validate()?;
            completed.validate()?;
        }
        Ok(())
    }

    #[test]
    fn non_shell_and_incomplete_calls_keep_their_structured_titles() -> Result<()> {
        let profile = WidthProfile {
            columns: 160,
            ..WidthProfile::default()
        };
        for source in [
            r#"rg({"glob":"*.lua","paths":["nvim/lua"],"pattern":"TODO"})"#,
            r#"fixture.inspect({"command":"rg -n TODO src"})"#,
            r#"read_bash({"shellId":"42"})"#,
            r#"bash({"command":["rg","TODO","src"]})"#,
            r#"bash({"command":""})"#,
            r#"bash({"command":"rg"#,
            "bash",
        ] {
            assert!(shell_tool_display(source).is_none(), "{source}");
            assert_eq!(
                tool_heading(&profile, "tool_call", source, "0.1s")?,
                format!("• 0.1s {source}")
            );
        }
        Ok(())
    }

    #[test]
    fn expanded_tools_preserve_arguments_beyond_the_heading_width() -> Result<()> {
        let profile = WidthProfile {
            columns: 48,
            ..WidthProfile::default()
        };
        let renderer = TranscriptRenderer::new(&profile)?;
        let source =
            r#"fixture.inspect({"path":"src/deeply/nested/module.rs","offset":123,"limit":456})"#;
        let call = crate::turn::ToolCall {
            id: "call".into(),
            kind: "tool_call".into(),
            title: source.into(),
            output: Default::default(),
            started_at_ms: Some(0),
            completed_at_ms: Some(100),
            checklist_item_id: None,
            status: "completed".into(),
            failed: false,
            change: Default::default(),
        };
        let mut output =
            ToolOutputView::new(crate::buffer::identity::DisplayId::test(), "tool result\n");
        output.heading(&call);
        let header = renderer.tool_header(
            BlockId("call".into()),
            TargetId("call".into()),
            crate::buffer::transcript::ToolHeading {
                kind: "tool_call",
                elapsed_ms: Some(100),
                failed: false,
                title: source,
                expanded: true,
            },
        )?;
        assert!(!header.text.row(0).unwrap().contains("456"));
        let body = renderer.tool_body(
            &crate::buffer::identity::DisplayId::test(),
            &output,
            true,
            0,
        )?;
        let rows = body.text.wire_rows();
        let joined = rows
            .iter()
            .map(|row| row.trim().trim_start_matches('└').trim())
            .collect::<String>();
        assert!(
            joined.contains(source),
            "expanded call lost arguments: {joined}"
        );
        assert!(joined.contains("tool result"));
        body.validate()?;
        Ok(())
    }

    #[test]
    fn tool_titles_strip_launchers_and_close_truncated_arguments() -> Result<()> {
        let profile = WidthProfile {
            columns: 90,
            ..WidthProfile::default()
        };
        assert_eq!(
            tool_heading(
                &profile,
                "command",
                r#""C:\Program Files\PowerShell\7\pwsh.exe" -NoProfile -Command 'git status --short'"#,
                "2s"
            )?,
            "• 2s git status --short"
        );
        assert_eq!(
            tool_heading(&profile, "command", "bash -lc 'cargo test'", "2s")?,
            "• 2s cargo test"
        );
        assert_eq!(
            crate::permissions::command::display_command(
                "pwsh -File build.ps1",
                crate::permissions::shell::CommandShell::PowerShell
            )
            .source,
            "build.ps1"
        );
        let call = r#"sem.sem_context({"entity_name":"ServiceBusSender","file_path":"service-bus-queue.ts","fresh":true})"#;
        let heading = tool_heading(&profile, "tool_call", call, "2s")?;
        assert!(heading.starts_with("• 2s sem.sem_context({"));
        assert!(heading.ends_with("…\"})"), "{heading}");
        assert!(profile.cells(&heading, 0)? <= profile.columns);
        assert!(!heading.contains('\n'));
        Ok(())
    }

    #[test]
    fn completed_tool_preview_retains_folded_output_and_command_captures() -> Result<()> {
        let profile = WidthProfile::default();
        let renderer = TranscriptRenderer::new(&profile)?;
        let block = renderer.tool_preview(
            BlockId("tool:1".into()),
            TargetId("expand:1".into()),
            crate::buffer::transcript::ToolHeading {
                kind: "command",
                elapsed_ms: Some(2000),
                failed: false,
                title: "cargo test --lib parser",
                expanded: false,
            },
            &ToolOutputPreview {
                row: vec!["first", "second", "third", "fourth"],
                hidden_rows: 96,
                total_rows: 100,
            },
        )?;
        assert_eq!(
            block.text.wire_rows(),
            vec![
                "•    2s cargo test --lib parser",
                "└  first",
                "   second",
                "   third",
                "   fourth",
                "   …(96 earlier lines)"
            ]
        );
        assert_eq!(
            block.metadata.target[0].range.end.row,
            block.text.row_count()
        );
        assert!(block.metadata.decoration.iter().any(|decoration| {
            decoration.capture == "ForgeHarnessCommand"
                && decoration.range.start.column == "•    2s ".len()
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
            crate::buffer::transcript::ToolHeading {
                kind: "command",
                elapsed_ms: Some(2000),
                failed: false,
                title: "cargo check",
                expanded: false,
            },
            &ToolOutputPreview {
                row: vec![],
                hidden_rows: 0,
                total_rows: 0,
            },
        )?;

        assert_eq!(
            block.text.wire_rows(),
            vec!["•    2s cargo check", "└  no output"]
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
        let output = super::super::tool::ToolOutputView::new(
            crate::buffer::identity::DisplayId::test(),
            "one\r\ntwo\nthree\nfour\nfive\n",
        );
        let block = renderer.tool_preview(
            BlockId("active:tool".into()),
            TargetId("active:tool".into()),
            crate::buffer::transcript::ToolHeading {
                kind: "tool_call",
                elapsed_ms: Some(2000),
                failed: true,
                title: "docs_lookup(crate, Item)",
                expanded: false,
            },
            &output.preview(false)?,
        )?;

        assert_eq!(
            block.text.wire_rows(),
            vec![
                "•    2s docs_lookup(crate, Item)",
                "└  two",
                "   three",
                "   four",
                "   five",
                "   …(1 earlier line)"
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
    fn wrapped_tool_output_limits_preview_and_preserves_expansion() -> Result<()> {
        let profile = WidthProfile {
            columns: 24,
            ..WidthProfile::default()
        };
        let renderer = TranscriptRenderer::new(&profile)?;
        let source = format!("{}\nsecond\nthird\n", "界".repeat(60));
        let output = super::super::tool::ToolOutputView::new(
            crate::buffer::identity::DisplayId::test(),
            &source,
        );
        let render = |expanded| {
            renderer.tool_preview(
                BlockId("wrapped:tool".into()),
                TargetId("wrapped:tool".into()),
                crate::buffer::transcript::ToolHeading {
                    kind: "command",
                    elapsed_ms: Some(10),
                    failed: false,
                    title: "inspect",
                    expanded,
                },
                &output.preview(expanded)?,
            )
        };
        let preview = render(false)?;
        let rows = preview.text.wire_rows();
        assert_eq!(
            rows.len(),
            6,
            "preview must contain a heading, four output rows, and a hidden-count row"
        );
        assert_eq!(rows[5], "   …(1 earlier line)");
        assert!(rows.iter().any(|row| row.contains("second")));
        assert!(rows.iter().any(|row| row.contains("third")));
        let expanded = render(true)?;
        let restored = expanded
            .text
            .wire_rows()
            .iter()
            .skip(1)
            .map(|row| {
                row.strip_prefix("└  ")
                    .or_else(|| row.strip_prefix("   "))
                    .unwrap()
            })
            .collect::<String>();
        assert_eq!(restored, source.replace('\n', ""));
        preview.validate()?;
        expanded.validate()?;
        Ok(())
    }

    #[test]
    fn narrow_window_keeps_prompt_marker_and_source() -> Result<()> {
        let profile = WidthProfile {
            columns: 1,
            ..WidthProfile::default()
        };
        let block = TranscriptRenderer::new(&profile)?.prompt(BlockId("prompt:1".into()), "abc")?;
        assert_eq!(block.text.wire_rows().concat(), "● abc");
        assert_eq!(
            block.metadata.decoration[0].range.end.row,
            block.text.row_count()
        );
        Ok(())
    }

    #[test]
    fn wrapped_prompt_keeps_continuations_aligned_with_concealed_marker() -> Result<()> {
        let profile = WidthProfile {
            columns: 12,
            ..WidthProfile::default()
        };
        let block = TranscriptRenderer::new(&profile)?
            .prompt(BlockId("prompt:wrapped".into()), "alpha beta gamma")?;

        assert_eq!(block.text.wire_rows(), vec!["● alpha ", "beta gamma"]);
        assert_eq!(block.metadata.decoration[0].range.end.row, 2);
        block.validate()?;
        Ok(())
    }

    #[test]
    fn multiline_prompt_preserves_source_indentation_without_extra_padding() -> Result<()> {
        let profile = WidthProfile::default();
        let block = TranscriptRenderer::new(&profile)?.prompt(
            BlockId("prompt:multiline".into()),
            "first\n  indented\n\nlast",
        )?;
        assert_eq!(
            block.text.wire_rows(),
            vec!["● first", "  indented", "", "last"]
        );
        block.validate()?;
        Ok(())
    }

    #[test]
    fn response_preserves_markdown_source_for_neovim_renderer() -> Result<()> {
        let profile = WidthProfile {
            columns: 14,
            ..WidthProfile::default()
        };
        let rendered = TranscriptRenderer::new(&profile)?.response(
            BlockId("response".into()),
            "A [界 link](https://example.test).\n\nMore text wraps.\r\n\n\r\n",
        )?;
        assert!(rendered.block.metadata.gutter.is_empty());
        assert_eq!(rendered.block.metadata.layout.as_ref().unwrap().indent, 2);
        let marker = rendered
            .block
            .metadata
            .layout
            .as_ref()
            .unwrap()
            .marker
            .as_ref()
            .unwrap();
        assert_eq!(marker.text, "●");
        assert_eq!(marker.capture, "ForgeHarnessResponse");
        assert!(rendered.block.metadata.markdown);
        assert_eq!(rendered.block.metadata.target[0].range.start.column, 2);
        assert_eq!(
            rendered.block.text.wire_rows(),
            vec!["A [界 link](https://example.test).", "", "More text wraps."]
        );
        rendered.block.validate()?;
        Ok(())
    }

    #[test]
    fn response_and_commentary_present_file_labels_with_original_destinations() -> Result<()> {
        let source = "Added [PROJECT.md](/D:/code/test-plan/PROJECT.md) describing the repo layout and [CONTRIBUTING.md](/D:/code/test-plan/CONTRIBUTING.md) with the run command. [web](https://example.test)";
        let expected = "Added PROJECT.md describing the repo layout and CONTRIBUTING.md with the run command. [web](https://example.test)";
        for columns in [60, 113] {
            let profile = WidthProfile {
                columns,
                ..WidthProfile::default()
            };
            let renderer = TranscriptRenderer::new(&profile)?;
            for rendered in [
                renderer.response(BlockId("response".into()), source)?,
                renderer.commentary(BlockId("commentary".into()), source)?,
            ] {
                assert_eq!(rendered.block.text.wire_rows(), vec![expected]);
                assert_eq!(rendered.link.len(), 3);
                assert_eq!(
                    rendered.link[0].destination,
                    "/D:/code/test-plan/PROJECT.md"
                );
                assert_eq!(
                    rendered.link[1].destination,
                    "/D:/code/test-plan/CONTRIBUTING.md"
                );
                let target = &rendered.block.metadata.target[1];
                assert_eq!(
                    &expected[target.range.start.column..target.range.end.column],
                    "CONTRIBUTING.md"
                );
                rendered.block.validate()?;
            }
        }
        Ok(())
    }

    #[test]
    fn response_normalizes_latex_before_markdown_presentation() -> Result<()> {
        let rendered = TranscriptRenderer::new(&WidthProfile::default())?.response(
            BlockId("equation".into()),
            "\\[\nL_o(x,\\omega_o)\n=\nL_e(x,\\omega_o) + \\int_{\\Omega} L_i\n\\]\n\nHere \\(L_o\\) is outgoing radiance.",
        )?;
        assert_eq!(
            rendered.block.text.wire_rows(),
            vec![
                "\\[",
                "L_o(x,\\omega_o) = L_e(x,\\omega_o) + \\int_{\\Omega} L_i",
                "\\]",
                "",
                "Here $L_o$ is outgoing radiance.",
            ]
        );
        assert!(rendered.block.metadata.markdown);
        Ok(())
    }

    #[test]
    fn commentary_preserves_markdown_and_its_link_target() -> Result<()> {
        let rendered = TranscriptRenderer::new(&WidthProfile::default())?.commentary(
            BlockId("commentary".into()),
            "**Review** [details](https://example.test)\n\nNext paragraph.\r\n\n",
        )?;
        assert!(rendered.block.metadata.markdown);
        assert_eq!(
            rendered.block.text.wire_rows(),
            vec![
                "**Review** [details](https://example.test)",
                "",
                "Next paragraph."
            ]
        );
        assert_eq!(
            rendered
                .block
                .metadata
                .layout
                .as_ref()
                .unwrap()
                .marker
                .as_ref()
                .unwrap()
                .text,
            "○"
        );
        assert_eq!(rendered.link[0].destination, "https://example.test");
        Ok(())
    }
}
#[test]
fn single_large_output_line_has_a_bounded_preview() -> Result<()> {
    let source = "λ".repeat(1024 * 1024);
    let rows = tool_output_rows(
        &WidthProfile::default(),
        &ToolOutputPreview {
            row: vec![&source],
            hidden_rows: 0,
            total_rows: 1,
        },
        false,
    )?;
    assert_eq!(rows.len(), 5);
    assert!(rows[4].contains("1 earlier line"));
    assert!(rows.iter().map(String::len).sum::<usize>() < 1024);
    assert_eq!(source.len(), 2 * 1024 * 1024);
    Ok(())
}
