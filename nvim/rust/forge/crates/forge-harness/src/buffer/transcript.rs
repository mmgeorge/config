use anyhow::{Result, ensure};
use forge_buffer::block::{
    BlockMetadata, BufferBlock, Conceal, ContentLayout, Decoration, TargetRange, TextChunk, TextPosition, TextRange,
};
use forge_buffer::identity::{BlockId, TargetId};
use forge_buffer::markdown::{MarkdownRenderer, RenderedMarkdown};
use forge_buffer::text::BufferText;
use forge_buffer::width::WidthProfile;

use super::markdown_math;
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
        Self::heading_marker(&mut block, "ForgeHarnessPrompt");
        Ok(block)
    }

    pub fn response(&self, id: BlockId, source: &str) -> Result<RenderedMarkdown> {
        let source = markdown_math::normalize(source);
        let mut rendered = MarkdownRenderer::source(id, &source, self.profile)?;
        rendered.block.metadata.markdown = true;
        rendered.block.metadata.layout = Some(ContentLayout {
            indent: 2, marker: None, source_indent: 0,
        });
        Ok(rendered)
    }

    pub fn commentary(&self, id: BlockId, source: &str) -> Result<RenderedMarkdown> {
        let source = markdown_math::normalize(source);
        let mut rendered = MarkdownRenderer::source(id, &source, self.profile)?;
        let block = &mut rendered.block;
        block.metadata.markdown = true;
        block.metadata.decoration.clear();
        block.metadata.layout = Some(ContentLayout {
            indent: 2,
            source_indent: 0,
            marker: Some(TextChunk {
                text: "↳".into(),
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
        if block.text.row(0).is_some_and(|row| row.starts_with("▸ ")) {
            block.metadata.layout = Some(ContentLayout {
                indent: 2,
                source_indent: 0,
                marker: Some(TextChunk { text: "▸".into(), capture: capture.into() }),
            });
            block.metadata.conceal.push(Conceal {
                range: TextRange { start: TextPosition { row: 0, column: 0 },
                    end: TextPosition { row: 0, column: "▸ ".len() } },
                replacement: String::new(), line: false, priority: 100,
            });
        }
    }

    pub fn tool_preview(
        &self,
        id: BlockId,
        target: TargetId,
        kind: &str,
        _status: &str,
        failed: bool,
        title: &str,
        output: &ToolOutputPreview<'_>,
        expanded: bool,
    ) -> Result<BufferBlock> {
        id.validate()?;
        target.validate()?;
        ensure!(
            title.len() <= 4096,
            "tool title exceeds transcript capacity"
        );
        let arguments = expanded.then(|| title.split_once('('))
            .flatten().filter(|_| kind == "tool_call")
            .and_then(|(name, arguments)| arguments.strip_suffix(')').map(|arguments| (name, arguments)));
        let mut row = if let Some((name, arguments)) = arguments {
            let mut row = vec![tool_heading(self.profile, kind, name)?];
            row.extend(tool_body_rows(self.profile, arguments, true)?);
            row
        } else if expanded {
            self.profile.wrap_plain(&format!("  • {title}"), 4.min(self.profile.columns - 1))?
        } else {
            vec![tool_heading(self.profile, kind, title)?]
        };
        let title_rows = row.len();
        for (index, text) in output.row.iter().enumerate() {
            row.extend(tool_body_rows(self.profile, text, index == 0)?);
        }
        if output.row.is_empty() {
            row.extend(tool_body_rows(self.profile, "no output", true)?);
        }
        if output.hidden_rows > 0 {
            row.extend(tool_body_rows(self.profile,
                &format!("…({} hidden)", output.hidden_rows), false,
            )?);
        }
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
        decorate_tool_heading(&mut block, if arguments.is_some() { 1 } else { title_rows }, kind, failed);
        if arguments.is_some() && title_rows > 1 {
            block.metadata.decoration.push(Decoration {
                range: TextRange {
                    start: TextPosition { row: 1, column: 0 },
                    end: TextPosition { row: title_rows, column: 0 },
                },
                capture: "ForgeHarnessMcpArguments".into(),
                priority: 110,
            });
        }
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

/// Wraps body content before adding its marker so long tokens cannot strand the marker.
fn tool_body_rows(profile: &WidthProfile, text: &str, branch: bool) -> Result<Vec<String>> {
    let mut content_profile = profile.clone();
    let margin = 6.min(content_profile.columns - 1);
    content_profile.columns -= margin;
    Ok(content_profile.wrap_plain(text, 0)?.into_iter().enumerate().map(|(index, text)| {
        let prefix = if branch && index == 0 && margin == 6 { "    └ ".into() } else { " ".repeat(margin) };
        format!("{prefix}{text}")
    }).collect())
}

/// Formats a bounded display title without changing the retained provider call.
fn tool_heading(profile: &WidthProfile, kind: &str, title: &str) -> Result<String> {
    let title = if kind == "command" { shell_command(title) } else { title };
    let normalized = title.split_whitespace().collect::<Vec<_>>().join(" ");
    let full = format!("  • {normalized}");
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
                if escaped { escaped = false; }
                else if character == '\\' { escaped = true; }
                else if character == '"' { quoted = false; }
            } else {
                match character {
                    '"' => quoted = true,
                    '(' => closing.push(')'),
                    '{' => closing.push('}'),
                    '[' => closing.push(']'),
                    ')' | '}' | ']' => { closing.pop(); }
                    _ => {}
                }
            }
        }
        if escaped { continue; }
        let suffix: String = closing.iter().rev().collect();
        let candidate = format!("{prefix}…{}{suffix}", if quoted { "\"" } else { "" });
        if profile.cells(&candidate, 0)? > profile.columns { break; }
        best = candidate;
    }
    Ok(best)
}

/// Removes recognized shell launchers only when their command switch is present.
fn shell_command(title: &str) -> &str {
    let title = title.trim();
    let (executable, mut remaining) = shell_token(title);
    let executable = executable.rsplit(['/', '\\']).next().unwrap_or(executable).to_ascii_lowercase();
    let powershell = matches!(executable.as_str(), "pwsh" | "pwsh.exe" | "powershell" | "powershell.exe");
    let posix = matches!(executable.as_str(), "sh" | "bash" | "zsh" | "fish");
    if !powershell && !posix { return title; }
    while !remaining.is_empty() {
        let (argument, tail) = shell_token(remaining);
        if (powershell && argument.eq_ignore_ascii_case("-command"))
            || (posix && matches!(argument, "-c" | "-lc" | "-ic")) {
            let command = tail.trim();
            if command.is_empty() { return title; }
            if let Some(quote) = command.chars().next().filter(|value| matches!(value, '\'' | '"')) {
                if command.len() >= 2 && command.ends_with(quote) {
                    return &command[1..command.len() - 1];
                }
            }
            return command;
        }
        if !matches!(argument.to_ascii_lowercase().as_str(), "-noprofile" | "-nologo" | "-noninteractive" | "-l") {
            return title;
        }
        remaining = tail;
    }
    title
}

/// Separates a launcher argument while retaining the command payload verbatim.
fn shell_token(text: &str) -> (&str, &str) {
    let text = text.trim_start();
    if let Some(quote) = text.chars().next().filter(|value| matches!(value, '\'' | '"')) {
        if let Some(end) = text[1..].find(quote) {
            return (&text[1..end + 1], text[end + 2..].trim_start());
        }
    }
    let end = text.find(char::is_whitespace).unwrap_or(text.len());
    (&text[..end], text[end..].trim_start())
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
            text.find("• ").map_or(text.len(), |column| column + "• ".len())
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
            text.find("• ")
                .map(|column| column + "• ".len())
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
    fn expanded_mcp_keeps_name_arguments_and_response_on_distinct_rows() -> Result<()> {
        let profile = WidthProfile { columns: 70, ..WidthProfile::default() };
        let renderer = TranscriptRenderer::new(&profile)?;
        let arguments = r#"{"entity_name":"CosmosDbClient","file_path":"cosmos-db-client.ts","hops":1,"token_budget":3500}"#;
        let block = renderer.tool_preview(
            BlockId("tool".into()), TargetId("tool".into()), "tool_call", "completed", false,
            &format!("sem.sem_context({arguments})"),
            &ToolOutputPreview { row: vec!["response"], hidden_rows: 0, total_rows: 1 }, true,
        )?;
        let rows = block.text.wire_rows();
        assert_eq!(rows[0], "  • sem.sem_context");
        assert!(rows[1].starts_with("    └ {\"entity_name\""));
        assert_eq!(rows.last(), Some(&"    └ response"));
        let restored = rows[1..rows.len() - 1].iter().enumerate()
            .map(|(index, row)| if index == 0 { row.trim_start_matches("    └ ") } else { row.trim_start() })
            .collect::<String>();
        assert_eq!(restored, arguments);
        assert!(rows.iter().all(|row| profile.cells(row, 0).unwrap() <= profile.columns));
        Ok(())
    }

    #[test]
    fn long_json_response_starts_beside_branch_in_preview_and_expansion() -> Result<()> {
        let response = r#"{"ok":false,"tool":"harness_plan_read","phase":"semantic_execution","code":"semantic_execution_failed","message":"plan controls require Harness Plan mode"}"#;
        for columns in [40, 70, 120] {
            let profile = WidthProfile { columns, ..WidthProfile::default() };
            let renderer = TranscriptRenderer::new(&profile)?;
            for expanded in [false, true] {
                let block = renderer.tool_preview(
                    BlockId("tool".into()), TargetId("tool".into()), "tool_call", "completed", true,
                    "harness_plan_read", &ToolOutputPreview { row: vec![response], hidden_rows: 0, total_rows: 1 }, expanded,
                )?;
                let rows = block.text.wire_rows();
                assert!(rows[1].starts_with("    └ {\"ok\":false"));
                let restored = rows[1..].iter().enumerate().map(|(index, row)| {
                    if index == 0 { row.strip_prefix("    └ ").unwrap() } else { row.strip_prefix("      ").unwrap() }
                }).collect::<String>();
                assert_eq!(restored, response);
                assert!(rows.iter().all(|row| profile.cells(row, 0).unwrap() <= columns));
            }
        }
        Ok(())
    }

    #[test]
    fn tool_titles_strip_launchers_and_close_truncated_arguments() -> Result<()> {
        let profile = WidthProfile { columns: 90, ..WidthProfile::default() };
        assert_eq!(tool_heading(&profile, "command", r#""C:\Program Files\PowerShell\7\pwsh.exe" -NoProfile -Command 'git status --short'"#)?, "  • git status --short");
        assert_eq!(tool_heading(&profile, "command", "bash -lc 'cargo test'" )?, "  • cargo test");
        assert_eq!(shell_command("pwsh -File build.ps1"), "pwsh -File build.ps1");
        let call = r#"sem.sem_context({"entity_name":"ServiceBusSender","file_path":"service-bus-queue.ts","fresh":true})"#;
        let heading = tool_heading(&profile, "tool_call", call)?;
        assert!(heading.starts_with("  • sem.sem_context({"));
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
            "command",
            "completed",
            false,
            "cargo test --lib parser",
            &ToolOutputPreview {
                row: vec!["first", "second", "third", "fourth"],
                hidden_rows: 96,
                total_rows: 100,
            },
            false,
        )?;
        assert_eq!(
            block.text.wire_rows(),
            vec![
                "  • cargo test --lib parser",
                "    └ first",
                "      second",
                "      third",
                "      fourth",
                "      …(96 hidden)"
            ]
        );
        assert_eq!(block.metadata.target[0].range.end.row, block.text.row_count());
        assert!(block.metadata.decoration.iter().any(|decoration| {
            decoration.capture == "ForgeHarnessCommand"
                && decoration.range.start.column == "  • ".len()
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
                row: vec![],
                hidden_rows: 0,
                total_rows: 0,
            },
            false,
        )?;

        assert_eq!(
            block.text.wire_rows(),
            vec!["  • cargo check", "    └ no output"]
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
        let output = super::super::tool::ToolOutputView::new("preview".into(), "one\r\ntwo\nthree\nfour\nfive\n".into())?;
        let block = renderer.tool_preview(
            BlockId("active:tool".into()),
            TargetId("active:tool".into()),
            "tool_call",
            "in_progress",
            true,
            "docs_lookup(crate, Item)",
            &output.preview(false),
            false,
        )?;

        assert_eq!(
            block.text.wire_rows(),
            vec![
                "  • docs_lookup(crate, Item)",
                "    └ one",
                "      two",
                "      three",
                "      four",
                "      …(1 hidden)"
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
    fn response_preserves_markdown_source_for_neovim_renderer() -> Result<()> {
        let profile = WidthProfile {
            columns: 14,
            ..WidthProfile::default()
        };
        let rendered = TranscriptRenderer::new(&profile)?.response(
            BlockId("response".into()),
            "A [界 link](https://example.test).\n\nMore text wraps.",
        )?;
        assert!(rendered.block.metadata.gutter.is_empty());
        assert_eq!(rendered.block.metadata.layout.as_ref().unwrap().indent, 2);
        assert!(rendered.block.metadata.layout.as_ref().unwrap().marker.is_none());
        assert!(rendered.block.metadata.markdown);
        assert_eq!(rendered.block.metadata.target[0].range.start.column, 2);
        assert_eq!(rendered.block.text.wire_rows(), vec![
            "A [界 link](https://example.test).", "", "More text wraps."
        ]);
        rendered.block.validate()?;
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
            "**Review** [details](https://example.test)",
        )?;
        assert!(rendered.block.metadata.markdown);
        assert_eq!(rendered.block.text.wire_rows(), vec!["**Review** [details](https://example.test)"]);
        assert_eq!(rendered.block.metadata.layout.as_ref().unwrap().marker.as_ref().unwrap().text, "↳");
        assert_eq!(rendered.link[0].destination, "https://example.test");
        Ok(())
    }
}
