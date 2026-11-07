use anyhow::{Result, ensure};
use serde::Deserialize;
use std::path::Path;

use super::request::{Arguments, Operation};

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct SourceLine {
    pub line: u32,
    pub text: String,
    #[serde(default)]
    pub hit: bool,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct Row {
    pub path: String,
    pub line: u32,
    pub column: u32,
    pub text: String,
    pub modified: Option<bool>,
    #[serde(default)]
    pub indent: usize,
    #[serde(default)]
    pub excerpt: Vec<SourceLine>,
    pub excerpt_path: Option<String>,
    pub offset: Option<u32>,
    pub section: Option<String>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct Coverage {
    pub name: String,
    pub root: String,
    pub state: String,
    pub message: Option<String>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct Reply {
    pub rows: Vec<Row>,
    #[serde(rename = "source")]
    _source: String,
    pub next_offset: Option<u32>,
    pub next_line: Option<u32>,
    pub warnings: Option<Vec<String>>,
    pub hover: Option<String>,
    pub freshness: Option<String>,
    pub message: Option<String>,
    #[serde(default)]
    pub coverage: Vec<Coverage>,
    #[serde(default)]
    pub omitted_visibility: Vec<Row>,
    #[serde(default)]
    pub omitted_visibility_count: usize,
}

pub(super) fn render(reply: Reply, arguments: &Arguments, workspace: &Path) -> Result<String> {
    let maximum = (arguments.token_budget.unwrap_or(2000) as usize * 4).max(128);
    if arguments.operation == Operation::Paths {
        return render_paths(reply, workspace, maximum);
    }
    let mut output = String::new();
    let mut previous_path = String::new();
    let mut previous_section = String::new();
    let mut previous_signature = None;
    let mut next_offset = reply.next_offset;
    let mut next_line = reply.next_line;
    if !reply.coverage.is_empty() {
        let complete = reply
            .coverage
            .iter()
            .all(|coverage| coverage.state == "complete");
        output.push_str(if complete {
            "coverage: complete\n"
        } else {
            "coverage: partial\n"
        });
        for coverage in &reply.coverage {
            ensure!(
                matches!(
                    coverage.state.as_str(),
                    "complete" | "failed" | "unsupported"
                ),
                "invalid LSP coverage state"
            );
            output.push_str(&format!(
                "{} [{}]: {}{}\n",
                coverage.name,
                super::service::display_path(&coverage.root, workspace),
                coverage.state,
                coverage
                    .message
                    .as_ref()
                    .map(|message| format!(" — {message}"))
                    .unwrap_or_default()
            ));
        }
        output.push('\n');
    }
    if let Some(freshness) = &reply.freshness {
        ensure!(
            matches!(freshness.as_str(), "current" | "stale" | "unknown"),
            "invalid diagnostics freshness"
        );
        output.push_str(&format!("freshness: {freshness}\n\n"));
    }
    let call_width = reply
        .rows
        .iter()
        .map(|row| row.indent * 2 + row.text.chars().count())
        .max()
        .unwrap_or(0);
    for (index, row) in reply.rows.iter().enumerate() {
        ensure!(
            row.line > 0 && row.column > 0 && row.indent <= 8,
            "invalid LSP result position"
        );
        let display = super::service::display_path(&row.path, workspace);
        if arguments.operation == Operation::Status {
            let (status, capabilities) = row.text.split_once('\n').unwrap_or((&row.text, ""));
            output.push_str(&format!(
                "{status}  root={}\n{capabilities}\n",
                if display == workspace.to_string_lossy() {
                    "."
                } else {
                    &display
                }
            ));
            continue;
        }
        let signature = (row.line, row.column, row.text.as_str());
        let mut block = String::new();
        let section = row.section.as_deref().unwrap_or("");
        if arguments.operation == Operation::Calls {
            let name = format!("{}{}", "  ".repeat(row.indent), row.text);
            block.push_str(&format!(
                "{name:call_width$}  {display}:{}:{}{}\n",
                row.line,
                row.column,
                if row.modified == Some(true) {
                    " (unsaved)"
                } else {
                    ""
                }
            ));
        } else {
            if section != previous_section && !section.is_empty() {
                block.push_str(&format!("\n{section}\n"));
            }
            if row.path != previous_path || section != previous_section {
                if !output.is_empty() && !output.ends_with("\n\n") {
                    block.push('\n');
                }
                if arguments.operation != Operation::Diagnostics {
                    if arguments.operation == Operation::Overview {
                        let previous = super::service::display_path(&previous_path, workspace);
                        let previous_parts: Vec<_> = previous.split('/').collect();
                        let parts: Vec<_> = display.split('/').collect();
                        for (part_index, part) in parts.iter().enumerate() {
                            if part_index + 1 == parts.len()
                                || previous_parts.get(part_index) != Some(part)
                            {
                                block.push_str(&format!(
                                    "{}{}{}\n",
                                    "  ".repeat(part_index),
                                    part,
                                    if part_index + 1 < parts.len() {
                                        "/"
                                    } else {
                                        ""
                                    }
                                ));
                            }
                        }
                    } else {
                        block.push_str(&format!(
                            "{display}{}\n",
                            if row.modified == Some(true) {
                                " (unsaved)"
                            } else {
                                ""
                            }
                        ));
                    }
                }
                previous_signature = None;
            }
            if !row.text.is_empty() && previous_signature != Some(signature) {
                block.push_str(&location(
                    row.line,
                    Some(row.column),
                    false,
                    &format!("{}{}", "  ".repeat(row.indent), row.text),
                ));
            }
            if let Some(path) = &row.excerpt_path {
                block.push_str(&format!(
                    "  call sites: {}\n",
                    super::service::display_path(path, workspace)
                ));
            }
            for line in &row.excerpt {
                let column = (arguments.operation == Operation::Context
                    && row.text.is_empty()
                    && line.line == row.line)
                    .then_some(row.column);
                block.push_str(&location(line.line, column, line.hit, &line.text));
            }
        }
        if output.len() + block.len() + 128 > maximum && !output.is_empty() {
            if arguments.operation == Operation::Context {
                if row.section.is_none() {
                    next_line = row.excerpt.first().map(|line| line.line);
                } else {
                    output.push_str("partial: related summaries exceeded token_budget\n");
                }
            } else {
                next_offset = Some(
                    row.offset
                        .unwrap_or(arguments.offset.unwrap_or(0) + index as u32),
                );
            }
            break;
        }
        if output.len() + block.len() + 128 > maximum {
            output.push_str("partial: Next result exceeds token_budget.\n");
            if arguments.operation == Operation::Context {
                next_line = row.excerpt.first().map(|line| line.line);
            } else {
                next_offset = Some(
                    row.offset
                        .unwrap_or(arguments.offset.unwrap_or(0) + index as u32),
                );
            }
            break;
        }
        output.push_str(&block);
        previous_path.clone_from(&row.path);
        previous_section = section.to_owned();
        previous_signature = Some(signature);
    }
    if let Some(hover) = reply.hover.as_ref().filter(|text| !text.is_empty())
        && output.len() + hover.len() + 128 <= maximum
    {
        output.push_str(&format!("\ntype information\n{hover}\n"));
    }
    if reply.rows.is_empty() {
        let partial = reply
            .warnings
            .as_ref()
            .is_some_and(|warnings| !warnings.is_empty())
            || reply.omitted_visibility_count > 0
            || reply
                .coverage
                .iter()
                .any(|coverage| coverage.state != "complete");
        let message = if partial {
            "No results returned.\n"
        } else {
            match arguments.operation {
                Operation::Definition => match arguments.kind.as_deref() {
                    Some("type") => "No type definitions found.\n",
                    Some("implementation") => "No implementations found.\n",
                    _ => "No definitions found.\n",
                },
                Operation::References => {
                    if arguments.scope.as_deref().unwrap_or("workspace") == "workspace" {
                        "No references found in workspace.\n"
                    } else {
                        "No references found.\n"
                    }
                }
                Operation::Diagnostics => "No reported diagnostics.\n",
                Operation::Status => "No project language servers.\n",
                Operation::Overview | Operation::Symbols => "No matching declarations found.\n",
                _ => "No results.\n",
            }
        };
        output.push_str(message);
    }
    if let Some(message) = &reply.message {
        let message = super::service::single_line(message, 256);
        if output.len() + message.len() < maximum {
            output.push_str(&format!("{message}\n"));
        }
    }
    if let Some(line) = next_line {
        output.push_str(&format!("truncated: next_line={line}\n"));
    }
    if let Some(offset) = next_offset {
        output.push_str(&format!("more: next_offset={offset}\n"));
    }
    append_notes(&mut output, &reply, workspace, maximum);
    Ok(output)
}

fn render_paths(reply: Reply, workspace: &Path, maximum: usize) -> Result<String> {
    let width = reply
        .rows
        .iter()
        .map(|row| row.indent * 2 + row.text.chars().count())
        .max()
        .unwrap_or(0);
    let mut output = String::new();
    let mut index = 0;
    while index < reply.rows.len() {
        let mut block = String::new();
        ensure!(
            reply.rows[index].indent == 0,
            "call path must start at its root"
        );
        for depth in 0..9 {
            let Some(row) = reply.rows.get(index) else {
                break;
            };
            if depth > 0 && row.indent == 0 {
                break;
            }
            ensure!(
                row.line > 0 && row.column > 0 && row.indent == depth,
                "invalid call path position"
            );
            let name = format!("{}{}", "  ".repeat(depth), row.text);
            block.push_str(&format!(
                "{name:width$}  {}:{}:{}{}\n",
                super::service::display_path(&row.path, workspace),
                row.line,
                row.column,
                if row.modified == Some(true) {
                    " (unsaved)"
                } else {
                    ""
                }
            ));
            index += 1;
        }
        if output.len() + block.len() + 128 > maximum {
            output.push_str("partial: Call-path output exceeds token_budget.\n");
            break;
        }
        if !output.is_empty() {
            output.push('\n');
        }
        output.push_str(&block);
    }
    if let Some(message) = &reply.message {
        let message = super::service::single_line(message, 256);
        if output.len() + message.len() < maximum {
            output.push_str(&message);
            output.push('\n');
        }
    }
    append_notes(&mut output, &reply, workspace, maximum);
    Ok(output)
}

fn append_notes(output: &mut String, reply: &Reply, workspace: &Path, maximum: usize) {
    if reply.omitted_visibility_count > 0 {
        let header = format!(
            "partial: visibility unknown for {} omitted declarations\n",
            reply.omitted_visibility_count
        );
        if output.len() + header.len() <= maximum {
            output.push_str(&header);
            let width = reply
                .omitted_visibility
                .iter()
                .map(|row| row.text.chars().count())
                .max()
                .unwrap_or(0);
            let mut shown = 0;
            for row in &reply.omitted_visibility {
                let name = super::service::single_line(&row.text, 256);
                let line = format!(
                    "  {name:width$}  {}:{}:{}\n",
                    super::service::display_path(&row.path, workspace),
                    row.line,
                    row.column
                );
                if output.len() + line.len() + 48 > maximum {
                    break;
                }
                output.push_str(&line);
                shown += 1;
            }
            if shown < reply.omitted_visibility_count {
                let remaining = format!(
                    "  {} additional declarations omitted.\n",
                    reply.omitted_visibility_count - shown
                );
                if output.len() + remaining.len() <= maximum {
                    output.push_str(&remaining);
                }
            }
        }
    }
    let warnings = reply.warnings.as_deref().unwrap_or_default();
    let mut rendered = 0;
    for warning in warnings.iter().take(8) {
        let warning = super::service::single_line(warning, 256);
        if output.len() + warning.len() + 12 <= maximum {
            output.push_str(&format!("partial: {warning}\n"));
            rendered += 1;
        }
    }
    if rendered < warnings.len() {
        let summary = "partial: Additional coverage warnings omitted.\n";
        if output.len() + summary.len() <= maximum {
            output.push_str(summary);
        }
    }
}

fn location(line: u32, column: Option<u32>, hit: bool, text: &str) -> String {
    let column = column
        .map(|column| format!(":{column:<3}"))
        .unwrap_or_else(|| "    ".into());
    format!(
        "{}{line:>5}{column}  {text}\n",
        if hit { "> " } else { "  " }
    )
}

#[cfg(test)]
mod tests {}
