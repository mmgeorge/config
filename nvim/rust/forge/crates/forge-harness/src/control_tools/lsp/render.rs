use anyhow::{Result, ensure};
use serde::Deserialize;
use std::path::Path;

use super::contract::{Arguments, Operation};

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
pub(super) struct Reply {
    pub rows: Vec<Row>,
    pub source: String,
    pub next_offset: Option<u32>,
    pub next_line: Option<u32>,
    pub warnings: Option<Vec<String>>,
    pub hover: Option<String>,
    pub freshness: Option<String>,
    pub message: Option<String>,
    #[serde(default)]
    pub omitted_visibility: Vec<Row>,
    #[serde(default)]
    pub omitted_visibility_count: usize,
}

pub(super) fn render(reply: Reply, arguments: &Arguments, workspace: &Path) -> Result<String> {
    ensure!(
        reply.rows.len() <= 100
            && reply.omitted_visibility.len() <= 16
            && reply.source.len() <= 256,
        "invalid LSP result size"
    );
    let maximum = (arguments.token_budget.unwrap_or(2000) as usize * 4)
        .min(64 * 1024)
        .max(128);
    if arguments.operation == Operation::Paths {
        return render_paths(reply, workspace, maximum);
    }
    let mut output = String::new();
    let mut previous_path = String::new();
    let mut previous_section = String::new();
    let mut previous_signature = None;
    let mut next_offset = reply.next_offset;
    let mut next_line = reply.next_line;
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
        let display = super::display_path(&row.path, workspace);
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
                        let mut indent = 0;
                        let previous = super::display_path(&previous_path, workspace);
                        let previous_parts: Vec<_> = previous.split('/').collect();
                        let parts: Vec<_> = display.split('/').collect();
                        for (part_index, part) in parts.iter().enumerate() {
                            if part_index + 1 == parts.len()
                                || previous_parts.get(part_index) != Some(part)
                            {
                                block.push_str(&format!(
                                    "{}{}{}\n",
                                    "  ".repeat(indent),
                                    part,
                                    if part_index + 1 < parts.len() {
                                        "/"
                                    } else {
                                        ""
                                    }
                                ));
                            }
                            indent += 1;
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
                    super::display_path(path, workspace)
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
    if let Some(hover) = reply.hover.as_ref().filter(|text| !text.is_empty()) {
        if output.len() + hover.len() + 128 <= maximum {
            output.push_str(&format!("\ntype information\n{hover}\n"));
        }
    }
    if reply.rows.is_empty() {
        let partial = reply
            .warnings
            .as_ref()
            .is_some_and(|warnings| !warnings.is_empty())
            || reply.omitted_visibility_count > 0;
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
        let message = super::single_line(message, 256);
        if output.len() + message.len() + 1 <= maximum {
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
    ensure!(
        output.len() <= 64 * 1024,
        "LSP output exceeded transport limit"
    );
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
                super::display_path(&row.path, workspace),
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
        let message = super::single_line(message, 256);
        if output.len() + message.len() + 1 <= maximum {
            output.push_str(&message);
            output.push('\n');
        }
    }
    append_notes(&mut output, &reply, workspace, maximum);
    ensure!(
        output.len() <= 64 * 1024,
        "LSP output exceeded transport limit"
    );
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
                let name = super::single_line(&row.text, 256);
                let line = format!(
                    "  {name:width$}  {}:{}:{}\n",
                    super::display_path(&row.path, workspace),
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
        let warning = super::single_line(warning, 256);
        if output.len() + warning.len() + 12 <= maximum {
            output.push_str(&format!("partial: {warning}\n"));
            rendered += 1;
        }
    }
    if rendered < warnings.len() {
        let summary = "partial: Additional coverage warnings omitted.\n";
        if output.len() + summary.len() <= maximum { output.push_str(summary); }
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
mod tests {
    use super::*;

    #[test]
    fn visibility_omissions_name_declarations_without_retry_advice() {
        use serde_json::json;
        let arguments = Arguments::parse("lsp_overview", json!({})).unwrap();
        let reply = serde_json::from_value(
            json!({"source":"buffer","rows":[],"omitted_visibility_count":2,
            "omitted_visibility":[
                {"path":"/repo/store.rs","line":21,"column":8,"text":"Store::save"},
                {"path":"/repo/store.rs","line":35,"column":8,"text":"Store::flush"}
            ]}),
        )
        .unwrap();
        let output = render(reply, &arguments, Path::new("/repo")).unwrap();
        assert!(output.contains("partial: visibility unknown for 2 omitted declarations\n"));
        assert!(output.contains("Store::save   store.rs:21:8"));
        assert!(output.contains("Store::flush  store.rs:35:8"));
        assert!(!output.contains("retry") && !output.contains("No matching declarations"));
    }

    #[test]
    fn empty_outcomes_keep_partial_and_diagnostic_freshness_distinct() {
        use serde_json::json;
        for (name, arguments, expected) in [
            (
                "lsp_definition",
                json!({"symbol":"save"}),
                "No definitions found.",
            ),
            (
                "lsp_references",
                json!({"symbol":"save"}),
                "No references found in workspace.",
            ),
            (
                "lsp_diagnostics",
                json!({"path":"store.rs"}),
                "No reported diagnostics.",
            ),
            ("lsp_status", json!({}), "No project language servers."),
        ] {
            let arguments = Arguments::parse(name, arguments).unwrap();
            let reply =
                serde_json::from_value(json!({"source":"buffer","rows":[],"freshness":"unknown"}))
                    .unwrap();
            let output = render(reply, &arguments, Path::new("/repo")).unwrap();
            assert!(output.contains(expected), "{output}");
        }
        let arguments = Arguments::parse("lsp_references", json!({"symbol":"save"})).unwrap();
        let reply = serde_json::from_value(
            json!({"source":"buffer","rows":[],"warnings":["Results omitted by read policy"]}),
        )
        .unwrap();
        let output = render(reply, &arguments, Path::new("/repo")).unwrap();
        assert!(!output.contains("No references found"));
        assert!(output.contains("partial: Results omitted by read policy"));
    }

    #[test]
    fn locations_share_a_gutter_without_separators() {
        assert_eq!(
            location(12, Some(9), false, "  pub field: usize"),
            "     12:9      pub field: usize\n"
        );
        assert_eq!(
            location(119, None, true, "    store.save()?;"),
            ">   119          store.save()?;\n"
        );
    }

    #[test]
    fn focused_tools_render_source_without_redundant_query_headers() {
        use serde_json::json;
        let base = json!({"path":"/repo/src/store.rs","line":12,"column":12,"text":"pub struct Store","modified":false});
        for name in ["lsp_overview", "lsp_symbols", "lsp_definition"] {
            let arguments = Arguments::parse(
                name,
                if name == "lsp_definition" {
                    json!({"path":"src/store.rs","line":12,"column":12})
                } else {
                    json!({})
                },
            )
            .unwrap();
            let reply =
                serde_json::from_value(json!({"rows":[base.clone()],"source":"buffer"})).unwrap();
            let expected = if name == "lsp_overview" {
                "src/\n  store.rs\n     12:12   pub struct Store\n"
            } else {
                "src/store.rs\n     12:12   pub struct Store\n"
            };
            assert_eq!(
                render(reply, &arguments, Path::new("/repo")).unwrap(),
                expected,
                "{name}"
            );
        }
        for name in ["lsp_references", "lsp_context"] {
            let input = json!({"path":"src/store.rs","line":12,"column":12});
            let arguments = Arguments::parse(name, input).unwrap();
            let reply = serde_json::from_value(json!({"source":"buffer","rows":[{
                "path":"/repo/src/app.rs","line":20,"column":4,"text":if name == "lsp_context" {""} else {"fn run()"},
                "excerpt":[{"line":20,"text":"fn run() {"},{"line":21,"text":"    store.save();","hit":name != "lsp_context"},{"line":22,"text":"}"}]
            }]})).unwrap();
            let output = render(reply, &arguments, Path::new("/repo")).unwrap();
            assert!(output.starts_with("src/app.rs\n"));
            assert!(output.contains(if name == "lsp_context" {
                "     20:4    fn run() {\n"
            } else {
                ">    21          store.save();\n"
            }));
            assert!(
                !output.contains('|')
                    && !output.contains("defined at")
                    && !output.contains("references:")
            );
        }
        let arguments =
            Arguments::parse("lsp_diagnostics", json!({"path":"src/store.rs"})).unwrap();
        let reply =
            serde_json::from_value(json!({"source":"buffer","freshness":"unknown","rows":[base]}))
                .unwrap();
        assert_eq!(
            render(reply, &arguments, Path::new("/repo")).unwrap(),
            "freshness: unknown\n\n     12:12   pub struct Store\n"
        );
        let arguments = Arguments::parse("lsp_status", json!({})).unwrap();
        let reply = serde_json::from_value(json!({"source":"workspace","rows":[{"path":"/repo","line":1,"column":1,"text":"rust-analyzer  ready\n  definition, context"}]})).unwrap();
        assert_eq!(
            render(reply, &arguments, Path::new("/repo")).unwrap(),
            "rust-analyzer  ready  root=.\n  definition, context\n"
        );
    }

    #[test]
    fn calls_align_root_and_children_without_source_excerpts() {
        use serde_json::json;
        for direction in ["incoming", "outgoing"] {
            let arguments = Arguments::parse(
                "lsp_calls",
                json!({"path":"src/store.rs","line":12,"column":8,"direction":direction}),
            )
            .unwrap();
            let reply = serde_json::from_value(json!({"source":"buffer","rows":[
                {"path":"/repo/src/store.rs","line":12,"column":8,"text":"Store::save"},
                {"path":"/repo/src/app.rs","line":40,"column":4,"text":"Application::persist","indent":1,"offset":0,
                 "excerpt":[{"line":42,"text":"irrelevant source"}]},
                {"path":"/repo/src/cache.rs","line":9,"column":4,"text":"Cache::flush","indent":1,"offset":1}
            ]})).unwrap();
            assert_eq!(
                render(reply, &arguments, Path::new("/repo")).unwrap(),
                "Store::save             src/store.rs:12:8\n  Application::persist  src/app.rs:40:4\n  Cache::flush          src/cache.rs:9:4\n"
            );
        }
    }

    #[test]
    fn calls_paginate_children_without_counting_the_root() {
        use serde_json::json;
        let arguments = Arguments::parse("lsp_calls", json!({"path":"src/store.rs","line":12,"column":8,"direction":"outgoing","token_budget":100})).unwrap();
        let reply = serde_json::from_value(json!({"source":"buffer","rows":[
            {"path":"/repo/src/store.rs","line":12,"column":8,"text":"Store::save"},
            {"path":format!("/repo/{}.rs", "a".repeat(300)),"line":1,"column":1,"text":"flush","indent":1,"offset":0}
        ]})).unwrap();
        let output = render(reply, &arguments, Path::new("/repo")).unwrap();
        assert!(output.starts_with("Store::save"));
        assert!(output.ends_with("more: next_offset=0\n"));
        assert!(!output.contains("flush"));
    }

    #[test]
    fn paths_keep_each_route_complete_and_separate() {
        use serde_json::json;
        let arguments = Arguments::parse(
            "lsp_paths",
            json!({"from":{"symbol":"start"},"to":{"symbol":"end"}}),
        )
        .unwrap();
        let reply = serde_json::from_value(json!({"source":"buffer","rows":[
            {"path":"/repo/a.rs","line":1,"column":4,"text":"start"},
            {"path":"/repo/b.rs","line":3,"column":4,"text":"end","indent":1},
            {"path":"/repo/a.rs","line":1,"column":4,"text":"start"},
            {"path":"/repo/c.rs","line":2,"column":4,"text":"via","indent":1},
            {"path":"/repo/b.rs","line":3,"column":4,"text":"end","indent":2}
        ]}))
        .unwrap();
        let output = render(reply, &arguments, Path::new("/repo")).unwrap();
        assert_eq!(
            output,
            "start    a.rs:1:4\n  end    b.rs:3:4\n\nstart    a.rs:1:4\n  via    c.rs:2:4\n    end  b.rs:3:4\n"
        );
        let small = Arguments::parse(
            "lsp_paths",
            json!({"from":{"symbol":"start"},"to":{"symbol":"end"},"token_budget":50}),
        )
        .unwrap();
        let reply = serde_json::from_value(json!({"source":"buffer","rows":[
            {"path":"/repo/a.rs","line":1,"column":4,"text":"start"},
            {"path":"/repo/b.rs","line":3,"column":4,"text":"x".repeat(100),"indent":1}
        ]}))
        .unwrap();
        let output = render(reply, &small, Path::new("/repo")).unwrap();
        assert!(!output.contains("start"));
        assert!(output.starts_with("partial: Call-path output exceeds token_budget."));
    }

    #[test]
    fn budget_returns_the_first_unrendered_result() {
        use serde_json::json;
        let arguments = Arguments::parse("lsp_symbols", json!({"token_budget":100})).unwrap();
        let reply = serde_json::from_value(json!({"source":"buffer","rows":[
            {"path":"/repo/a.rs","line":1,"column":4,"text":"fn first()","offset":0},
            {"path":"/repo/a.rs","line":2,"column":4,"text":"x".repeat(300),"offset":1}
        ]}))
        .unwrap();
        let output = render(reply, &arguments, Path::new("/repo")).unwrap();
        assert!(output.contains("fn first()") && output.ends_with("more: next_offset=1\n"));
    }
}
