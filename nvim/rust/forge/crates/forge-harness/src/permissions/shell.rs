use super::command::{CommandInvocation, CommandNormalization, shell_token_list};
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::ops::{ControlFlow, Range};
use std::time::{Duration, Instant};
use tree_sitter::{Language, Node, Parser, Tree};

#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CommandShell {
    #[serde(rename = "powershell")]
    PowerShell,
    Bash,
    Zsh,
    Nushell,
    Unknown,
}

impl Default for CommandShell {
    fn default() -> Self {
        if cfg!(windows) {
            Self::PowerShell
        } else {
            std::env::var("SHELL").map_or(Self::Zsh, |shell| Self::from_executable(&shell))
        }
    }
}

impl CommandShell {
    pub fn from_executable(executable: &str) -> Self {
        let name = executable.rsplit(['/', '\\']).next().unwrap_or(executable);
        match name.to_ascii_lowercase().trim_end_matches(".exe") {
            "pwsh" | "powershell" => Self::PowerShell,
            "bash" | "sh" => Self::Bash,
            "zsh" => Self::Zsh,
            "nu" | "nushell" => Self::Nushell,
            _ => Self::Unknown,
        }
    }

    fn grammar(self) -> Option<Language> {
        match self {
            Self::PowerShell => Some(tree_sitter_powershell::LANGUAGE.into()),
            Self::Bash => Some(tree_sitter_bash::LANGUAGE.into()),
            Self::Zsh => Some(tree_sitter_zsh::LANGUAGE.into()),
            Self::Nushell => Some(forge_diff::syntax::SyntaxLanguage::Nu.grammar()),
            Self::Unknown => None,
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Carries a syntax capture in zero-based, end-exclusive source byte coordinates.
pub struct CommandHighlight {
    /// Locates the captured token in the unformatted command.
    pub range: Range<usize>,
    /// Names the foreground highlight used by the shared picker renderer.
    pub group: String,
    /// Orders nested captures above their containing expression.
    pub priority: usize,
}

fn parse_tree(command: &str, shell: CommandShell) -> Option<Tree> {
    let language = shell.grammar()?;
    if command.trim().is_empty() || command.len() > 65_536 {
        return None;
    }
    let mut parser = Parser::new();
    let parse_source = parse_source(command, shell);
    let started = Instant::now();
    let mut progress = |_: &tree_sitter::ParseState| {
        if started.elapsed() >= Duration::from_millis(25) {
            ControlFlow::Break(())
        } else {
            ControlFlow::Continue(())
        }
    };
    parser.set_language(&language).ok().and_then(|()| {
        parser.parse_with_options(
            &mut |offset, _| &parse_source.as_bytes()[offset..],
            None,
            Some(tree_sitter::ParseOptions::new().progress_callback(&mut progress)),
        )
    })
}

/// Colors shell syntax independently of whether permission analysis can resolve execution.
pub fn highlights(command: &str, shell: CommandShell) -> Vec<CommandHighlight> {
    let Some(tree) = parse_tree(command, shell) else {
        return Vec::new();
    };
    let mut result = Vec::new();
    let mut pending = vec![(tree.root_node(), 0)];
    let mut visited = 0;
    while let Some((node, depth)) = pending.pop() {
        visited += 1;
        if depth > 64 || visited > 8192 {
            break;
        }
        let group = match node.kind() {
            "comment" | "line_comment" | "block_comment" => Some("Comment"),
            "command_parameter" | "long_flag" | "short_flag" => Some("ForgeHarnessOption"),
            "string_literal"
            | "string"
            | "raw_string"
            | "ansi_c_string"
            | "val_string"
            | "val_interpolated"
            | "here_string_literal" => Some("String"),
            "variable" | "braced_variable" | "val_variable" | "variable_name"
            | "simple_expansion" | "variable_ref" => Some("Identifier"),
            "integer_literal" | "real_literal" | "number" | "val_number" | "val_int"
            | "val_float" => Some("Number"),
            "word" if text(node, command).starts_with('-') => Some("ForgeHarnessOption"),
            _ if !node.is_named() => match text(node, command) {
                "if" | "else" | "elseif" | "elif" | "then" | "fi" | "for" | "foreach" | "while"
                | "until" | "do" | "done" | "switch" | "case" | "esac" | "try" | "catch"
                | "finally" | "match" | "loop" | "in" | "break" | "continue" | "return" | "let"
                | "mut" | "repeat" | "always" => Some("Keyword"),
                "{" | "}" | "(" | ")" | "[" | "]" => Some("Delimiter"),
                "|" | "||" | "&&" | ";" | "=" | "=>" | "!" => Some("Operator"),
                _ => None,
            },
            _ => None,
        };
        if let Some(group) = group {
            result.push(CommandHighlight {
                range: node.byte_range(),
                group: group.into(),
                priority: depth,
            });
        }
        if node.kind() == "command" {
            let field = match shell {
                CommandShell::PowerShell => "command_name",
                CommandShell::Nushell => "head",
                _ => "name",
            };
            let mut cursor = node.walk();
            if let Some(name) = node
                .children_by_field_name(field, &mut cursor)
                .find(|child| child.is_named())
            {
                result.push(CommandHighlight {
                    range: name.byte_range(),
                    group: "ForgeHarnessCommand".into(),
                    priority: depth,
                });
            }
        }
        let mut cursor = node.walk();
        pending.extend(
            node.children(&mut cursor)
                .collect::<Vec<_>>()
                .into_iter()
                .rev()
                .map(|child| (child, depth + 1)),
        );
    }
    result
}

pub(super) fn parse(command: &str, shell: CommandShell) -> CommandNormalization {
    let mut result = CommandNormalization::default();
    let Some(tree) = parse_tree(command, shell) else {
        result.ambiguous = true;
        return result;
    };
    if tree.root_node().has_error() {
        result.ambiguous = true;
        return result;
    }
    let mut pending = vec![(tree.root_node(), 0)];
    let mut visited = 0;
    let mut validated_structure = false;
    while let Some((node, depth)) = pending.pop() {
        visited += 1;
        if depth > 64 || visited > 8192 || unsupported(node, command, shell) {
            result.ambiguous = true;
            break;
        }
        if node.kind() == "command" {
            match invocation(node, command, shell) {
                Ok(Some(invocation)) => result.invocation_list.push(invocation),
                Ok(None) => {
                    validated_structure = true;
                }
                Err(()) => {
                    result.ambiguous = true;
                    break;
                }
            }
        }
        validated_structure |= match shell {
            CommandShell::PowerShell => matches!(
                node.kind(),
                "if_statement"
                    | "foreach_statement"
                    | "for_statement"
                    | "while_statement"
                    | "do_statement"
                    | "switch_statement"
                    | "try_statement"
            ),
            CommandShell::Bash | CommandShell::Zsh => matches!(
                node.kind(),
                "test_command"
                    | "if_statement"
                    | "for_statement"
                    | "while_statement"
                    | "case_statement"
            ),
            CommandShell::Nushell => {
                node.kind().starts_with("ctrl_") || node.kind() == "where_command"
            }
            CommandShell::Unknown => false,
        };
        let mut cursor = node.walk();
        let children = node.named_children(&mut cursor).collect::<Vec<_>>();
        pending.extend(children.into_iter().rev().map(|child| (child, depth + 1)));
    }
    if result.invocation_list.is_empty() && !validated_structure {
        result.ambiguous = true;
    }
    result
}

// The pinned grammar confuses a leading ForEach-Object with the foreach keyword.
// Replace only its hyphen in the parser input. All spans still address the original source.
fn parse_source(source: &str, shell: CommandShell) -> Cow<'_, str> {
    if shell != CommandShell::PowerShell {
        return Cow::Borrowed(source);
    }
    let mut rewritten = source.as_bytes().to_vec();
    for (start, window) in source.as_bytes().windows(14).enumerate() {
        if window.eq_ignore_ascii_case(b"ForEach-Object")
            && (start == 0 || b" \t\r\n|;&{(".contains(&source.as_bytes()[start - 1]))
        {
            rewritten[start + 7] = b'_';
        }
    }
    Cow::Owned(String::from_utf8(rewritten).expect("ASCII substitution preserves UTF-8"))
}

fn text<'source>(node: Node<'_>, source: &'source str) -> &'source str {
    &source[node.byte_range()]
}

fn contains(node: Node<'_>, kind: &str) -> bool {
    let mut pending = vec![node];
    while let Some(node) = pending.pop() {
        if node.kind() == kind {
            return true;
        }
        let mut cursor = node.walk();
        pending.extend(node.named_children(&mut cursor));
    }
    false
}

fn argument_nodes(node: Node<'_>) -> Vec<Node<'_>> {
    let mut result = Vec::new();
    let mut pending = vec![node];
    while let Some(child) = pending.pop() {
        if child != node
            && matches!(
                child.kind(),
                "command" | "sub_expression" | "expr_parenthesized"
            )
        {
            continue;
        }
        result.push(child);
        if matches!(child.kind(), "script_block_expression" | "val_closure") {
            continue;
        }
        let mut cursor = child.walk();
        pending.extend(child.named_children(&mut cursor));
    }
    result
}

fn unsupported(node: Node<'_>, source: &str, shell: CommandShell) -> bool {
    match shell {
        CommandShell::PowerShell => match node.kind() {
            "function_statement"
            | "class_statement"
            | "enum_statement"
            | "data_statement"
            | "inlinescript_statement"
            | "parallel_statement"
            | "sequence_statement"
            | "invokation_expression"
            | "invokation_foreach_expression"
            | "cast_expression"
            | "type_literal"
            | "redirection"
            | "stop_parsing"
            | "switch_filename" => true,
            "variable" | "braced_variable" => {
                text(node, source).to_ascii_lowercase().contains("$env:")
            }
            "command_invokation_operator" => text(node, source) == ".",
            _ => false,
        },
        CommandShell::Bash | CommandShell::Zsh => {
            (shell == CommandShell::Zsh
                && node.kind() == "expansion"
                && text(node, source).starts_with("${~"))
                || matches!(
                    node.kind(),
                    "function_definition"
                        | "file_redirect"
                        | "heredoc_redirect"
                        | "herestring_redirect"
                        | "variable_assignment"
                        | "declaration_command"
                        | "unset_command"
                        | "expansion_flags"
                        | "qualified_expression"
                        | "arithmetic_call"
                )
        }
        CommandShell::Nushell => match node.kind() {
            "decl_def" | "decl_alias" | "decl_extern" | "decl_module" | "decl_export"
            | "decl_use" | "stmt_source" | "overlay_use" | "overlay_hide" | "overlay_new"
            | "hide_env" | "hide_mod" | "env_var" | "redirection" | "attribute" => true,
            "assignment" => text(node, source).trim_start().starts_with("$env."),
            "ctrl_do" => {
                !(contains(node, "val_closure") || contains(node, "block"))
                    || text(node, source).contains("--env")
            }
            _ => false,
        },
        CommandShell::Unknown => true,
    }
}

fn invocation(
    node: Node<'_>,
    source: &str,
    shell: CommandShell,
) -> Result<Option<CommandInvocation>, ()> {
    let field = match shell {
        CommandShell::PowerShell => "command_name",
        CommandShell::Nushell => "head",
        _ => "name",
    };
    let mut cursor = node.walk();
    let name = node
        .children_by_field_name(field, &mut cursor)
        .find(|child| child.is_named())
        .ok_or(())?;
    if contains(name, "variable")
        || contains(name, "val_variable")
        || contains(name, "variable_ref")
        || contains(name, "command_substitution")
        || contains(name, "simple_expansion")
        || contains(name, "expansion")
        || contains(name, "command_name_expr")
        || contains(name, "arithmetic_expansion")
        || contains(name, "val_interpolated")
        || contains(name, "expr_parenthesized")
    {
        return Err(());
    }
    let name_text = text(name, source).trim();
    if shell == CommandShell::PowerShell
        && text(node, source).trim() == name_text
        && name_text.split_once("..").is_some_and(|(first, last)| {
            first.parse::<i64>().is_ok() && last.parse::<i64>().is_ok()
        })
    {
        return Ok(None);
    }
    let name_token = shell_token_list(name_text, shell).map_err(|_| ())?;
    let executable = name_token.join(" ").trim_start_matches('^').to_owned();
    if executable.is_empty() || executable.starts_with('$') {
        return Err(());
    }
    let folded = executable.to_ascii_lowercase();
    if matches!(
        folded.as_str(),
        "eval"
            | "source"
            | "source-env"
            | "."
            | "invoke-expression"
            | "iex"
            | "run-external"
            | "builtin"
            | "command"
            | "exec"
    ) {
        return Err(());
    }
    let block_kind = match shell {
        CommandShell::PowerShell => "script_block_expression",
        CommandShell::Nushell => "val_closure",
        _ => "__none__",
    };
    let argument_list = argument_nodes(node);
    let has_block = argument_list.iter().any(|child| child.kind() == block_kind);
    let structural = match shell {
        CommandShell::PowerShell => matches!(
            folded.as_str(),
            "foreach-object" | "foreach" | "%" | "where-object" | "where" | "?"
        ),
        CommandShell::Nushell => matches!(
            folded.as_str(),
            "each" | "par-each" | "where" | "filter" | "reduce" | "any" | "all" | "do"
        ),
        _ => false,
    };
    if structural {
        if !has_block {
            return Err(());
        }
        if argument_list.iter().any(|child| {
            matches!(
                child.kind(),
                "variable"
                    | "member_access"
                    | "string_literal"
                    | "generic_token"
                    | "integer_literal"
                    | "parenthesized_expression"
                    | "val_variable"
                    | "val_interpolated"
                    | "expr_parenthesized"
            )
        }) {
            return Err(());
        }
        if shell == CommandShell::PowerShell {
            for child in &argument_list {
                if child.kind() == "command_parameter"
                    && !matches!(
                        text(*child, source).to_ascii_lowercase().as_str(),
                        "-begin" | "-process" | "-end" | "-filterscript"
                    )
                {
                    return Err(());
                }
            }
        }
        if shell == CommandShell::Nushell && folded == "do" && text(node, source).contains("--env")
        {
            return Err(());
        }
    }
    if has_block && !structural {
        return Err(());
    }
    let raw = text(node, source);
    let command = raw.trim().to_owned();
    let source_start = node.start_byte() + raw.len() - raw.trim_start().len();
    let source_range = source_start..source_start + command.len();
    let mut token_list = shell_token_list(&command, shell).map_err(|_| ())?;
    if token_list.is_empty() {
        return Err(());
    }
    let command_name = executable
        .rsplit(['/', '\\'])
        .next()
        .unwrap_or(&executable)
        .trim_end_matches(".exe")
        .to_owned();
    if name_token.len() == 1 {
        token_list[0] = command_name.clone();
    }
    Ok(Some(CommandInvocation {
        source: command,
        source_range,
        token_list,
        shell,
        command_name,
        structural,
    }))
}

#[cfg(test)]
#[path = "shell_tests.rs"]
mod tests;
