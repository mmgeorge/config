use super::shell::{self, CommandShell};
use anyhow::Result;
use std::ops::Range;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CommandInvocation {
    pub shell: CommandShell,
    pub command_name: String,
    pub structural: bool,
    pub source: String,
    /// Identifies this invocation in the original input, including encoded launcher arguments.
    pub source_range: Range<usize>,
    pub token_list: Vec<String>,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct CommandNormalization {
    pub invocation_list: Vec<CommandInvocation>,
    pub ambiguous: bool,
}

/// Projects a command payload for presentation while retaining its original source coordinates.
pub struct CommandDisplay {
    pub source: String,
    pub shell: CommandShell,
    byte_range_list: Vec<Range<usize>>,
}

impl CommandDisplay {
    /// Maps an executable occurrence through removed launchers and decoded argument quoting.
    pub fn project_range(&self, range: &Range<usize>) -> Option<Range<usize>> {
        let mut matched = self
            .byte_range_list
            .iter()
            .enumerate()
            .filter(|(_, original)| original.start < range.end && original.end > range.start);
        let first = matched.next()?.0;
        Some(first..matched.last().map_or(first, |(index, _)| index) + 1)
    }
}

/// Removes shell launchers from display text without changing permission or execution input.
pub fn display_command(command: &str, shell: CommandShell) -> CommandDisplay {
    let mut display = CommandDisplay {
        source: command.to_owned(),
        shell,
        byte_range_list: (0..command.len()).map(|index| index..index + 1).collect(),
    };
    for _ in 0..16 {
        let Ok(mut tokens) = shell_tokens(&display.source, display.shell) else {
            break;
        };
        let Some(first) = tokens.first() else {
            break;
        };
        let executable = first
            .text
            .rsplit(['/', '\\'])
            .next()
            .unwrap_or(&first.text)
            .to_ascii_lowercase();
        let interpreter = CommandShell::from_executable(&executable);
        let fish = executable.trim_end_matches(".exe") == "fish";
        let cmd = executable.trim_end_matches(".exe") == "cmd";
        if interpreter == CommandShell::Unknown && !fish && !cmd {
            break;
        }
        let Some(marker) = tokens
            .iter()
            .enumerate()
            .skip(1)
            .find_map(|(index, token)| {
                let argument = token.text.to_ascii_lowercase();
                let command_switch = match interpreter {
                    CommandShell::PowerShell => {
                        matches!(argument.as_str(), "-command" | "-c" | "-file" | "-f")
                    }
                    CommandShell::Nushell => matches!(argument.as_str(), "-c" | "--commands"),
                    _ if cmd => matches!(argument.as_str(), "/c" | "/k"),
                    _ => matches!(argument.as_str(), "-c" | "-lc" | "-ic" | "-lic"),
                };
                command_switch.then_some(index)
            })
        else {
            break;
        };
        let Some(payload) = tokens.get(marker + 1) else {
            break;
        };
        let (source, mapping) = if marker + 2 == tokens.len() {
            let payload = tokens.swap_remove(marker + 1);
            (payload.text, payload.byte_range_list)
        } else {
            let range = payload.range.start..display.source.trim_end().len();
            (
                display.source[range.clone()].to_owned(),
                range.map(|index| index..index + 1).collect(),
            )
        };
        let mapping = mapping
            .into_iter()
            .map(|range| {
                display.byte_range_list[range.start].start
                    ..display.byte_range_list[range.end - 1].end
            })
            .collect();
        display = CommandDisplay {
            source,
            shell: if fish {
                CommandShell::Bash
            } else {
                interpreter
            },
            byte_range_list: mapping,
        };
    }
    display
}

pub fn command_token_list(command: &str) -> Result<Vec<String>> {
    let mut result = Vec::new();
    let mut current = String::new();
    let mut quote = None;
    let mut started = false;
    let mut character_stream = command.chars().peekable();
    while let Some(character) = character_stream.next() {
        if let Some(active_quote) = quote {
            if character == active_quote {
                quote = None;
            } else if character == '\\'
                && active_quote == '"'
                && character_stream
                    .peek()
                    .is_some_and(|next| matches!(next, '"' | '\\'))
            {
                current.push(character_stream.next().unwrap());
            } else {
                current.push(character);
            }
            continue;
        }
        if character == '\'' || character == '"' {
            quote = Some(character);
            started = true;
        } else if character.is_whitespace() {
            if started {
                result.push(std::mem::take(&mut current));
                started = false;
            }
        } else {
            current.push(character);
            started = true;
        }
    }
    anyhow::ensure!(quote.is_none(), "ambiguous command quoting");
    if started {
        result.push(current);
    }
    Ok(result)
}

#[derive(Default)]
struct ShellToken {
    text: String,
    range: Range<usize>,
    byte_range_list: Vec<Range<usize>>,
}

impl ShellToken {
    fn append(&mut self, character: char, range: Range<usize>) {
        self.text.push(character);
        self.byte_range_list
            .extend(std::iter::repeat_n(range, character.len_utf8()));
    }
}

struct ShellScript {
    source: String,
    shell: CommandShell,
    byte_range_list: Vec<Range<usize>>,
}

pub(super) fn shell_token_list(command: &str, shell: CommandShell) -> Result<Vec<String>> {
    Ok(shell_tokens(command, shell)?
        .into_iter()
        .map(|token| token.text)
        .collect())
}

fn shell_tokens(command: &str, shell: CommandShell) -> Result<Vec<ShellToken>> {
    let mut result = Vec::new();
    let mut current = ShellToken::default();
    let mut quote = None;
    let mut started = false;
    let mut characters = command.char_indices().peekable();
    while let Some((offset, character)) = characters.next() {
        if !started && !character.is_whitespace() {
            current.range.start = offset;
        }
        if let Some(active) = quote {
            if character == active {
                if shell == CommandShell::PowerShell
                    && active == '\''
                    && characters
                        .peek()
                        .is_some_and(|(_, character)| *character == '\'')
                {
                    let (final_offset, final_character) = characters.next().unwrap();
                    current.append(character, offset..final_offset + final_character.len_utf8());
                } else {
                    quote = None;
                }
            } else if (shell == CommandShell::PowerShell && active == '"' && character == '`')
                || (shell != CommandShell::PowerShell
                    && active == '"'
                    && character == '\\'
                    && characters
                        .peek()
                        .is_some_and(|(_, next)| matches!(next, '"' | '\\' | '$' | '`')))
            {
                if let Some((final_offset, next)) = characters.next() {
                    current.append(next, offset..final_offset + next.len_utf8());
                }
            } else {
                current.append(character, offset..offset + character.len_utf8());
            }
        } else if character == '\''
            || character == '"'
            || (shell == CommandShell::Nushell && character == '`')
        {
            quote = Some(character);
            started = true;
        } else if (shell == CommandShell::PowerShell && character == '`')
            || (matches!(shell, CommandShell::Bash | CommandShell::Zsh) && character == '\\')
        {
            if let Some((final_offset, next)) = characters.next() {
                current.append(next, offset..final_offset + next.len_utf8());
                started = true;
            }
        } else if character.is_whitespace() {
            if started {
                current.range.end = offset;
                result.push(std::mem::take(&mut current));
                started = false;
            }
        } else {
            current.append(character, offset..offset + character.len_utf8());
            started = true;
        }
    }
    anyhow::ensure!(quote.is_none(), "ambiguous command quoting");
    if started {
        current.range.end = command.len();
        result.push(current);
    }
    Ok(result)
}

pub fn normalize_command(command: &str) -> CommandNormalization {
    normalize_command_in(command, CommandShell::default())
}

pub fn normalize_command_in(command: &str, shell: CommandShell) -> CommandNormalization {
    normalize_nested(command, shell, 0)
}

fn normalize_nested(command: &str, shell: CommandShell, depth: usize) -> CommandNormalization {
    if depth >= 16 {
        return CommandNormalization {
            ambiguous: true,
            ..Default::default()
        };
    }
    if let Some(script) = shell_script(command, shell) {
        return normalize_script(script, depth, 0);
    }
    let parsed = shell::parse(command, shell);
    if parsed.ambiguous {
        return parsed;
    }
    let mut result = CommandNormalization::default();
    for invocation in parsed.invocation_list {
        let executable = invocation.token_list[0].to_ascii_lowercase();
        let nested_shell = CommandShell::from_executable(&executable);
        if nested_shell == CommandShell::Unknown {
            if matches!(executable.as_str(), "cmd" | "fish") {
                result.ambiguous = true;
            }
            result.invocation_list.push(invocation);
            continue;
        }
        let Some(script) = shell_script(&invocation.source, shell) else {
            result.ambiguous = true;
            result.invocation_list.push(invocation);
            continue;
        };
        let nested = normalize_script(script, depth, invocation.source_range.start);
        result.ambiguous |= nested.ambiguous;
        result.invocation_list.extend(nested.invocation_list);
    }
    result
}

pub fn shell_script_argument(tokens: &[String]) -> Option<(usize, CommandShell)> {
    let shell = CommandShell::from_executable(tokens.first()?);
    if shell == CommandShell::Unknown {
        return None;
    }
    let marker = tokens.iter().position(|token| match shell {
        CommandShell::PowerShell => {
            token.eq_ignore_ascii_case("-command") || token.eq_ignore_ascii_case("-c")
        }
        CommandShell::Nushell => matches!(token.as_str(), "-c" | "--commands"),
        _ => matches!(token.as_str(), "-c" | "-lc" | "-ic" | "-lic"),
    })?;
    if marker + 2 != tokens.len()
        || !tokens[1..marker].iter().all(|token| {
            matches!(
                token.to_ascii_lowercase().as_str(),
                "-noprofile"
                    | "-nologo"
                    | "-noninteractive"
                    | "--no-config-file"
                    | "--no-history"
                    | "--no-std-lib"
                    | "-l"
            )
        })
    {
        return None;
    }
    Some((marker, shell))
}

fn shell_script(source: &str, input_shell: CommandShell) -> Option<ShellScript> {
    let mut tokens = shell_tokens(source, input_shell).ok()?;
    let token_text = tokens
        .iter()
        .map(|token| token.text.clone())
        .collect::<Vec<_>>();
    let (marker, shell) = shell_script_argument(&token_text)?;
    let script = tokens.swap_remove(marker + 1);
    let argument = &source[script.range.clone()];
    if !argument.starts_with('\'') && (argument.contains('$') || argument.contains('`')) {
        return None;
    }
    Some(ShellScript {
        source: script.text,
        shell,
        byte_range_list: script.byte_range_list,
    })
}

fn normalize_script(
    script: ShellScript,
    depth: usize,
    source_offset: usize,
) -> CommandNormalization {
    let mut result = normalize_nested(&script.source, script.shell, depth + 1);
    for invocation in &mut result.invocation_list {
        if let Some((first, last)) = script
            .byte_range_list
            .get(invocation.source_range.start)
            .zip(
                invocation
                    .source_range
                    .end
                    .checked_sub(1)
                    .and_then(|index| script.byte_range_list.get(index)),
            )
        {
            invocation.source_range = source_offset + first.start..source_offset + last.end;
        } else {
            result.ambiguous = true;
        }
    }
    result
}

pub fn broad_command_pattern(invocation: &CommandInvocation) -> Option<String> {
    (!invocation.command_name.is_empty()).then(|| format!("{} *", invocation.command_name))
}

pub fn exact_command_pattern(invocation: &CommandInvocation) -> Option<String> {
    (!invocation.token_list.is_empty()).then(|| {
        invocation
            .token_list
            .iter()
            .map(|token| {
                if token.is_empty()
                    || token.chars().any(|character| {
                        character.is_whitespace()
                            || matches!(character, ';' | '|' | '&' | '\'' | '"')
                    })
                {
                    format!("\"{}\"", token.replace('\\', "\\\\").replace('"', "\\\""))
                } else {
                    token.clone()
                }
            })
            .collect::<Vec<_>>()
            .join(" ")
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn shell_launcher_display_matches_cross_platform_fixtures() {
        #[derive(serde::Deserialize)]
        struct Fixture {
            shell: CommandShell,
            source: String,
            expected: String,
        }
        let fixture_list: Vec<Fixture> = serde_json::from_str(include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../../../tests/forge/fixtures/command_display.json"
        )))
        .unwrap();
        for fixture in fixture_list {
            let display = display_command(&fixture.source, fixture.shell);
            assert_eq!(
                display.source, fixture.expected,
                "{:?}: {}",
                fixture.shell, fixture.source
            );
        }
    }

    #[test]
    fn display_coordinates_retain_executable_occurrences_through_nested_shells() {
        for (shell, source) in [
            (
                CommandShell::PowerShell,
                "pwsh -Command 'probe ''it''''s data''; probe λ'",
            ),
            (
                CommandShell::Bash,
                r#"bash -c 'pwsh -Command "probe λ; probe two"'"#,
            ),
            (CommandShell::Zsh, "zsh -c 'probe λ; probe two'"),
            (CommandShell::Nushell, "nu -c 'probe λ; probe two'"),
        ] {
            let display = display_command(source, shell);
            let parsed = normalize_command_in(source, shell);
            assert!(!parsed.ambiguous, "{source}");
            for invocation in parsed.invocation_list {
                let range = display.project_range(&invocation.source_range).unwrap();
                assert_eq!(&display.source[range], invocation.source, "{source}");
            }
        }
    }

    #[test]
    fn splits_compounds_without_splitting_quoted_separators() {
        let result = normalize_command("git status && rg \"a|b\" src | Select-Object -First 1");
        assert!(!result.ambiguous);
        assert_eq!(result.invocation_list.len(), 3);
        assert_eq!(result.invocation_list[0].token_list[0], "git");
        assert_eq!(result.invocation_list[2].token_list[0], "Select-Object");
    }

    #[test]
    fn approval_patterns_preserve_quoted_command_boundaries() {
        let original = normalize_command_in(
            r#"tool "a;b" "a|b" "a&b" "a'b" "a\"b" """#,
            CommandShell::Bash,
        );
        assert!(!original.ambiguous);
        assert_eq!(original.invocation_list.len(), 1);
        let pattern = exact_command_pattern(&original.invocation_list[0]).unwrap();
        assert_eq!(
            command_token_list(&pattern).unwrap(),
            original.invocation_list[0].token_list
        );
    }

    #[test]
    fn unwraps_windows_shell_launchers_without_losing_path_separators() {
        let result = normalize_command(
            r#""C:\Program Files\PowerShell\7\pwsh.exe" -NoProfile -Command "git commit -m test""#,
        );
        assert!(!result.ambiguous);
        assert_eq!(
            result.invocation_list,
            vec![CommandInvocation {
                shell: CommandShell::PowerShell,
                command_name: "git".into(),
                structural: false,
                source: "git commit -m test".into(),
                source_range: 62..80,
                token_list: vec!["git".into(), "commit".into(), "-m".into(), "test".into()],
            }]
        );
    }

    #[test]
    fn preserves_complete_script_blocks_and_original_quotes() {
        let block = r#"ForEach-Object { Get-ChildItem "$_.FullName" -Directory -Filter "bevy*0.19.1"; Write-Output 'done' }"#;
        let result = normalize_command_in(
            &format!("Get-ChildItem registry -Directory | {block}"),
            CommandShell::PowerShell,
        );
        assert!(!result.ambiguous);
        assert_eq!(result.invocation_list.len(), 4);
        assert_eq!(result.invocation_list[1].source, block);
        assert!(result.invocation_list[1].structural);
        assert_eq!(
            result.invocation_list[2].source,
            r#"Get-ChildItem "$_.FullName" -Directory -Filter "bevy*0.19.1""#
        );
        let nested = normalize_command_in(
            "Write-Output @(Get-Item one; Get-Item two) @{ name = 'x'; value = @(1, 2) }",
            CommandShell::PowerShell,
        );
        assert!(!nested.ambiguous);
        assert!(nested.invocation_list.len() >= 3);
        assert!(normalize_command("ForEach-Object { Get-ChildItem; Write-Output 'done'").ambiguous);
        assert!(normalize_command("tool (one]").ambiguous);
    }
}
