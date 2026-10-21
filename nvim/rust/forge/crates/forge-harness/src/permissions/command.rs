use super::shell::{self, CommandAnalysisFailure, CommandShell};
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
/// Retains every resolved invocation and any failure preventing authorization.
pub struct CommandNormalization {
    /// Executable occurrences recovered before analysis completed or failed.
    pub invocation_list: Vec<CommandInvocation>,
    /// Failure that prevents using partial invocation results for authorization.
    pub failure: Option<CommandAnalysisFailure>,
}

/// Projects a command payload for presentation while retaining its original source coordinates.
pub struct CommandDisplay {
    pub source: String,
    pub shell: CommandShell,
    byte_range_list: Vec<Range<usize>>,
}

impl CommandDisplay {
    /// Summarizes literal file operations without changing execution or approval input.
    pub(crate) fn summary(&self) -> Option<String> {
        if self.source.chars().any(|character| {
            matches!(
                character,
                '\n' | '\r' | '|' | ';' | '&' | '<' | '>' | '$' | '`' | '(' | ')' | '{' | '}'
            )
        }) {
            return None;
        }
        let tokens = shell_tokens(&self.source, self.shell).ok()?;
        let command = tokens.first()?.text.as_str();
        let command = if self.shell == CommandShell::PowerShell {
            command.to_ascii_lowercase()
        } else {
            command.to_owned()
        };
        let arguments: Vec<&str> = tokens
            .iter()
            .skip(1)
            .map(|token| token.text.as_str())
            .collect();
        match command.as_str() {
            "get-content" | "cat" | "head" | "tail" => read_summary(&command, &arguments),
            "get-childitem" | "ls" => list_summary(&command, &arguments, self.shell),
            "select-string" | "grep" => search_summary(&command, &arguments),
            "open" if self.shell == CommandShell::Nushell => read_summary(&command, &arguments),
            "glob"
                if self.shell == CommandShell::Nushell
                    && arguments.len() == 1
                    && !arguments[0].starts_with('-') =>
            {
                Some(format!("Find files · {}", arguments[0]))
            }
            _ => None,
        }
    }

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

fn read_summary(command: &str, arguments: &[&str]) -> Option<String> {
    let mut path = None;
    let mut range = None;
    let mut raw = false;
    let mut arguments = arguments.iter().copied();
    while let Some(argument) = arguments.next() {
        let option = if command == "get-content" {
            argument.to_ascii_lowercase()
        } else {
            argument.to_owned()
        };
        match (command, option.as_str()) {
            ("get-content", "-path" | "-literalpath") => {
                if path.is_some() {
                    return None;
                }
                path = Some(arguments.next()?);
            }
            ("get-content", "-totalcount" | "-tail") | ("head" | "tail", "-n" | "--lines") => {
                if range.is_some() {
                    return None;
                }
                let count = arguments.next()?.parse::<u64>().ok()?;
                let position = if command == "tail" || argument.eq_ignore_ascii_case("-tail") {
                    "last"
                } else {
                    "first"
                };
                range = Some(format!("{position} {count} lines"));
            }
            ("get-content", "-raw") | ("open", "--raw" | "-r") => raw = true,
            _ if !argument.starts_with('-') && path.is_none() => path = Some(argument),
            _ => return None,
        }
    }
    let path = path.filter(|path| !path.is_empty())?;
    if raw && range.is_some() {
        return None;
    }
    let range = range.or_else(|| {
        matches!(command, "head" | "tail").then(|| {
            format!(
                "{} 10 lines",
                if command == "head" { "first" } else { "last" }
            )
        })
    });
    let suffix = range.map_or_else(
        || if raw { " · raw".into() } else { String::new() },
        |range| format!(" · {range}"),
    );
    Some(format!("Read · {path}{suffix}"))
}

fn list_summary(command: &str, arguments: &[&str], shell: CommandShell) -> Option<String> {
    let mut path = None;
    let mut all = false;
    let mut long = false;
    let mut recursive = false;
    let mut filter = None;
    let mut arguments = arguments.iter().copied();
    while let Some(argument) = arguments.next() {
        let option = if command == "get-childitem" {
            argument.to_ascii_lowercase()
        } else {
            argument.to_owned()
        };
        match (command, option.as_str()) {
            ("get-childitem", "-path" | "-literalpath") => {
                if path.is_some() {
                    return None;
                }
                path = Some(arguments.next()?);
            }
            ("get-childitem", "-recurse") => recursive = true,
            ("get-childitem", "-force") | ("ls", "-a" | "--all") => all = true,
            ("ls", "-l") if shell != CommandShell::Nushell => long = true,
            ("ls", "-la" | "-al") if shell != CommandShell::Nushell => {
                all = true;
                long = true;
            }
            ("get-childitem", "-filter") => {
                if filter.is_some() {
                    return None;
                }
                filter = Some(arguments.next()?);
            }
            _ if !argument.starts_with('-') && path.is_none() => path = Some(argument),
            _ => return None,
        }
    }
    let mut title = format!("List · {}", path.unwrap_or("."));
    if recursive {
        title.push_str(" · recursive");
    }
    if all {
        title.push_str(" · all");
    }
    if long {
        title.push_str(" · long");
    }
    if let Some(filter) = filter {
        title.push_str(&format!(" · {filter}"));
    }
    Some(title)
}

fn search_summary(command: &str, arguments: &[&str]) -> Option<String> {
    let mut path = None;
    let mut pattern = None;
    let mut line_numbers = false;
    let mut arguments = arguments.iter().copied();
    while let Some(argument) = arguments.next() {
        let option = if command == "select-string" {
            argument.to_ascii_lowercase()
        } else {
            argument.to_owned()
        };
        match (command, option.as_str()) {
            ("select-string", "-path" | "-literalpath") => {
                if path.is_some() {
                    return None;
                }
                path = Some(arguments.next()?);
            }
            ("select-string", "-pattern") => {
                if pattern.is_some() {
                    return None;
                }
                pattern = Some(arguments.next()?);
            }
            ("grep", "-n" | "--line-number") if pattern.is_none() => line_numbers = true,
            ("grep", _) if !argument.starts_with('-') && pattern.is_none() => {
                pattern = Some(argument)
            }
            ("grep", _) if !argument.starts_with('-') && path.is_none() => path = Some(argument),
            _ => return None,
        }
    }
    let label = if command == "grep" {
        "Search"
    } else {
        "Search files"
    };
    let pattern = serde_json::to_string(pattern?).ok()?;
    Some(format!(
        "{label} · {pattern} · {}{}",
        path?,
        if line_numbers { " · line numbers" } else { "" }
    ))
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
        if interpreter == CommandShell::Nushell
            && nushell_file_arguments(tokens.iter().skip(1).map(|token| token.text.as_str()))
        {
            break;
        }
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
        if quote.is_none() {
            let next = characters.peek().map(|(_, character)| *character);
            anyhow::ensure!(
                !(matches!(shell, CommandShell::Bash | CommandShell::Zsh)
                    && character == '$'
                    && matches!(next, Some('\'' | '"')))
                    && !(shell == CommandShell::PowerShell
                        && ((character == '@' && matches!(next, Some('\'' | '"')))
                            || matches!(character, '‘' | '’' | '“' | '”')))
                    && !(shell == CommandShell::Nushell
                        && !started
                        && character == 'r'
                        && next == Some('#')),
                "unsupported shell string encoding"
            );
        }
        if !started && !character.is_whitespace() {
            current.range.start = offset;
        }
        if let Some(active) = quote {
            if character == active {
                if shell == CommandShell::PowerShell
                    && characters
                        .peek()
                        .is_some_and(|(_, character)| *character == active)
                {
                    let (final_offset, final_character) = characters.next().unwrap();
                    current.append(character, offset..final_offset + final_character.len_utf8());
                } else {
                    quote = None;
                }
            } else if (shell == CommandShell::PowerShell && active == '"' && character == '`')
                || (matches!(shell, CommandShell::Bash | CommandShell::Zsh)
                    && active == '"'
                    && character == '\\'
                    && characters
                        .peek()
                        .is_some_and(|(_, next)| matches!(next, '"' | '\\' | '$' | '`' | '\n')))
                || (shell == CommandShell::Nushell && active == '"' && character == '\\')
            {
                let (decoded, end) = decode_shell_escape(&mut characters, shell, false)?;
                if let Some(decoded) = decoded {
                    current.append(decoded, offset..end);
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
            let (decoded, end) = decode_shell_escape(&mut characters, shell, !started)?;
            if let Some(decoded) = decoded {
                current.append(decoded, offset..end);
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

fn decode_shell_escape(
    characters: &mut std::iter::Peekable<std::str::CharIndices<'_>>,
    shell: CommandShell,
    at_word_boundary: bool,
) -> Result<(Option<char>, usize)> {
    let (offset, character) = characters
        .next()
        .ok_or_else(|| anyhow::anyhow!("unfinished shell escape"))?;
    let mut end = offset + character.len_utf8();
    if character == '\n'
        && (matches!(shell, CommandShell::Bash | CommandShell::Zsh)
            || (shell == CommandShell::PowerShell && at_word_boundary))
    {
        return Ok((None, end));
    }
    if shell == CommandShell::PowerShell
        && at_word_boundary
        && character == '\r'
        && characters
            .peek()
            .is_some_and(|(_, character)| *character == '\n')
    {
        let (offset, _) = characters.next().unwrap();
        return Ok((None, offset + 1));
    }
    if matches!(shell, CommandShell::Bash | CommandShell::Zsh) {
        return Ok((Some(character), end));
    }
    let decoded = match character {
        '0' => '\0',
        'a' => '\u{7}',
        'b' => '\u{8}',
        'e' => '\u{1b}',
        'f' => '\u{c}',
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        'v' if shell == CommandShell::PowerShell => '\u{b}',
        'u' => {
            anyhow::ensure!(
                characters
                    .next()
                    .is_some_and(|(_, character)| character == '{'),
                "invalid Unicode shell escape"
            );
            let mut digits = String::new();
            loop {
                let (offset, character) = characters
                    .next()
                    .ok_or_else(|| anyhow::anyhow!("unfinished Unicode shell escape"))?;
                end = offset + character.len_utf8();
                if character == '}' {
                    break;
                }
                anyhow::ensure!(
                    character.is_ascii_hexdigit() && digits.len() < 6,
                    "invalid Unicode shell escape"
                );
                digits.push(character);
            }
            u32::from_str_radix(&digits, 16)
                .ok()
                .and_then(char::from_u32)
                .ok_or_else(|| anyhow::anyhow!("invalid Unicode shell escape"))?
        }
        character
            if shell == CommandShell::PowerShell
                || matches!(
                    character,
                    '\\' | '"' | '\'' | '/' | '(' | ')' | '{' | '}' | '$' | '^' | '#' | '|' | '~'
                ) =>
        {
            character
        }
        _ => anyhow::bail!("invalid Nushell escape"),
    };
    Ok((Some(decoded), end))
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
            failure: Some(CommandAnalysisFailure::Incomplete(
                "nested shell analysis budget exhausted".into(),
            )),
            ..Default::default()
        };
    }
    if let Some(script) = shell_script(command, shell) {
        return normalize_script(script, depth, 0);
    }
    let parsed = shell::parse(command, shell);
    if parsed.failure.is_some() {
        return parsed;
    }
    let mut result = CommandNormalization::default();
    for invocation in parsed.invocation_list {
        let executable = invocation.token_list[0].to_ascii_lowercase();
        let nested_shell = CommandShell::from_executable(&executable);
        if nested_shell == CommandShell::Unknown
            || (nested_shell == CommandShell::Nushell
                && nushell_file_arguments(invocation.token_list.iter().skip(1).map(String::as_str)))
        {
            if matches!(executable.as_str(), "cmd" | "fish") {
                result.failure.get_or_insert_with(|| {
                    CommandAnalysisFailure::Unsupported(format!("shell {executable}"))
                });
            }
            result.invocation_list.push(invocation);
            continue;
        }
        let Some(script) = shell_script(&invocation.source, shell) else {
            result.failure.get_or_insert_with(|| {
                CommandAnalysisFailure::Unsupported("shell launcher arguments".into())
            });
            result.invocation_list.push(invocation);
            continue;
        };
        let nested = normalize_script(script, depth, invocation.source_range.start);
        if result.failure.is_none() {
            result.failure = nested.failure;
        }
        result.invocation_list.extend(nested.invocation_list);
    }
    result
}

fn nushell_file_arguments<'a>(arguments: impl Iterator<Item = &'a str>) -> bool {
    for argument in arguments {
        match argument {
            "-n" | "--no-config-file" | "--no-history" | "--no-std-lib" | "--stdin" | "-l"
            | "--login" => continue,
            _ => {
                return !argument.is_empty()
                    && !argument.starts_with('-')
                    && !argument.contains(['$', '`', '*', '?', '[', ']', '{', '}', '(', ')', '~']);
            }
        }
    }
    false
}

pub fn shell_script_argument(tokens: &[String]) -> Option<(usize, CommandShell)> {
    let shell = CommandShell::from_executable(tokens.first()?);
    if shell == CommandShell::Unknown
        || (shell == CommandShell::Nushell
            && nushell_file_arguments(tokens.iter().skip(1).map(String::as_str)))
    {
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
            result.failure.get_or_insert_with(|| {
                CommandAnalysisFailure::Incomplete("nested shell source mapping failed".into())
            });
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
    fn summarizes_simple_file_operations_in_each_shell() {
        for (shell, source, expected) in [
            (
                CommandShell::PowerShell,
                "Get-Content src/main.rs",
                "Read · src/main.rs",
            ),
            (
                CommandShell::PowerShell,
                "Get-Content -LiteralPath 'src/main file.rs' -TotalCount 40",
                "Read · src/main file.rs · first 40 lines",
            ),
            (
                CommandShell::PowerShell,
                "Get-Content src/main.rs -Tail 20",
                "Read · src/main.rs · last 20 lines",
            ),
            (CommandShell::PowerShell, "Get-ChildItem src", "List · src"),
            (
                CommandShell::PowerShell,
                "Get-ChildItem src -Recurse -Filter *.rs",
                "List · src · recursive · *.rs",
            ),
            (
                CommandShell::PowerShell,
                "Select-String -Path src/*.rs -Pattern TODO",
                "Search files · \"TODO\" · src/*.rs",
            ),
            (
                CommandShell::PowerShell,
                "GET-CONTENT -PATH src/main.rs -TOTALCOUNT 40",
                "Read · src/main.rs · first 40 lines",
            ),
            (CommandShell::Bash, "cat src/main.rs", "Read · src/main.rs"),
            (
                CommandShell::Bash,
                "head -n 40 src/main.rs",
                "Read · src/main.rs · first 40 lines",
            ),
            (
                CommandShell::Bash,
                "tail -n 20 src/main.rs",
                "Read · src/main.rs · last 20 lines",
            ),
            (CommandShell::Bash, "ls -la src", "List · src · all · long"),
            (
                CommandShell::Bash,
                "grep -n TODO src/main.rs",
                "Search · \"TODO\" · src/main.rs · line numbers",
            ),
            (
                CommandShell::Nushell,
                "open --raw src/main.rs",
                "Read · src/main.rs · raw",
            ),
            (CommandShell::Nushell, "ls -a src", "List · src · all"),
            (
                CommandShell::Nushell,
                "glob 'src/**/*.rs'",
                "Find files · src/**/*.rs",
            ),
            (
                CommandShell::PowerShell,
                "bash -lc 'cat src/main.rs'",
                "Read · src/main.rs",
            ),
            (
                CommandShell::PowerShell,
                "nu -c 'open --raw src/main.rs'",
                "Read · src/main.rs · raw",
            ),
        ] {
            assert_eq!(
                display_command(source, shell).summary().as_deref(),
                Some(expected),
                "{source}"
            );
        }
    }

    #[test]
    fn command_summaries_preserve_unrecognized_or_compound_sources() {
        for (shell, source) in [
            (CommandShell::PowerShell, "rg -n TODO src"),
            (CommandShell::Bash, "rg -n TODO src"),
            (CommandShell::Nushell, "rg -n TODO src"),
            (
                CommandShell::PowerShell,
                "Get-Content src/main.rs; Remove-Item other.rs",
            ),
            (
                CommandShell::PowerShell,
                "Get-Content src/main.rs | Select-Object -First 5",
            ),
            (CommandShell::PowerShell, "Get-Content src/main.rs -Unknown"),
            (CommandShell::Bash, "cat src/main.rs && touch other.rs"),
            (CommandShell::Bash, "cat $(find src)"),
            (CommandShell::Bash, "cat src/main.rs > other.rs"),
            (CommandShell::Bash, "head -N 40 src/main.rs"),
            (CommandShell::Bash, "grep -n TODO src/main.rs other.rs"),
            (CommandShell::Bash, "grep -e TODO src/main.rs"),
            (CommandShell::Bash, "open src/main.rs"),
            (CommandShell::Nushell, "open --raw src/main.rs | lines"),
            (CommandShell::Nushell, "ls --unknown src"),
        ] {
            let display = display_command(source, shell);
            assert!(display.summary().is_none(), "{source}");
            assert_eq!(display.source, source);
        }
    }

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
            assert!(parsed.failure.is_none(), "{source}");
            for invocation in parsed.invocation_list {
                let range = display.project_range(&invocation.source_range).unwrap();
                assert_eq!(&display.source[range], invocation.source, "{source}");
            }
        }
    }

    #[test]
    fn preserves_nushell_script_file_invocations_and_their_arguments() {
        for shell in [
            CommandShell::Bash,
            CommandShell::Zsh,
            CommandShell::PowerShell,
            CommandShell::Nushell,
        ] {
            for source in [
                "nu --no-config-file .forge/checks/lua-tests.nu all --all --timeout 30sec",
                "nu --no-config-file --stdin .forge/checks/lua-tests.nu affected --timeout 30sec",
                "nu -n 'checks with spaces.nu' --commands 'script argument'",
                "nu checks.nu -c 'script argument'",
                "nu --login --no-history --no-std-lib checks.nu",
            ] {
                let parsed = normalize_command_in(source, shell);
                assert_eq!(parsed.failure, None, "{shell:?}: {source}");
                assert_eq!(parsed.invocation_list.len(), 1, "{shell:?}: {source}");
                let invocation = &parsed.invocation_list[0];
                assert_eq!(invocation.source, source);
                assert_eq!(invocation.source_range, 0..source.len());
                assert_eq!(invocation.token_list[0], "nu");
                assert!(!invocation.structural);
                assert_eq!(display_command(source, shell).source, source);
                assert!(shell_script_argument(&invocation.token_list).is_none());
            }
        }
    }

    #[test]
    fn retains_pipeline_and_nested_launcher_targets_around_nushell_script_files() {
        for source in [
            "nu --no-config-file checks.nu | git status",
            "bash -c 'nu --no-config-file checks.nu | git status'",
        ] {
            let parsed = normalize_command_in(source, CommandShell::Zsh);
            assert_eq!(parsed.failure, None, "{source}");
            assert_eq!(parsed.invocation_list.len(), 2);
            for (invocation, expected) in parsed
                .invocation_list
                .iter()
                .zip(["nu --no-config-file checks.nu", "git status"])
            {
                assert_eq!(invocation.source, expected);
                assert_eq!(&source[invocation.source_range.clone()], expected);
            }
        }
    }

    #[test]
    fn unsupported_nushell_launchers_do_not_become_script_file_targets() {
        for source in [
            "nu --no-config-file",
            "nu --commands",
            "nu --unknown checks.nu",
            "nu --commands 'git status' extra",
            "nu --execute 'git status'",
            "nu $launcher",
            "nu \"$launcher\"",
            "nu *.nu",
        ] {
            let parsed = normalize_command_in(source, CommandShell::Zsh);
            assert!(parsed.failure.is_some(), "{source}");
        }
        let inline = normalize_command_in("nu --no-config-file -c 'git status'", CommandShell::Zsh);
        assert_eq!(inline.failure, None);
        assert_eq!(inline.invocation_list.len(), 1);
        assert_eq!(inline.invocation_list[0].token_list, ["git", "status"]);
    }

    #[test]
    fn splits_compounds_without_splitting_quoted_separators() {
        let result = normalize_command("git status && rg \"a|b\" src | Select-Object -First 1");
        assert!(result.failure.is_none());
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
        assert!(original.failure.is_none());
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
        assert!(result.failure.is_none());
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
        assert!(result.failure.is_none());
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
        assert!(nested.failure.is_none());
        assert!(nested.invocation_list.len() >= 3);
        assert!(
            normalize_command_in(
                "ForEach-Object { Get-ChildItem; Write-Output 'done'",
                CommandShell::PowerShell,
            )
            .failure
            .is_some()
        );
        assert!(
            normalize_command_in("tool (one]", CommandShell::PowerShell)
                .failure
                .is_some()
        );
    }
}
