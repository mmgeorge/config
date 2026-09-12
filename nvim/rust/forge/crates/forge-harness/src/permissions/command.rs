use anyhow::Result;
use std::path::Path;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CommandInvocation {
    pub token_list: Vec<String>,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct CommandNormalization {
    pub invocation_list: Vec<CommandInvocation>,
    pub ambiguous: bool,
}

pub fn command_token_list(command: &str) -> Result<Vec<String>> {
    let mut result = Vec::new();
    let mut current = String::new();
    let mut quote = None;
    let mut character_stream = command.chars().peekable();
    while let Some(character) = character_stream.next() {
        if let Some(active_quote) = quote {
            if character == active_quote {
                quote = None;
            } else if character == '\\'
                && active_quote == '"'
                && character_stream.peek() == Some(&'"')
            {
                current.push('"');
                character_stream.next();
            } else {
                current.push(character);
            }
            continue;
        }
        if character == '\'' || character == '"' {
            quote = Some(character);
        } else if character.is_whitespace() {
            if !current.is_empty() {
                result.push(std::mem::take(&mut current));
            }
        } else {
            current.push(character);
        }
    }
    anyhow::ensure!(quote.is_none(), "ambiguous command quoting");
    if !current.is_empty() {
        result.push(current);
    }
    Ok(result)
}

fn split_compound_command(command: &str) -> Result<Vec<String>> {
    let mut result = Vec::new();
    let mut current = String::new();
    let mut quote = None;
    let mut escaped = false;
    let character_list = command.chars().collect::<Vec<_>>();
    let mut index = 0;
    while index < character_list.len() {
        let character = character_list[index];
        if escaped {
            current.push(character);
            escaped = false;
            index += 1;
            continue;
        }
        if character == '\\' && quote.is_some() {
            current.push(character);
            escaped = true;
            index += 1;
            continue;
        }
        if let Some(active_quote) = quote {
            current.push(character);
            if character == active_quote {
                quote = None;
            }
            index += 1;
            continue;
        }
        if character == '\'' || character == '"' {
            quote = Some(character);
            current.push(character);
            index += 1;
            continue;
        }
        let is_separator =
            character == ';' || character == '\n' || character == '|' || character == '&';
        if is_separator {
            let value = current.trim();
            if !value.is_empty() {
                result.push(value.to_owned());
            }
            current.clear();
            if index + 1 < character_list.len() && character_list[index + 1] == character {
                index += 1;
            }
        } else {
            current.push(character);
        }
        index += 1;
    }
    anyhow::ensure!(quote.is_none(), "ambiguous command quoting");
    let value = current.trim();
    if !value.is_empty() {
        result.push(value.to_owned());
    }
    Ok(result)
}

fn normalize_executable(value: &str) -> String {
    let name = Path::new(value)
        .file_name()
        .and_then(|value| value.to_str())
        .unwrap_or(value);
    name.strip_suffix(".exe").unwrap_or(name).to_owned()
}

fn unwrap_shell_invocation(token_list: &[String]) -> Option<String> {
    let executable = token_list.first()?.to_ascii_lowercase();
    let marker_list: &[&str] = match executable.as_str() {
        "pwsh" | "powershell" | "powershell_ise" => &["-command", "-c"],
        "cmd" => &["/c", "-c"],
        "sh" | "bash" | "zsh" | "fish" => &["-c"],
        _ => return None,
    };
    let command_index = token_list.iter().position(|token| {
        marker_list
            .iter()
            .any(|marker| token.eq_ignore_ascii_case(marker))
    })? + 1;
    (command_index < token_list.len()).then(|| token_list[command_index..].join(" "))
}

pub fn normalize_command(command: &str) -> CommandNormalization {
    if command.contains("$(") || command.contains("`(") {
        return CommandNormalization {
            ambiguous: true,
            ..Default::default()
        };
    }
    let Ok(command_list) = split_compound_command(command) else {
        return CommandNormalization {
            ambiguous: true,
            ..Default::default()
        };
    };
    let mut invocation_list = Vec::new();
    for command in command_list {
        let Ok(mut token_list) = command_token_list(&command) else {
            return CommandNormalization {
                invocation_list,
                ambiguous: true,
            };
        };
        if token_list.is_empty() {
            continue;
        }
        token_list[0] = normalize_executable(&token_list[0]);
        if let Some(nested_command) = unwrap_shell_invocation(&token_list) {
            let nested = normalize_command(&nested_command);
            invocation_list.extend(nested.invocation_list);
            if nested.ambiguous {
                return CommandNormalization {
                    invocation_list,
                    ambiguous: true,
                };
            }
            continue;
        }
        invocation_list.push(CommandInvocation { token_list });
    }
    CommandNormalization {
        invocation_list,
        ambiguous: false,
    }
}

pub fn broad_command_pattern(invocation: &CommandInvocation) -> Option<String> {
    invocation
        .token_list
        .first()
        .map(|command| format!("{command} *"))
}

pub fn exact_command_pattern(invocation: &CommandInvocation) -> Option<String> {
    (!invocation.token_list.is_empty()).then(|| {
        invocation
            .token_list
            .iter()
            .map(|token| {
                if token.chars().any(char::is_whitespace) {
                    format!("\"{}\"", token.replace('"', "\\\""))
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
    fn splits_compounds_without_splitting_quoted_separators() {
        let result = normalize_command("git status && rg \"a|b\" src | Select-Object -First 1");
        assert!(!result.ambiguous);
        assert_eq!(result.invocation_list.len(), 3);
        assert_eq!(result.invocation_list[0].token_list[0], "git");
        assert_eq!(result.invocation_list[2].token_list[0], "Select-Object");
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
                token_list: vec!["git".into(), "commit".into(), "-m".into(), "test".into()],
            }]
        );
    }
}
