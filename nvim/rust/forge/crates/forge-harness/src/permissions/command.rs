//! Permission rule patterns derived from parsed shell commands.

pub use forge_shell::command::{
    CommandDisplay, CommandInvocation, CommandNormalization, command_token_list, display_command,
    normalize_command, normalize_command_in, shell_script_argument,
};
use forge_shell::shell::CommandShell;

/// PowerShell cmdlets that write file contents, which agents must replace with their file-editing tool.
const SHELL_FILE_WRITER_LIST: [&str; 3] = ["Set-Content", "Add-Content", "Out-File"];

/// Name the first shell file writer in a command so Harness can redirect the agent before approval.
///
/// Invocations recovered from partially analyzed commands still count, because a file writer is
/// rejected regardless of whether the rest of the command could be authorized.
pub fn shell_file_writer(command: &str, shell: CommandShell) -> Option<&'static str> {
    normalize_command_in(command, shell)
        .invocation_list
        .iter()
        .find_map(|invocation| {
            SHELL_FILE_WRITER_LIST
                .into_iter()
                .find(|writer| invocation.command_name.eq_ignore_ascii_case(writer))
        })
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

    /// Find file writers in pipelines, sequences, and Windows launchers without flagging reads.
    #[test]
    fn shell_file_writers_are_found_inside_pipelines_and_launchers() {
        for (command, writer) in [
            (
                "'text' | Set-Content -Encoding utf8 'D:/plan/docs/overview.md'",
                "Set-Content",
            ),
            ("Get-Date; add-content log.txt 'entry'", "Add-Content"),
            (
                r#""C:\Program Files\PowerShell\7\pwsh.exe" -NoProfile -Command "'x' | Out-File notes.md""#,
                "Out-File",
            ),
        ] {
            assert_eq!(
                shell_file_writer(command, CommandShell::PowerShell),
                Some(writer),
                "{command}"
            );
        }
        assert_eq!(
            shell_file_writer("Get-Content -Raw src/main.rs", CommandShell::PowerShell),
            None
        );
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
}
