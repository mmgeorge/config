//! Permission rule patterns derived from parsed shell commands.

pub use forge_shell::command::{
    CommandDisplay, CommandInvocation, CommandNormalization, command_token_list, display_command,
    normalize_command, normalize_command_in, shell_script_argument,
};

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
    use forge_shell::shell::CommandShell;

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
